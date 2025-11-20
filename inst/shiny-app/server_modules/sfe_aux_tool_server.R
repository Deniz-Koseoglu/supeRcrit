auxiliary_tools_server <- function(input, output, session, defaults, i18n, sfe_rv) {
  ns <- session$ns

  # Reactive values for general solvent mixture (iscrit_gen)
  rv_gen_solvents <- reactiveVal(list("CO2", "Ethanol")) # Start with 2 solvents
  rv_gen_fractions <- reactiveVal(c(0.5, 0.5)) # Corresponding fractions

  # Display available solvents data
  output$show_solv_table <- DT::renderDataTable({
    suppressWarnings(solvents_data <- show_solv())
    DT::datatable(solvents_data, options = list(scrollX = TRUE, paging = FALSE))
  })

  # Display GCM method selection chart
  output$show_gcm_table <- DT::renderDataTable({
    suppressWarnings(gcm_data <- show_gcm())
    DT::datatable(gcm_data, options = list(scrollX = TRUE, paging = FALSE))
  })

  # Get all available solvents for dynamic dropdowns
  all_solvents <- reactive({
    suppressWarnings(show_solv()$Solvent)
  })

  # Dynamic UI for general solvent inputs
  output$gen_solvent_inputs <- renderUI({
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()
    num_solvents <- length(solvents)

    input_elements <- lapply(seq_len(num_solvents), function(i) {
      current_solvent <- solvents[[i]]
      current_fraction <- fractions[[i]]

      # Filter choices for dropdown: exclude already selected solvents
      available_choices <- setdiff(all_solvents(), solvents[-i])

      fluidRow(
        column(6,
               selectInput(ns(paste0("gen_solv_", i)), i18n$t(paste("Solvent", i)),
                           choices = available_choices, selected = current_solvent)
        ),
        column(4,
               numericInput(ns(paste0("gen_frac_", i)), i18n$t(paste("Fraction", i)),
                            value = current_fraction, min = 0, max = 1, step = 0.01)
        ),
        column(2,
               # Remove button for 3rd solvent only
               if (i == 3) {
                 actionButton(ns(paste0("remove_solv_", i)), "x", class = "btn-danger btn-xs")
               } else {
                 NULL
               }
        )
      )
    })

    # Add button for 3rd solvent if less than 3 solvents are selected
    if (num_solvents < 3) {
      input_elements <- c(input_elements, list(
        actionButton(ns("add_solvent"), i18n$t("Add Solvent"), class = "btn-success")
      ))
    }

    # Display total fraction
    total_fraction <- sum(fractions)
    total_color <- if (abs(total_fraction - 1) < 0.001) "green" else "red"

    input_elements <- c(input_elements, list(
      hr(),
      p(strong(i18n$t("Total Fraction:")), span(sprintf("%.2f", total_fraction), style = paste0("color:", total_color, ";")))
    ))

    tagList(input_elements)
  })

  # Add solvent button logic
  observeEvent(input$add_solvent, {
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()
    if (length(solvents) < 3) {
      # Find a solvent not already in the list
      available_solvents <- setdiff(all_solvents(), solvents)
      new_solvent <- if (length(available_solvents) > 0) available_solvents[1] else ""

      rv_gen_solvents(c(solvents, new_solvent))
      rv_gen_fractions(c(fractions, 0)) # Add with default fraction 0
    }
  })

  # Remove solvent button logic (for the 3rd solvent)
  observeEvent(input$remove_solv_3, {
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()
    if (length(solvents) == 3) {
      rv_gen_solvents(solvents[-3])
      rv_gen_fractions(fractions[-3])
    }
  })

  # Handle CO2-Ethanol Critical Parameters calculation
  observeEvent(input$calc_etoh_crit, {
    req(input$etoh_co2_frac, input$etoh_pres, input$etoh_temp, input$etoh_method)

    withProgress(message = i18n$t("Calculating CO2-Ethanol Critical Parameters..."), {
      results <- tryCatch({
        iscrit_etoh(
          fracs = input$etoh_co2_frac,
          pres = input$etoh_pres,
          temp = input$etoh_temp,
          units = "mol", # iscrit_etoh uses mol fraction
          method = input$etoh_method
        )
      }, error = function(e) {
        showNotification(paste(i18n$t("Error calculating CO2-Ethanol critical params:"), e$message), type = "error")
        return(NULL)
      })

      if (!is.null(results)) {
        output$iscrit_etoh_results <- renderUI({
          tagList(
            h5(i18n$t("Statement:"), results$statement),
            h5(i18n$t("Critical Temperature (K):"), results$results$Tc),
            h5(i18n$t("Critical Pressure (MPa):"), results$results$Pc)
          )
        })
      }
    })
  })

  # Handle General Solvent Mixtures Critical Parameters calculation
  observeEvent(input$calc_gen_crit, {
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()

    # Actual values from inputs at calculation time
    actual_solvents <- lapply(seq_along(solvents), function(i) {
      input[[paste0("gen_solv_", i)]]
    })

    actual_fractions <- as.numeric(lapply(seq_along(fractions), function(i) {
      val <- input[[paste0("gen_frac_", i)]]
      if (is.null(val)) 0 else val
    }))

    req(length(actual_solvents) >= 2, length(actual_solvents) <= 3) # Ensure 2 or 3 solvents
    req(all(!is.na(actual_solvents)), all(actual_solvents != "")) # Ensure solvents are selected
    req(all(!is.na(actual_fractions)), all(actual_fractions >= 0)) # Ensure fractions are valid numbers

    # Ensure fractions sum to 1 (or close to 1 due to floating point)
    if (abs(sum(actual_fractions) - 1) > 0.001) {
      showNotification(i18n$t("Fractions must sum to 1."), type = "error")
      return(NULL)
    }

    # Check for duplicate solvents
    if (length(unique(actual_solvents)) != length(actual_solvents)) {
      showNotification(i18n$t("Duplicate solvents are not allowed."), type = "error")
      return(NULL)
    }

    withProgress(message = i18n$t("Calculating General Critical Parameters..."), {
      results <- tryCatch({
        iscrit_gen(
          solv = unlist(actual_solvents),
          fracs = unlist(actual_fractions),
          pres = input$gen_pres,
          temp = input$gen_temp,
          units = input$gen_units,
          tc = input$gen_tc_method,
          pc = input$gen_pc_method
        )
      }, error = function(e) {
        showNotification(paste(i18n$t("Error calculating general critical params:"), e$message), type = "error")
        return(NULL)
      })

      if (!is.null(results)) {
        output$iscrit_gen_results <- renderUI({
          tagList(
            h5(i18n$t("Statement:"), results$statement),
            DT::renderDataTable({
              req(!is.null(results$results_df))
              DT::datatable(results$results_df, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
            })
          )
        })
      }
    })
  })

  # Handle iscrit_demo execution (integrated into etoh tab)
  observeEvent(input$run_etoh_demo, {
    withProgress(message = i18n$t("Running CO2-Ethanol Demo..."), {
      demo_results <- tryCatch({
        iscrit_demo()
      }, error = function(e) {
        showNotification(paste(i18n$t("Error running demo:"), e$message), type = "error")
        return(NULL)
      })

      if (!is.null(demo_results)) {
        output$iscrit_etoh_demo_results <- renderUI({
          tagList(
                h5(i18n$t("Demo Plots:")),
            renderPlot({
              # Assuming plotlist contains ggplot objects
         
              print(demo_results$plots[[1]]) # Tc plot
              print(demo_results$plots[[2]]) # Pc plot
            }),
            h5(i18n$t("Demo Data:")),
            DT::renderDataTable({
              DT::datatable(demo_results$data, options = list(scrollX = TRUE, paging = FALSE), rownames = FALSE)
            })
        
          )
        })
      }
    })
  })

  # Intro Help Button Observers for SFE Aux Tools
  observeEvent(input$sfe_aux_tools_etoh_help, {
    introjs(session, options = intro_steps_sfe_aux_tool_etoh(ns, i18n))
  })

  observeEvent(input$sfe_aux_tools_gen_help, {
    introjs(session, options = intro_steps_sfe_aux_tool_gen(ns, i18n))
  })
}
