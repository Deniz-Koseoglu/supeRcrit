library(shinyjs)

miscomp_server <- function(input, output, session, defaults, i18n, sfe_rv) {
  ns <- session$ns
  rv <- reactiveValues(
    miscomp_results = NULL,
    manual_solutes = list(), # List of solute vectors for manual entry
    selected_saved_calc_ids = character(0), # IDs of selected saved calculations
    saved_blends = list() #list of lists
  )

  # --- Solute Selection Logic ---

  # Render the list of saved calculations as checkboxes
  output$saved_calculations_list_ui <- renderUI({
    choices <- get_calculation_choices()
    if (length(choices) == 0 || (length(choices) == 1 && choices[[1]] == "")) {
      return(p(i18n$t("No saved GCM calculations found.")))
    }
    checkboxGroupInput(
      ns("saved_calculation_checkboxes"),
      label = i18n$t("Select saved calculations:"),
      choices = choices,
      selected = rv$selected_saved_calc_ids
    )
  })

  # Update selected_saved_calc_ids when checkboxes change
  observeEvent(input$saved_calculation_checkboxes, {
    rv$selected_saved_calc_ids <- input$saved_calculation_checkboxes
  })

  # Load selected calculations into manual solutes
  observeEvent(input$load_selected_calculations, {
    for (id in rv$selected_saved_calc_ids) {
      calc <- get_calculation_by_id(as.numeric(id))
      if (!is.null(calc)) {
        solute_id <- paste0("loaded_", id)
        rv$manual_solutes[[solute_id]] <- list(
          SMILES = calc$parameters$smiles_input,
          CAS = calc$parameters$cas_input,
          Name = calc$parameters$name_input
        )
      }
    }
    # Clear the selected checkboxes after loading
    rv$selected_saved_calc_ids <- character(0)
    updateCheckboxGroupInput(session, "saved_calculation_checkboxes", selected = character(0))
  })

  # Add manual solute
  observeEvent(input$add_manual_solute, {
    req(input$manual_smiles != "" || input$manual_cas != "" || input$manual_name != "")

    new_solute_id <- paste0("manual_", length(rv$manual_solutes) + 1)
    rv$manual_solutes[[new_solute_id]] <- list(
      SMILES = input$manual_smiles,
      CAS = input$manual_cas,
      Name = input$manual_name
    )

    # Clear inputs
    updateTextInput(session, "manual_smiles", value = "")
    updateTextInput(session, "manual_cas", value = "")
    updateTextInput(session, "manual_name", value = "")
  })

  # Render added solutes with remove buttons
  output$added_solutes_ui <- renderUI({
    if (length(rv$manual_solutes) == 0) {
      return(p(i18n$t("No solutes added yet.")))
    }

    items <- lapply(names(rv$manual_solutes), function(solute_id) {
      solute <- rv$manual_solutes[[solute_id]]
      fluidRow(
        column(8, p(strong(solute$Name), " (", solute$CAS, ")")),
        column(4, actionButton(ns(paste0("remove_manual_solute_", solute_id)), "×",
          class = "btn-xs btn-danger"
        ))
      )
    })

    tagList(items)
  })

  # Observe for removing manual solutes
  observe({
    if (length(rv$manual_solutes) > 0) {
      lapply(names(rv$manual_solutes), function(solute_id) {
        observeEvent(input[[paste0("remove_manual_solute_", solute_id)]], {
          rv$manual_solutes[[solute_id]] <- NULL
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    }
  })

  # Combine all selected solutes (from saved and manual entry)
  combined_solutes <- reactive({
    sols_list <- list()

    # Add solutes from saved calculations
    if (length(rv$selected_saved_calc_ids) > 0) {
      for (id in rv$selected_saved_calc_ids) {
        calc <- get_calculation_by_id(as.numeric(id))
        if (!is.null(calc)) {
          sols_list[[length(sols_list) + 1]] <- c(
            calc$parameters$smiles_input,
            calc$parameters$cas_input,
            calc$parameters$name_input
          )
        }
      }
    }

    # Add manually entered solutes
    if (length(rv$manual_solutes) > 0) {
      for (solute_id in names(rv$manual_solutes)) {
        solute <- rv$manual_solutes[[solute_id]]
        sols_list[[length(sols_list) + 1]] <- c(
          solute$SMILES,
          solute$CAS,
          solute$Name
        )
      }
    }
    sols_list
  })

  # --- Blend Management Logic (Copied from sfe_mod) ---
  # Global list to keep track of fraction observers for cleanup
  fraction_observers <- list()

  # 1. Add solvent to current blend
  observeEvent(input$add_to_blend, {
    req(input$blend_solvent_selector)
    solvent <- input$blend_solvent_selector

    # Check if already added
    if (solvent %in% names(rv$current_blend)) {
      showNotification(i18n$t("Solvent already in blend!"), type = "warning")
      return()
    }

    # Add with default fraction 0
    rv$current_blend[[solvent]] <- 0
    
    # Ensure the input for the newly added solvent is reset
    session$sendCustomMessage("resetInput", list(id = ns(paste0("frac_", solvent)), value = 0))
  })

  # 2. Render current blend UI (dynamic fraction inputs)
  output$current_blend_ui <- renderUI({
    if (length(rv$current_blend) == 0) {
      return(p(i18n$t("No solvents added yet.")))
    }

    # Create fraction inputs for each solvent
    inputs <- lapply(names(rv$current_blend), function(solvent) {
      fluidRow(
        column(6, p(strong(solvent))),
        column(4, numericInput(ns(paste0("frac_", solvent)), NULL,
          value = rv$current_blend[[solvent]],
          min = 0, step = 0.1, width = "80px"
        )),
        column(2, actionButton(ns(paste0("remove_", solvent)), "×",
          class = "btn-xs btn-danger"
        ))
      )
    })

    # Add total display
    total <- sum(sapply(rv$current_blend, function(x) x), na.rm = TRUE)
    total_color <- if (total == 100) "green" else if (total < 100) "orange" else "red"

    tagList(
      inputs,
      hr(),
      p(
        strong(i18n$t("Total:")),
        span(paste0(round(total, 1), "%"), style = paste0("color:", total_color, ";"))
      )
    )
  })

  # 3. Update fractions when changed (with observer cleanup)
  observe({
    current_solvents <- names(rv$current_blend)
    
    # Clean up old observers for solvents that are no longer in the blend
    old_solvents <- setdiff(names(fraction_observers), current_solvents)
    for (old_s in old_solvents) {
      if (!is.null(fraction_observers[[old_s]])) {
        fraction_observers[[old_s]]$destroy() # Destroy the observer
        fraction_observers[[old_s]] <- NULL
      }
    }
    
    # Create or update observers for current solvents
    for (solvent in current_solvents) {
      if (is.null(fraction_observers[[solvent]])) { # Only create if it doesn't exist
        local({
          s <- solvent
          fraction_observers[[s]] <<- observeEvent(input[[paste0("frac_", s)]], {
            val <- input[[paste0("frac_", s)]]
            if (!is.null(val) && !is.na(val)) {
              # Update rv$current_blend directly from input value
              rv$current_blend[[s]] <- as.numeric(val)
            }
          }, ignoreInit = TRUE, ignoreNULL = TRUE)
        })
      }
    }
  })

  # 4. Clear current blend
  observeEvent(input$clear_blend, {
    old_solvents <- names(rv$current_blend)
    
    # Clear reactive value
    rv$current_blend <- list()
    
    # Clear associated numeric inputs in UI
    for (s in old_solvents) {
      updateNumericInput(session, paste0("frac_", s), value = NULL)
      session$sendCustomMessage("resetInput", list(id = ns(paste0("frac_", s)), value = 0))
    }
  })

  # 5. Save blend
  observeEvent(input$save_blend, {
    req(length(rv$current_blend) >= 2)

    # Recalculate total based on current input values, not rv$current_blend directly
    # This ensures we use the most up-to-date values from the UI
    current_fractions <- sapply(names(rv$current_blend), function(s) {
      input_val <- input[[paste0("frac_", s)]]
      if (is.null(input_val) || is.na(input_val)) 0 else as.numeric(input_val)
    })
    total <- sum(current_fractions, na.rm = TRUE)

    if (total != 100) {
      showNotification(i18n$t("Total percentage must be exactly 100% to save the blend."), type = "error")
      return()
    }

    # Create blend object (deep copy) using the current_fractions
    blend <- list(
      solvents = as.character(names(rv$current_blend)),
      fractions = as.numeric(current_fractions),
      preview = paste0(
        paste(names(rv$current_blend), collapse = ":"),
        " (", paste(sprintf("%.1f", current_fractions), collapse = ":"), ")"
      )
    )

    # Add to saved blends
    rv$saved_blends[[length(rv$saved_blends) + 1]] <- blend

    # Reset fractions to 0 for new blend with same solvents
    for (s in names(rv$current_blend)) {
      rv$current_blend[[s]] <- 0
    }

    showNotification(i18n$t("Blend saved!"), type = "message")
  })

  # 6. Render saved blends
  output$saved_blends_ui <- renderUI({
    if (length(rv$saved_blends) == 0) {
      return(p(i18n$t("No blends saved yet.")))
    }

    blend_items <- lapply(seq_along(rv$saved_blends), function(i) {
      blend <- rv$saved_blends[[i]]
      fluidRow(
        column(9, p(paste0(i, ". ", blend$preview))),
        column(3, actionButton(ns(paste0("del_blend_", i)), "×",
          class = "btn-xs btn-danger",
          onclick = paste0("Shiny.setInputValue('", ns("last_deleted_blend_id"), "', 'del_blend_", i, "', {priority: 'event'})")
        ))
      )
    })

    tagList(
      blend_items,
      # Hidden input to capture the ID of the clicked delete button
      shinyjs::hidden(actionButton(ns("last_deleted_blend_id"), "", style = "display: none;"))
    )
  })

  # 7. Observe for removing solvents from current blend
  observe({
    req(length(rv$current_blend) > 0)
    for (solvent in names(rv$current_blend)) {
      local({
        s <- solvent
        observeEvent(input[[paste0("remove_", s)]], {
          # Clear the input value in the UI
          updateNumericInput(session, paste0("frac_", s), value = NULL)
          session$sendCustomMessage("resetInput", list(id = ns(paste0("frac_", s)), value = 0))
          
          # Remove from reactive value
          rv$current_blend[[s]] <- NULL
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    }
  })

  # 8. Observe for deleting saved blends (Revised approach)
  observeEvent(input$last_deleted_blend_id, {
    req(input$last_deleted_blend_id)
    
    deleted_idx_str <- sub("del_blend_", "", input$last_deleted_blend_id)
    deleted_idx <- as.numeric(deleted_idx_str)
    
    if (!is.na(deleted_idx) && deleted_idx > 0 && deleted_idx <= length(rv$saved_blends)) {
      isolate({
        rv$saved_blends <- rv$saved_blends[-deleted_idx]
      })
    }
    session$sendCustomMessage("resetInput", list(id = ns(input$last_deleted_blend_id), value = 0))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # --- Main miscomp execution logic ---
  observeEvent(input$run_miscomp, {
    # 1. Validate minimum solutes
    if (length(combined_solutes()) < 2) {
      showNotification(i18n$t("Please add at least 2 solutes for comparison."), type = "error")
      return(NULL)
    }

    # 2. Validate GCM methods
    if (input$gcm_tb == "none" && input$gcm_crit == "none" && input$gcm_hsp == "none") {
      showNotification(i18n$t("Please select at least one GCM method (Boiling Point, Critical Parameters, or HSP)."), type = "error")
      return(NULL)
    }

    # 3. Parse pressure and vfrac inputs
    pres_values <- as.numeric(strsplit(input$pres_input, ",")[[1]])
    pres_values <- pres_values[!is.na(pres_values)]
    if (length(pres_values) == 0) {
      showNotification(i18n$t("Please enter valid pressure values."), type = "error")
      return(NULL)
    }
    if (length(pres_values) > 6) {
      showNotification(i18n$t("Maximum 6 pressure values are allowed."), type = "error")
      return(NULL)
    }

    vfrac_values <- as.numeric(strsplit(input$vfrac_input, ",")[[1]])
    vfrac_values <- vfrac_values[!is.na(vfrac_values)]
    if (length(vfrac_values) == 0) {
      showNotification(i18n$t("Please enter valid volume fraction values."), type = "error")
      return(NULL)
    }

    # 4. Prepare modifier and modfracs
    modifier_param <- input$modifier_selection
    modfracs_param <- NA

    if (length(rv$saved_blends) > 0) {
      # If a blend is selected, use it. miscomp only takes one modifier/blend.
      # For simplicity, we'll assume the user selects either a single modifier OR a saved blend.
      # If multiple blends are saved, we'd need a way to select one.
      # For now, let's just take the first saved blend if no single modifier is selected.
      if (is.null(modifier_param) || modifier_param == "") {
        selected_blend <- rv$saved_blends[[1]] # Assuming only one blend can be active for miscomp
        modifier_param <- selected_blend$solvents
        modfracs_param <- selected_blend$fractions
      }
    }

    if (is.null(modifier_param) || modifier_param == "") {
      showNotification(i18n$t("Please select a co-solvent or define a blend."), type = "error")
      return(NULL)
    }

    # 5. Prepare colors
    cols_param <- "default"
    if (input$use_custom_colors) {
      custom_colors <- c(
        input$col_one, input$col_two, input$col_three,
        input$col_four, input$col_five, input$col_six
      )
      # Filter out NULLs and ensure valid colors
      custom_colors <- custom_colors[!sapply(custom_colors, is.null)]
      if (length(custom_colors) > 0) {
        cols_param <- setNames(custom_colors, c("one", "two", "three", "four", "five", "six")[1:length(custom_colors)])
      }
    }

    withProgress(message = i18n$t("Running Miscibility Comparison..."), {
      miscomp_output <- tryCatch(
        {
          miscomp(
            sols = combined_solutes(),
            tb = input$gcm_tb,
            crit = input$gcm_crit,
            hsp = input$gcm_hsp,
            modif = modifier_param,
            modfracs = modfracs_param,
            pres = pres_values,
            pres_comp = as.numeric(input$pres_comp),
            cols = cols_param,
            plt_title = input$plt_title,
            temp = input$temp,
            vfrac = vfrac_values,
            simplicity = input$gcm_simplicity,
            draw = FALSE, # Important for Shiny apps
            silent = TRUE
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during miscibility comparison:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(miscomp_output)) {
        rv$miscomp_results <- miscomp_output

        # Render data table
        output$miscibility_data_table <- DT::renderDataTable({
          req(rv$miscomp_results$data)
          DT::datatable(
            rv$miscomp_results$data,
            options = list(scrollX = TRUE, paging = FALSE),
            rownames = FALSE,
            filter = "top"
          )
        })

# Render dynamic plot tabs (plotly olarak güncellendi)
output$plots_tabs_ui <- renderUI({
  req(rv$miscomp_results$plots)
  plot_names <- names(rv$miscomp_results$plots)
  tabs <- lapply(plot_names, function(pname) {
    tabPanel(pname,
             plotlyOutput(ns(paste0("plot_", make.names(pname))), height = "600px"))  # plotOutput -> plotlyOutput
  })
  do.call(tabsetPanel, tabs)
})

# Render each plot (plotly olarak güncellendi)
observe({
  req(rv$miscomp_results$plots)
  plot_names <- names(rv$miscomp_results$plots)
  for (pname in plot_names) {
    local({
      plot_name <- pname
      output_id <- paste0("plot_", make.names(plot_name))
      output[[output_id]] <- renderPlotly({  # renderPlot -> renderPlotly
        rv$miscomp_results$plots[[plot_name]]
      })
    })
  }
})

# ... mevcut kod altında ...
      }
    })
  })

  # Update pres_comp choices based on entered pressures
  observe({
    pres_values <- as.numeric(strsplit(input$pres_input, ",")[[1]])
    pres_values <- pres_values[!is.na(pres_values)]
    if (length(pres_values) > 0) {
      updateSelectInput(session, "pres_comp",
        choices = pres_values,
        selected = pres_values[1]
      )
    }
  })

  # Intro Help Button Observers for SFE Misc Comp
  observeEvent(input$sfe_misc_comp_help, {
    introjs(session, options = intro_steps_sfe_misc_comp_load(ns, i18n))
  })

  observeEvent(input$sfe_misc_comp_manual_help, {
    introjs(session, options = intro_steps_sfe_misc_comp_manual(ns, i18n))
  })

  observeEvent(input$sfe_misc_comp_gcm_help, {
    introjs(session, options = intro_steps_sfe_misc_comp_gcm(ns, i18n))
  })

  observeEvent(input$sfe_misc_comp_blend_help, {
    introjs(session, options = intro_steps_sfe_misc_comp_cosolvent_blend(ns, i18n))
  })

  observeEvent(input$sfe_misc_comp_process_help, {
    introjs(session, options = intro_steps_sfe_misc_comp_process(ns, i18n))
  })

  observeEvent(input$sfe_misc_comp_plot_help, {
    introjs(session, options = intro_steps_sfe_misc_comp_plot(ns, i18n))
  })

}
