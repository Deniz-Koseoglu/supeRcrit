# BIC Kinetic Modeling Server Module
kinetic_bic_server <- function(input, output, session, defaults, i18n) {
  # Load required libraries
  library(dplyr)
  library(DT)
  library(plotly)
  library(zip) # For zipping export files
  library(supeRcrit) # Assuming bicmod is part of supeRcrit

  # Helper for creating namespaced ids inside this module
  ns <- session$ns

 
  # Null coalescing operator
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Reactive values for storing results
  kinetic_results <- reactiveValues(
    full_result = NULL, 
    sim_result = NULL, 
    ct_result = NULL, 
    cmp_result = NULL, 
    plots_list = NULL, 
    data = NULL, 
    input_params = NULL, 
    available_models = c(),
    predict_result = NULL,  # predict_bic() sonuçları
    predict_data = NULL     # Input verisi
  )

  # Dynamic UI for data input (CSV upload or manual text input)
  output$data_input_ui <- renderUI({
    if (input$input_type == "csv") {
      fileInput(ns("file_upload"), i18n$t("Upload CSV File"),
        accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv")
      )
    } else {
      textAreaInput(ns("manual_data"), i18n$t("Enter Time and Yield Data (CSV format, max 10 rows)"),
        rows = 10, placeholder = "Time_min,Yield_g,Solvent_mL\n1,0.1,10\n2,0.18,20\n..."
      )
    }
  })

  # Reactive for default OEC variable selections
  default_oec_vars <- reactive({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 2) {
      return(list(x_var = NULL, y_var = NULL, slv_var = "None"))
    }

    slv_var_default <- "None"
    solvent_column <- grep("solv", colnames(data), ignore.case = TRUE, value = TRUE)
    if (length(solvent_column) > 0) {
      slv_var_default <- solvent_column[1]
    }

    list(
      x_var = colnames(data)[1],
      y_var = colnames(data)[2],
      slv_var = slv_var_default
    )
  })

  # Render dynamic UI for OEC variables
  output$oec_x_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 1) {
      return(NULL)
    }
    choices <- colnames(data)
    selected <- input$oec_x_var %||% default_oec_vars()$x_var
    selectInput(ns("oec_x_var"), i18n$t("Time Variable"), choices = choices, selected = selected)
  })

  output$oec_y_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data) || ncol(data) < 2) {
      return(NULL)
    }
    choices <- colnames(data)
    selected <- input$oec_y_var %||% default_oec_vars()$y_var
    selectInput(ns("oec_y_var"), i18n$t("Response Variable"), choices = choices, selected = selected)
  })

  output$oec_slv_var_ui <- renderUI({
    data <- oec_data()
    if (is.null(data)) {
      return(NULL)
    }
    choices <- c("None", colnames(data))
    selected <- input$oec_slv_var %||% default_oec_vars()$slv_var
    selectInput(ns("oec_slv_var"), i18n$t("Solvent Variable (Optional)"), choices = choices, selected = selected)
  })

  # Reactive for validated OEC data
  validated_oec_data <- reactive({
    data <- oec_data()
    req(data)

    # Use input values if available, otherwise use defaults
    x_var <- input$oec_x_var %||% default_oec_vars()$x_var
    y_var <- input$oec_y_var %||% default_oec_vars()$y_var
    slv_var <- input$oec_slv_var %||% default_oec_vars()$slv_var

    req(x_var, y_var) # Ensure x and y variables are selected

    if (!x_var %in% colnames(data) || !y_var %in% colnames(data)) {
      showNotification(i18n$t("Selected OEC variables not found in data."), type = "error")
      return(NULL)
    }
    if (!is.numeric(data[[x_var]]) || !is.numeric(data[[y_var]])) {
      showNotification(i18n$t("Time and response columns must be numeric."), type = "error")
      return(NULL)
    }
    data
  })

  # Reactive for parsed OEC data
  oec_data <- reactive({
    req(input$input_type)
    data <- NULL

    if (input$input_type == "csv") {
      req(input$file_upload)
      tryCatch(
        {
          data <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error loading CSV:"), e$message), type = "error")
          return(NULL)
        }
      )
    } else { # Manual input
      req(input$manual_data)
      if (nchar(input$manual_data) > 0) {
        tryCatch(
          {
            data <- read.csv(text = input$manual_data, stringsAsFactors = FALSE)
          },
          error = function(e) {
            showNotification(paste(i18n$t("Error parsing manual data:"), e$message), type = "error")
            return(NULL)
          }
        )
      }
    }
    data
  })

  # Calculate kinetic model
  observeEvent(input$calculate, {
    req(validated_oec_data())

    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Calculating BIC model..."), value = 0, {
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))

          # Prepare OEC variables, using input values or defaults
          x_var_val <- input$oec_x_var %||% default_oec_vars()$x_var
          y_var_val <- input$oec_y_var %||% default_oec_vars()$y_var
          slv_var_val <- input$oec_slv_var %||% default_oec_vars()$slv_var

          oec_vars_list <- c(x = x_var_val, y = y_var_val)
          if (!is.null(slv_var_val) && !is.na(slv_var_val) && slv_var_val != "None" && nchar(slv_var_val) > 0) {
            oec_vars_list <- c(oec_vars_list, slv = slv_var_val)
          }

          # Prepare parameters for bicmod
          pars_list <- c(
            pres = input$pres,
            temp = input$temp,
            mass_in = input$mass_in,
            moisture = input$moisture,
            D = input$D,
            L = input$L,
            etoh = input$etoh,
            dr = input$dr,
            dp = input$dp,
            n = input$n,
            flow = input$flow
          )

          # Add cu parameter if provided (not NA)
          if (!is.null(input$cu) && !is.na(input$cu)) {
            pars_list <- c(pars_list, cu = input$cu)
          }

          # Prepare flowpar (temperature and pressure for flow measurement)
          flowpar_vec <- if (!is.null(input$flowpar_temp) && !is.null(input$flowpar_pres) &&
            !is.na(input$flowpar_temp) && !is.na(input$flowpar_pres)) {
            c(input$flowpar_pres, input$flowpar_temp)
          } else {
            rep(NA, 2)
          }

          # Prepare optimization estimates
          opt_est_val <- if (input$opt_est_type == "default") {
            "default"
          } else {
            # Validate and convert custom estimates
            custom_est <- c(
              r = as.numeric(input$r_est),
              ksas = as.numeric(input$ksas_est),
              qc = as.numeric(input$qc_est),
              thetaf = as.numeric(input$thetaf_est),
              ti = as.numeric(input$ti_est),
              kf = as.numeric(input$kf_est),
              c3 = as.numeric(input$c3_est)
            )

            # Check for invalid values
            if (any(is.na(custom_est))) {
              stop("All optimization parameters must be valid numbers")
            }
            custom_est
          }

          # Prepare units
          units_list <- c(flow = input$flow_units, resp = input$resp_units)

          incProgress(0.6, detail = i18n$t("Running model..."))

          # Call bicmod function
          model_result <- supeRcrit::bicmod(
            oec = validated_oec_data(),
            oec_vars = oec_vars_list,
            pars = pars_list,
            opt_est = opt_est_val,
            etoh_frac = input$etoh_frac,
            flowpar = flowpar_vec,
            ro_co2 = input$ro_co2,
            tmax = input$tmax,
            qmax = input$qmax,
            cumulative = input$cumulative,
            mass_flow = input$mass_flow,
            draw = TRUE,
            aggreg = input$aggreg,
            modtype = input$modtype,
            units = units_list
          )

          # Fix duplicate names in plots list
          plots_list <- model_result$plots
          if (!is.null(plots_list) && length(plots_list) > 0) {
            # Make names unique by adding suffixes
            names(plots_list) <- make.unique(names(plots_list), sep = "_")
          }

          # Store results
          kinetic_results$full_result <- model_result
          kinetic_results$sim_result <- model_result$sim
          kinetic_results$ct_result <- model_result$ct
          kinetic_results$cmp_result <- model_result$cmp
          kinetic_results$plots_list <- plots_list # Use the fixed plots list
          kinetic_results$data <- model_result$data
          kinetic_results$input_params <- model_result$input
     

          # Determine available models based on fixed plot names
          available <- names(plots_list)
          # Clean up the names for available models (remove suffixes if needed)
          available_clean <- gsub("_\\d+$", "", available) # Remove _1, _2 etc suffixes
          # available_clean <- unique(available_clean)
          kinetic_results$available_models <- available_clean

          # DEBUG: Plot isimlerini konsola yazdır
          # print(paste("Original Plot Names:", paste(names(model_result$plots), collapse = ", ")))
          # print(paste("Fixed Plot Names:", paste(names(plots_list), collapse = ", ")))
          # print(paste("Available Models:", paste(kinetic_results$available_models, collapse = ", ")))

          # print("----****----")
          # browser()

          # Determine available models
          available <- c()
          if (!is.null(model_result$sim)) available <- c(available, "sim")
          if (!is.null(model_result$ct)) available <- c(available, "ct")
          if (!is.null(model_result$cmp)) {
            # Check for cmp2 and cmp3 plots specifically if cmp is present
            if (!is.null(model_result$plots$cmp2)) available <- c(available, "cmp2")
            if (!is.null(model_result$plots$cmp3)) available <- c(available, "cmp3")
          }
          kinetic_results$available_models <- available

          # DEBUG: Plot isimlerini ve available_models'ı konsola yazdır
          # print(paste("Plots List Names:", paste(names(kinetic_results$plots_list), collapse = ", ")))
          # print(paste("Available Models:", paste(kinetic_results$available_models, collapse = ", ")))

          incProgress(1, detail = i18n$t("Completed!"))
        })

        showNotification(i18n$t("BIC model calculated successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error calculating BIC model:"), e$message), type = "error")
      
        kinetic_results$full_result <- NULL
        kinetic_results$sim_result <- NULL
        kinetic_results$ct_result <- NULL
        kinetic_results$cmp_result <- NULL
        kinetic_results$plots_list <- NULL
        kinetic_results$data <- NULL
        kinetic_results$input_params <- NULL
        kinetic_results$available_models <- c()
      }
    )
  })

  # Reactive for conditional panels
  output$has_sim <- reactive({
    !is.null(kinetic_results$sim_result)
  })
  outputOptions(output, "has_sim", suspendWhenHidden = FALSE)

  output$has_ct <- reactive({
    !is.null(kinetic_results$ct_result)
  })
  outputOptions(output, "has_ct", suspendWhenHidden = FALSE)

  output$has_cmp <- reactive({
    !is.null(kinetic_results$cmp_result)
  })
  outputOptions(output, "has_cmp", suspendWhenHidden = FALSE)

  output$has_multiple_models <- reactive({
    length(kinetic_results$available_models) > 1
  })
  outputOptions(output, "has_multiple_models", suspendWhenHidden = FALSE)

  # Plot selector UI
  output$plot_selector_ui <- renderUI({
    req(kinetic_results$plots_list)

    # Get all available plot names
    plot_names <- names(kinetic_results$plots_list)
    req(plot_names)

    # Create display choices
    choices_display <- c()
    choices_values <- c()
    plot_names <- sort(plot_names)


    for (plot_name in plot_names) {
      # Clean name for display (remove suffixes)
      clean_name <- gsub("_\\d+$", "", plot_name)
      clean_name <- plot_name

      if (clean_name == "sim") {
        display_name <- i18n$t("Simplified Model")
      } else if (clean_name == "ct") {
        display_name <- i18n$t("Characteristic Times Model")
      } else if (clean_name == "cmp2") {
        display_name <- i18n$t("Complete Model (2 Regions)")
      } else if (clean_name == "cmp3") {
        display_name <- i18n$t("Complete Model (3 Regions)")
      } else if (clean_name == "sim_1") {
        display_name <- i18n$t("Simplified Model (time-based)")
      } else if (clean_name == "cmp2_1") {
        display_name <- i18n$t("Complete Model (2 Regions) (time-based)")
      } else if (clean_name == "cmp3_1") {
        display_name <- i18n$t("Complete Model (3 Regions) (time-based)")
      } else {
        display_name <- plot_name
      }

      # If there are duplicates, add suffix to display name
      if (plot_name != clean_name) {
        suffix <- gsub(".*_", "", plot_name)
        display_name <- paste(display_name, paste0("(", suffix, ")"))
      }

      choices_display <- c(choices_display, display_name)
      choices_values <- c(choices_values, plot_name)
    }

    # Liste ile choices oluşturma
    choices_list <- setNames(choices_values, choices_display)

    selectInput(ns("selected_plot"),
      i18n$t("Select Model Plot"),
      choices = choices_list,
      selected = choices_values[1]
    )
  })

  # Render selected model plot
  output$selected_model_plot <- renderPlotly({
    req(input$selected_plot, kinetic_results$plots_list)

    plot_obj <- kinetic_results$plots_list[[input$selected_plot]]
    req(plot_obj)


  
    tryCatch(
      {
        if ("ggplot" %in% class(plot_obj)) {
          # ggplot
          #print("Converting ggplot to plotly")
          ggplotly(plot_obj)
        } else if ("plotly" %in% class(plot_obj)) {
          # plotly 
          #print("Already a plotly object")
          plot_obj
        } else {
          # other
          #print("Unknown plot type, trying direct conversion")
          ggplotly(plot_obj)
        }
      },
      error = function(e) {
        print(paste("Error converting plot:", e$message))

        plotly_empty() %>%
          add_annotations(
            text = paste("Error displaying plot:", e$message),
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      }
    )
  })


  # Render selected model plot
  output$selected_model_plot <- renderPlotly({
    req(input$selected_plot, kinetic_results$plots_list)

    plot_list <- kinetic_results$plots_list
    # plot_obj <- kinetic_results$plots_list[[input$selected_plot]]
    plot_obj <- kinetic_results$plots_list[[input$selected_plot]]
  
    req(plot_obj)
    ggplotly(plot_obj)
  })

  # Model Comparison Table
  output$model_comparison_table <- DT::renderDataTable({
    req(kinetic_results$available_models)

    comp_data <- data.frame()

    if ("sim" %in% kinetic_results$available_models && !is.null(kinetic_results$sim_result$resid)) {
      comp_data <- rbind(comp_data, data.frame(
        Model = i18n$t("Simplified"),
        AARD = sprintf("%.6f", kinetic_results$sim_result$resid["aard"]),
        RMSE = sprintf("%.10f", kinetic_results$sim_result$resid["rmse"]),
        R2 = sprintf("%.6f", kinetic_results$sim_result$resid["r2"])
      ))
    }

    if ("ct" %in% kinetic_results$available_models && !is.null(kinetic_results$ct_result$resid)) {
      comp_data <- rbind(comp_data, data.frame(
        Model = i18n$t("Characteristic Times"),
        AARD = sprintf("%.6f", kinetic_results$ct_result$resid["aard"]),
        RMSE = sprintf("%.10f", kinetic_results$ct_result$resid["rmse"]),
        R2 = sprintf("%.6f", kinetic_results$ct_result$resid["r2"])
      ))
    }

    if ("cmp2" %in% kinetic_results$available_models && !is.null(kinetic_results$cmp_result$resid)) {
      comp_data <- rbind(comp_data, data.frame(
        Model = i18n$t("Complete Model"),
        AARD = sprintf("%.6f", kinetic_results$cmp_result$resid["aard"]),
        RMSE = sprintf("%.10f", kinetic_results$cmp_result$resid["rmse"]),
        R2 = sprintf("%.6f", kinetic_results$cmp_result$resid["r2"])
      ))
    }


    DT::datatable(comp_data, options = list(pageLength = 25, dom = "t")) # 't' removes search/pagination
  })

  # SIM Model Outputs
  output$sim_mod_pars_table <- DT::renderDataTable({
    req(kinetic_results$sim_result)
    
    # Parameter isimlerini KaTeX formatında eşleştir (%%...%% formatı)
    param_mapping <- c(
      "beta1" = "%%beta_1%%",
      "beta" = "%%\\beta%%",
      "G0" = "%%G_0%%",
      "G" = "%%G%%",
      "kf" = "%%k_f%%",
      "kfa0" = "%%k_fa_0%%",
      "ksas" = "%%k_sa_s%%",
      "qm" = "%%q_m%%",
      "qn" = "%%q_n%%",
      "qs" = "%%q_s%%",
      "r" = "%%r%%",
      "ti" = "%%t_i%%",
      "thetaf" = "%%\\theta_f%%",
      "thetae" = "%%\\theta_e%%"
    )
    
    param_names <- names(kinetic_results$sim_result$mod_pars)
    
    # Description mapping
    descriptions <- sapply(param_names, function(p) {
      switch(p,
        "c1" = i18n$t("Constant C₁"),
        "c2" = i18n$t("Constant C₂"),
        "qm" = i18n$t("Relative amount of expended solvent at end of CER (kg/kg)"),
        "r" = i18n$t("Grinding efficiency (fraction of broken cells)"),
        "ksas" = i18n$t("Solid phase mass transfer coefficient × area (1/m/s)"),
        "G" = i18n$t("Initial fraction of solute in broken cells"),
        p  # fallback
      )
    })

    # Eşleşen KaTeX formatlarını bul
    display_names <- ifelse(param_names %in% names(param_mapping),
                           param_mapping[param_names],
                           param_names)
    
    data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(kinetic_results$sim_result$mod_pars)),
      Description = descriptions
    ) %>%
      DT::datatable(
        options = list(
          pageLength = 25, 
          dom = "t"
        ),
        rownames = FALSE,
        escape = FALSE
      )
  })

  output$sim_fitted_pars <- renderUI({
    req(kinetic_results$sim_result)
    pars <- kinetic_results$sim_result$fit_pars
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 4px;",
      lapply(pars, function(p) {
        tags$span(
          style = "background-color: #f8f9fa; padding: 2px 6px; border-radius: 3px; font-family: monospace; font-size: 12px;",
          p
        )
      })
    )
  })

  output$sim_statistics_table <- DT::renderDataTable({
    req(kinetic_results$sim_result)
    data.frame(
      Statistic = names(kinetic_results$sim_result$resid),
      Value = sprintf("%.6f", as.numeric(kinetic_results$sim_result$resid))
    ) %>%
      DT::datatable(options = list(pageLength = 25, dom = "t"), rownames = FALSE)
  })

  output$sim_ordt_table <- DT::renderDataTable(
    {
      req(kinetic_results$sim_result)
      dt_output <- DT::datatable(
        kinetic_results$sim_result$ordt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_kinetic_bic_sim_observed&prediction_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_bic_sim_observed&prediction_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_bic_sim_observed&prediction_data"))
          )
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "t", "y", "mod_y"), digits = 5)
    },
    server = FALSE
  )

  output$sim_mdt_table <- DT::renderDataTable(
    {
      req(kinetic_results$sim_result)
      dt_output <- DT::datatable(
        kinetic_results$sim_result$mdt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_bic_detailed-prediction_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_bic_sim_detailed-predictionn_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_bic_sim_detailed-prediction_data"))
          )
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y", "t"), digits = 5)
    },
    server = FALSE
  )

  # CT Model Outputs
  output$ct_mod_pars_table <- DT::renderDataTable({
    req(kinetic_results$ct_result)
    
    # Parameter isimlerini KaTeX formatında eşleştir (%%...%% formatı)
    param_mapping <- c(
      "beta1" = "%%beta_1%%",
      "beta" = "%%\\beta%%",
      "G0" = "%%G_0%%",
      "G" = "%%G%%",
      "kf" = "%%k_f%%",
      "kfa0" = "%%k_fa_0%%",
      "ksas" = "%%k_sa_s%%",
      "qm" = "%%q_m%%",
      "qn" = "%%q_n%%",
      "qs" = "%%q_s%%",
      "r" = "%%r%%",
      "ti" = "%%t_i%%",
      "tc" = "%%t_c%%",
      "tf" = "%%t_f%%",
      "thetaf" = "%%\\theta_f%%",
      "thetae" = "%%\\theta_e%%"
    )
    
    param_names <- names(kinetic_results$ct_result$mod_pars)
    
    # Description mapping
    descriptions <- sapply(param_names, function(p) {
      switch(p,
        "thetaf" = i18n$t("External mass transfer resistance"),
        "ti" = i18n$t("Extraction time/duration of FER"),
        "tprime" = i18n$t("Extraction time at end of CER"),
        "eprime" = i18n$t("Yield at end of CER (g/g total dry solid)"),
        "G" = i18n$t("Initial fraction of solute in broken cells"),
        p  # fallback
      )
    })

    # Eşleşen KaTeX formatlarını bul
    display_names <- ifelse(param_names %in% names(param_mapping),
                           param_mapping[param_names],
                           param_names)
    
    data.frame(
      Parameter = display_names,
      Value = sprintf("%.6f", as.numeric(kinetic_results$ct_result$mod_pars)),
      Description = descriptions
    ) %>%
      DT::datatable(
        options = list(
          pageLength = 25, 
          dom = "t"
        ),
        rownames = FALSE,
        escape = FALSE
      )
  })

  output$ct_fitted_pars <- renderUI({
    req(kinetic_results$ct_result)
    pars <- kinetic_results$ct_result$fit_pars
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 4px;",
      lapply(pars, function(p) {
        tags$span(
          style = "background-color: #f8f9fa; padding: 2px 6px; border-radius: 3px; font-family: monospace; font-size: 12px;",
          p
        )
      })
    )
  })

  output$ct_statistics_table <- DT::renderDataTable({
    req(kinetic_results$ct_result)
    data.frame(
      Statistic = names(kinetic_results$ct_result$resid),
      Value = sprintf("%.6f", as.numeric(kinetic_results$ct_result$resid))
    ) %>%
      DT::datatable(options = list(pageLength = 25, dom = "t"), rownames = FALSE)
  })

  output$ct_ordt_table <- DT::renderDataTable(
    {
      req(kinetic_results$ct_result)
      dt_output <- DT::datatable(
        kinetic_results$ct_result$ordt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_bic_ct_observed&prediction_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_bic_ct_observed&prediction_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_bic_ct_observed&prediction_data"))
          )
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y", "mod_y"), digits = 5)
    },
    server = FALSE
  )

  output$ct_mdt_table <- DT::renderDataTable(
    {
      req(kinetic_results$ct_result)
      dt_output <- DT::datatable(
        kinetic_results$ct_result$mdt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_bic_ct_detailed-prediction_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_bic_ct_detailed-prediction_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_bic_ct_detailed-prediction_data"))
          )
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y"), digits = 5)
    },
    server = FALSE
  )



output$cmp_mod_pars_table <- DT::renderDataTable({
  req(kinetic_results$cmp_result)
  
  # Parameter isimlerini KaTeX formatında eşleştir (%%...%% formatı)
  param_mapping <- c(
    "beta1" = "%%beta_1%%",
    "beta" = "$$\\beta$$",
    "G0" = "%%G_0%%",
    "G" = "%%G%%",
    "kf" = "%%k_f%%",
    "kfa0" = "%%k_fa_0%%",
    "ksas" = "%%k_sa_s%%",
    "qm" = "%%q_m%%",
    "qn" = "%%q_n%%",
    "qs" = "%%q_s%%",
    "r" = "%%r%%",
    "thetae" = "%%\\theta_e%%"
  )
  
  param_names <- names(kinetic_results$cmp_result$mod_pars)
  
  # Description mapping
  descriptions <- sapply(param_names, function(p) {
    switch(p,
      "thetae" = i18n$t("External mass transfer resistance"),
      "kfa0" = i18n$t("Fluid phase mass transfer coefficient × area (1/m/s)"),
      "qm" = i18n$t("Relative amount of expended solvent at end of CER (kg/kg)"),
      "qn" = i18n$t("FER for 3-period model"),
      "qs" = i18n$t("DC for 2-period model"),
      "beta" = i18n$t("Coefficient β"),
      "G" = i18n$t("Initial fraction of solute in broken cells"),
      "kf" = i18n$t("Fluid mass transfer coefficient (1/s)"),
      "r" = i18n$t("Grinding efficiency (fraction of broken cells)"),
      "ksas" = i18n$t("Solid phase mass transfer coefficient × area (1/m/s)"),
      p  # fallback
    )
  })

  # Eşleşen KaTeX formatlarını bul
  display_names <- ifelse(param_names %in% names(param_mapping),
                         param_mapping[param_names],
                         param_names)
  
  data.frame(
    Parameter = display_names,
    Value = sprintf("%.6f", as.numeric(kinetic_results$cmp_result$mod_pars)),
    Description = descriptions
  ) %>%
    DT::datatable(
      options = list(
        pageLength = 25, 
        dom = "t"
      ),
      rownames = FALSE,
      escape = FALSE
    )
})



  output$cmp_fitted_pars <- renderUI({
    req(kinetic_results$cmp_result)
    pars <- kinetic_results$cmp_result$fit_pars
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 4px;",
      lapply(pars, function(p) {
        tags$span(
          style = "background-color: #f8f9fa; padding: 2px 6px; border-radius: 3px; font-family: monospace; font-size: 12px;",
          p
        )
      })
    )
  })

  output$cmp_statistics_table <- DT::renderDataTable({
    req(kinetic_results$cmp_result)
    data.frame(
      Statistic = names(kinetic_results$cmp_result$resid),
      Value = sprintf("%.6f", as.numeric(kinetic_results$cmp_result$resid))
    ) %>%
      DT::datatable(options = list(pageLength = 25, dom = "t"), rownames = FALSE)
  })

  output$cmp_ordt_table <- DT::renderDataTable(
    {
      req(kinetic_results$cmp_result)
      # CMP model's ordt might have y_cmp2, y_cmp3. We need to handle this dynamically.
      # For simplicity, let's just show the whole table.
      dt_output <- DT::datatable(
        kinetic_results$cmp_result$ordt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_bic_cmp_observed&prediction_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_bic_cmp_observed&prediction_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_bic_cmp_observed&prediction_data"))
          )
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("t", "x", "y", "y_cmp3", "y_cmp2"), digits = 5)
    },
    server = FALSE
  )

  output$cmp_mdt_table <- DT::renderDataTable(
    {
      req(kinetic_results$cmp_result)
      dt_output <- DT::datatable(
        kinetic_results$cmp_result$mdt,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", filename = generate_filename_with_timestamp("supercrit_bic_cmp_detailed-prediction_data")),
            list(extend = "excel", filename = generate_filename_with_timestamp("supercrit_bic_cmp_detailed-prediction_data")),
            list(extend = "pdf", filename = generate_filename_with_timestamp("supercrit_bic_cmp_detailed-prediction_data"))
          )
        ),
        rownames = FALSE
      )
      dt_output <- dt_output %>% DT::formatRound(columns = c("x", "y", "t"), digits = 5)
    },
    server = FALSE
  )





  # Reactive to check if export button should be visible
  output$show_export_button <- reactive({
    !is.null(kinetic_results$full_result)
  })
  outputOptions(output, "show_export_button", suspendWhenHidden = FALSE)

  # Reset function
  observeEvent(input$reset, {
    # Reset all inputs to default values
    updateRadioButtons(session, "input_type", selected = "csv")
    # The dynamic selectInputs will reset automatically when oec_data() becomes NULL
    # or when new data is loaded.
    updateNumericInput(session, "pres", value = defaults$pres)
    updateNumericInput(session, "temp", value = defaults$temp)
    updateNumericInput(session, "mass_in", value = defaults$mass_in)
    updateNumericInput(session, "moisture", value = defaults$moisture)
    updateNumericInput(session, "D", value = defaults$D)
    updateNumericInput(session, "L", value = defaults$L)
    updateNumericInput(session, "etoh", value = defaults$etoh)
    updateNumericInput(session, "dr", value = defaults$dr)
    updateNumericInput(session, "dp", value = defaults$dp)
    updateNumericInput(session, "n", value = defaults$n)
    updateNumericInput(session, "flow", value = defaults$flow)
    updateNumericInput(session, "cu", value = defaults$cu)
    updateCheckboxInput(session, "cumulative", value = defaults$cumulative)
    updateCheckboxInput(session, "mass_flow", value = defaults$mass_flow)

    # Advanced parameters
    updateNumericInput(session, "etoh_frac", value = defaults$etoh_frac)
    updateNumericInput(session, "flowpar_temp", value = defaults$flowpar_temp)
    updateNumericInput(session, "flowpar_pres", value = defaults$flowpar_pres)
    updateNumericInput(session, "ro_co2", value = defaults$ro_co2)
    updateNumericInput(session, "tmax", value = defaults$tmax)
    updateNumericInput(session, "qmax", value = defaults$qmax)

    # Model settings
    updateSelectInput(session, "modtype", selected = defaults$modtype)
    updateSelectInput(session, "aggreg", selected = defaults$aggreg)
    updateSelectInput(session, "flow_units", selected = defaults$flow_units)
    updateSelectInput(session, "resp_units", selected = defaults$resp_units)

    # Optimization parameters
    updateRadioButtons(session, "opt_est_type", selected = defaults$opt_est_type)
    updateTextInput(session, "r_est", value = defaults$r_est)
    updateTextInput(session, "ksas_est", value = defaults$ksas_est)
    updateTextInput(session, "qc_est", value = defaults$qc_est)
    updateTextInput(session, "thetaf_est", value = defaults$thetaf_est)
    updateTextInput(session, "ti_est", value = defaults$ti_est)
    updateTextInput(session, "kf_est", value = defaults$kf_est)
    updateTextInput(session, "c3_est", value = defaults$c3_est)

    # Clear results
    kinetic_results$full_result <- NULL
    kinetic_results$sim_result <- NULL
    kinetic_results$ct_result <- NULL
    kinetic_results$cmp_result <- NULL
    kinetic_results$plots_list <- NULL
    kinetic_results$data <- NULL
    kinetic_results$input_params <- NULL
    kinetic_results$available_models <- c()

    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Intro Help Button Observers
  observeEvent(input$input_data_oec_help, {
    introjs(session, options = intro_steps_kinetic_bic_input_data(ns, i18n))
  })

  observeEvent(input$process_params_help, {
    introjs(session, options = intro_steps_kinetic_bic_process_params(ns, i18n))
  })

  observeEvent(input$advanced_params_help, {
    introjs(session, options = intro_steps_kinetic_bic_advanced_params(ns, i18n))
  })

  observeEvent(input$model_settings_help, {
    introjs(session, options = intro_steps_kinetic_bic_model_settings(ns, i18n))
  })

  # observeEvent(input$optimization_params_help, {
  #   introjs(session, options = intro_steps_kinetic_bic_optimization_params(ns, i18n))
  # })

  observeEvent(input$optimization_params_help, {
    opt_est_custom <- FALSE
    if (input$opt_est_type == "custom") {
      opt_est_custom <- TRUE
    }
    introjs(session, options = intro_steps_kinetic_bic_optimization_params(ns, i18n, opt_est_custom))
  })

  # ============================================================================
  # UNITS MODAL
  # ============================================================================

  observeEvent(input$show_units, {
    showModal(modalDialog(
      title = i18n$t("BIC Model Parameter Units"),
      DT::dataTableOutput(ns("units_table")),
      easyClose = TRUE,
      size = "l"
    ))
  })

  output$units_table <- DT::renderDataTable({
    show_pars("bic")
  })

  # ============================================================================
  # PREDICTION FUNCTIONALITY
  # ============================================================================

  # Conditional panel for predictions tab
  output$has_full_result <- reactive({
    !is.null(kinetic_results$full_result)
  })
  outputOptions(output, "has_full_result", suspendWhenHidden = FALSE)

  # Dynamic model selector - only show available models
  output$predict_model_selector_ui <- renderUI({
    req(kinetic_results$available_models)
    
    available <- kinetic_results$available_models
    
    # Create choices with display names
    choices <- c()
    if ("sim" %in% available) choices <- c(choices, "Simplified (SIM)" = "sim")
    if ("ct" %in% available) choices <- c(choices, "Characteristic Times (CT)" = "ct")
    if ("cmp2" %in% available) choices <- c(choices, "Complete 2-Period (CMP2)" = "cmp2")
    if ("cmp3" %in% available) choices <- c(choices, "Complete 3-Period (CMP3)" = "cmp3")
    
    selectInput(ns("predict_model"),
                i18n$t("Select Model Type"),
                choices = choices,
                selected = choices[1])
  })

  # Dynamic UI for prediction data input
  output$predict_data_input_ui <- renderUI({
    if (input$predict_input_type == "csv") {
      fileInput(ns("predict_file_upload"), 
                i18n$t("Upload CSV (single column)"),
                accept = c("text/csv", ".csv"))
    } else {
      textAreaInput(ns("predict_manual_data"), 
                    i18n$t("Enter values (one per line)"),
                    rows = 8,
                    placeholder = "10\n20\n30\n40\n50\n...")
    }
  })

  # Parse prediction input data
  predict_input_data <- reactive({
    req(input$predict_input_type)
    
    data <- NULL
    
    if (input$predict_input_type == "csv") {
      req(input$predict_file_upload)
      tryCatch({
        data <- read.csv(input$predict_file_upload$datapath, 
                         header = FALSE)[,1]
        data <- as.numeric(data)
        data <- data[!is.na(data)]
      }, error = function(e) {
        showNotification(paste(i18n$t("Error loading CSV:"), 
                              e$message), type = "error")
        return(NULL)
      })
    } else {
      req(input$predict_manual_data)
      if (nchar(input$predict_manual_data) > 0) {
        tryCatch({
          data <- as.numeric(strsplit(input$predict_manual_data, 
                                     "\\n")[[1]])
          data <- data[!is.na(data)]
        }, error = function(e) {
          showNotification(paste(i18n$t("Error parsing data:"), 
                                e$message), type = "error")
          return(NULL)
        })
      }
    }
    
    data
  })

  # Calculate predictions
  observeEvent(input$predict_calculate, {
    # Check if model exists
    req(kinetic_results$full_result)
    req(predict_input_data())
    req(input$predict_model)
    req(input$predict_units)
    
    # Validate CT model with time units
    if (input$predict_model == "ct" && input$predict_units != "time") {
      showNotification(i18n$t("Characteristic Times (CT) model only accepts 'Time (minutes)' units."), 
                      type = "warning")
      return(NULL)
    }
    
    tryCatch({
      withProgress(message = i18n$t("Calculating predictions..."), 
                   value = 0, {
        
        incProgress(0.5, detail = i18n$t("Running predict_bic..."))
        
        # Call predict_bic
        pred_result <- supeRcrit::predict_bic(
          input = kinetic_results$full_result,
          newdata = predict_input_data(),
          units = input$predict_units,
          get_yields = input$predict_get_yields
        )
        
        # Store results
        kinetic_results$predict_result <- pred_result
        
        incProgress(1, detail = i18n$t("Completed!"))
      })
      
      showNotification(i18n$t("Predictions calculated successfully!"), 
                      type = "message")
    }, error = function(e) {
      showNotification(paste(i18n$t("Error calculating predictions:"), 
                            e$message), type = "error")
      kinetic_results$predict_result <- NULL
    })
  })

  # Render predictions table
  output$predict_results_table <- DT::renderDataTable({
    req(kinetic_results$predict_result)
    req(input$predict_model)
    
    # Get selected model predictions
    preds <- kinetic_results$predict_result$predictions[[input$predict_model]]
    
    req(preds)
    
    DT::datatable(
      preds,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "csv", 
               filename = generate_filename_with_timestamp(
                 paste0("supercrit_bic_predictions_", 
                        input$predict_model))),
          list(extend = "excel", 
               filename = generate_filename_with_timestamp(
                 paste0("supercrit_bic_predictions_", 
                        input$predict_model))),
          list(extend = "pdf", 
               filename = generate_filename_with_timestamp(
                 paste0("supercrit_bic_predictions_", 
                        input$predict_model)))
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = colnames(preds), digits = 6)
  }, server = FALSE)

  # Render unit chart
  output$predict_unit_chart <- DT::renderDataTable({
    req(kinetic_results$predict_result)
    
    DT::datatable(
      kinetic_results$predict_result$unit_chart,
      options = list(pageLength = 5, dom = "t"),
      rownames = FALSE
    )
  })

  # Render description
  output$predict_description <- renderUI({
    req(kinetic_results$predict_result)
    
    HTML(paste0("<p>", gsub("\n", "<br/>", kinetic_results$predict_result$description), "</p>"))
  })

  # Reset predictions
  observeEvent(input$predict_reset, {
    kinetic_results$predict_result <- NULL
    kinetic_results$predict_data <- NULL
    
    # Reset inputs
    if (!is.null(input$predict_model)) {
      # Get first available model
      if (length(kinetic_results$available_models) > 0) {
        updateSelectInput(session, "predict_model", 
                         selected = kinetic_results$available_models[1])
      }
    }
    updateRadioButtons(session, "predict_input_type", selected = "manual")
    updateSelectInput(session, "predict_units", selected = "sm")
    updateCheckboxInput(session, "predict_get_yields", value = TRUE)
    
    showNotification(i18n$t("Predictions reset"), type = "message")
  })

  # Export kinetic model results
  output$export_kinetic <- downloadHandler(
    filename = function() {
      # Generate filename with timestamp
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("supercrit_kinetic_bic_export_", timestamp, ".zip")
    },
    content = function(file) {
      req(kinetic_results$full_result)

      tryCatch(
        {
          # Create temporary directory
          temp_dir <- tempdir()

          # Call kin_export with temporary directory (only modres for individual models)
          kin_export(
            modres = kinetic_results$full_result,
            expath = temp_dir,
            silent = TRUE
          )

          # Find the created directory (should start with "KIN_")
          kin_dirs <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
          kin_dirs <- kin_dirs[grepl("^KIN_", basename(kin_dirs))]

          if (length(kin_dirs) > 0) {
            # Use the most recent one
            kin_dir <- kin_dirs[order(basename(kin_dirs), decreasing = TRUE)][1]

            # Zip the directory
            zip::zip(zipfile = file, files = basename(kin_dir), root = temp_dir)

            showNotification(i18n$t("Kinetic model results exported successfully!"), type = "message")
          } else {
            stop("Could not find exported files directory")
          }
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error exporting kinetic model results:"), e$message), type = "error")
        }
      )
    }
  )
}
