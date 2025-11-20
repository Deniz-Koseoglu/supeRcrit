library(shinyjs)

miscibility_optimization_server <- function(input, output, session, defaults, i18n, sfe_rv) {
  # Load the helper function from general_helpers.R

  ns <- session$ns
  rv <- reactiveValues(
    miscibility_results = NULL,
    modifier_ranking = NULL,
    solute_data = NULL, # Keep for comparison list, but not for direct sfe_mod input
    gcm_results = NULL, # Keep for comparison list, but not for direct sfe_mod input
    sfe_mod_output = NULL,

    
    current_blend = list(), #list(solvents, fractions)
    saved_blends = list() #list of lists
  )

  # Reactive value to store the currently selected calculation ID from the list
  rv_selected_calc_id <- reactiveVal(NULL)

  # Render the list of saved calculations
  output$saved_calculations_list_ui <- renderUI({
    # Add dependency to accordion - refresh list when accordion is opened
    input$load_saved_accordion

    choices <- get_calculation_choices()

    if (length(choices) == 0 || (length(choices) == 1 && choices[[1]] == "")) {
      return(p(i18n$t("No saved GCM calculations found.")))
    }

    # Create radio buttons for each saved calculation
    radioButtons(
      ns("saved_calculation_radio"),
      label = i18n$t("Select a saved calculation:"),
      choices = choices,
      selected = if (!is.null(rv_selected_calc_id())) rv_selected_calc_id() else character(0)
    )
  })

  # Observe selection of radio buttons
  observeEvent(input$saved_calculation_radio, {
    rv_selected_calc_id(as.numeric(input$saved_calculation_radio))
  })

  # Toggle SMILES input visibility
  observeEvent(input$specify_smiles, {
    shinyjs::toggle("smiles_div", condition = input$specify_smiles)
  })

  # Observe "Load Selected Calculation" button click
  observeEvent(input$load_selected_calculation, {
    req(rv_selected_calc_id()) # Ensure a calculation is selected

    calc <- get_calculation_by_id(rv_selected_calc_id())

    if (is.null(calc)) {
      showNotification(i18n$t("Selected calculation not found."), type = "error")
      return()
    }

    # Populate inputs
    updateTextInput(session, "cas_input", value = calc$parameters$cas_input)
    updateTextInput(session, "name_input", value = calc$parameters$name_input)

    # Handle SMILES visibility and value
    updateCheckboxInput(session, "specify_smiles", value = calc$parameters$specify_smiles)
    if (calc$parameters$specify_smiles) {
      updateTextInput(session, "smiles_input", value = calc$parameters$smiles_input)
      shinyjs::show("smiles_div")
    } else {
      updateTextInput(session, "smiles_input", value = "")
      shinyjs::hide("smiles_div")
    }

    updateSelectInput(session, "gcm_tb", selected = calc$parameters$gcm_tb)
    updateSelectInput(session, "gcm_crit", selected = calc$parameters$gcm_crit)
    updateSelectInput(session, "gcm_hsp", selected = calc$parameters$gcm_hsp)
    updateSelectInput(session, "gcm_simplicity", selected = calc$parameters$gcm_simplicity)
    updateNumericInput(session, "gcm_gorder", value = calc$parameters$gcm_gorder)

    showNotification(i18n$t("Calculation loaded successfully."), type = "message")
  })

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
  # This observer will listen for any button click that matches the pattern "del_blend_X"
  observeEvent(input$last_deleted_blend_id, {
    req(input$last_deleted_blend_id)
    
    # Extract the index from the input ID
    # The input ID will be something like "del_blend_1", "del_blend_2", etc.
    deleted_idx_str <- sub("del_blend_", "", input$last_deleted_blend_id)
    deleted_idx <- as.numeric(deleted_idx_str)
    
    # Ensure the index is valid and within the bounds of rv$saved_blends
    if (!is.na(deleted_idx) && deleted_idx > 0 && deleted_idx <= length(rv$saved_blends)) {
      isolate({
        rv$saved_blends <- rv$saved_blends[-deleted_idx]
      })
    }
    
    # Reset the input value to allow for repeated clicks on the same button ID
    # (though the button itself will be re-rendered with a new ID if the list changes)
    session$sendCustomMessage("resetInput", list(id = ns(input$last_deleted_blend_id), value = 0))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$optimize_miscibility, {
    # 1. Build solute vector from local inputs
    solute_input_vector <- c()
    if (!is.null(input$smiles_input) && input$smiles_input != "") {
      solute_input_vector <- c(solute_input_vector, input$smiles_input)
    }
    if (!is.null(input$cas_input) && input$cas_input != "") {
      solute_input_vector <- c(solute_input_vector, input$cas_input)
    }
    if (!is.null(input$name_input) && input$name_input != "") {
      solute_input_vector <- c(solute_input_vector, input$name_input)
    }

    req(length(solute_input_vector) > 0) # Ensure at least one solute identifier is provided

    # Input validation for GCM methods
    if (input$gcm_tb == "none" && input$gcm_crit == "none" && input$gcm_hsp == "none") {
      showNotification(i18n$t("Please select at least one GCM method (Boiling Point, Critical Parameters, or HSP)."), type = "error")
      return(NULL)
    }

    # Input validation for pressure and temperature ranges
    req(input$pressure_min, input$pressure_max, input$temperature_min, input$temperature_max)
    if (input$pressure_min >= input$pressure_max) {
      showNotification(i18n$t("Minimum pressure must be less than maximum pressure."), type = "error")
      return(NULL)
    }
    if (input$temperature_min >= input$temperature_max) {
      showNotification(i18n$t("Minimum temperature must be less than maximum temperature."), type = "error")
      return(NULL)
    }

    # Check for modifier selection and saved blends
    if (length(input$modifier_selection) == 0 && length(rv$saved_blends) == 0) {
      showNotification(i18n$t("Please select at least one co-solvent or define a blend for miscibility optimization."), type = "error")
      return(NULL)
    }

    withProgress(message = i18n$t("Optimizing Miscibility..."), {
      # Build modif and modfracs parameters
      modif_list <- list()
      modfracs_list <- list()

      # 1. Add individual modifiers
      if (!is.null(input$modifier_selection) && length(input$modifier_selection) > 0) {
        for (mod in input$modifier_selection) {
          modif_list[[length(modif_list) + 1]] <- mod
          #modfracs_list[[length(modfracs_list) + 1]] <- NA # No fractions for single modifiers
        }
      }

      # 2. Add saved blends
      if (length(rv$saved_blends) > 0) {
        for (blend in rv$saved_blends) {
          modif_list[[length(modif_list) + 1]] <- blend$solvents

          # Prepare modfracs
          if (length(blend$solvents) == 2 &&
            blend$fractions[1] == blend$fractions[2] &&
            sum(blend$fractions) == 100) {
            modfracs_list[[length(modfracs_list) + 1]] <- blend$fractions[1]
          } else {
            modfracs_list[[length(modfracs_list) + 1]] <- blend$fractions
          }
        }
      }
  
      # Prepare modfracs parameter (NA if no blends, otherwise the list)
      modfracs_param <- if (length(modfracs_list) > 0) modfracs_list else NA

      pres_seq <- seq(input$pressure_min, input$pressure_max, length.out = 10)

      temps_seq <- seq(input$temperature_min, input$temperature_max, length.out = 10)
  
      vfrac_val <- input$vfrac_input / 100

      # Then proceed with the tryCatch (unchanged)
      sfe_mod_output <- tryCatch(
        {
          sfe_mod(
            solute = solute_input_vector, # Use the directly provided input vector
            tb = input$gcm_tb,
            crit = input$gcm_crit,
            hsp = input$gcm_hsp,
            modif = modif_list,
            modfracs = modfracs_param,
            pres = pres_seq, # Use the generated sequence
            temps = temps_seq, # Use the generated sequence
            vfrac = vfrac_val, # Use the converted fraction
            silent = TRUE
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during miscibility optimization:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(sfe_mod_output)) {
        # Store the full output for export
        rv$sfe_mod_output <- sfe_mod_output

        # Populate rv$solute_data and rv$gcm_results from sfe_mod_output for comparison list
        rv$solute_data <- sfe_mod_output$solute_ids
        rv$gcm_results <- list(
          pares = sfe_mod_output$parameters,
          methods = list(tb = input$gcm_tb, crit = input$gcm_crit, hsp = input$gcm_hsp)
        )

        # Process Miscibility Enhancement results for plotting
        miscib_enh_list <- sfe_mod_output$sfe$Miscib_Enhancement
        miscib_enh_df_list <- lapply(names(miscib_enh_list), function(modifier_name) {
          df <- as.data.frame(miscib_enh_list[[modifier_name]])
          df$Pressure_bar <- as.numeric(rownames(df))
          df_long <- tidyr::gather(df, key = "Temperature_C", value = "ME_percent", -Pressure_bar)
          df_long$Temperature_C <- as.numeric(gsub("X", "", df_long$Temperature_C))
          df_long$Modifier <- modifier_name
          df_long$Pressure_MPa <- df_long$Pressure_bar / 10 # Convert bar to MPa
          df_long$Temperature_K <- df_long$Temperature_C + 273.15 # Convert Celsius to Kelvin
          df_long
        })
        rv$miscibility_results <- do.call(rbind, miscib_enh_df_list)

        rv$modifier_ranking <- data.frame(sfe_mod_output$sfe$Modifier_Ranking)
        colnames(rv$modifier_ranking) <- c(i18n$t("Modifier"), i18n$t("Occurrence (%)"))

        # Render Value Boxes
        output$best_modifier_vb <- renderValueBox({
          req(rv$modifier_ranking)
          best_mod <- rv$modifier_ranking[1, i18n$t("Modifier")]
          valueBox(best_mod, i18n$t("Best Co-Solvent"), icon = icon("star"), color = "green")
        })

        output$max_me_vb <- renderValueBox({
          req(rv$miscibility_results)
          max_me <- max(rv$miscibility_results$ME_percent, na.rm = TRUE)
          valueBox(paste0(round(max_me, 2), " %"), i18n$t("Max ME (%)"), icon = icon("percent"), color = "blue")
        })

        output$num_conditions_vb <- renderValueBox({
          req(sfe_mod_output$sfe$Best_Modifier)
          num_conditions <- nrow(sfe_mod_output$sfe$Best_Modifier) * ncol(sfe_mod_output$sfe$Best_Modifier)
          valueBox(num_conditions, i18n$t("Test Conditions"), icon = icon("thermometer-half"), color = "purple")
        })

        output$num_modifiers_vb <- renderValueBox({
          num_individual_modifiers <- length(input$modifier_selection)
          num_blends <- length(rv$saved_blends)
          total_configs <- num_individual_modifiers + num_blends
          valueBox(total_configs, i18n$t("Test Configurations"), icon = icon("flask"), color = "yellow")
        })


        output$best_modifier_table <- renderDT({
          req(sfe_mod_output$sfe$Best_Modifier)
          best_mod_matrix <- sfe_mod_output$sfe$Best_Modifier
          df <- as.data.frame(best_mod_matrix)
          df <- cbind(Pressure_bar = rownames(df), df)

        
          methods <- unique(unlist(best_mod_matrix))

          distinct_colors <- c(
            "#E41A1C", 
            "#377EB8", 
            "#4DAF4A", 
            "#984EA3", 
            "#FF7F00", 
            "#FFFF33", 
            "#A65628", 
            "#F781BF",
            "#999999", 
            "#66C2A5" 
          )

          base_colors <- distinct_colors[1:length(methods)]

          my_colors <- sapply(base_colors, function(col) {
            rgb_val <- col2rgb(col)
            rgb(rgb_val[1] / 255, rgb_val[2] / 255, rgb_val[3] / 255, alpha = 0.5)
          })

          datatable(
            df,
            options = list(
              dom = "t", 
              pageLength = nrow(df), 
              paging = FALSE,
              searching = FALSE, 
              scrollX = TRUE,
              scrollY = "400px"
            ),
            rownames = FALSE
          ) %>%
            formatStyle(
              columns = colnames(df)[-1],
              backgroundColor = styleEqual(methods, my_colors),
              color = "black",
              fontWeight = "bold"
            )
        })



        output$miscibility_results_table <- DT::renderDataTable({
          req(rv$miscibility_results)
          DT::datatable(
            rv$miscibility_results,
            options = list(scrollX = TRUE, paging = FALSE),
            rownames = FALSE,
            filter = "top"
          )
        })

        output$modifier_ranking_table <- DT::renderDataTable({
          req(rv$modifier_ranking)
          DT::datatable(
            rv$modifier_ranking,
            options = list(dom = "t", scrollX = TRUE),
            rownames = FALSE
          )
        })

        # Prepare data for Co-Solvent Comparison Table
        solv_list <- unique(rv$miscibility_results$Modifier)
        reslist <- list()
        for (i in solv_list) {
          min_val <- min(rv$miscibility_results$ME_percent[rv$miscibility_results$Modifier == i], na.rm = TRUE)
          max_val <- max(rv$miscibility_results$ME_percent[rv$miscibility_results$Modifier == i], na.rm = TRUE)
          reslist[[i]] <- c(i, round(min_val, 2), round(max_val, 2))
        }
        rv$miscibility_comparison_data <- setNames(
          do.call(rbind.data.frame, reslist),
          c(i18n$t("Co-solvent"), i18n$t("Min"), i18n$t("Max"))
        )

        # Update co-solvent selection for Miscibility Enhancement tab
        updateSelectInput(session, "miscibility_enhancement_solvent_selection",
          choices = solv_list,
          selected = solv_list[1]
        )
      }
    })
  })

  # Render Co-Solvent Comparison Table
  output$miscibility_comparison_table <- DT::renderDataTable({
    req(rv$miscibility_comparison_data)
    DT::datatable(
      rv$miscibility_comparison_data,
      options = list(dom = "t", scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Render Miscibility Enhancement Table for selected co-solvent
  output$miscibility_enhancement_table <- DT::renderDataTable({
    req(input$miscibility_enhancement_solvent_selection, rv$miscibility_results)

    selected_modifier <- input$miscibility_enhancement_solvent_selection

    # Filter data for the selected modifier
    filtered_data <- rv$miscibility_results[rv$miscibility_results$Modifier == selected_modifier, ]

    # Reshape to wide format (Pressure as rows, Temperature as columns)
    # Need to handle potential missing combinations if not all P/T exist for a modifier

    # Get unique pressures and temperatures
    pressures <- sort(unique(filtered_data$Pressure_bar))
    temperatures <- sort(unique(filtered_data$Temperature_C))

    # Create an empty matrix
    me_matrix <- matrix(NA,
      nrow = length(pressures), ncol = length(temperatures),
      dimnames = list(Pressure = pressures, Temperature = temperatures)
    )

    # Populate the matrix
    for (i in 1:nrow(filtered_data)) {
      p_idx <- which(pressures == filtered_data$Pressure_bar[i])
      t_idx <- which(temperatures == filtered_data$Temperature_C[i])
      me_matrix[p_idx, t_idx] <- round(filtered_data$ME_percent[i], 2)
    }

    DT::datatable(
      as.data.frame(me_matrix),
      options = list(scrollX = TRUE, paging = FALSE),
      rownames = TRUE # Show pressures as row names
    )
  })

  # Observe Add to Comparison button
  observeEvent(input$add_to_comparison, {
    req(rv$solute_data, rv$miscibility_results, rv$modifier_ranking)

    # Store the current optimization result for multi-comparison
    comparison_entry <- list(
      solute_name = rv$solute_data$IDs$Name,
      miscibility_data = rv$miscibility_results,
      modifier_ranking = rv$modifier_ranking,
      timestamp = Sys.time()
    )

    # Add to a global reactiveValue or save to a temporary file for multi-comparison
    # Assuming sfe_rv$comparison_list is defined in app.R or a global scope
    isolate({
      if (is.null(sfe_rv$comparison_list)) {
        sfe_rv$comparison_list <- list()
      }
      sfe_rv$comparison_list[[length(sfe_rv$comparison_list) + 1]] <- comparison_entry
    })

    showNotification(i18n$t("Optimization results added to comparison list."), type = "message")
  })

  # Reactive to check if export button should be visible
  output$show_export_button <- reactive({
    !is.null(rv$sfe_mod_output)
  })
  outputOptions(output, "show_export_button", suspendWhenHidden = FALSE)

  # Download handler for export button
  output$export_results <- downloadHandler(
    filename = function() {
      req(rv$sfe_mod_output)
      solute_name <- rv$sfe_mod_output$solute_ids["Name"]
      solute_cas <- rv$sfe_mod_output$solute_ids["CAS"]
      base_name <- paste0("HSP_", solute_name, "_", solute_cas)
      paste0(generate_filename_with_timestamp(base_name), ".zip")
    },
    content = function(file) {
      req(rv$sfe_mod_output)

      withProgress(message = i18n$t("Exporting results..."), value = 0, {
        tryCatch(
          {
            incProgress(0.3, detail = i18n$t("Generating export files..."))

            # Create temporary directory for export
            temp_dir <- tempdir()

            # Call hsp_export function
            hsp_export(
              input = rv$sfe_mod_output,
              addres = NA,
              plotpars = "default",
              plot_format = "png",
              expath = temp_dir,
              silent = TRUE
            )

            incProgress(0.6, detail = i18n$t("Creating archive..."))

            # Find the created folder (it has timestamp in name)
            exported_folders <- list.dirs(temp_dir, full.names = TRUE, recursive = FALSE)
            hsp_folder <- exported_folders[grepl("^HSP_", basename(exported_folders))]

            if (length(hsp_folder) > 0) {
              # Get the most recent one
              latest_folder <- hsp_folder[which.max(file.info(hsp_folder)$mtime)]

              # Create zip file
              zip_file <- paste0(temp_dir, "/export.zip")
              zip::zip(
                zipfile = zip_file,
                files = basename(latest_folder),
                root = temp_dir
              )

              incProgress(0.9, detail = i18n$t("Finalizing..."))

              # Copy to download location
              file.copy(zip_file, file, overwrite = TRUE)

              incProgress(1, detail = i18n$t("Complete!"))

              showNotification(i18n$t("Export completed successfully!"), type = "message")
            } else {
              stop("Export folder not created")
            }
          },
          error = function(e) {
            showNotification(paste(i18n$t("Error exporting results:"), e$message),
              type = "error"
            )
          }
        )
      })
    }
  )

  # Intro Help Button Observer for SFE Misc Opt
  observeEvent(input$sfe_misc_opt_latest_help, {
    introjs(session, options = intro_steps_sfe_misc_opt_latest(ns, i18n))
  })
    observeEvent(input$sfe_misc_opt_gcm_help, {
    introjs(session, options = intro_steps_sfe_misc_opt_gcm(ns, i18n))
  })
    observeEvent(input$sfe_misc_opt_solute_help, {
    introjs(session, options = intro_steps_sfe_misc_opt_solute(ns, i18n))
  })
    observeEvent(input$sfe_misc_opt_solvent_blend_help, {
    introjs(session, options = intro_steps_sfe_misc_opt_solvent_blend(ns, i18n))
  })
    observeEvent(input$sfe_misc_opt_pressure_help, {
    introjs(session, options = intro_steps_sfe_misc_opt_pressure(ns, i18n))
  })
    observeEvent(input$sfe_misc_opt_help, {
    introjs(session, options = intro_steps_sfe_misc_opt(ns, i18n))
  })

  #return(rv) # Return reactive values to app.R for inter-module communication
}
