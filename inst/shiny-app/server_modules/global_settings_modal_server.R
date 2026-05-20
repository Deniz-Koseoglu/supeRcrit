# Global Settings Modal Server
# Handles JSON import/export with per-module selection

global_settings_modal_server <- function(id, main_input, main_session, default_settings, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n_r <- reactive({ i18n })

    # =========================================================================
    # MODULE REGISTRY
    # =========================================================================
    # checkbox_id → default_settings key
    checkbox_to_keys <- list(
      mod_com_analysis             = "com_analysis",
      mod_sfe_co_sel               = "sfe_co_sel",
      mod_solute_characterization  = "solute_characterization",
      mod_miscibility_optimization = "miscibility_optimization",
      mod_miscibility_comparison   = "miscibility_comparison",
      mod_auxiliary_tools           = "auxiliary_tools",
      mod_doe_design               = "doe_design",
      mod_doe_analysis             = "doe_analysis",
      mod_doe_desir                = "doe_desir",
      mod_kinetic_tws              = "kinetic_tws",
      mod_kinetic_bic              = "kinetic_bic",
      mod_kinetic_aux_tool         = "kinetic_aux_tool"
    )

    # Parent → children
    parent_children <- list(
      mod_com_analysis = character(0),
      mod_sfe_co_sel   = c("mod_solute_characterization", "mod_miscibility_optimization",
                           "mod_miscibility_comparison", "mod_auxiliary_tools"),
      mod_doe          = c("mod_doe_design", "mod_doe_analysis", "mod_doe_desir"),
      mod_kinetic      = c("mod_kinetic_tws", "mod_kinetic_bic", "mod_kinetic_aux_tool")
    )

    # All individual checkbox IDs (that map to settings keys)
    all_cb_ids <- names(checkbox_to_keys)
    # All checkbox IDs including parent-only toggles
    every_cb_id <- unique(c(all_cb_ids, names(parent_children)))

    # Human-readable labels
    key_labels <- reactive({
      c(
        com_analysis = i18n_r()$t("Cost Analysis"),
        sfe_co_sel = i18n_r()$t("SFE Co-Solvent Selection"),
        solute_characterization = i18n_r()$t("Solute Characterization"),
        miscibility_optimization = i18n_r()$t("Miscibility Optimization"),
        miscibility_comparison = i18n_r()$t("Miscibility Comparison"),
        auxiliary_tools = i18n_r()$t("Auxiliary Tools"),
        doe_design = i18n_r()$t("DOE Design"),
        doe_analysis = i18n_r()$t("DOE Analysis"),
        doe_desir = i18n_r()$t("Desirability Function"),
        kinetic_tws = i18n_r()$t("Two-Site Desorption"),
        kinetic_bic = i18n_r()$t("Broken-and-Intact Cells"),
        kinetic_aux_tool = i18n_r()$t("Kinetic Auxiliary Tool")
      )
    })

    # =========================================================================
    # PARENT TOGGLE → CHILDREN (using local() for proper closure)
    # =========================================================================
    for(pid in names(parent_children)) {
      local({
        my_pid <- pid
        my_children <- parent_children[[my_pid]]
        if(length(my_children) > 0) {
          observeEvent(input[[my_pid]], {
            val <- isTRUE(input[[my_pid]])
            for(child in my_children) {
              updateCheckboxInput(session, child, value = val)
            }
          }, ignoreInit = TRUE)
        }
      })
    }

    # =========================================================================
    # SELECT ALL / DESELECT ALL
    # =========================================================================
    observeEvent(input$select_all, {
      for(cb_id in every_cb_id) {
        updateCheckboxInput(session, cb_id, value = TRUE)
      }
    })

    observeEvent(input$deselect_all, {
      for(cb_id in every_cb_id) {
        updateCheckboxInput(session, cb_id, value = FALSE)
      }
    })

    # =========================================================================
    # HELPER: Get selected settings keys
    # =========================================================================
    get_selected_keys <- function() {
      keys <- c()
      for(cb_id in all_cb_ids) {
        if(isTRUE(input[[cb_id]])) {
          key <- checkbox_to_keys[[cb_id]]
          if(key %in% names(default_settings)) {
            keys <- c(keys, key)
          }
        }
      }
      unique(keys)
    }

    # =========================================================================
    # IMPORT FROM JSON
    # =========================================================================
    observeEvent(input$confirm_import, {
      file_info <- input$import_json_file
      if(is.null(file_info)) {
        showNotification(i18n_r()$t("Please select a JSON file first."),
                         type = "warning", duration = 4)
        return()
      }

      selected_keys <- get_selected_keys()
      if(length(selected_keys) == 0) {
        showNotification(i18n_r()$t("Please select at least one module to import."),
                         type = "warning", duration = 4)
        return()
      }

      tryCatch({
        settings <- jsonlite::read_json(file_info$datapath, simplifyVector = FALSE)

        if(!is.list(settings) || length(settings) == 0) {
          showNotification(i18n_r()$t("Invalid settings file format."),
                           type = "error", duration = 5)
          return()
        }

        filtered <- settings[intersect(names(settings), selected_keys)]
        if(length(filtered) == 0) {
          showNotification(i18n_r()$t("The selected file does not contain settings for the chosen modules."),
                           type = "warning", duration = 5)
          return()
        }

        apply_global_settings(main_session, filtered, default_settings)

        imported_labels <- unname(key_labels()[names(filtered)])
        imported_labels <- imported_labels[!is.na(imported_labels)]
        showNotification(
          paste0(i18n_r()$t("Settings imported for:"), " ", paste(imported_labels, collapse = ", ")),
          type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste(i18n_r()$t("Error importing settings:"), e$message),
                         type = "error", duration = 5)
      })
    })

    # =========================================================================
    # LOAD DEFAULTS
    # =========================================================================
    observeEvent(input$load_defaults, {
      selected_keys <- get_selected_keys()
      if(length(selected_keys) == 0) {
        showNotification(i18n_r()$t("Please select at least one module to reset."),
                         type = "warning", duration = 4)
        return()
      }

      defaults_to_apply <- default_settings[intersect(names(default_settings), selected_keys)]
      if(length(defaults_to_apply) == 0) return()

      apply_global_settings(main_session, defaults_to_apply, default_settings)

      reset_labels <- unname(key_labels()[names(defaults_to_apply)])
      reset_labels <- reset_labels[!is.na(reset_labels)]
      showNotification(
        paste0(i18n_r()$t("Defaults loaded for:"), " ", paste(reset_labels, collapse = ", ")),
        type = "message", duration = 5)
    })

    # =========================================================================
    # EXPORT TO JSON
    # =========================================================================
    output$export_to_json <- downloadHandler(
      filename = function() {
        fn <- input$export_filename
        if (is.null(fn) || trimws(fn) == "") fn <- "supercrit_settings.json"
        if (!grepl("\\.json$", fn, ignore.case = TRUE)) fn <- paste0(fn, ".json")
        fn
      },
      content = function(file) {
        tryCatch({
          settings <- collect_global_settings(main_input, default_settings)
          jsonlite::write_json(settings, file, pretty = TRUE, auto_unbox = TRUE)
          showNotification(i18n_r()$t("Settings exported successfully!"), type = "message", duration = 3)
        }, error = function(e) {
          showNotification(paste(i18n_r()$t("Error exporting settings:"), e$message),
                           type = "error", duration = 5)
        })
      }
    )

  })
}
