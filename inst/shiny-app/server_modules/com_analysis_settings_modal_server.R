
com_analysis_settings_modal_server <- function(id,
                                               app_name,
                                               get_current,
                                               apply_func,
                                               i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store available settings
    available_settings_rv <- reactiveVal(NULL)

    # ---- helpers ---------------------------------------
    refresh_choices <- function() {
      choices <- list_saved_settings(app_name)
      available_settings_rv(choices) # Update the reactive value
    }

    # ---- dynamic UI for load tab -------------------------------------------
    output$load_settings_selection_ui <- renderUI({
      choices <- available_settings_rv() # Use the reactive value
      if (length(choices) > 0) {
        selectInput(
          ns("selected_settings"), # Changed ID to avoid conflict with updateSelectInput
          label = i18n$t("Saved Settings"),
          choices = choices,
          selected = choices[[1]]
        )
      } else {
        p(i18n$t("No saved settings found."))
      }
    })

    # Observe when the "Load" tab is selected
    observeEvent(input$settings_tabs, {
      if (input$settings_tabs == "Load") {
        refresh_choices()
      }
    })

    # ---- SAVE --------------------------------------------------------------
    observeEvent(input$confirm_save, {
      req(input$save_settings_name)
      nm <- trimws(input$save_settings_name)
      if (nm == "") {
        showNotification(i18n$t("Please enter a settings name."), type = "warning")
        return(invisible())
      }
      save_settings(app_name, get_current(), nm)
      showNotification(paste(i18n$t("Settings saved as"), nm), type = "message")
      # Add a small delay to ensure file system synchronization before refreshing choices
      Sys.sleep(0.5)
      refresh_choices() # Refresh after saving
    })

    # ---- LOAD --------------------------------------------------------------
    observeEvent(input$confirm_load, {
      req(input$selected_settings) # Use the new ID
      st <- load_settings(app_name, input$selected_settings)
      apply_func(st)
      showNotification(paste(i18n$t("Settings"), input$selected_settings, i18n$t("loaded.")), type = "message")
    })

    # ---- IMPORT ------------------------------------------------------------
    observeEvent(input$confirm_import, {
      req(input$import_file)
      tryCatch({
        imported_settings <- import_settings_file(app_name, input$import_file$datapath)
        apply_func(imported_settings)
        showNotification(i18n$t("Settings imported and applied."), type = "message")
        refresh_choices() # Refresh after import
      }, error = function(e) {
        showNotification(paste(i18n$t("Error importing settings:"), e$message), type = "error")
      })
    })

    # ---- EXPORT ------------------------------------------------------------
    output$confirm_export <- downloadHandler(
      filename = function() {
        nm <- input$export_filename
        if (!grepl("\\.json$", nm, ignore.case = TRUE)) nm <- paste0(nm, ".json")
        nm
      },
      content = function(file) {
        export_settings_file(get_current(), file)
      }
    )

    # Initial refresh of choices when the module starts
    refresh_choices()
  })
}
