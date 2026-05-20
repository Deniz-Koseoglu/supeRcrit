doe_analysis_save_modal_ui <- function(id, i18n, default_directory = NULL) {

  ns <- NS(id)
  
  # Ensure default_directory has a value
 if (is.null(default_directory) || default_directory == "") {
    default_directory <- file.path(
      system.file(package = "supeRcrit"),
      "shiny-app", "config", "user-settings", "doe_analysis"
    )
  }
  
  modalDialog(
    title = i18n$t("Save Analysis Settings"),
    size = "m",
    textInput(ns("analysis_name"), i18n$t("Analysis Name"), placeholder = i18n$t("Enter a name for your analysis")),
    uiOutput(ns("design_name_preview")), # Filename preview
    hr(),
    checkboxInput(ns("save_to_disk"), i18n$t("Also save to disk"), value = FALSE),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("save_to_disk")),
      div(
        style = "margin-left: 20px;",
        div(
          style = "display: flex; gap: 10px; align-items: flex-end;",
          div(
            style = "flex-grow: 1;",
            textInput(
              ns("save_directory"), 
              i18n$t("Save Directory"), 
              value = default_directory,
              width = "100%"
            )
          ),
          div(
            style = "margin-bottom: 15px;",
            shinyFiles::shinyDirButton(
              ns("browse_directory"),
              label = i18n$t("Browse..."),
              title = i18n$t("Select Save Directory"),
              class = "btn-default"
            )
          )
        ),
        tags$small(
          class = "text-muted",
          i18n$t("Leave empty to use the default package directory.")
        )
      )
    ),
    footer = tagList(
      modalButton(i18n$t("Cancel")),
      actionButton(ns("confirm_save_analysis"), i18n$t("Save"), class = "btn btn-primary", style = "color: white;")
    )
  )
}
