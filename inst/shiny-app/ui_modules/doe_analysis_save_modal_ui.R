doe_analysis_save_modal_ui <- function(id, i18n) {
  ns <- NS(id)
  modalDialog(
    title = i18n$t("Save Analysis Settings"),
    size = "s",
    textInput(ns("analysis_name"), i18n$t("Analysis Name:"), placeholder = i18n$t("Enter a name for your analysis")),
    uiOutput(ns("design_name_preview")), # Filename preview
    footer = tagList(
      modalButton(i18n$t("Cancel")),
      actionButton(ns("confirm_save_analysis"), i18n$t("Save"), class = "btn btn-primary")
    )
  )
}
