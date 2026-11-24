doe_analysis_save_modal_ui <- function(id, i18n, default_directory = NULL) {

  ns <- NS(id)
  
  # Ensure default_directory has a value
 if (is.null(default_directory) || default_directory == "") {
    default_directory <- supercrit_user_dir(file.path("user-settings", "doe_analysis"))
  }
  
  modalDialog(
    title = i18n$t("Save Analysis Settings"),
    size = "m",
    div(class = "alert alert-info", style = "padding:8px 12px; font-size:13px; margin-bottom:12px;",
      icon("info-circle"), " ",
      i18n$t("Save this analysis to the current session so you can reload or compare it later. It is NOT written to a file on disk.")),
    # Analysis Name with the "Also save to disk" toggle inline to its right. The label sits on its own
    # line (full width); the input BOX and the checkbox share a center-aligned flex row below it. We zero
    # the Bootstrap form-group / .checkbox margins WITHIN this row (scoped by the wrapper id) so the two
    # items are just their own boxes — align-items:center then centres the checkbox exactly against the
    # 34px input box, instead of the fragile "flex-end + guessed margin" that left it floating too high.
    div(
      id = ns("name_row_wrap"),
      tags$style(HTML(sprintf(
        "#%s .form-group{margin-bottom:0 !important;} #%s .checkbox{margin-top:0 !important;margin-bottom:0 !important;}",
        ns("name_row_wrap"), ns("name_row_wrap")))),
      tags$label(class = "control-label", `for` = ns("analysis_name"), i18n$t("Analysis Name")),
      div(
        style = "display: flex; gap: 14px; align-items: center;",
        div(
          style = "flex-grow: 1;",
          textInput(ns("analysis_name"), label = NULL,
            placeholder = i18n$t("Enter a name for your analysis"), width = "100%")
        ),
        div(
          style = "flex-shrink: 0; white-space: nowrap;",
          checkboxInput(ns("save_to_disk"), i18n$t("Also save to disk"), value = FALSE)
        )
      )
    ),
    uiOutput(ns("design_name_preview")), # Filename preview
    hr(),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("save_to_disk")),
      # No left indent here — keeps the Save Directory field's left edge aligned vertically with the
      # Analysis Name field above (the previous margin-left:20px pushed it out of alignment).
      div(
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
