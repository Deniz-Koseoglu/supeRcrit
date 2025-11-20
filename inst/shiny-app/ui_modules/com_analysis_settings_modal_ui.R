# com_analysis_settings_modal_ui.R

com_analysis_settings_modal_ui <- function(id, i18n=NULL) {
  ns <- NS(id)


  modalDialog(
    title = i18n$t("Manage Settings"),
    size = "m",
    easyClose = TRUE,

    # ---- body ---------------------------------------------------------------
    fluidPage(
      tabsetPanel(
        id = ns("settings_tabs"),

        # ---- SAVE TAB --------------------------------------------------------
        tabPanel(
          i18n$t("Save"),
          wellPanel(
            tags$h4(i18n$t("Save Current Settings")),
            textInput(
              ns("save_settings_name"),
              i18n$t("Settings Name"),
              placeholder = i18n$t("e.g., black cumin")
            ),
            actionButton(
              ns("confirm_save"),
              i18n$t("Save"),
              class = "btn-primary",
              icon = icon("save")
            )
          )
        ),

        # ---- LOAD TAB -------------------------------------------------------
        tabPanel(
          i18n$t("Load"),
          wellPanel(
            tags$h4(i18n$t("Load Saved Settings")),
            uiOutput(ns("load_settings_selection_ui")),
            actionButton(
              ns("confirm_load"),
              i18n$t("Load"),
              class = "btn-primary",
              icon = icon("folder-open")
            )
          )
        ),

        # ---- IMPORT TAB ------------------------------------------------------
        tabPanel(
          i18n$t("Import"),
          wellPanel(
            tags$h4(i18n$t("Import Settings from File")),
            fileInput(
              ns("import_file"),
              i18n$t("Choose JSON File"),
              multiple = FALSE,
              accept = c("application/json", ".json")
            ),
            actionButton(
              ns("confirm_import"),
              i18n$t("Import"),
              class = "btn-primary",
              icon = icon("upload")
            )
          )
        ),

        # ---- EXPORT TAB ------------------------------------------------------
        tabPanel(
          i18n$t("Export"),
          wellPanel(
            tags$h4(i18n$t("Export Current Settings")),
            textInput(
              ns("export_filename"),
              i18n$t("Filename"),
              value = "com_analysis_settings.json"
            ),
            downloadButton(
              ns("confirm_export"),
              i18n$t("Export"),
              class = "btn-primary",
              icon = icon("file-export")
            )
          )
        )
      )
    ),

    # ---- footer -------------------------------------------------------------
    footer = tagList(
      modalButton(i18n$t("Close"))
    )
  )
}
