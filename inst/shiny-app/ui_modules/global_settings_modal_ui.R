# Global Settings Modal UI
# Import/Export settings as JSON with per-module selection

global_settings_modal_ui <- function(id, i18n) {
  ns <- NS(id)

  # ---- Helper: module group box with fixed height body ----
  make_group_box <- function(group_cb_id, group_label, sub_items = NULL) {
    has_subs <- !is.null(sub_items) && length(sub_items) > 0

    tags$div(
      class = "smb",
      tags$div(
        class = "smb-hd",
        tags$div(
          class = "smb-hd-cb",
          checkboxInput(ns(group_cb_id), NULL, value = TRUE, width = "auto")
        ),
        tags$span(class = "smb-hd-txt", group_label)
      ),
      tags$div(
        class = "smb-bd",
        if(has_subs) {
          tagList(lapply(sub_items, function(sub) {
            checkboxInput(ns(sub$id), i18n$t(sub$label), value = TRUE)
          }))
        } else {
          tags$span(class = "text-muted", style = "font-size: 12px; font-style: italic;",
                    i18n$t("No submodules"))
        }
      )
    )
  }

  modalDialog(
    title = tagList(icon("cog"), " ", i18n$t("Manage Settings")),
    size = "l",
    easyClose = TRUE,

    tags$style(HTML("
      /* Module boxes */
      .smb {
        border: 1px solid #bec8be;
        border-radius: 5px;
        margin-bottom: 14px;
        background: #fff;
        overflow: hidden;
      }
      .smb-hd {
        background: linear-gradient(135deg, #00a65a, #008d4c);
        padding: 6px 12px;
        display: flex;
        align-items: center;
        gap: 4px;
      }
      .smb-hd-cb { flex-shrink: 0; }
      .smb-hd-cb .form-group { margin-bottom: 0 !important; }
      .smb-hd-cb .checkbox { margin: 0 !important; padding: 0 !important; }
      .smb-hd-cb .checkbox label { padding-left: 22px; min-height: 18px; }
      .smb-hd-cb .checkbox input[type='checkbox'] {
        width: 17px; height: 17px; margin-top: 0; accent-color: white;
      }
      .smb-hd-txt {
        color: white; font-size: 15px; font-weight: 700;
        letter-spacing: 0.2px; line-height: 1.2;
      }
      .smb-bd {
        padding: 8px 14px 4px 14px;
        min-height: 120px;
      }
      .smb-bd .form-group { margin-bottom: 0; }
      .smb-bd .checkbox { margin-top: 3px; margin-bottom: 3px; }

      /* Buttons */
      .btn-green { background: #00a65a; border-color: #008d4c; color: #fff; }
      .btn-green:hover, .btn-green:focus { background: #008d4c; color: #fff; }

      /* Wells */
      .settings-well { background: #f8faf8; border: 1px solid #d2d6d2; }

      /* Title row with right-aligned buttons */
      .settings-title-row {
        display: flex; align-items: center; justify-content: space-between;
        margin-bottom: 14px; flex-wrap: wrap; gap: 8px;
      }
      .settings-title-row h4 { margin: 0; }
    ")),

    fluidPage(
      tabsetPanel(
        id = ns("settings_tabs"),
        type = "tabs",

        # ==== TAB 1: Import ==================================================
        tabPanel(
          title = i18n$t("Import"),
          value = "import",
          br(),

          wellPanel(
            class = "settings-well",

            # ---- File input section ----
            tags$h4(tags$strong(i18n$t("Import Settings from File"))),
            tags$strong(i18n$t("Choose JSON File")),
            fileInput(ns("import_json_file"), NULL,
                      multiple = FALSE,
                      accept = c("application/json", ".json"),
                      buttonLabel = i18n$t("Browse"),
                      placeholder = i18n$t("No file selected")),

            tags$hr(),

            # ---- Module selection section ----
            # Title + Select/Deselect buttons on same row
            tags$div(
              class = "settings-title-row",
              tags$h4(tags$strong(i18n$t("Select Modules to Import or Load Defaults"))),
              tags$div(
                actionButton(ns("select_all"), i18n$t("Select All"),
                             class = "btn-default", icon = icon("check-square")),
                actionButton(ns("deselect_all"), i18n$t("Deselect All"),
                             class = "btn-default", icon = icon("square"),
                             style = "margin-left: 4px;")
              )
            ),

            # 2x2 grid
            fluidRow(
              column(6,
                make_group_box("mod_com_analysis", i18n$t("Cost Analysis")),
                make_group_box("mod_sfe_co_sel", i18n$t("SFE Co-Solvent Selection"), list(
                  list(id = "mod_solute_characterization", label = "Solute Characterization"),
                  list(id = "mod_miscibility_optimization", label = "Miscibility Optimization"),
                  list(id = "mod_miscibility_comparison", label = "Miscibility Comparison"),
                  list(id = "mod_auxiliary_tools", label = "Auxiliary Tools")
                ))
              ),
              column(6,
                make_group_box("mod_doe", i18n$t("Design of Experiments"), list(
                  list(id = "mod_doe_design", label = "DOE Design"),
                  list(id = "mod_doe_analysis", label = "DOE Analysis"),
                  list(id = "mod_doe_desir", label = "Desirability Function")
                )),
                make_group_box("mod_kinetic", i18n$t("Kinetic Modeling"), list(
                  list(id = "mod_kinetic_tws", label = "Two-Site Desorption"),
                  list(id = "mod_kinetic_bic", label = "Broken-and-Intact Cells"),
                  list(id = "mod_kinetic_aux_tool", label = "Kinetic Auxiliary Tool")
                ))
              )
            ),

            # Action buttons (no hr above)
            div(
              style = "display: flex; gap: 10px;",
              actionButton(ns("confirm_import"),
                           tagList(icon("upload"), " ", i18n$t("Import")),
                           class = "btn-green"),
              actionButton(ns("load_defaults"),
                           tagList(icon("undo"), " ", i18n$t("Load Defaults")),
                           class = "btn-default")
            )
          )
        ),

        # ==== TAB 2: Export ==================================================
        tabPanel(
          title = i18n$t("Export"),
          value = "export",
          br(),
          wellPanel(
            class = "settings-well",
            tags$h4(tags$strong(i18n$t("Export Current Settings"))),
            tags$p(class = "text-muted",
                   i18n$t("Download all current settings as a JSON file.")),
            tags$strong(i18n$t("Filename")),
            textInput(ns("export_filename"), NULL, value = "supercrit_settings.json"),
            downloadButton(ns("export_to_json"),
                         tagList(icon("file-export"), " ", i18n$t("Export")),
                         class = "btn-success")
          )
        )
      )
    ),

    footer = tagList(modalButton(i18n$t("Cancel")))
  )
}
