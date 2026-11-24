# Process Simulation - Fractionation Column UI Module
# Counter-current supercritical-CO2 fractionation column (Naphtali-Sandholm
# equilibrium-stage solver, GC(A)-EOS). The feed (oil) solutes are chosen from the
# session components; CO2 is the implicit solvent. Results: products, stage
# profiles and hydraulic sizing. Conventions mirror the Phase Behaviour module.
eos_column_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML(
      ".eos-mass-toggle .checkbox { margin: 0 !important; min-height: 0; }
       .eos-mass-toggle .form-group { margin: 0 !important; }
       .eos-mass-toggle .checkbox label { font-size: 14px; font-weight: normal; padding-left: 22px; }
       .eos-opt-row .form-group { margin-bottom: 4px; }
       .eos-opt-row .eos-opt-name { padding-top: 7px; font-size: 14px; font-weight: 600; }
       /* inline row checkbox sitting left of its name, vertically aligned with the fields */
       .eos-opt-check .checkbox { margin: 0; min-height: 0; padding-top: 7px; }
       .eos-opt-check .checkbox label { font-size: 14px; font-weight: 600; padding-left: 22px; }
       .eos-opt-hdr { margin-bottom: 1px; }
       .eos-sf-unit, .eos-sf-unit .form-group { margin: 0 !important; }
       .eos-sf-unit .shiny-options-group { margin: 0; }
       .eos-sf-unit .radio-inline { font-size: 11px; margin: 0 0 0 8px; min-height: 0; padding-top: 0; }
       .eos-sf-unit .radio-inline input { margin-top: 1px; }
       /* vertically centre the spec-mode radios on the sibling HETP input (no pixel offset) */
       .eos-radio-center { display: flex; align-items: center; min-height: 34px; }
       .eos-radio-center .form-group { margin-bottom: 0; }
       .eos-radio-center .shiny-options-group { margin-top: 0; }
       /* keep the derived-N note tight under the packed-height field */
       .eos-height-spec .form-group { margin-bottom: 3px; }
       .eos-derived-n { margin: 0 0 4px 0; }
       /* compact vertical radios for the Sizing diameter mode */
       .eos-size-radio .radio { margin: 2px 0; min-height: 0; }
       .eos-size-radio .radio label { font-size: 12px; padding-left: 20px; }
       .eos-size-radio .control-label { margin-bottom: 2px; }"))),
    fluidRow(
      # ====== INPUTS (left) ===============================================
      column(
        width = 5,
        fluidRow(
          style = "display: flex; align-items: stretch;",
          column(6, style = "display: flex; align-items: center;",
            actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
          column(6, style = "display: flex; align-items: center;",
            actionButton(ns("run"), i18n$t("Run Column"), icon = icon("play"),
              class = "btn-primary btn-block", style = "color: white;"))
        ),
        br(),
        box(
          title = div(style = "display: flex; align-items: center;",
            span(i18n$t("Column Setup")),
            div(uiOutput(ns("eos_column_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))),
          status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE,

          shinydashboardPlus::accordion(
            id = ns("col_accordion"),

            accordionItem(
              title = i18n$t("Feed solutes"), value = "feed_solutes", collapsed = FALSE,
              div(style = "margin-bottom: 10px;",
                actionButton(ns("load_examples"), i18n$t("Load Examples"), icon = icon("flask"), class = "btn btn-info btn-sm"),
                actionButton(ns("load_from_disk"), i18n$t("Load from Disk"), icon = icon("folder-open"),
                  class = "btn btn-default btn-sm", style = "margin-left: 5px;")),
              uiOutput(ns("components_ui")),
              div(class = "alert alert-info", style = "padding: 6px 10px; font-size: 12px; margin: 10px 0;",
                icon("info-circle"), " ",
                i18n$t("The liquid feed (oil) is fractionated counter-currently with CO2 as the supercritical solvent.")),
              uiOutput(ns("model_note_ui"))
            ),

            accordionItem(
              title = i18n$t("Feed composition (oil)"), value = "feed_composition", collapsed = TRUE,
              uiOutput(ns("composition_ui"))
            ),

            accordionItem(
              title = i18n$t("Column parameters"), value = "column_parameters", collapsed = TRUE,
              uiOutput(ns("column_params_ui")),
              uiOutput(ns("gradient_ui")),
              uiOutput(ns("advanced_ui"))
            )
          )
        ),
        # ---- Optimization (optional) -------------------------------------
        box(
          title = div(style = "display: flex; align-items: center;",
            span(i18n$t("Optimization (optional)"))),
          status = "primary", solidHeader = TRUE, width = NULL,
          collapsible = TRUE, collapsed = TRUE,
          uiOutput(ns("optimize_ui"))
        )
      ),

      # ====== RESULTS (right, wide) =======================================
      column(
        width = 7,
        uiOutput(ns("results_titlebar")),
        tabBox(
          id = ns("result_tabs"), width = NULL,
          tabPanel(title = i18n$t("Products"), value = "products", uiOutput(ns("products_result"))),
          tabPanel(title = i18n$t("Sizing"), value = "sizing", uiOutput(ns("sizing_result"))),
          tabPanel(title = i18n$t("Optimization"), value = "optimize", uiOutput(ns("optimize_result")))
        )
      )
    )
  )
}
