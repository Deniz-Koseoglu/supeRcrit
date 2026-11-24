# Process Simulation - Phase Behaviour UI Module
# CO2 + solute(s) phase behaviour with the GC(A)-EOS. Solutes are chosen from the
# session components (built in Components & Parameters, or loaded here); CO2 is
# the implicit light component. All three calculations (flash, P-x,y diagram,
# critical locus) are tabs, computed lazily on first view.
eos_phase_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    # compact, vertically-aligned mass-fractions toggle next to the Total badge
    tags$head(tags$style(HTML(
      ".eos-mass-toggle .checkbox { margin: 0 !important; min-height: 0; }
       .eos-mass-toggle .form-group { margin: 0 !important; }
       .eos-mass-toggle .checkbox label { font-size: 14px; font-weight: normal; padding-left: 22px; }"))),
    fluidRow(
      # ====== INPUTS (left) ===============================================
      column(
        width = 5,
        # Action row: Reset | Calculate | Export (Export = success dropdown with CSV + PDF, enabled once a
        # calculation has produced results — consistent with the other modules).
        fluidRow(
          style = "display: flex; align-items: stretch;",
          column(4, style = "display: flex; align-items: center;",
            actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
          column(4, style = "display: flex; align-items: center;",
            actionButton(ns("run"), i18n$t("Calculate"), icon = icon("play"),
              class = "btn-primary btn-block", style = "color: white;")),
          # Shared Export dropdown — the hand-rolled conditionalPanel version shrank to content width inside the
          # flex column; rp_export_dropdown_col wraps it in a width:100% div + btn-block so it fills the column.
          rp_export_dropdown_col(ns, i18n, "has_phase_results", width = 4,
            extra_style = "display: flex; align-items: center;")
        ),
        br(),
        box(
          title = div(style = "display: flex; align-items: center;",
            span(i18n$t("Phase Calculation")),
            div(uiOutput(ns("eos_phase_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))),
          status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE,

          shinydashboardPlus::accordion(
            id = ns("phase_accordion"),

            accordionItem(
              title = i18n$t("Solutes"), value = "solutes", collapsed = FALSE,
              div(style = "margin-bottom: 10px;",
                actionButton(ns("load_examples"), i18n$t("Load Examples"), icon = icon("flask"), class = "btn btn-info btn-sm"),
                actionButton(ns("load_from_disk"), i18n$t("Load from Disk"), icon = icon("folder-open"),
                  class = "btn btn-default btn-sm", style = "margin-left: 5px;")),
              uiOutput(ns("components_ui")),
              div(class = "alert alert-info", style = "padding: 6px 10px; font-size: 12px; margin: 10px 0;",
                icon("info-circle"), " ",
                i18n$t("Phase behaviour is evaluated for a mixture of CO2 with the selected solute(s).")),
              uiOutput(ns("model_note_ui"))
            ),

            accordionItem(
              title = i18n$t("Conditions"), value = "conditions", collapsed = TRUE,
              uiOutput(ns("conditions_ui")),
              uiOutput(ns("composition_ui"))
            ),

            accordionItem(
              title = i18n$t("Critical locus temperatures"), value = "locus_temps", collapsed = TRUE,
              uiOutput(ns("locus_mode_ui")),
              uiOutput(ns("locus_range_ui"))
            )
          )
        )
      ),

      # ====== RESULTS (right, wide) — tabs, lazily computed ===============
      column(
        width = 7,
        tabBox(
          id = ns("result_tabs"), width = NULL,
          tabPanel(title = i18n$t("Flash"), value = "flash", uiOutput(ns("flash_result"))),
          tabPanel(title = i18n$t("P-x,y Diagram"), value = "pxy", uiOutput(ns("pxy_result"))),
          tabPanel(title = i18n$t("Critical Locus"), value = "locus", uiOutput(ns("locus_result")))
        )
      )
    )
  )
}
