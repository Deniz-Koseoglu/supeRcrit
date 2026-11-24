auxiliary_tools_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tabBox(
          width = NULL,
          id = ns("aux_tools_tabset"),
          tabPanel(
            title = i18n$t("Mixture Critical Parameters"),
            value = "mixture_crit_params",
            fluidRow(
              # Left: Button + Inputs
              column(
                width = 4,
                fluidRow(
                  column(4, actionButton(ns("reset_crit"), i18n$t("Reset"), class = "btn btn-default btn-block")),
                  column(8, actionButton(ns("calc_crit"), i18n$t("Calculate Critical Parameters"), class = "btn btn-primary btn-block", style = "color: white;"))
                ),
                br(),
                box(
                  title = div(
                    style = "display: flex; align-items: center;",
                    span(i18n$t("Parameters")),
                    div(uiOutput(ns("sfe_aux_tool_crit_params_HELP")))
                  ),
                  status = "success",
                  solidHeader = TRUE,
                  width = NULL,
                  # Inner tabs for mixture type
                  tabsetPanel(
                    id = ns("mixture_type_tabs"),
                    tabPanel(
                      title = i18n$t("CO\u2082 + Ethanol"),
                      value = "co2_etoh",
                      br(),
                      div(id = ns("etoh_co2_frac_wrapper"), uiOutput(ns("etoh_co2_frac_ui"))),
                      div(id = ns("etoh_method_wrapper"), uiOutput(ns("etoh_method_ui")))
                    ),
                    tabPanel(
                      title = i18n$t("General Mixture"),
                      value = "gen_mixture",
                      br(),
                      uiOutput(ns("gen_solvent_inputs")),
                      hr(),
                      fluidRow(
                        column(6, div(id = ns("gen_tc_method_wrapper"), uiOutput(ns("gen_tc_method_ui")))),
                        column(6, div(id = ns("gen_pc_method_wrapper"), uiOutput(ns("gen_pc_method_ui"))))
                      )
                    )
                  ),
                  # Shared: Pressure and Temperature
                  fluidRow(
                    column(6, div(id = ns("shared_pres_wrapper"), uiOutput(ns("shared_pres_ui")))),
                    column(6, div(id = ns("shared_temp_wrapper"), uiOutput(ns("shared_temp_ui"))))
                  )
                )
              ),
              # Right: Results
              column(
                width = 8,
                box(
                  title = i18n$t("Results"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  # Placeholder when no results
                  conditionalPanel(
                    condition = "!output.has_crit_results",
                    ns = ns,
                    div(
                      style = "text-align: center; padding: 50px; color: #888;",
                      icon("flask", style = "font-size: 48px; margin-bottom: 15px;"),
                      h4(i18n$t("Output data will appear here")),
                      p(i18n$t("Set the parameters and click Calculate to see results."))
                    )
                  ),
                  # Actual results
                  conditionalPanel(
                    condition = "output.has_crit_results",
                    ns = ns,
                    uiOutput(ns("crit_results"))
                  )
                ),
                # CO2-EtOH demo (shown only when on CO2+EtOH tab and results exist)
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'co2_etoh'", ns("mixture_type_tabs")),
                  box(
                    title = i18n$t("Show model data within experimental range"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    # Generated the first time this accordion is opened (the outputs are lazy while collapsed).
                    h4(i18n$t("Critical Temperature Plot")),
                    plotOutput(ns("iscrit_etoh_demo_tc_plot")),
                    hr(),
                    h4(i18n$t("Critical Pressure Plot")),
                    plotOutput(ns("iscrit_etoh_demo_pc_plot")),
                    hr(),
                    h4(i18n$t("Data Table")),
                    DT::dataTableOutput(ns("iscrit_etoh_demo_table"))
                  )
                )
              )
            )
          ),
          # ---- Raw scCO2 solubility source data (experimental measurements from 69 primary papers) ----
          tabPanel(
            title = i18n$t("Solubility Source Data"),
            value = "solub_source",
            fluidRow(
              column(
                width = 4,
                box(
                  title = i18n$t("Experimental scCO₂ solubility"),
                  status = "success", solidHeader = TRUE, width = NULL,
                  uiOutput(ns("ssrc_compound_ui")),
                  uiOutput(ns("ssrc_units_ui")),
                  # Two toggles inline on one row; each label is kept on a single line (no wrapping in any language).
                  div(style = "display:flex; flex-wrap:nowrap; gap:18px; margin-top:6px;",
                    div(class = "ck-flush", style = "white-space:nowrap;",
                      checkboxInput(ns("ssrc_cosolv"), i18n$t("Include co-solvent systems"), value = FALSE)),
                    div(class = "ck-flush", style = "white-space:nowrap;",
                      checkboxInput(ns("ssrc_logy"), i18n$t("Logarithmic y-axis"), value = TRUE))),
                  hr(),
                  uiOutput(ns("ssrc_info"))
                )
              ),
              column(
                width = 8,
                box(
                  title = i18n$t("Solubility isotherms"),
                  status = "primary", solidHeader = TRUE, width = NULL,
                  conditionalPanel("output.ssrc_has == false", ns = ns,
                    div(style = "text-align:center; padding:40px; color:#888;",
                      icon("chart-line", style = "font-size:42px; margin-bottom:12px;"),
                      h4(i18n$t("Select a compound to view its measured solubility.")))),
                  conditionalPanel("output.ssrc_has == true", ns = ns,
                    div(style = "display:flex; justify-content:flex-end; gap:8px; margin-bottom:6px;",
                      downloadButton(ns("ssrc_plot_export"), i18n$t("Export plot (PDF)"), class = "btn btn-success btn-sm", style = "color:white;"),
                      downloadButton(ns("ssrc_export"), i18n$t("Export data (CSV)"), class = "btn btn-success btn-sm", style = "color:white;")),
                    plotly::plotlyOutput(ns("ssrc_plot"), height = "440px"))
                ),
                conditionalPanel("output.ssrc_has == true", ns = ns,
                  box(
                    title = i18n$t("Measured data points"),
                    status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                    DT::dataTableOutput(ns("ssrc_table"))
                  ))
              )
            )
          ),
          tabPanel(
            title = i18n$t("Available Solvents Database"),
            value = "solvents_db",
            box(
              title = div(
                style = "display: flex; align-items: center;",
                span(i18n$t("Available Solvents Data (HSP co-solvent mode only)")),
                div(uiOutput(ns("sfe_aux_tool_solvents_db_HELP")))
              ),
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              DT::dataTableOutput(ns("show_solv_table"))
            )
          ),
          tabPanel(
            title = i18n$t("GCM Method Selection Chart"),
            value = "gcm_chart",
            box(
              title = div(
                style = "display: flex; align-items: center;",
                span(i18n$t("GCM Method Selection Chart")),
                div(uiOutput(ns("sfe_aux_tool_gcm_chart_HELP")))
              ),
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              DT::dataTableOutput(ns("show_gcm_table"))
            )
          )
        )
      )
    )
  )
}
