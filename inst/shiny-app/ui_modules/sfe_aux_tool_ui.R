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
                    # Placeholder when no demo results
                    conditionalPanel(
                      condition = "!output.has_demo_results",
                      ns = ns,
                      div(
                        style = "text-align: center; padding: 30px; color: #888;",
                        icon("chart-area", style = "font-size: 36px; margin-bottom: 10px;"),
                        p(i18n$t("Click the button below to display model data."))
                      )
                    ),
                    conditionalPanel(
                      condition = "output.has_demo_results",
                      ns = ns,
                      uiOutput(ns("iscrit_etoh_demo_results"))
                    ),
                    actionButton(ns("run_etoh_demo"), i18n$t("Display and plot data"), class = "btn btn-primary", style = "color: white;")
                  )
                )
              )
            )
          ),
          tabPanel(
            title = i18n$t("Available Solvents Database"),
            value = "solvents_db",
            box(
              title = div(
                style = "display: flex; align-items: center;",
                span(i18n$t("Available Solvents Data")),
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
