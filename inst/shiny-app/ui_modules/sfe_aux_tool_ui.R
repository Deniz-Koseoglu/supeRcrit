auxiliary_tools_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             tabBox(
               width = NULL,
               id = ns("aux_tools_tabset"),
               tabPanel(
                 title = i18n$t("CO2-Ethanol Critical Parameters"),
                 value = "etoh_crit_params",
                 box(
                   title = i18n$t("Binary CO2-Ethanol Mixtures (iscrit_etoh)"),
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   actionButton(ns("sfe_aux_tools_etoh_help"), "",
                     icon = icon("question-circle"),
                     class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
                   ),
                   div(id = ns("etoh_co2_frac_wrapper"), numericInput(ns("etoh_co2_frac"), i18n$t("CO2 Molar Fraction (0-1)"), value = 0.5, min = 0, max = 1, step = 0.01)),
                   div(id = ns("etoh_pres_wrapper"), numericInput(ns("etoh_pres"), i18n$t("Pressure (bar)"), value = 400)),
                   div(id = ns("etoh_temp_wrapper"), numericInput(ns("etoh_temp"), i18n$t("Temperature (Celsius)"), value = 45)),
                   div(id = ns("etoh_method_wrapper"), radioButtons(ns("etoh_method"), i18n$t("Method:"),
                                choices = c("chueh", "redlich", "both"), selected = "redlich", inline = TRUE)),
                   div(id = ns("calc_etoh_crit_wrapper"), actionButton(ns("calc_etoh_crit"), i18n$t("Calculate CO2-EtOH Critical Params"), class = "btn-info")),
                   uiOutput(ns("iscrit_etoh_results")),
                   hr(),
                   h4(i18n$t("CO2-Ethanol Demo")),
                   div(id = ns("run_etoh_demo_wrapper"), actionButton(ns("run_etoh_demo"), i18n$t("Show Validation Plot"), class = "btn-info")),
                   uiOutput(ns("iscrit_etoh_demo_results"))
                 )
               ),
               tabPanel(
                 title = i18n$t("General Mixture Critical Parameters"),
                 value = "gen_crit_params",
                 box(
                   title = i18n$t("General Solvent Mixtures (iscrit_gen)"),
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   actionButton(ns("sfe_aux_tools_gen_help"), "",
                     icon = icon("question-circle"),
                     class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
                   ),
                   uiOutput(ns("gen_solvent_inputs")), # Dynamic UI for solvent selection and fractions
                   hr(),
                   div(id = ns("gen_units_wrapper"), radioButtons(ns("gen_units"), i18n$t("Units:"), choices = c("mass", "mol"), selected = "mol", inline = TRUE)),
                   div(id = ns("gen_pres_wrapper"), numericInput(ns("gen_pres"), i18n$t("Pressure (bar)"), value = 400)),
                   div(id = ns("gen_temp_wrapper"), numericInput(ns("gen_temp"), i18n$t("Temperature (Celsius)"), value = 45)),
                   div(id = ns("gen_tc_method_wrapper"), selectInput(ns("gen_tc_method"), i18n$t("Critical Temperature Method:"),
                               choices = c("all", "KAY", "LI", "FECP", "HECP", "TANG1", "TANG2", "TANG3", "TANG4"), selected = "all")),
                   div(id = ns("gen_pc_method_wrapper"), selectInput(ns("gen_pc_method"), i18n$t("Critical Pressure Method:"),
                               choices = c("all", "KAY", "LI", "HECP", "TANG1", "TANG2", "TANG3", "TANG4"), selected = "all")),
                   div(id = ns("calc_gen_crit_wrapper"), actionButton(ns("calc_gen_crit"), i18n$t("Calculate General Critical Params"), class = "btn-info")),
                   uiOutput(ns("iscrit_gen_results"))
                 )
               ),
               tabPanel(
                 title = i18n$t("Available Solvents Database"),
                 value = "solvents_db",
                 box(
                   title = i18n$t("Available Solvents Data"),
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
                   title = i18n$t("GCM Method Selection Chart"),
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
