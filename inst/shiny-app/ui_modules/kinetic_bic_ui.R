# BIC Kinetic Modeling UI Module
kinetic_bic_ui <- function(id, defaults, i18n) {
  ns <- NS(id)


  tagList(
    column(
      width = 5,
      fluidRow(
        column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(4, actionButton(ns("calculate"), i18n$t("Calculate"), class = "btn btn-info btn-block"))
      ),
      br(),
      box(
        title = i18n$t("BIC Model Parameters"),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        shinydashboardPlus::accordion(
          id = ns("param_accordion"),
          accordionItem(
            title = div(
              i18n$t("Input Data and Variables")
            ),
            value = "input_data_oec_section",
            collapsed = FALSE,
            fluidRow(
              column(12, actionButton(ns("input_data_oec_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(12, radioButtons(ns("input_type"), i18n$t("Select Input Type:"),
                choices = c(
                  "Upload CSV File" = "csv",
                  "Manual Text Input" = "manual"
                ),
                selected = defaults$input_type
              ))
            ),
            column(12, tags$pre(
              id = ns("column_format_example"),
              HTML(paste0(
                "Time_min,Yield_g,Solvent_mL <br/>",
                "0,   0.0000,          0<br/>",
                "15,  0.0400,      56200<br/>",
                "30,  0.0123,      57600<br/>",
                "..."
              ))
            )),
            uiOutput(ns("data_input_ui")), # Dynamic UI for CSV or manual input
            fluidRow(
              column(12, helpText(i18n$t("Variables will be automatically detected from your data."))),
              column(6, uiOutput(ns("oec_x_var_ui"))), # Dynamic selectInput for time variable
              column(6, uiOutput(ns("oec_y_var_ui"))), # Dynamic selectInput for response variable
              column(6, uiOutput(ns("oec_slv_var_ui"))) # Dynamic selectInput for solvent (optional)
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Process Parameters")
            ),
            value = "process_params_section",
            collapsed = TRUE,
            fluidRow(
              column(12, actionButton(ns("process_params_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(6, numericInput(ns("pres"), i18n$t("Pressure (bar)"), value = defaults$pres, min = 0)),
              column(6, numericInput(ns("temp"), i18n$t("Temperature (°C)"), value = defaults$temp, min = 0)),
              column(6, numericInput(ns("mass_in"), i18n$t("Mass of Raw Material (g)"), value = defaults$mass_in, min = 0)),
              column(6, numericInput(ns("moisture"), i18n$t("Moisture Content (%)"), value = defaults$moisture, min = 0, max = 100)),
              column(6, numericInput(ns("D"), i18n$t("Extractor Diameter (m)"), value = defaults$D, min = 0)),
              column(6, numericInput(ns("L"), i18n$t("Extractor Length (m)"), value = defaults$L, min = 0)),
              column(6, numericInput(ns("etoh"), i18n$t("Ethanol Co-solvent Fraction"), value = defaults$etoh, min = 0, max = 1)),
              column(6, numericInput(ns("dr"), i18n$t("Real Density (g/L)"), value = defaults$dr, min = 0)),
              column(6, numericInput(ns("dp"), i18n$t("Apparent Density (g/L)"), value = defaults$dp, min = 0)),
              column(6, numericInput(ns("n"), i18n$t("Number of Observations (CER)"), value = defaults$n, min = 1)),
              column(6, numericInput(ns("flow"), i18n$t("Flow Rate (mL/min or g/min)"), value = defaults$flow)),
              column(6, numericInput(ns("cu"), i18n$t("Max Extractable Material Fraction"), value = defaults$cu, min = 0, max = 1)),
              column(6, checkboxInput(ns("cumulative"), i18n$t("Cumulative Data"), value = defaults$cumulative)),
              column(6, checkboxInput(ns("mass_flow"), i18n$t("Mass Flow Rate"), value = defaults$mass_flow))
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Advanced Parameters")
            ),
            value = "advanced_params_section",
            collapsed = TRUE,
            fluidRow(
              column(12, actionButton(ns("advanced_params_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5-px;"
              )),
              column(6, numericInput(ns("etoh_frac"), i18n$t("Ethanol Co-solvent Fraction (0-0.99)"),
                value = defaults$etoh_frac, min = 0, max = 0.99, step = 0.01
              )),
              column(6, numericInput(ns("flowpar_temp"), i18n$t("Flow Measurement Temperature (°C)"),
                value = defaults$flowpar_temp, min = 0
              )),
              column(6, numericInput(ns("flowpar_pres"), i18n$t("Flow Measurement Pressure (bar)"),
                value = defaults$flowpar_pres, min = 0
              )),
              column(6, numericInput(ns("ro_co2"), i18n$t("CO2 Density (g/L, optional)"),
                value = defaults$ro_co2, min = 0
              )),
              column(6, numericInput(ns("tmax"), i18n$t("Max Time for CT Model (min, optional)"),
                value = defaults$tmax, min = 0
              )),
              column(6, numericInput(ns("qmax"), i18n$t("Max Solvent for Other Models (kg/kg, optional)"),
                value = defaults$qmax, min = 0
              ))
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Model Settings")
            ),
            value = "model_settings_section",
            collapsed = TRUE,
            fluidRow(
              column(12, actionButton(ns("model_settings_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(6, selectInput(ns("modtype"), i18n$t("Model Type"),
                choices = c("all", "sim", "ct", "cmp3", "cmp2", "cu"), selected = defaults$modtype
              )),
              column(6, selectInput(ns("aggreg"), i18n$t("Optimization Aggregation"),
                choices = c("aard", "mean"), selected = defaults$aggreg
              )),
              column(6, selectInput(ns("flow_units"), i18n$t("Flow Units"),
                choices = c("mL/min", "g/min", "kg/h", "L/h", "none"), selected = defaults$flow_units
              )),
              column(6, selectInput(ns("resp_units"), i18n$t("Response Units"),
                choices = c("g", "percent", "permille", "ppm", "ppb"), selected = defaults$resp_units
              )) # Closing parenthesis for fluidRow
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Optimization Parameters")
            ),
            value = "optimization_params_section",
            collapsed = TRUE,
            fluidRow(
              column(12, actionButton(ns("optimization_params_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(12, radioButtons(ns("opt_est_type"), i18n$t("Initial Parameter Estimates:"),
                choices = c(
                  "Use Default Values" = "default",
                  "Custom Values" = "custom"
                ),
                selected = defaults$opt_est_type
              ))
            ), # Closing parenthesis for column
            conditionalPanel(
              condition = paste0("input['", ns("opt_est_type"), "'] == 'custom'"),
              fluidRow(
                column(6, textInput(ns("r_est"), i18n$t("r (grinding efficiency)"), value = defaults$r_est)),
                column(6, textInput(ns("ksas_est"), i18n$t("ksas (k_s * a_s)"), value = defaults$ksas_est)),
                column(6, textInput(ns("qc_est"), i18n$t("qc (relative solvent at CER end)"), value = defaults$qc_est)),
                column(6, textInput(ns("thetaf_est"), i18n$t("thetaf (external mass transfer)"), value = defaults$thetaf_est)),
                column(6, textInput(ns("ti_est"), i18n$t("ti (FER duration)"), value = defaults$ti_est)),
                column(6, textInput(ns("kf_est"), i18n$t("kf (fluid mass transfer)"), value = defaults$kf_est)),
                column(6, textInput(ns("c3_est"), i18n$t("c3 (for cu estimation)"), value = defaults$c3_est))
              ) # Closing parenthesis for fluidRow
            )
          )
        )
      )
    ),


    # Right Panel - Results
    column(
      width = 7,
      tabBox(
        width = NULL,
        height = "calc(100vh - 100px)",
        # Tab 1: Model Plots
        tabPanel(
          title = i18n$t("Model Plots"),
          value = "model_plots",
          # Export button (üstte) - only show when model is calculated
          conditionalPanel(
            condition = paste0("output['", ns("show_export_button"), "']"),
            fluidRow(
              column(12,
                style = "display: flex; justify-content: center;",
                downloadButton(ns("export_kinetic"), i18n$t("Export Results"), class = "btn btn-default",style="margin-left:5px;"),
                actionButton(ns("show_units"), "", icon = icon("question-circle"), class = "btn-info btn-xs", style = "float: right;")
              )
            ),
            br()
          ),
          fluidRow(
            column(6, uiOutput(ns("plot_selector_ui"))),
            column(12, plotlyOutput(ns("selected_model_plot")))
          )
        ),

        # Tab 2: Simplified Model (SIM)
        tabPanel(
          title = i18n$t("Simplified Model (SIM)"),
          value = "sim_model_details",
          conditionalPanel(
            condition = "output.has_sim",
            ns = ns,
            fluidRow(
              column(12, box(
                title = i18n$t("Model Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("sim_mod_pars_table"))
              )),
              column(12, box(
                title = i18n$t("Fitted Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                uiOutput(ns("sim_fitted_pars"))
              )),
              column(12, box(
                title = i18n$t("Model Statistics"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("sim_statistics_table"))
              )),
              column(12, box(
                title = i18n$t("Observed vs Predicted Data"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("sim_ordt_table"))
              )),
              column(12, box(
                title = i18n$t("Detailed Predictions (CER/FER)"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("sim_mdt_table"))
              ))
            )
          )
        ),
        # Tab 3: Characteristic Times (CT)
        tabPanel(
          title = i18n$t("Characteristic Times (CT)"),
          value = "ct_model_details",
          conditionalPanel(
            condition = "output.has_ct",
            ns = ns,
            fluidRow(
              column(12, box(
                title = i18n$t("Model Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("ct_mod_pars_table"))
              )),
              column(12, box(
                title = i18n$t("Fitted Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                uiOutput(ns("ct_fitted_pars"))
              )),
              column(12, box(
                title = i18n$t("Model Statistics"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("ct_statistics_table"))
              )),
              column(12, box(
                title = i18n$t("Observed vs Predicted Data"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("ct_ordt_table"))
              )),
              column(12, box(
                title = i18n$t("Detailed Predictions (CER/FER)"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("ct_mdt_table"))
              ))
            )
          )
        ),
        # Tab 4: Complete Model (CMP)
        tabPanel(
          title = i18n$t("Complete Model (CMP)"),
          value = "cmp_model_details",
          conditionalPanel(
            condition = "output.has_cmp",
            ns = ns,
            fluidRow(
              column(12, box(
                title = i18n$t("Model Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("cmp_mod_pars_table"))
              )),
              column(12, box(
                title = i18n$t("Fitted Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                uiOutput(ns("cmp_fitted_pars"))
              )),
              column(12, box(
                title = i18n$t("Model Statistics"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("cmp_statistics_table"))
              )),
              column(12, box(
                title = i18n$t("Observed vs Predicted Data"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("cmp_ordt_table"))
              )),
              column(12, box(
                title = i18n$t("Detailed Predictions (CER/FER)"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("cmp_mdt_table"))
              ))
            )
          )
        ),
        # Tab 6: Predictions
        tabPanel(
          title = i18n$t("Predictions"),
          value = "predictions_tab",
          conditionalPanel(
            condition = "output.has_full_result", # Sadece model hesaplandıysa göster
            ns = ns,
            fluidRow(
              # Prediction Settings - 12 column width
              column(
                width = 12,
                box(
                  title = i18n$t("Prediction Settings"),
                  width = NULL,
                  status = "info",
                  solidHeader = TRUE,
                  # Action buttons row
                  fluidRow(
                    column(6, actionButton(ns("predict_calculate"),
                      i18n$t("Calculate Predictions"),
                      class = "btn btn-primary btn-block"
                    )),
                    column(6, actionButton(ns("predict_reset"),
                      i18n$t("Reset Predictions"),
                      class = "btn btn-default btn-block"
                    ))
                  ),
                  br(),
                  # Input Data Type and Calculate Percentage side by side (6-6)
                  fluidRow(
                    column(
                      6,
                      div(
                        id = ns("predict_input_type_wrapper"),
                        radioButtons(ns("predict_input_type"),
                          i18n$t("Input Data Type:"),
                          choices = c(
                            "Manual Input" = "manual",
                            "Upload CSV" = "csv"
                          ),
                          selected = "manual"
                        )
                      )
                    ),
                    column(
                      6,
                      div(
                        id = ns("predict_get_yields_wrapper"),
                        checkboxInput(ns("predict_get_yields"),
                          i18n$t("Calculate mass and percentage yields"),
                          value = TRUE
                        )
                      )
                    )
                  ),
                  # Select Model and Input Units side by side (6-6)
                  fluidRow(
                    column(
                      6,
                      div(
                        id = ns("predict_model_wrapper"),
                        uiOutput(ns("predict_model_selector_ui"))
                      )
                    ), # Dynamic model selector
                    column(
                      6,
                      div(
                        id = ns("predict_units_wrapper"),
                        selectInput(ns("predict_units"),
                          i18n$t("Input Units"),
                          choices = c(
                            "Solvent/Material Ratio (q)" = "sm",
                            "Time (minutes)" = "time"
                          ),
                          selected = "sm"
                        )
                      )
                    )
                  ),
                  # Data input wrapper (full width)
                  div(
                    id = ns("predict_data_input_wrapper"),
                    uiOutput(ns("predict_data_input_ui"))
                  ),
                  helpText(i18n$t("Note: Characteristic Times (CT) model only accepts 'Time (minutes)' units."))
                )
              )
            ),

            # Results - 12 column width
            fluidRow(
              column(
                width = 12,
                box(
                  title = i18n$t("Prediction Results"),
                  width = NULL,
                  status = "success",
                  solidHeader = TRUE,
                  DT::dataTableOutput(ns("predict_results_table"))
                )
              )
            ),

            # Chart - 12 column width
            fluidRow(
              column(
                width = 12,
                box(
                  title = i18n$t("Unit Reference Chart"),
                  width = NULL,
                  status = "info",
                  solidHeader = TRUE,
                  DT::dataTableOutput(ns("predict_unit_chart"))
                )
              )
            ),

            # Information - 12 column width
            fluidRow(
              column(
                width = 12,
                box(
                  title = i18n$t("Model Information"),
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  htmlOutput(ns("predict_description"))
                )
              )
            )
          )
        ),
        # Tab 5: Model Comparison
        tabPanel(
          title = i18n$t("Model Comparison"),
          value = "model_comparison",
          conditionalPanel(
            condition = "output.has_multiple_models",
            ns = ns,
            fluidRow(
              column(12, box(
                title = i18n$t("Model Comparison Statistics"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("model_comparison_table"))
              ))
            )
          )
        )
      )
    )
  )
}
