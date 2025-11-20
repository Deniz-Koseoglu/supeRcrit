# Two-Site Kinetic Modeling UI Module
kinetic_tws_ui <- function(id, defaults, i18n) {
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
        title = i18n$t("Kinetic Model Parameters"),
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
              )),
              column(12, tags$pre(
                id = ns("column_format_example"),
                HTML(paste0(
                  "Example csv format:<br/>",
                  "Time_min,Yield_80C,Yield_100C,Yield_120C<br/>",
                  "0,0.00,0.00,0.00<br/>",
                  "20,12.71,17.01,18.56<br/>",
                  "40,21.76,29.53,34.92<br/>",
                  "..."
                ))
              )),
            ),
            uiOutput(ns("data_input_ui")), # Dynamic UI for CSV or manual input
            fluidRow(
              column(12, helpText(i18n$t("Variables will be automatically detected from your data."))),
              column(6, div(id = ns("oec_x_var_ui_wrapper"), uiOutput(ns("oec_x_var_ui")))), # Dynamic selectInput for time variable
              column(6, div(id = ns("oec_y_var_ui_wrapper"), uiOutput(ns("oec_y_var_ui")))), # Dynamic selectInput for response variable
              column(6, div(id = ns("oec_slv_var_ui_wrapper"), uiOutput(ns("oec_slv_var_ui")))) # Dynamic selectInput for solvent (optional)
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
              column(6, numericInput(ns("m_in"), i18n$t("Mass of Raw Material (g)"), value = defaults$m_in, min = 0)),
              column(6, numericInput(ns("flow"), i18n$t("Flow Rate (mL/min or g/min)"), value = defaults$flow, min = 0)),
              column(6, numericInput(ns("c0"), i18n$t("Max Possible Yield (units of response)"), value = defaults$c0, min = 0)),
              column(6, numericInput(ns("f"), i18n$t("Fraction of Easily Desorbed Solute (F)"), value = defaults$f, min = 0, max = 1)),
              column(6, checkboxInput(ns("cumulative"), i18n$t("Cumulative Data"), value = defaults$cumulative)),
              column(6, checkboxInput(ns("mass_flow"), i18n$t("Mass Flow Rate"), value = defaults$mass_flow))
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Model Parameters")
            ),
            value = "model_params_section",
            collapsed = TRUE,
            fluidRow(
              column(12, actionButton(ns("model_params_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(12, div(id = ns("optmet_wrapper"), selectInput(ns("optmet"), i18n$t("Optimization Method"),
                choices = c("nlopt", "nlrob"), selected = defaults$optmet
              ))),
              column(6, div(id = ns("tmax_wrapper"), title = "Max Time for Predictions(min)", numericInput(ns("tmax"), i18n$t("Max Time (min)"), value = defaults$tmax))),
              column(6, div(id = ns("qmax_wrapper"), numericInput(ns("qmax"), i18n$t("Max Q"), value = defaults$qmax))),
              column(6, div(id = ns("k1_est_wrapper"), textInput(ns("k1_est"), i18n$t("Initial k1 Estimate"), value = defaults$k1_est))),
              column(6, div(id = ns("k2_est_wrapper"), textInput(ns("k2_est"), i18n$t("Initial k2 Estimate"), value = defaults$k2_est))),
              column(12, div(id = ns("f_est_wrapper"), textInput(ns("f_est"), i18n$t("Initial F Estimate (if not in Process Params)"), value = defaults$f_est))),
              column(6, div(id = ns("flow_units_wrapper"), textInput(ns("flow_units"), i18n$t("Flow Units (e.g., mL/min, g/min)"), value = defaults$flow_units))),
              column(6, div(id = ns("resp_units_wrapper"), textInput(ns("resp_units"), i18n$t("Response Units (e.g., permille, g)"), value = defaults$resp_units))),
              column(6, div(id = ns("plot_x_units_wrapper"), textInput(ns("plot_x_units"), i18n$t("Plot X-axis Units (time or q)"), value = defaults$plot_x_units))),
              column(6, div(id = ns("plot_y_units_wrapper"), textInput(ns("plot_y_units"), i18n$t("Plot Y-axis Units (abs or cc0)"), value = defaults$plot_y_units))),
              column(6, div(id = ns("flowpar_temp_wrapper"), numericInput(ns("flowpar_temp"), i18n$t("Flow Rate Meas. Temp (°C)"), value = defaults$flowpar_temp))),
              column(6, div(id = ns("flowpar_pres_wrapper"), numericInput(ns("flowpar_pres"), i18n$t("Flow Rate Meas. Pres (bar)"), value = defaults$flowpar_pres))),
              column(12, div(id = ns("ro_h2o_wrapper"), numericInput(ns("ro_h2o"), i18n$t("Water Density (g/L)"), value = defaults$ro_h2o)))
            )
          ) # Close accordionItem
        ) # Close accordion
      ), # Close box
    ), # Close column (width = 5)

    # Right Panel - Results
    column(
      width = 7,
      tabBox(
        width = NULL,
        height = "calc(100vh - 100px)",
        tabPanel(
          title = i18n$t("Model Plot"),
          value = "model_plot",
          # Export button (üstte) - only show when model is calculated
          conditionalPanel(
            condition = "output.show_export_button",
            ns = ns,  # ← Bu satırı yukarı taşıdım
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
            column(12, plotlyOutput(ns("kinetic_plot")))
          )
        ),
        tabPanel(
          title = i18n$t("Model Summary"),
          value = "model_summary",
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Model Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("model_parameters_table"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Fitted Parameters"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                uiOutput(ns("fitted_parameters_output"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Model Statistics"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("model_statistics_table"))
              )
            )
          )
        ),
        tabPanel(
          title = i18n$t("Model Details"),
          value = "model_details",
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Observed vs Predicted Data"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("observed_predicted_table"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Model Data"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("model_data_table"))
              )
            )
          )
        ),
        tabPanel(
          title = i18n$t("Predictions"),
          value = "predictions",
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Prediction Inputs"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                radioButtons(ns("predict_input_type"),
                             i18n$t("Input Data Type:"),
                             choices = c("Manual Input" = "manual",
                                         "Upload CSV" = "csv"),
                             selected = "manual"),
                uiOutput(ns("predict_data_input_ui")),
                checkboxInput(ns("prediction_get_yields"), i18n$t("Calculate Yields"), value = TRUE),
                numericInput(ns("prediction_moisture"), i18n$t("Moisture Content (%) (Optional)"), value = NA, min = 0, max = 100),
                actionButton(ns("generate_predictions"), i18n$t("Generate Predictions"), class = "btn btn-success")
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Prediction Results"),
                width = NULL,
                status = "info",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("prediction_results_table"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Model Validity"),
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                htmlOutput(ns("prediction_description"))
              )
            )
          )
        )
      )
    )
  )
}
