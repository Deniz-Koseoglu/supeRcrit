# COM Analysis UI Module
com_analysis_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    column(
      width = 5,
      fluidRow(
        column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(4, actionButton(ns("calculate"), i18n$t("Calculate"), class = "btn btn-info btn-block")),
        column(4, actionButton(ns("open_settings"), i18n$t("Settings"), icon = icon("cog"), class = "btn btn-default btn-block"))
      ),
      br(),
      box(
        title = i18n$t("Input Parameters"),
        width = NULL,
        # status = "primary",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        # style = "height: calc(100vh - 100px);",#overflow-y: scroll",

        # Accordion for parameter groups
        shinydashboardPlus::accordion(
          id = ns("param_accordion"),

          # Input Data Section
          accordionItem(
            title = div(
              i18n$t("Input Data"),
            ),
            value = "input_data_section",
            collapsed = FALSE,
            fluidRow(
              column(12, actionButton(ns("input_data_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(12, radioButtons(ns("input_type"), i18n$t("Select Input Type:"),
                choices = c(
                  "Single Value" = "single",
                  "Upload CSV" = "csv",
                  "Manual Text Input" = "text"
                ),
                selected = "single"
              )),
              conditionalPanel(
                condition = "input.input_type == 'single'",
                ns = ns,
                column(6, numericInput(ns("single_time"), i18n$t("Time (min)"), value = 180, min = 0)),
                column(6, numericInput(ns("single_yield"), i18n$t("Yield (g)"), value = 7, min = 0))
              ),
              conditionalPanel(
                condition = "input.input_type == 'csv'",
                ns = ns,
                column(12, fileInput(ns("csv_file"), i18n$t("Upload CSV File"), accept = ".csv")),
                column(6, selectInput(ns("time_column"), i18n$t("Select Time Column:"),
                  choices = NULL, selected = NULL
                )),
                column(6, selectInput(ns("yield_column"), i18n$t("Select Yield Column:"),
                  choices = NULL, selected = NULL
                ))
              ),
              conditionalPanel(
                condition = "input.input_type == 'text'",
                ns = ns,
                column(12, textAreaInput(ns("text_input"), i18n$t("Enter Time and Yield Data (CSV format, max 10 rows)"),
                  rows = 10,
                  placeholder = "time,yield\n5,0.80\n9,2.18\n...\n"
                ))
              ),
              column(12, verbatimTextOutput(ns("input_data_preview")))
            )
          ),

          # Calculation Mode
          accordionItem(
            title = div(
              i18n$t("Calculation Mode")
            ),
            value = "calc_mode",
            collapsed = TRUE,
            fluidRow(
              column(
                12,
                actionButton(ns("calc_mode_help"), "",
                  icon = icon("question-circle"),
                  class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
                )
              ),
              column(6, radioButtons(ns("comode"), i18n$t("Calculation Mode"),
                choices = c("SFE" = "sfe", "SWE" = "swe"),
                selected = defaults$comode
              )),
              column(6, checkboxInput(ns("use_coefs"), i18n$t("Use Pre-defined Coefficients"), value = defaults$use_coefs))
            )
          ),

          # General Parameters
          accordionItem(
            title = div(
              i18n$t("General Parameters"),
            ),
            value = "gen_params",
            collapsed = TRUE,
            fluidRow(
              column(12, actionButton(ns("gen_params_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(6, numericInput(ns("volex"), i18n$t("Extractor Volume (L)"), value = defaults$volex, min = 0)),
              column(6, numericInput(ns("load"), i18n$t("Load (kg)"), value = defaults$load, min = 0)),
              column(6, numericInput(ns("pres"), i18n$t("Pressure (bar)"), value = defaults$pres, min = 0)),
              column(6, numericInput(ns("temp"), i18n$t("Temperature (°C)"), value = defaults$temp, min = 0)),
              column(6, numericInput(ns("flow"), i18n$t("Flow Rate (mL/min)"), value = defaults$flow, min = 0)),
              column(6, numericInput(ns("extime"), i18n$t("Extraction Time (min)"), value = defaults$extime, min = 0)),
              column(6, numericInput(ns("dilfac"), i18n$t("Dilution Factor"), value = defaults$dilfac, min = 0)),
              column(6, numericInput(ns("pr_sale"), i18n$t("Sales Price (USD/kg)"), value = defaults$pr_sale, min = 0))
            )
          ),

          # Other parameters
          accordionItem(
            title = div(
              i18n$t("Other parameters")
            ),
            value = "other_params",
            collapsed = TRUE,
            fluidRow(
              column(12, actionButton(ns("other_params_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              )),
              column(7, textInput(ns("aux_price"), i18n$t("Auxiliary Material Price (USD/kg)"), value = defaults$aux_price)),
              column(5, textInput(ns("aux_fraction"), i18n$t("Required Fraction"), value = defaults$aux_fraction)),
              column(6, numericInput(ns("cosol_loss"), i18n$t("Co-solvent Loss (fraction)"), value = defaults$cosol_loss, min = 0.001, max = 0.99)),
              column(6, textInput(ns("flowpar"), i18n$t("Flow Parameter"), value = defaults$flowpar)),
              column(12, numericInput(ns("csol_flow"), i18n$t("Co-solvent Flow Rate (mL/min)"), value = defaults$csol_flow, min = 0))
            )
          ),

          # Cost of Raw Materials
          accordionItem(
            title = div(
              i18n$t("Cost of Raw Materials")
            ),
            value = "crm_params",
            collapsed = TRUE,
            fluidRow(
              column(
                12,
                actionButton(ns("crm_params_help"), "",
                  icon = icon("question-circle"),
                  class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
                )
              ),
              column(6, numericInput(ns("bh"), i18n$t("Bed Height (cm)"), value = defaults$bh, min = 0)),
              column(6, numericInput(ns("id"), i18n$t("Inner Diameter (cm)"), value = defaults$id, min = 0)),
              column(6, numericInput(ns("pr_mat"), i18n$t("Raw Material Price (USD/kg)"), value = defaults$pr_mat, min = 0)),
              column(6, numericInput(ns("pr_msol"), i18n$t("Main Solvent Price (USD/kg)"), value = defaults$pr_msol, min = 0)),
              column(6, numericInput(ns("pr_csol"), i18n$t("Co-solvent Price (USD/kg)"), value = defaults$pr_csol, min = 0)),
              column(6, numericInput(ns("recp"), i18n$t("Recovery Pressure (bar)"), value = defaults$recp, min = 0)),
              column(6, numericInput(ns("rect"), i18n$t("Recovery Temperature (°C)"), value = defaults$rect, min = 0)),
              column(6, numericInput(ns("sept"), i18n$t("Separator Temperature (°C)"), value = defaults$sept, min = 0))
            )
          ),

          # Cost of Utilities
          accordionItem(
            title = div(
              i18n$t("Cost of Utilities"),
            ),
            value = "cut_params",
            collapsed = TRUE,
            fluidRow(
              column(
                12,
                actionButton(ns("cut_params_help"), "",
                  icon = icon("question-circle"),
                  class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
                )
              ),
              column(6, numericInput(ns("pw_main"), i18n$t("Main Power (kW)"), value = defaults$pw_main, min = 0)),
              column(6, numericInput(ns("pr_kwh"), i18n$t("Energy Price (USD/kWh)"), value = defaults$pr_kwh, min = 0)),
              column(6, numericInput(ns("pw_dry"), i18n$t("Drying Power (kW)"), value = defaults$pw_dry, min = 0)),
              column(6, numericInput(ns("cap_dry"), i18n$t("Drying Capacity (kg/h)"), value = defaults$cap_dry, min = 0)),
              column(6, numericInput(ns("pw_com"), i18n$t("Comminution Power (kW)"), value = defaults$pw_com, min = 0)),
              column(6, numericInput(ns("cap_com"), i18n$t("Comminution Capacity (kg/h)"), value = defaults$cap_com, min = 0)),
              column(6, numericInput(ns("pw_evap"), i18n$t("Evaporation Power (kW)"), value = defaults$pw_evap, min = 0)),
              column(6, numericInput(ns("cap_evap"), i18n$t("Evaporation Capacity (kg/h)"), value = defaults$cap_evap, min = 0))
            )
          ),

          # Cost of Labor
          accordionItem(
            title = div(
              i18n$t("Cost of Labor"),
            ),
            value = "col_params",
            collapsed = TRUE,
            fluidRow(
              column(
                12,
                actionButton(ns("col_params_help"), "",
                  icon = icon("question-circle"),
                  class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
                )
              ),
              column(6, numericInput(ns("oper"), i18n$t("Number of Operators"), value = defaults$oper, min = 0)),
              column(6, numericInput(ns("whr"), i18n$t("Shift Duration (hours)"), value = defaults$whr, min = 0)),
              column(6, numericInput(ns("shifts"), i18n$t("Number of Shifts"), value = defaults$shifts, min = 0)),
              column(6, numericInput(ns("wage"), i18n$t("Monthly Wage (USD)"), value = defaults$wage, min = 0)),
              column(6, numericInput(ns("wdays"), i18n$t("Working Days (monthly)"), value = defaults$wdays, min = 0))
            )
          ),

          # Fixed Costs & Tax Rate
          accordionItem(
            title = div(
              i18n$t("Fixed Costs & Tax Rate"),
            ),
            value = "fci_params",
            collapsed = TRUE,
            fluidRow(
              column(
                12,
                actionButton(ns("fci_params_help"), "",
                  icon = icon("question-circle"),
                  class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
                )
              ),
              column(6, numericInput(ns("capex"), i18n$t("Capital Investment (USD)"), value = defaults$capex, min = 0)),
              column(6, numericInput(ns("maint"), i18n$t("Maintenance Cost (USD)"), value = defaults$maint, min = 0)),
              column(6, numericInput(ns("other"), i18n$t("Other Fixed Costs (USD)"), value = defaults$other, min = 0)),
              column(6, numericInput(ns("depr"), i18n$t("Depreciation Rate"), value = defaults$depr, min = 0, max = 1)),
              column(6, textInput(ns("taxrate"), i18n$t("Tax Rate"), value = defaults$taxrate))
            )
          )
        )

        # Calculation button
      )
    ),

    # Right Panel - Results
    column(
      width = 7,
      tabBox(
        width = NULL,
        height = "calc(100vh - 100px)",
        # Tab 1 - General Results
        tabPanel(
          title = i18n$t("General Results"),
          value = "general",
               # Export All Results (top) - only show when calculation is completed
          conditionalPanel(
            condition = "output.show_export_all_results",
            ns = ns,
            fluidRow(
              column(12,
                style = "display: flex; justify-content: center;",
                downloadButton(ns("download_all_results"), i18n$t("Export Results"), class = "btn btn-default", style = "margin-left:5px;"),
                actionButton(ns("show_units"), "", icon = icon("question-circle"), class = "btn-info btn-xs", style = "float: right;")
              )
            ),
            br()
          ),


          # Info boxes for key metrics
          fluidRow(
            infoBoxOutput(ns("extract_box"), width = 6),
            infoBoxOutput(ns("profit_box"), width = 6)
          ),
          fluidRow(
            infoBoxOutput(ns("margin_box"), width = 6),
            infoBoxOutput(ns("payback_box"), width = 6)
          ),

     
          tags$p(
            i18n$t("*MONTHLY"),
            style = "font-size: 15px; color: grey;"
          ),

          # Time selection slider
          conditionalPanel(
            condition = "input.input_type != 'single' && output.has_multi_data",
            ns = ns,
            fluidRow(
              column(12, sliderInput(ns("time_slider"), i18n$t("Select Time (min):"),
                min = 0, max = 1, value = 0.5, step = 1, width = "100%"
              ))
            )
          ),

          # ROI and Payback Chart
          fluidRow(
            box(
              title = i18n$t("5-Year ROI and Payback Period"),
              width = NULL,
              status = "success",
              solidHeader = TRUE,
              plotlyOutput(ns("roi_plot"), height = "300px")
            )
          ),

          # Monthly Table
          fluidRow(
            box(
              title = i18n$t("Monthly Table"),
              width = NULL,
              status = "success",
              solidHeader = TRUE,
              DT::dataTableOutput(ns("roi_monthly_table")),
              br()
              # downloadButton(ns("export_monthly"), i18n$t("Export Monthly Data"), class = "btn btn-default")
            )
          )
        ),



        # Tab 2 - Plot Analysis
        tabPanel(
          title = i18n$t("Optimal Performance Window"),
          value = "plot_analysis",
          conditionalPanel(
            condition = "input.input_type == 'single'",
            ns = ns,
            h4(i18n$t("Plots are available only when multiple time-yield data points are provided."))
          ),
          conditionalPanel(
            condition = "input.input_type != 'single'",
            ns = ns,
            fluidRow(
              box(
                title = i18n$t("Payback Plot"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                plotlyOutput(ns("payback_plot"), height = "300px")
              )
            ),
            fluidRow(
              box(
                title = i18n$t("Specific Cost Plot"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                plotlyOutput(ns("sc_plot"), height = "300px")
              )
            )
          )
        ),
        # Tab 3 - Detailed Results 
        tabPanel(
          title = i18n$t("Detailed Results"),
          value = "detailed",
          box(
            title = i18n$t("Detailed Cost Distribution"),
            width = NULL,
            status = "success",
            solidHeader = TRUE,
            DT::dataTableOutput(ns("detailed_table")),
            br()
          )
        ),

        # Tab 4 - Server Call
        tabPanel(
          title = i18n$t("Server Call"),
          value = "server_call",
          verbatimTextOutput(ns("call_output"))
        )
     
      )
    )
  )
}
