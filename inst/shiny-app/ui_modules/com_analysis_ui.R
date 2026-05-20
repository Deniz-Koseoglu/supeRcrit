# COM Analysis UI Module
com_analysis_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    column(
      width = 5,
      fluidRow(
        column(6, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(6, actionButton(ns("calculate"), i18n$t("Calculate"), class = "btn btn-primary btn-block", style = "color: white;"))
      ),
      br(),
      # Help button ile başlık ekle

      box(
        title = div(
          style = "display: flex; align-items: center;",
          span(i18n$t("Cost of Manufacturing")),
          div(uiOutput(ns("com_analysis_HELP"))),
          div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
        ),
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
            # Step 1: Select input type with Load Example button inline
            tags$p(tags$strong(i18n$t("Data Source")),
              input_help(i18n$t("Choose how to provide extraction data. Upload CSV loads a file with time and yield columns. Single Value calculates COM at one specific point. Manual Input lets you type data directly into a table."),
                         title = i18n$t("Data Source"), buttonLabel = i18n$t("OK")),
              style = "margin-top: 10px; margin-bottom: 8px; font-size: 14px; color: #333;"),
            div(style = "display: flex; align-items: baseline; gap: 15px; flex-wrap: wrap;",
              div(style = "margin-bottom: 0;",
                radioButtons(ns("input_type"), NULL,
                  choiceNames = list(i18n$t("Upload CSV"), i18n$t("Single Value"), i18n$t("Manual Input")),
                  choiceValues = c("csv", "single", "text"),
                  selected = defaults$input_type,
                  inline = TRUE
                )
              ),
              actionButton(ns("load_example_data"), i18n$t("Load Example"),
                icon = icon("flask"),
                class = "btn btn-info btn-sm",
                style = "margin-bottom: 15px;"
              )
            ),
            
            # CSV file upload with Reload button (only shown in CSV mode)
            conditionalPanel(
              condition = "input.input_type == 'csv'",
              ns = ns,
              uiOutput(ns("csv_file_div")),
              uiOutput(ns("column_validation_message"))
            ),
            
            
            # Data Preview section (shown in CSV and text modes, not single)
            conditionalPanel(
              condition = "input.input_type != 'single'",
              ns = ns,
              tags$p(tags$strong(i18n$t("Data Preview")), style = "margin-top: 15px; margin-bottom: 8px; font-size: 14px; color: #333;"),
              uiOutput(ns("data_preview_ui"))
            ),
            
            # Section 3: Primary Response Settings (combined variable selection + response parameters)
            tags$hr(style = "margin: 15px 0; border-top: 1px dashed #ccc;"),
            tags$p(tags$strong(i18n$t("Primary Response")), style = "margin-top: 10px; margin-bottom: 8px; font-size: 14px; color: #333;"),
            
            # For CSV/text mode: show column selectors (dynamically rendered)
            conditionalPanel(
              condition = "input.input_type != 'single'",
              ns = ns,
              uiOutput(ns("primary_response_ui"))
            ),
            
            # For single mode only: show response value, time, name, unit, etc.
            conditionalPanel(
              condition = "input.input_type == 'single'",
              ns = ns,
              fluidRow(
                column(4, numericInput(ns("single_yield"),
                  tags$span(i18n$t("Response"),
                    input_help(i18n$t("Measured yield or response value at the specified extraction time."),
                               title = i18n$t("Response"), buttonLabel = i18n$t("OK"))), value = defaults$single_yield, min = 0)),
                column(4,
                  textInput(ns("response_name"),
                    tags$span(i18n$t("Response Name"),
                      input_help(i18n$t("A descriptive name for the primary response (e.g. Total Extract, Beta-Carotene). Shown in results tables and plots."),
                                 title = i18n$t("Response Name"), buttonLabel = i18n$t("OK"))), value = defaults$response_name %||% "Primary")
                ),
                column(4, 
                  selectInput(ns("response_unit_single"),
                    tags$span(i18n$t("Response Unit"),
                      input_help(i18n$t("Unit of the response data. Grams for absolute mass, percent or permille for yield relative to raw material."),
                                 title = i18n$t("Response Unit"), buttonLabel = i18n$t("OK"))),
                    choices = c("%" = "percent", "g" = "g", "kg" = "kg"),
                    selected = defaults$response_unit %||% "percent"
                  )
                )
              ),
              fluidRow(
                column(4, uiOutput(ns("single_time_ui"))),
                column(4,
                  numericInput(ns("dilfac_single"),
                    tags$span(i18n$t("Dilution Factor"),
                      input_help(i18n$t("Factor by which the extract is diluted during analysis. The measured response is multiplied by this factor to obtain the true yield."),
                                 title = i18n$t("Dilution Factor"), buttonLabel = i18n$t("OK"))), value = defaults$dilfac, min = 0)
                ),
                column(4,
                  numericInput(ns("pr_sale_single"),
                    tags$span(i18n$t("Sales Price (USD/kg)"),
                      input_help(i18n$t("Selling price of the extract in US dollars per kilogram. Used to calculate revenue and payback period."),
                                 title = i18n$t("Sales Price"), buttonLabel = i18n$t("OK"))), value = defaults$pr_sale, min = 0)
                )
              )
            ),
            
            # Section 4: Additional Responses (dynamically rendered based on data availability)
            uiOutput(ns("additional_responses_section_ui"))
          ),

          # Calculation Mode
          accordionItem(
            title = div(
              i18n$t("Calculation Mode")
            ),
            value = "calc_mode",
            collapsed = TRUE,
            fluidRow(
            ),
            # Process Type Selection
            fluidRow(
              column(12,
                uiOutput(ns("comode_ui"))
              )
            ),
            tags$hr(style = "margin: 10px 0;"),
            # COM Equation Section
            fluidRow(
              column(12,
                tags$label(i18n$t("COM Calculation Method"), class = "control-label", style = "font-weight: bold;"),
                uiOutput(ns("use_coefs_ui")),
                # Dynamic equation display with inline coefficient inputs
                uiOutput(ns("com_equation_display"))
              )
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
              column(6, numericInput(ns("volex"),
                tags$span(i18n$t("Extractor Volume (L)"),
                  input_help(i18n$t("Internal volume of each extraction vessel in liters. Used to calculate apparent density, bed geometry, and equipment purchase cost."),
                             title = i18n$t("Extractor Volume"), buttonLabel = i18n$t("OK"))), value = defaults$volex, min = 0)),
              column(6, uiOutput(ns("load_input_ui"))),
              column(6, uiOutput(ns("pres_input_ui"))),
              column(6, uiOutput(ns("temp_input_ui"))),
              column(6, uiOutput(ns("flow_input_ui"))),
              column(6, numericInput(ns("csol_flow"),
                tags$span(i18n$t("Co-Solvent Flow (mL/min)"),
                  input_help(i18n$t("Volumetric flow rate of co-solvent (e.g. ethanol) in mL/min. Set to 0 if no co-solvent is used."),
                             title = i18n$t("Co-Solvent Flow"), buttonLabel = i18n$t("OK"))), value = defaults$csol_flow, min = 0)),
              column(6, uiOutput(ns("extime_input_ui"))),
              column(6, uiOutput(ns("cosol_loss_input_ui"))),
              column(6, uiOutput(ns("msol_loss_frac_input_ui"))),
              column(6, uiOutput(ns("moisture_content_input_ui")))
            )
          ),

          # Other parameters
          accordionItem(
            title = div(
              i18n$t("Other parameters")
            ),
            value = "other_params",
            collapsed = TRUE,
            # Auxiliary Material Parameters (Dynamic)
            tags$label(i18n$t("Auxiliary Materials"), style = "font-weight: bold; margin-bottom: 5px; display: block;"),
            div(id = ns("aux_materials_container"),
              # Dynamic auxiliary material inputs will be inserted here
              uiOutput(ns("aux_materials_ui"))
            ),
            fluidRow(
              column(12,
                actionButton(ns("add_aux_material"), i18n$t("Add Material"), icon = icon("plus"), class = "btn-success btn-sm", style = "margin-bottom: 10px;")
              )
            ),
            # Flow Measurement Parameters
            tags$hr(style = "margin: 10px 0; border-top: 1px dashed #ccc;"),
            tags$label(i18n$t("Flow Measurement Parameters"), style = "font-weight: bold; margin-bottom: 5px; display: block;"),
            fluidRow(
              column(12, uiOutput(ns("flowpar_auto_ui")))
            ),
            fluidRow(
              column(6, uiOutput(ns("flowpar_pres_input_ui"))),
              column(6, uiOutput(ns("flowpar_temp_input_ui")))
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
              column(6, uiOutput(ns("bh_input_ui"))),
              column(6, uiOutput(ns("id_input_ui"))),
              column(6, numericInput(ns("pr_mat"),
                tags$span(i18n$t("Raw Material Price (USD/kg)"),
                  input_help(i18n$t("Purchase price of the raw material in US dollars per kilogram. The largest contributor to the Cost of Raw Materials (CRM)."),
                             title = i18n$t("Material Price"), buttonLabel = i18n$t("OK"))), value = defaults$pr_mat, min = 0)),
              column(6, numericInput(ns("pr_msol"),
                tags$span(i18n$t("Main Solvent Price (USD/kg)"),
                  input_help(i18n$t("Purchase price of the main solvent (CO2) per kilogram. Only the non-recovered fraction is costed."),
                             title = i18n$t("Solvent Price"), buttonLabel = i18n$t("OK"))), value = defaults$pr_msol, min = 0)),
              column(6, numericInput(ns("pr_csol"),
                tags$span(i18n$t("Co-solvent Price (USD/kg)"),
                  input_help(i18n$t("Purchase price of the co-solvent per kilogram. Only the lost fraction (set in Other Parameters) is costed."),
                             title = i18n$t("Co-solvent Price"), buttonLabel = i18n$t("OK"))), value = defaults$pr_csol, min = 0)),
              column(6, uiOutput(ns("recp_input_ui"))),
              column(6, uiOutput(ns("rect_input_ui"))),
              column(6, uiOutput(ns("sept_input_ui")))
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
              column(6, numericInput(ns("pw_main"),
                tags$span(i18n$t("Main Power (kW)"),
                  input_help(i18n$t("Power consumption of the main extraction system (pump, heater, cooler) in kilowatts. The primary contributor to the Cost of Utilities (CUT)."),
                             title = i18n$t("Main Power"), buttonLabel = i18n$t("OK"))), value = defaults$pw_main, min = 0)),
              column(6, numericInput(ns("pr_kwh"),
                tags$span(i18n$t("Energy Price (USD/kWh)"),
                  input_help(i18n$t("Electricity cost in US dollars per kilowatt-hour. Applied to all power-consuming operations."),
                             title = i18n$t("Energy Price"), buttonLabel = i18n$t("OK"))), value = defaults$pr_kwh, min = 0)),
              column(6, numericInput(ns("pw_dry"),
                tags$span(i18n$t("Drying Power (kW)"),
                  input_help(i18n$t("Power consumption of the drying equipment in kilowatts."),
                             title = i18n$t("Drying Power"), buttonLabel = i18n$t("OK"))), value = defaults$pw_dry, min = 0)),
              column(6, numericInput(ns("cap_dry"),
                tags$span(i18n$t("Drying Capacity (kg/h)"),
                  input_help(i18n$t("Processing capacity of the dryer in kilograms per hour."),
                             title = i18n$t("Drying Capacity"), buttonLabel = i18n$t("OK"))), value = defaults$cap_dry, min = 0)),
              column(6, numericInput(ns("pw_com"),
                tags$span(i18n$t("Comminution Power (kW)"),
                  input_help(i18n$t("Power consumption of the grinding/milling equipment in kilowatts."),
                             title = i18n$t("Comminution Power"), buttonLabel = i18n$t("OK"))), value = defaults$pw_com, min = 0)),
              column(6, numericInput(ns("cap_com"),
                tags$span(i18n$t("Comminution Capacity (kg/h)"),
                  input_help(i18n$t("Processing capacity of the grinder/mill in kilograms per hour."),
                             title = i18n$t("Comminution Capacity"), buttonLabel = i18n$t("OK"))), value = defaults$cap_com, min = 0)),
              column(6, numericInput(ns("pw_evap"),
                tags$span(i18n$t("Evaporation Power (kW)"),
                  input_help(i18n$t("Power consumption of the evaporation/solvent recovery unit in kilowatts."),
                             title = i18n$t("Evaporation Power"), buttonLabel = i18n$t("OK"))), value = defaults$pw_evap, min = 0)),
              column(6, numericInput(ns("cap_evap"),
                tags$span(i18n$t("Evaporation Capacity (kg/h)"),
                  input_help(i18n$t("Processing capacity of the evaporator in kilograms per hour."),
                             title = i18n$t("Evaporation Capacity"), buttonLabel = i18n$t("OK"))), value = defaults$cap_evap, min = 0))
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
              column(6, numericInput(ns("oper"),
                tags$span(i18n$t("Number of Operators"),
                  input_help(i18n$t("Number of personnel operating the extraction system per shift. Used to calculate the Cost of Labor (COL)."),
                             title = i18n$t("Operators"), buttonLabel = i18n$t("OK"))), value = defaults$oper, min = 0)),
              column(6, numericInput(ns("whr"),
                tags$span(i18n$t("Shift Duration (hours)"),
                  input_help(i18n$t("Duration of one work shift in hours."),
                             title = i18n$t("Shift Duration"), buttonLabel = i18n$t("OK"))), value = defaults$whr, min = 0)),
              column(6, numericInput(ns("shifts"),
                tags$span(i18n$t("Number of Shifts"),
                  input_help(i18n$t("Number of work shifts per day."),
                             title = i18n$t("Shifts"), buttonLabel = i18n$t("OK"))), value = defaults$shifts, min = 0)),
              column(6, numericInput(ns("wage"),
                tags$span(i18n$t("Monthly Wage (USD)"),
                  input_help(i18n$t("Monthly salary per operator in US dollars."),
                             title = i18n$t("Wage"), buttonLabel = i18n$t("OK"))), value = defaults$wage, min = 0)),
              column(6, numericInput(ns("wdays"),
                tags$span(i18n$t("Working Days (monthly)"),
                  input_help(i18n$t("Number of working days per month."),
                             title = i18n$t("Working Days"), buttonLabel = i18n$t("OK"))), value = defaults$wdays, min = 0))
            )
          ),

          # Fixed Costs and Tax Rate
          accordionItem(
            title = div(
              i18n$t("Fixed Costs and Tax Rate"),
            ),
            value = "fci_params",
            collapsed = TRUE,
            fluidRow(
              column(6, numericInput(ns("capex"),
                tags$span(i18n$t("Capital Investment (USD)"),
                  input_help(i18n$t("Total capital expenditure (CAPEX) for the extraction system in US dollars. Can be estimated automatically using Turton et al. coefficients."),
                             title = i18n$t("CAPEX"), buttonLabel = i18n$t("OK"))), value = defaults$capex, min = 0)),
              column(6, numericInput(ns("maint"),
                tags$span(i18n$t("Maintenance Cost (USD/month)"),
                  input_help(i18n$t("Monthly maintenance and repair costs in US dollars."),
                             title = i18n$t("Maintenance"), buttonLabel = i18n$t("OK"))), value = defaults$maint, min = 0)),
              column(6, numericInput(ns("other"),
                tags$span(i18n$t("Other Fixed Costs (USD/month)"),
                  input_help(i18n$t("Other monthly fixed costs such as insurance, rent, and administrative overhead."),
                             title = i18n$t("Other Costs"), buttonLabel = i18n$t("OK"))), value = defaults$other, min = 0)),
              column(6, uiOutput(ns("depr_input_ui"))),
              column(6, uiOutput(ns("taxrate_input_ui")))
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
        id = ns("results_tabs"),
        width = NULL,
        height = "calc(100vh - 100px)",
        
        # Tab 1 - Detailed Results (MOVED TO FIRST POSITION)
        tabPanel(
          title = i18n$t("Detailed Results"),
          value = "detailed",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.show_export_all_results",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("list-alt", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and run the calculation to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.show_export_all_results",
            ns = ns,
            
            # Export All Results Button (MOVED HERE)
            div(
              id = ns("export_button_wrapper_detailed"),
              fluidRow(
                column(12,
                  style = "display: flex; justify-content: center; margin-bottom: 15px;",
                  downloadButton(ns("download_all_results"), i18n$t("Export Results"), class = "btn btn-success btn-lg", style = "margin-left:5px;")
                )
              )
            ),
            
            # Time selection slider (only shown for multi-value data)
            conditionalPanel(
              condition = "output.has_multi_data",
              ns = ns,
              fluidRow(
                column(12, sliderInput(ns("time_slider_detailed"), i18n$t("Select Process Time (min)"),
                  min = 0, max = 1, value = 0.5, step = 1, width = "100%"
                )),
                column(12, uiOutput(ns("optimal_time_buttons_detailed")))
              )
            ),
            
            # Cost Breakdown Overview - Charts Row
            fluidRow(
              # Pie Chart for COM Components
              box(
                title = tagList(
                  i18n$t("Cost of Manufacturing Breakdown"),
                  tags$span(style = "float: right; margin-left: 10px;",
                    downloadButton(ns("export_com_breakdown_csv"), label = NULL,
                                   class = "btn-xs btn-default",
                                   style = "padding: 2px 6px; font-size: 11px;",
                                   icon = icon("download")),
                    tags$script(HTML(sprintf(
                      "document.querySelector('#%s').title = '%s';",
                      ns("export_com_breakdown_csv"),
                      i18n$t("Export plot data as CSV")
                    )))
                  )
                ),
                width = 5,
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput(ns("com_pie_chart"), height = "350px")
              ),
              # Horizontal Bar Chart for Cost Categories
              box(
                title = tagList(
                  i18n$t("Monthly Cost Summary"),
                  tags$span(style = "float: right; margin-left: 10px;",
                    downloadButton(ns("export_cost_summary_csv"), label = NULL,
                                   class = "btn-xs btn-default",
                                   style = "padding: 2px 6px; font-size: 11px;",
                                   icon = icon("download")),
                    tags$script(HTML(sprintf(
                      "document.querySelector('#%s').title = '%s';",
                      ns("export_cost_summary_csv"),
                      i18n$t("Export plot data as CSV")
                    )))
                  )
                ),
                width = 7,
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput(ns("cost_waterfall_chart"), height = "350px")
              )
            ),
            
            # Summary Cards Row
            fluidRow(
              column(12,
                box(
                  title = i18n$t("Cost Components at a Glance"),
                  width = NULL,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  fluidRow(
                    valueBoxOutput(ns("crm_value_box"), width = 3),
                    valueBoxOutput(ns("cut_value_box"), width = 3),
                    valueBoxOutput(ns("col_value_box"), width = 3),
                    valueBoxOutput(ns("fci_value_box"), width = 3)
                  )
                )
              )
            ),
            
            # Detailed Tables in Accordion
            fluidRow(
              column(12,
                box(
                  title = i18n$t("Detailed Cost Breakdown"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  # Materials & Requirements Section
                  shinydashboardPlus::accordion(
                    id = ns("detailed_accordion"),
                    
                    # Production Overview
                    accordionItem(
                      title = i18n$t("Production Overview"),
                      value = "production_overview",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("production_table")),
                      # Per-response extract breakdown (shown when multiple responses)
                      uiOutput(ns("per_response_extract_ui")),
                      br()
                    ),
                    
                    # Raw Materials (CRM Components)
                    accordionItem(
                      title = i18n$t("Cost of Raw Materials (CRM)"),
                      value = "crm_details",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("crm_table")),
                      br()
                    ),
                    
                    # Utilities (CUT Components)
                    accordionItem(
                      title = i18n$t("Cost of Utilities (CUT)"),
                      value = "cut_details",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("cut_table")),
                      br()
                    ),
                    
                    # COL & FMC
                    accordionItem(
                      title = i18n$t("Cost of Labour (COL) and Fixed Manufacturing Costs (FMC)"),
                      value = "col_fmc_details",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("col_fmc_table")),
                      br()
                    ),
                    
                    # Financial Results
                    accordionItem(
                      title = i18n$t("Financial Results"),
                      value = "financial_results",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("financial_table")),
                      # Per-response revenue breakdown (shown when multiple responses)
                      uiOutput(ns("per_response_revenue_ui")),
                      br()
                    )
                  )
                )
              )
            ),
            
            # Input Parameters Section
            fluidRow(
              column(12,
                box(
                  title = i18n$t("Input Parameters"),
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  shinydashboardPlus::accordion(
                    id = ns("input_params_accordion"),
                    
                    # General Parameters
                    accordionItem(
                      title = i18n$t("General Parameters"),
                      value = "input_general",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("input_general_table")),
                      br()
                    ),
                    
                    # Cost of Raw Materials Parameters
                    accordionItem(
                      title = i18n$t("Raw Material Parameters"),
                      value = "input_crm",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("input_crm_table")),
                      br()
                    ),
                    
                    # Cost of Utilities Parameters
                    accordionItem(
                      title = i18n$t("Utility Parameters"),
                      value = "input_cut",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("input_cut_table")),
                      br()
                    ),
                    
                    # Labour Parameters
                    accordionItem(
                      title = i18n$t("Labour Parameters"),
                      value = "input_col",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("input_col_table")),
                      br()
                    ),
                    
                    # Fixed Costs Parameters
                    accordionItem(
                      title = i18n$t("Fixed Cost Parameters"),
                      value = "input_fci",
                      collapsed = TRUE,
                      DT::dataTableOutput(ns("input_fci_table")),
                      br()
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Tab 2 - General Results
        tabPanel(
          title = i18n$t("General Results"),
          value = "general",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.show_export_all_results",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("chart-line", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and run the calculation to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.show_export_all_results",
            ns = ns,

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
              condition = "output.has_multi_data",
              ns = ns,
              fluidRow(
                column(12, sliderInput(ns("time_slider"), i18n$t("Select Process Time (min)"),
                  min = 0, max = 1, value = 0.5, step = 1, width = "100%"
                )),
                column(12, uiOutput(ns("optimal_time_buttons")))
              )
            ),

            # ROI and Payback Chart
            fluidRow(
              box(
                title = i18n$t("5-Year ROI and Payback Period"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                plotlyOutput(ns("roi_plot"), height = "300px")
              )
            ),

            # Monthly Table
            fluidRow(
              box(
                title = i18n$t("Monthly Table"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("roi_monthly_table")),
                br()
              )
            )
          )
        ),


        # Tab 3 - Plot Analysis
        tabPanel(
          title = i18n$t("Optimal Performance Window"),
          value = "plot_analysis",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.show_export_all_results",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("chart-area", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and run the calculation to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.show_export_all_results",
            ns = ns,
            conditionalPanel(
              condition = "!output.has_multi_data",
              ns = ns,
              h4(i18n$t("Plots are available only when multiple time-yield data points are provided."))
            ),
            conditionalPanel(
              condition = "output.has_multi_data",
              ns = ns,
              fluidRow(
                box(
                  title = tagList(
                    i18n$t("Payback Plot"),
                    tags$span(style = "float: right; margin-left: 10px;",
                      downloadButton(ns("export_payback_csv"), label = NULL,
                                     class = "btn-xs btn-default",
                                     style = "padding: 2px 6px; font-size: 11px;",
                                     icon = icon("download")),
                      tags$script(HTML(sprintf(
                        "document.querySelector('#%s').title = '%s';",
                        ns("export_payback_csv"),
                        i18n$t("Export plot data as CSV")
                      )))
                    )
                  ),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput(ns("payback_plot"), height = "300px"),
                  uiOutput(ns("payback_y_range_ui"))
                )
              ),
              fluidRow(
                box(
                  title = tagList(
                    i18n$t("Specific Cost Plot"),
                    tags$span(style = "float: right; margin-left: 10px;",
                      downloadButton(ns("export_sc_csv"), label = NULL,
                                     class = "btn-xs btn-default",
                                     style = "padding: 2px 6px; font-size: 11px;",
                                     icon = icon("download")),
                      tags$script(HTML(sprintf(
                        "document.querySelector('#%s').title = '%s';",
                        ns("export_sc_csv"),
                        i18n$t("Export plot data as CSV")
                      )))
                    )
                  ),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput(ns("sc_plot"), height = "300px"),
                  uiOutput(ns("sc_y_range_ui"))
                )
              )
            )
          )
        ),
        # Tab 4 - Performance Overview
        tabPanel(
          title = i18n$t("Performance Overview"),
          value = "performance_overview",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.show_export_all_results",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("table", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and run the calculation to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.show_export_all_results",
            ns = ns,
            conditionalPanel(
              condition = "output.has_multi_data",
              ns = ns,
              box(
                title = i18n$t("Performance Overview"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("performance_overview_table")),
                br()
              )
            ),
            conditionalPanel(
              condition = "!output.has_multi_data",
              ns = ns,
              h4(i18n$t("This overview is available only when multiple time-yield data points are provided."))
            )
          )
        ),
      )
    )
  )
}
