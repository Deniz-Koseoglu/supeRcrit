# BIC Kinetic Modeling UI Module
kinetic_bic_ui <- function(id, defaults, i18n) {
  ns <- NS(id)


  tagList(
    column(
      width = 5,
      fluidRow(
        column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(4, actionButton(ns("calculate"), i18n$t("Calculate"), class = "btn btn-primary btn-block", style = "color: white;"))
      ),
      br(),
      box(
        title = div(
          style = "display: flex; align-items: center;",
          span(i18n$t("Broken-And-Intact Cells")),
          div(uiOutput(ns("kinetic_bic_HELP"))),
          div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
        ),
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
            # Step 1: Select input type with Load Example button inline
            tags$p(tags$strong(i18n$t("1. Data Source")), style = "margin-top: 10px; margin-bottom: 8px; font-size: 14px; color: #333;"),
            div(style = "display: flex; align-items: baseline; gap: 15px; flex-wrap: wrap;",
              div(style = "margin-bottom: 0;",
                radioButtons(ns("input_type"), NULL,
                  choiceNames = list(i18n$t("Upload CSV File"), i18n$t("Manual Input")),
                  choiceValues = c("csv", "manual"),
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
            uiOutput(ns("data_input_ui")), # Dynamic UI for CSV upload (only shown in CSV mode)
            
            # Data Preview section title (styled like other section headers)
            tags$p(tags$strong(i18n$t("2. Data Preview")), style = "margin-top: 15px; margin-bottom: 8px; font-size: 14px; color: #333;"),
            
            # Data preview (shown in both modes)
            uiOutput(ns("data_preview_ui")),
            
            # Step 3: Variable selection and data options (extra top margin for spacing)
            tags$p(tags$strong(i18n$t("3. Variables & Options")), style = "margin-top: 20px; margin-bottom: 8px; font-size: 14px; color: #333;"),
            
            # Warning for insufficient columns
            uiOutput(ns("insufficient_columns_warning_ui")),
            
            # Line 1: All variable selectors (Solvent only shown when checkbox is checked)
            fluidRow(
              column(3, div(id = ns("oec_x_var_ui_wrapper"), uiOutput(ns("oec_x_var_ui")))),
              column(4, div(id = ns("oec_y_var_ui_wrapper"), uiOutput(ns("oec_y_var_ui")))),
              column(5, div(id = ns("oec_slv_var_ui_wrapper"), uiOutput(ns("oec_slv_var_ui"))))
            ),
            
            # Line 2: All checkboxes floated left
            div(style = "margin-top: 5px;",
              tags$span(style = "display: inline-block; margin-right: 20px;",
                uiOutput(ns("cumulative_checkbox_ui"), inline = TRUE)
              ),
              tags$span(style = "display: inline-block; margin-right: 20px;",
                uiOutput(ns("use_solvent_checkbox_ui"), inline = TRUE)
              ),
              tags$span(style = "display: inline-block;",
                uiOutput(ns("mass_flow_input_section_ui"))
              )
            ),
            
            # Warning and info messages (all in same location)
            uiOutput(ns("duplicate_column_warning_ui")),
            uiOutput(ns("cumulative_warning_ui")),
            uiOutput(ns("solvent_units_info_ui"))
          ),
          accordionItem(
            title = div(
              i18n$t("Process Parameters")
            ),
            value = "process_params_section",
            collapsed = TRUE,
            fluidRow(
              column(6, div(
                id = ns("resp_units_wrapper"),
                uiOutput(ns("resp_units_ui"))
              )),
              column(6, uiOutput(ns("pres_ui"))),
              column(6, uiOutput(ns("temp_ui"))),
              column(6, uiOutput(ns("mass_in_ui"))),
              column(6, uiOutput(ns("moisture_ui"))),
              column(6, uiOutput(ns("D_ui"))),
              column(6, uiOutput(ns("L_ui"))),
              column(6, uiOutput(ns("dr_ui"))),
              column(6, uiOutput(ns("dp_ui"))),
              column(6, uiOutput(ns("n_input_ui"))),
              column(6, uiOutput(ns("cu_ui")))
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Flow Parameters")
            ),
            value = "flow_params_section",
            collapsed = TRUE,
            # Conditional message when solvent column is selected
            uiOutput(ns("flow_params_disabled_message")),
            fluidRow(
              column(6, div(id = ns("flow_wrapper"), uiOutput(ns("flow_input_ui")))),
              column(6, div(
                id = ns("flow_units_wrapper"),
                uiOutput(ns("flow_units_ui"))
              )),
              column(6, uiOutput(ns("flowpar_pres_ui"))),
              column(6, uiOutput(ns("flowpar_temp_ui"))),
              column(6, uiOutput(ns("etoh_input_ui"))),
              column(6, uiOutput(ns("etoh_frac_ui")))
            ),
            # Hidden checkbox to store mass_flow state (used by toggle button)
            div(style = "display: none;",
              checkboxInput(ns("mass_flow"), "Mass Flow Rate", value = defaults$mass_flow)
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Advanced Parameters")
            ),
            value = "advanced_params_section",
            collapsed = TRUE,
            fluidRow(
              column(6, uiOutput(ns("ro_co2_ui"))),
              column(6, uiOutput(ns("tmax_ui"))),
              column(6, uiOutput(ns("qmax_ui")))
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Model Settings")
            ),
            value = "model_settings_section",
            collapsed = TRUE,
            fluidRow(
              column(6, uiOutput(ns("modtype_ui"))),
              column(6, div(
                id = ns("aggreg_wrapper"),
                uiOutput(ns("aggreg_ui"))
              ))
            )
          ),
          accordionItem(
            title = div(
              i18n$t("Optimization Parameters")
            ),
            value = "optimization_params_section",
            collapsed = TRUE,
            fluidRow(
              column(6, uiOutput(ns("r_est_ui"))),
              column(6, uiOutput(ns("ksas_est_ui"))),
              column(6, uiOutput(ns("qc_est_ui"))),
              column(6, uiOutput(ns("thetaf_est_ui"))),
              column(6, uiOutput(ns("ti_est_ui"))),
              column(6, uiOutput(ns("kf_est_ui"))),
              column(12,
                div(style = "margin-top: 5px;",
                  actionButton(ns("reset_opt_params"), i18n$t("Reset to Defaults"),
                    icon = icon("undo"), class = "btn btn-default btn-sm"
                  )
                )
              )
            )
          )
        )
      )
    ),


    # Right Panel - Results
    column(
      width = 7,
      tabBox(
        id = ns("results_tabs"),
        width = NULL,
        height = NULL,
        # Tab 1: Model Summary (unified)
        tabPanel(
          title = i18n$t("Model Summary"),
          value = "model_summary",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.has_any_results",
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
            condition = "output.has_any_results",
            ns = ns,
            # Export Results button (green, large)
            div(
              style = "display: flex; justify-content: center; margin-bottom: 15px;",
              downloadButton(ns("export_kinetic"), i18n$t("Export Results"), class = "btn btn-success btn-lg", style = "margin-left:5px;")
            ),
            # Model Comparison table (with highlighted best metrics)
            fluidRow(
              column(12, box(
                title = i18n$t("Model Comparison"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("model_comparison_table"))
              ))
            ),
            # Model selector + x-axis toggle inline
            div(
              style = "display: flex; align-items: flex-start; gap: 0; margin-bottom: 10px;",
              div(style = "flex: 1;", uiOutput(ns("model_detail_selector_ui"))),
              div(style = "border-left: 1px solid #ccc; height: 50px; margin: 5px 15px 0 15px;"),
              div(style = "flex: 1;", uiOutput(ns("plot_xaxis_toggle_ui")))
            ),
            # Model Plot
            fluidRow(
              column(12,
                plotlyOutput(ns("selected_model_plot")),
                div(style = "margin-bottom: 15px;")
              )
            ),
            # Model Parameters (combined with Fitted column)
            fluidRow(
              column(12, box(
                title = i18n$t("Model Parameters"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("unified_mod_pars_table"))
              ))
            ),
            # Observed vs Predicted Data
            fluidRow(
              column(12, box(
                title = i18n$t("Observed vs Predicted Data"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("unified_ordt_table"))
              ))
            ),
            # Detailed Predictions
            fluidRow(
              column(12, box(
                title = i18n$t("Detailed Predictions"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("unified_mdt_table"))
              ))
            )
          )
        ),
        # Tab 2: Predictions
        tabPanel(
          title = i18n$t("Predictions"),
          value = "predictions_tab",
          conditionalPanel(
            condition = "output.has_full_result",
            ns = ns,
            fluidRow(
              column(
                width = 12,
                box(
                  title = i18n$t("Prediction Inputs"),
                  width = NULL,
                  status = "success",
                  solidHeader = TRUE,
                  # Row 1: Model, Input Units, Calculate Yields inline (flexbox)
                  div(
                    style = "display: flex; align-items: flex-end; gap: 15px; margin-bottom: 5px;",
                    div(style = "flex: 1;", uiOutput(ns("predict_model_selector_ui"))),
                    div(style = "flex: 1;", uiOutput(ns("predict_units_ui"))),
                    div(style = "flex: 0 0 auto; padding-bottom: 10px;", uiOutput(ns("predict_get_yields_ui")))
                  ),
                  # Row 2: Input Format (inline)
                  radioButtons(ns("predict_input_type"),
                    i18n$t("Input Format:"),
                    choiceNames = list(i18n$t("Manual Input"), i18n$t("Sequence Input"), i18n$t("Upload CSV")),
                    choiceValues = c("manual", "sequence", "csv"),
                    selected = "manual",
                    inline = TRUE
                  ),
                  # Data input area
                  uiOutput(ns("predict_data_input_ui")),
                  uiOutput(ns("predict_range_warning_ui")),
                  div(style = "margin-top: 5px;"),
                  # Buttons at bottom
                  fluidRow(
                    column(6,
                      actionButton(ns("predict_calculate"),
                        i18n$t("Calculate Predictions"),
                        class = "btn btn-primary btn-block", icon = icon("calculator"),
                        style = "color: white !important;"
                      )
                    ),
                    column(6,
                      actionButton(ns("pred_reset_btn"),
                        i18n$t("Reset"),
                        class = "btn btn-danger btn-block", icon = icon("refresh"),
                        style = "color: white !important;"
                      )
                    )
                  )
                )
              )
            ),
            # Results (conditional)
            conditionalPanel(
              condition = "output.has_bic_predict_results",
              ns = ns,
              fluidRow(
                column(12, box(
                  title = i18n$t("Prediction Results"),
                  width = NULL, status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput(ns("predict_results_table"))
                ))
              ),
              fluidRow(
                column(12, box(
                  title = i18n$t("Unit Reference Table"),
                  width = NULL, status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput(ns("predict_unit_chart"))
                ))
              ),
              fluidRow(
                column(12, box(
                  title = i18n$t("Model Information"),
                  width = NULL, status = "primary", solidHeader = TRUE,
                  htmlOutput(ns("predict_description"))
                ))
              )
            )
          )
        ),

      )
    )
  )
}
