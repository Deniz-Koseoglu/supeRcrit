# DOE Analysis UI Module
doe_analysis_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
        .modal-backdrop {
          opacity: 0.3 !important;
        }
        .modal-dialog {
          margin-top: 50px;
        }
        /* Custom CSS for Diagnostics tabs */
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background-color: #e6f7ff !important; /* Light blue for active tab */
          color: #333 !important;
          border-top-color: #007bff !important;
        }
        .nav-tabs-custom > .nav-tabs > li > a {
          background-color: #f0f0f0 !important; /* Light gray for inactive tabs */
          color: #555 !important;
        }
        .nav-tabs-custom > .nav-tabs > li > a:hover {
          background-color: #e0e0e0 !important; /* Slightly darker gray on hover */
        }
        /*
        .tab-content {
          border: 1px solid #ddd;
          border-top: none;
          padding: 15px;
          background-color: #fff;
        }
        */
        # ... (önceki tags$head içeriği) ...
        /* Total Result Tab Styling (YENİ EKLEME) */
        .main-report-title {
          text-align: center;
          margin-bottom: 30px;
        }
        .main-report-title h2 {
          color: #0056b3; /* Koyu mavi */
          font-size: 2.2em;
          border-bottom: 2px solid #0056b3;
          padding-bottom: 10px;
          margin-top: 20px;
        }
        .report-section {
          background-color: #f8f9fa; /* Hafif gri */
          border: 1px solid #e9ecef;
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 25px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.05); /* Yumuşak gölge */
        }
        .report-section h3 {
          color: #007bff; /* Bootstrap primary blue */
          font-size: 1.6em;
          border-bottom: 1px solid #dee2e6;
          padding-bottom: 8px;
          margin-top: 0;
          margin-bottom: 18px;
        }
        .statement-item {
          position: relative;
          padding: 12px 18px;
          margin-bottom: 12px;
          border-radius: 6px;
          border-left: 6px solid; /* Renk kodlaması için sol kenarlık */
          box-shadow: 0 2px 4px rgba(0,0,0,0.03);
          background-color: #ffffff; /* Beyaz arka plan */
        }
        .statement-item p {
          margin: 0;
          line-height: 1.5;
          font-size: 0.95em;
        }
        .statement-item strong {
          font-weight: 700;
          color: #000; /* Kalın metin rengi */
        }

        /* Statement kategorilerine özel renkler */
        .model-summary { border-color: #007bff; background-color: #e7f3ff; }
        .model-equation { border-color: #343a40; background-color: #e2e6ea; } /* Koyu gri */
        .model-performance { border-color: #28a745; background-color: #e6ffed; }
        .trimming-info { border-color: #ffc107; background-color: #fff8e6; }
        .optimization-details { border-color: #6f42c1; background-color: #f5f0fa; }
        .general-statement { border-color: #6c757d; background-color: #f0f2f5; }
        .warning-statement { border-color: #dc3545; background-color: #ffe0e4; color: #dc3545; }

        /* Anahtar kelimeler için özel vurgulama */
        .highlight-max { color: #28a745; font-weight: bold; } /* Green for maximum */
        .highlight-min { color: #007bff; font-weight: bold; } /* Blue for minimum */
        .highlight-warning { color: #dc3545; font-weight: bold; } /* Red for warnings like 'out of range' */
      "))
    ),
    column(
      width = 5,
      fluidRow(
        column(4, div(id = ns("reset_wrapper"), actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block"))),
        column(4, div(id = ns("analyze_wrapper"), actionButton(ns("analyze"), i18n$t("Analyze"), class = "btn btn-primary btn-block", style = "color: white !important;"))),
        column(4, div(id = ns("save_analysis_wrapper"), actionButton(ns("save_analysis"), i18n$t("Save Analysis"), class = "btn btn-default btn-block")))
      ),
      br(),
      box(
        title = div(
          style = "display: flex; align-items: center;",
          span(i18n$t("DOE Analysis")),
          div(uiOutput(ns("doe_analysis_HELP"))),
          div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
        ),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        shinydashboardPlus::accordion(
          id = ns("doe_param_accordion"),

          # Data Source Section
          accordionItem(
            title = div(
              i18n$t("Data Source"),
            ),
            value = "data_source_section",
            collapsed = FALSE,
            div(style = "display: flex; align-items: baseline; gap: 15px; flex-wrap: wrap;",
              div(style = "margin-bottom: 0;",
                radioButtons(ns("data_source"), NULL,
                  choiceNames = list(i18n$t("Saved Designs"), i18n$t("Import CSV")),
                  choiceValues = c("saved", "csv"),
                  selected = defaults$data_source,
                  inline = TRUE
                )
              ),
              actionButton(ns("load_example_data"), i18n$t("Load Example"),
                icon = icon("flask"),
                class = "btn btn-info btn-sm",
                style = "margin-bottom: 15px;"
              )
            ),

            # Saved Designs Selection
            conditionalPanel(
              condition = "input.data_source == 'saved'",
              ns = ns,
              fluidRow(
                column(9, div(
                  id = ns("saved_design_wrapper"),
                  selectInput(ns("saved_design"), i18n$t("Select Saved Design"), choices = NULL)
                )),
                column(3, div(
                  style = "margin-top: 25px;",
                  actionButton(ns("load_saved_design"), i18n$t("Load"), 
                    icon = icon("upload"),
                    class = "btn btn-primary btn-block",
                    style = "color: white;"
                  )
                ))
              )
            ),

            # JSON Import
            # conditionalPanel(
            #   condition = "input.data_source == 'import'",
            #   ns = ns,
            #   fluidRow(
            #     column(12, fileInput(ns("import_file"), i18n$t("Import JSON File"), accept = ".json"))
            #   )
            # ),

            # CSV Import
            conditionalPanel(
              condition = "input.data_source == 'csv'",
              ns = ns,
              fluidRow(
                column(12, uiOutput(ns("import_file_csv_div")))
              )
            ),

            # Data Preview section
            tags$p(tags$strong(i18n$t("Data Preview")), style = "margin-top: 15px; margin-bottom: 8px; font-size: 14px; color: #333;"),
            uiOutput(ns("data_preview_ui"))
          ),

          # Response and Factor Selection Section
          accordionItem(
            title = div(
              i18n$t("Response and Factor Selection"),
            ),
            value = "response_factor_selection",
            collapsed = FALSE,
            fluidRow(
              column(12, uiOutput(ns("column_validation_message"))),
              column(6, div(
                id = ns("time_var_wrapper"),
                uiOutput(ns("time_var_ui"))
              )),
              column(6, div(
                id = ns("response_var_wrapper"),
                uiOutput(ns("response_var_ui"))
              ))
            ),
            # Coded factors info and uncoded factor associations
            fluidRow(
              column(12, uiOutput(ns("coded_factors_info_ui")))
            )
          ),

          # Analysis Parameters Section
          accordionItem(
            title = div(
              i18n$t("Analysis Parameters"),
            ),
            value = "analysis_params",
            collapsed = FALSE,
            fluidRow(
              column(6, div(
                id = ns("mod_order_wrapper"),
                uiOutput(ns("mod_order_ui"))
              )),
              column(6, div(
                id = ns("p_cutoff_wrapper"),
                uiOutput(ns("p_cutoff_ui"))
              ))
            ),
            fluidRow(
              column(6, div(
                id = ns("trim_method_wrapper"),
                uiOutput(ns("trim_method_ui"))
              )),
              column(6, div(
                id = ns("canon_thres_wrapper"),
                uiOutput(ns("canon_thres_ui"))
              ))
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

        # Tab 1 - Model Results
        tabPanel(
          title = i18n$t("Model Summary"),
          value = "model_results",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.has_results",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("chart-bar", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and run the analysis to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.has_results",
            ns = ns,
            # Export All Results (top)
            div(
              id = ns("export_all_results_wrapper"),
              fluidRow(
                column(12,
                  style = "display: flex; justify-content: center;",
                  downloadButton(ns("export_all_results"), i18n$t("Export Results"), class = "btn btn-success btn-lg", style = "margin-left:5px;")
                )
              ),
              br()
            ),

            # Initial and Final Model Comparison
            fluidRow(
              column(
                6,
                box(
                  title = i18n$t("Initial Model Summary"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  htmlOutput(ns("initial_model_summary"))
                )
              ),
              column(
                6,
                box(
                  title = i18n$t("Final Model Summary"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  htmlOutput(ns("final_model_summary"))
                )
              )
            ),

            # Model Trimming Information
            fluidRow(
              column(
                12,
                box(
                  title = i18n$t("Model Trimming Information"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  htmlOutput(ns("trimming_info"))
                )
              )
            ),

            # Optimal Conditions - FINAL MODEL
            fluidRow(
              column(
                12,
                box(
                  title = i18n$t("Optimal Conditions - Final Model (Canonical Analysis)"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  DT::dataTableOutput(ns("optimal_conditions"))
              )
            )
          ),

          # Canonical Analysis Summary
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Canonical Analysis Summary"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                htmlOutput(ns("canonical_analysis_summary"))
              )
            )
          )
          ) # Close conditionalPanel for results
        ),

        # Tab 2 - Optimization Results (with sub-tabs for Initial and Final models)
        tabPanel(
          title = i18n$t("Optimization Results"),
          value = "optimization_results",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.has_results",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("sliders-h", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and run the analysis to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.has_results",
            ns = ns,
            tabsetPanel(
              id = ns("optimization_model_tabs"),
              selected = "final_optimization",

              # Sub-tab: Final Model Optimization
            tabPanel(
              title = i18n$t("Final Model"),
              value = "final_optimization",
              br(),

              # Optimization Methods Summary
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Optimization Methods Summary"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    htmlOutput(ns("optimization_methods_summary_final"))
                  )
                )
              ),

              # Stationary Point Type and Predicted Response
              fluidRow(
                column(
                  6,
                  box(
                    title = i18n$t("Stationary Point Type"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("optimization_type_final"))
                  )
                ),
                column(
                  6,
                  box(
                    title = i18n$t("Predicted Response at Stationary Point"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("predicted_response_final"))
                  )
                )
              ),

              # Optimal Conditions Detailed (Coded & Decoded)
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Stationary Point (Coded and Decoded)"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("optimal_conditions_detailed_final"))
                  )
                )
              ),

              # Min/Max Comparison
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Min/Max Response Comparison (Traditional Optimization)"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("minmax_comparison_final"))
                  )
                )
              ),

              # Steepest Ascent
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Steepest Ascent Path"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("steepest_ascent_table_final"))
                  )
                )
              ),

              # Eigenvalues and Warnings
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Eigenvalues Analysis"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("eigenvalues_table_final"))
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Optimization Notes"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("ca_warnings_final"))
                  )
                )
              )
            ),

            # Sub-tab: Initial Model Optimization
            tabPanel(
              title = i18n$t("Initial Model"),
              value = "initial_optimization",
              br(),

              # Info box about initial vs final
              fluidRow(
                column(
                  12,
                  div(
                    class = "alert alert-info",
                    style = "margin: 0 15px 20px 15px;",
                    icon("info-circle"),
                    HTML(" <strong>Note:</strong> The Initial Model shows optimization results <em>before</em> model trimming.
                         If no trimming was performed (no insignificant terms removed), Initial and Final models will be identical.")
                  )
                )
              ),

              # Optimization Methods Summary
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Optimization Methods Summary"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    htmlOutput(ns("optimization_methods_summary_initial"))
                  )
                )
              ),

              # Stationary Point Type and Predicted Response
              fluidRow(
                column(
                  6,
                  box(
                    title = i18n$t("Stationary Point Type"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("optimization_type_initial"))
                  )
                ),
                column(
                  6,
                  box(
                    title = i18n$t("Predicted Response at Stationary Point"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("predicted_response_initial"))
                  )
                )
              ),

              # Optimal Conditions Detailed (Coded & Decoded)
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Stationary Point (Coded and Decoded)"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("optimal_conditions_detailed_initial"))
                  )
                )
              ),

              # Min/Max Comparison
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Min/Max Response Comparison (Traditional Optimization)"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("minmax_comparison_initial"))
                  )
                )
              ),

              # Steepest Ascent
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Steepest Ascent Path"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("steepest_ascent_table_initial"))
                  )
                )
              ),

              # Eigenvalues and Warnings
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Eigenvalues Analysis"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("eigenvalues_table_initial"))
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Optimization Notes"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("ca_warnings_initial"))
                  )
                )
              )
            )
          ) # Close tabsetPanel
          ) # Close conditionalPanel for results
        ),


        # Tab 3 - Diagnostics
        tabPanel(
          title = i18n$t("Diagnostics"),
          value = "diagnostics",
          tabsetPanel(
            id = ns("diagnostics_tabs"),
            selected = "final_diagnostics", # Set "Final" tab as active by default
            tabPanel(
              title = i18n$t("Final Model Diagnostics"),
              value = "final_diagnostics",
              # Final Model Plots
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Response vs Predicted"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_response_vs_predicted_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Response Diagnostics"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    htmlOutput(ns("final_response_diagnostics_plot"), height = "350px")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Residual vs Predicted"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_residual_vs_predicted_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Residual Diagnostics"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    htmlOutput(ns("final_residual_diagnostics_plot"), height = "350px")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Cook's Distance"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_cooks_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Pareto Plot"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_pareto_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Response vs Coded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_response_vs_coded"), height = "550px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Response vs Uncoded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_response_vs_uncoded"), height = "550px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Residual vs Coded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_residual_vs_coded"), height = "550px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Final Model: Residual vs Uncoded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_residual_vs_uncoded"), height = "550px")
                  )
                )
              )
            ),
            tabPanel(
              title = i18n$t("Initial Model Diagnostics"),
              value = "initial_diagnostics",
              # Initial Model Plots
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Response vs Predicted"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_response_vs_predicted_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Response Diagnostics"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    htmlOutput(ns("initial_response_diagnostics_plot"), height = "350px")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Residual vs Predicted"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_residual_vs_predicted_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Residual Diagnostics"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    htmlOutput(ns("initial_residual_diagnostics_plot"), height = "350px")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Cook's Distance"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_cooks_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Pareto Plot"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_pareto_plot"), height = "350px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Response vs Coded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_response_vs_coded"), height = "550px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Response vs Uncoded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_response_vs_uncoded"), height = "550px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Residual vs Coded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_residual_vs_coded"), height = "550px")
                  )
                ),
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model: Residual vs Uncoded"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_residual_vs_uncoded"), height = "550px")
                  )
                )
              )
            )
          )
        ),

        # Tab 4 - Model Details (with sub-tabs for Initial and Final)
        tabPanel(
          title = i18n$t("Model Details"),
          value = "detailed",
          tabsetPanel(
            id = ns("model_details_tabs"),
            selected = "final_details",

            # Sub-tab: Final Model Details
            tabPanel(
              title = i18n$t("Final Model"),
              value = "final_details",
              br(),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Final Model Equation"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("model_equation_final"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Model Coefficients"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("coefficients_table_final"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Model Data"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("model_data_table_final"))
                  )
                )
              )
            ),

            # Sub-tab: Initial Model Details
            tabPanel(
              title = i18n$t("Initial Model"),
              value = "initial_details",
              br(),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Initial Model Equation"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    htmlOutput(ns("model_equation_initial"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Model Coefficients"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("coefficients_table_initial"))
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  box(
                    title = i18n$t("Model Data"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    DT::dataTableOutput(ns("model_data_table_initial"))
                  )
                )
              )
            )
          )
        ),

        # Tab 5 - Total Result
        # tabPanel(
        #   title = i18n$t("Full Report"), # Sekme adını "Full Report" olarak değiştirdim
        #   value = "total_result",
        #   fluidRow(
        #     column(
        #       12,
        #       box(
        #         title = i18n$t("Comprehensive Analysis Report"),
        #         width = NULL,
        #         status = "primary",
        #         solidHeader = TRUE,
        #         uiOutput(ns("formatted_full_report")), # Tüm raporu HTML olarak gösterecek output
        #         br(),
        #         downloadButton(ns("download_formatted_report"), i18n$t("Download Report (HTML)"), class = "btn btn-primary", style = "color: white !important;"),
        #         downloadButton(ns("download_raw_statements"), i18n$t("Download Raw Statements (TXT)"), class = "btn btn-default")
        #       )
        #     )
        #   )
        # ),


        # Tab 6 - Predictions
        tabPanel(
          title = i18n$t("Predictions"),
          value = "predictions",
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Enter Factor Values"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                div(
                  style = "display: flex; align-items: flex-start; gap: 0; margin-bottom: 10px;",
                  div(
                    style = "flex: 1;",
                    radioButtons(ns("pred_input_type"),
                      i18n$t("Input Type"),
                      choiceNames = list(i18n$t("Single Values"), i18n$t("Multiple Values")),
                      choiceValues = c("single", "multiple"),
                      selected = "single",
                      inline = TRUE
                    )
                  ),
                  div(style = "border-left: 1px solid #ccc; height: 50px; margin: 5px 15px 0 15px;"),
                  div(
                    style = "flex: 1;",
                    radioButtons(ns("pred_coded"),
                      i18n$t("Value Type"),
                      choiceNames = list(i18n$t("Coded"), i18n$t("Uncoded")),
                      choiceValues = c("TRUE", "FALSE"),
                      selected = "TRUE",
                      inline = TRUE
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.pred_input_type == 'single'",
                  ns = ns,
                  fluidRow(
                    column(
                      12,
                      uiOutput(ns("pred_factor_inputs"))
                    )
                  ),
                  div(id = ns("pred_range_warning_wrapper"), uiOutput(ns("pred_range_warning")))
                ),
                conditionalPanel(
                  condition = "input.pred_input_type == 'multiple'",
                  ns = ns,
                  fluidRow(
                    column(12, uiOutput(ns("pred_import_csv_div")))
                  ),
                  uiOutput(ns("pred_multi_range_warning")),
                  fluidRow(
                    column(12, div(
                      id = ns("pred_multi_input_wrapper"),
                      style = "margin-bottom: 15px;",
                      rHandsontableOutput(ns("pred_multi_input"))
                    ))
                  )
                ),
                fluidRow(
                  column(
                    6,
                    actionButton(ns("predict_btn"), i18n$t("Predict"), class = "btn btn-primary btn-block", icon = icon("calculator"), style = "color: white !important;")
                  ),
                  column(
                    6,
                    actionButton(ns("pred_reset_btn"), i18n$t("Reset"), class = "btn btn-danger btn-block", icon = icon("refresh"), style = "color: white !important;")
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.has_predictions",
            ns = ns,
            fluidRow(
              column(
                12,
                box(
                  title = i18n$t("Prediction Results"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  fluidRow(
                    column(
                      12,
                      h4(i18n$t("Initial Model Predictions")),
                      DT::dataTableOutput(ns("pred_results_initial"))
                    )
                  ),
                  br(),
                  fluidRow(
                    column(
                      12,
                      h4(i18n$t("Final Model Predictions")),
                      DT::dataTableOutput(ns("pred_results_final"))
                    )
                  ),
                  br(),
                  fluidRow(
                    column(
                      12,
                      downloadButton(ns("download_predictions"), i18n$t("Download Predictions"), class = "btn btn-default btn-block")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
