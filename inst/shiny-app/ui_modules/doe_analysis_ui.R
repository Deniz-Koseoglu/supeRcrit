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
        column(4, div(id = ns("analyze_wrapper"), actionButton(ns("analyze"), i18n$t("Analyze"), class = "btn btn-info btn-block"))),
        column(4, div(id = ns("save_analysis_wrapper"), actionButton(ns("save_analysis"), i18n$t("Save Analysis"), class = "btn btn-primary btn-block", style = "color: white !important;")))
      ),
      br(),
      # Analysis Parameters
      box(
        title = div(
          i18n$t("Analysis Parameters"),
        ),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        fluidRow(
          column(12, actionButton(ns("doe_analysis_help"), "",
            icon = icon("question-circle"),
            class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
          )),
          column(6, div(
            id = ns("time_var_wrapper"),
            selectInput(ns("time_var"), i18n$t("Time Variable:"), choices = NULL)
          )),
          column(6, div(
            id = ns("response_var_wrapper"),
            selectInput(ns("response_var"), i18n$t("Response Variable:"), choices = NULL)
          ))
        ),
        fluidRow(
          column(12, div(
            id = ns("uc_facs_wrapper"),
            checkboxGroupInput(ns("uc_facs"), 
              i18n$t("Uncoded Factors:"),
              choices = NULL,
              selected = NULL,  # Will be set reactively
              inline = TRUE
            )
          ))
        ),
        fluidRow(
          column(6, div(
            id = ns("mod_order_wrapper"),
            selectInput(ns("mod_order"), i18n$t("Model Order:"),
              choices = c("Linear (1)" = 1, "Linear + Interactions (1.5)" = 1.5, "Quadratic (2)" = 2),
              selected = defaults$mod_order
            )
          )),
          column(6, div(
            id = ns("p_cutoff_wrapper"),
            numericInput(ns("p_cutoff"), i18n$t("P-value Cutoff"), value = defaults$p_cutoff, min = 0, max = 1, step = 0.01)
          ))
        ),
        fluidRow(
          column(6, div(
            id = ns("trim_method_wrapper"),
            selectInput(ns("trim_method"), i18n$t("Trim Method:"),
              choices = c("Stepwise" = "stepwise", "P-value Cutoff" = "p_cutoff", "Both" = "both", "None" = "none"),
              selected = defaults$trim_method
            )
          )),
          column(6, div(
            id = ns("which_facs_wrapper"),
            selectInput(ns("which_facs"), i18n$t("Factor Type:"),
              choices = c("Coded" = "coded", "Uncoded" = "uncoded"),
              selected = defaults$which_facs
            )
          ))
        )
      ),
      box(
        title = div(
          i18n$t("Data Source"),
        ),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        # Data Source Selection
        fluidRow(
          column(12, actionButton(ns("data_source_help"), "",
            icon = icon("question-circle"),
            class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
          )),
          column(12, div(
            id = ns("data_source_wrapper"),
            radioButtons(ns("data_source"), i18n$t("Select Data Source:"),
              choices = c(
                "Saved Designs" = "saved",
                "Import CSV" = "csv"
              ),
              selected = defaults$data_source
            )
          ))
        ),

        # Saved Designs Selection
        conditionalPanel(
          condition = "input.data_source == 'saved'",
          ns = ns,
          fluidRow(
            column(12, div(
              id = ns("saved_design_wrapper"),
              selectInput(ns("saved_design"), i18n$t("Select Saved Design:"), choices = NULL)
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
            column(12, fileInput(ns("import_file_csv"), i18n$t("Import CSV File"), accept = ".csv")),
            column(12, tags$pre(
              id = ns("column_format_example"),
              HTML(paste0(
                "Example csv format:<br/>",
                '"Standard_Order","A","B","C","P_bar","T_degC","EtOH_gmin","response"<br/>',
                '"1","-1.00","-1.00","-1.00","100.00","40.00","3.00","3.0213"<br/>',
                '"2","-1.00","1.00","-1.00","100.00","60.00","3.00","4.56"<br/>',
                "..."
              ))
            ))
          )
        ),

        # Data Preview with edit toggle
        fluidRow(
          column(12, h4(i18n$t("Data Preview")))
        ),
        wellPanel(
          h4(i18n$t("Column Configuration")),
          actionButton(ns("open_col_config"), i18n$t("Configure Columns"), class = "btn btn-primary btn-block", icon = icon("cog"), style = "color: white !important;")
        ),
        fluidRow(
          column(12, div(id = ns("data_preview_wrapper"), rHandsontableOutput(ns("data_preview"))))
        )
      )
    ),

    # Right Panel - Results
    column(
      width = 7,
      tabBox(
        width = NULL,
        height = "calc(100vh - 100px)",

        # Tab 1 - Model Results
        tabPanel(
          title = i18n$t("Model Summary"),
          value = "model_results",

          # Export All Results (top) - only show when analysis is completed
          conditionalPanel(
            condition = "output.show_export_all_results",
            ns = ns,
            fluidRow(
              column(12,
                style = "display: flex; justify-content: center;",
                downloadButton(ns("export_all_results"), i18n$t("Export All Results"), class = "btn btn-default", style = "margin-left:5px;")
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
                status = "success",
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
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput(ns("trimming_info"))
              )
            )
          ),

          # Optimal Conditions
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Optimal Conditions (Canonical Analysis)"),
                width = NULL,
                status = "success",
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
                status = "info",
                solidHeader = TRUE,
                htmlOutput(ns("canonical_analysis_summary"))
              )
            )
          )
        ),

        # Tab 2 - Optimization Results (NEW)
        tabPanel(
          title = i18n$t("Optimization Results"),
          value = "optimization_results",

          # Stationary Point Type and Predicted Response
          fluidRow(
            column(
              6,
              box(
                title = i18n$t("Stationary Point Type"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                htmlOutput(ns("optimization_type"))
              )
            ),
            column(
              6,
              box(
                title = i18n$t("Predicted Response at Stationary Point"),
                width = NULL,
                status = "info",
                solidHeader = TRUE,
                htmlOutput(ns("predicted_response"))
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
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("optimal_conditions_detailed"))
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
                DT::dataTableOutput(ns("minmax_comparison"))
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
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("steepest_ascent_table"))
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
                status = "warning",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("eigenvalues_table"))
              )
            ),
            column(
              12,
              box(
                title = i18n$t("Range Check and Warnings"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                htmlOutput(ns("ca_warnings"))
              )
            )
          )
        ),


        # Tab 3 - Diagnostics
        tabPanel(
          title = i18n$t("Diagnostics"),
          value = "diagnostics",
          tabsetPanel(
            id = ns("diagnostics_tabs"),
            selected = "final_diagnostics", # Set "Final" tab as active by default
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("initial_residual_vs_uncoded"), height = "550px")
                  )
                )
              )
            ),
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
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
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    plotlyOutput(ns("final_residual_vs_uncoded"), height = "550px")
                  )
                )
              )
            )
          )
        ),

        # Tab 4 - Detailed Results
        tabPanel(
          title = i18n$t("Model Details"),
          value = "detailed",
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Model Equations"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                htmlOutput(ns("model_equations"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Model Coefficients"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("coefficients_table"))
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

        # Tab 5 - Total Result
        tabPanel(
          title = i18n$t("Full Report"), # Sekme adını "Full Report" olarak değiştirdim
          value = "total_result",
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Comprehensive Analysis Report"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                uiOutput(ns("formatted_full_report")), # Tüm raporu HTML olarak gösterecek output
                br(),
                downloadButton(ns("download_formatted_report"), i18n$t("Download Report (HTML)"), class = "btn btn-primary", style = "color: white !important;"),
                downloadButton(ns("download_raw_statements"), i18n$t("Download Raw Statements (TXT)"), class = "btn btn-default")
              )
            )
          )
        ),


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
                status = "primary",
                solidHeader = TRUE,
                fluidRow(
                  column(
                    6,
                    radioButtons(ns("pred_input_type"),
                      i18n$t("Input Type:"),
                      choices = c("Single Values" = "single", "Multiple Values (CSV)" = "multiple"),
                      selected = "single"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.pred_input_type == 'single'",
                  ns = ns,
                  fluidRow(
                    column(
                      6,
                      radioButtons(ns("pred_coded"),
                        i18n$t("Value Type:"),
                        choices = c("Coded" = "TRUE", "Uncoded" = "FALSE"),
                        selected = "FALSE",
                        inline = TRUE
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      uiOutput(ns("pred_factor_inputs"))
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.pred_input_type == 'multiple'",
                  ns = ns,
                  fluidRow(
                    column(
                      12,
                      radioButtons(ns("pred_coded_multi"),
                        i18n$t("Value Type:"),
                        choices = c("Coded" = "TRUE", "Uncoded" = "FALSE"),
                        selected = "FALSE",
                        inline = TRUE
                      )
                    )
                  ),
                  fluidRow(
                    column(12, fileInput(ns("pred_import_csv"), i18n$t("Import CSV File"), accept = ".csv"))
                  ),
                  fluidRow(
                    column(12, rHandsontableOutput(ns("pred_multi_input")))
                  )
                ),
                fluidRow(
                  column(
                    12,
                    actionButton(ns("predict_btn"), i18n$t("Predict"), class = "btn btn-primary btn-block", icon = icon("calculator"), style = "color: white !important;")
                  )
                )
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Prediction Results"),
                width = NULL,
                status = "success",
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
}
