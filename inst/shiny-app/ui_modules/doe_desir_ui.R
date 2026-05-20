# DOE Desirability Function UI Module
doe_desir_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    # Left Panel - Input
    column(
      width = 5,
      fluidRow(
        column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(4, actionButton(ns("calculate"), i18n$t("Calculate"), class = "btn btn-primary btn-block", style = "color: white;")),
      ),
      br(),
      box(
        title = div(
          style = "display: flex; align-items: center;",
          span(i18n$t("Desirability Function")),
          div(uiOutput(ns("doe_desir_HELP"))),
          div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
        ),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        # Accordion for parameter groups
        shinydashboardPlus::accordion(
          id = ns("desir_accordion"),

          # Select DOE Analyses Section
          accordionItem(
            title = div(
              i18n$t("Select DOE Analyses")
            ),
            value = "select_analyses",
            collapsed = FALSE,
            fluidRow(
              # column(6, actionButton(ns("desir_selection_intro"), "",
              #   icon = icon("info-circle"),
              #   class = "btn-light btn-xs", style = "float: left; margin-top:-2px;"
              # )),
              # column(6, actionButton(ns("refresh_analyses"), "",
              #   icon = icon("refresh"),
              #   class = "btn-default btn-xs", style = "float: right; margin-top:-2px;",
              #   title = "Refresh analysis list"
              # ))
            ),
            # Load Examples button
            fluidRow(
              column(
                12,
                actionButton(ns("load_example_data"), i18n$t("Load Examples"),
                  icon = icon("flask"),
                  class = "btn btn-info btn-sm",
                  style = "margin-bottom: 10px;"
                )
              )
            ),
            fluidRow(
              column(
                12,
                p(i18n$t("Select 2-6 saved DOE analysis setups to combine with desirability function:"),
                  style = "font-size: 13px; color: #666;"
                )
              )
            ),
            fluidRow(
              column(
                12,
                uiOutput(ns("analysis_selection_ui"))
              )
            ),
            fluidRow(
              column(
                12,
                actionButton(ns("load_analyses"), i18n$t("Load Selected Analyses"),
                  class = "btn btn-primary btn-block", icon = icon("download"), style = "color: white;",
                  disabled = "disabled"
                )
              )
            ),
            br(),

            # Selected Analyses Preview
            uiOutput(ns("selected_analyses_preview"))
          ),

          # Desirability Settings
          accordionItem(
            title = div(
              i18n$t("Desirability Settings")
            ),
            value = "desir_settings",
            collapsed = FALSE,
            uiOutput(ns("desirability_settings_ui"))
          ),

          # Factor Range Settings
          accordionItem(
            title = div(
              i18n$t("Factor Range Settings")
            ),
            value = "factor_ranges",
            collapsed = TRUE,
            div(
              style = "display: flex; align-items: flex-end; gap: 10px; margin-bottom: 10px;",
              div(
                style = "flex: 1;",
                uiOutput(ns("dtype_ui"))
              ),
              div(
                style = "flex: 1; padding-bottom: 8px;",
                uiOutput(ns("use_default_ranges_ui"))
              )
            ),
            conditionalPanel(
              condition = sprintf("!input['%s']", ns("use_default_ranges")),
              uiOutput(ns("factor_range_ui"))
            )
          ),

          # Global Parameters
          accordionItem(
            title = div(
              i18n$t("Global Parameters")
            ),
            value = "global_params",
            collapsed = TRUE,
            fluidRow(
              column(
                3,
                div(
                  id = ns("modbase_wrapper"),
                  uiOutput(ns("modbase_ui"))
                )
              ),
              column(
                5,
                div(
                  id = ns("optmet_wrapper"),
                  uiOutput(ns("optmet_ui"))
                )
              ),
              column(
                4,
                div(
                  id = ns("kmed_wrapper"),
                  uiOutput(ns("kmed_ui"))
                )
              )
            ),
            fluidRow(
              column(
                6,
                div(
                  id = ns("spts_random_wrapper"),
                  uiOutput(ns("spts_random_ui"))
                )
              ),
              column(
                6,
                div(
                  id = ns("spts_data_wrapper"),
                  uiOutput(ns("spts_data_ui"))
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
        height = "calc(100vh - 100px)",

        # Tab 1 - Summary
        tabPanel(
          title = i18n$t("Summary"),
          value = "summary",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.has_results",
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
            condition = "output.has_results",
            ns = ns,
            # Export Results (top)
            div(
              id = ns("export_results_wrapper"),
              fluidRow(
                column(12,
                  style = "display: flex; justify-content: center; margin-bottom: 15px;",
                  downloadButton(ns("export_all"),
                    i18n$t("Export Results"),
                    class = "btn btn-success btn-lg",
                    style = "margin-left:5px;"
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                box(
                  title = i18n$t("Factor Limits"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                div(style = "overflow-x:auto", DT::dataTableOutput(ns("factor_limits_table")))
              )
            ),
            column(
              12,
              box(
                title = i18n$t("Response Limits"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                div(style = "overflow-x:auto", DT::dataTableOutput(ns("response_limits_table")))
              )
            )
          ),
          fluidRow(
            column(
              12,
              box(
                title = i18n$t("Model Summaries"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("model_summaries_table"))
              )
            )
          )
          ) # Close conditionalPanel
        ),

        # Tab 2 - Optimization Results
        tabPanel(
          title = i18n$t("Optimization Results"),
          value = "optimization",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.has_results",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("sliders-h", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and run the calculation to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.has_results",
            ns = ns,
            box(
              title = i18n$t("Optimal Solutions"),
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              DT::dataTableOutput(ns("unique_solutions_table"))
            ),
            box(
              title = i18n$t("All Optimization Outputs"),
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput(ns("output_data_table"))
          ),
          box(
            title = i18n$t("Original Data with Desirabilities"),
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput(ns("orig_data_table"))
          )
          ) # Close conditionalPanel
        )
      )
    ),
    
    # JavaScript to handle equation view button clicks
    tags$script(HTML(sprintf("
      $(document).on('click', '.view-equation-btn', function() {
        var equation = $(this).data('equation');
        var response = $(this).data('response');
        var modelType = $(this).data('model-type');
        
        Shiny.setInputValue('%s', {
          equation: equation,
          response: response,
          model_type: modelType,
          timestamp: new Date().getTime()
        });
      });
    ", ns("equation_modal_data"))))
  )
}
