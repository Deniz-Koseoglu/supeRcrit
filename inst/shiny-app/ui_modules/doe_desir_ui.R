# DOE Desirability Function UI Module
doe_desir_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    # Left Panel - Input
    column(
      width = 5,
      fluidRow(
        column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(4, actionButton(ns("calculate"), i18n$t("Calculate"), class = "btn btn-info btn-block")),
    
      ),
      br(),
      
      # Analysis Selection Box
      box(
        title = div(
          i18n$t("Select DOE Analyses"),
          actionButton(ns("desir_help"), "", icon = icon("question-circle"),
                      class = "btn-info btn-xs", style = "float: right; margin-top: -5px;")
        ),
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        fluidRow(
          column(12,
            p(i18n$t("Select 2-6 saved DOE analysis setups to combine with desirability function:"),
              style = "font-size: 13px; color: #666;")
          )
        ),
        
        fluidRow(
          column(12,
            uiOutput(ns("analysis_selection_ui"))
          )
        ),
        
        fluidRow(
          column(12,
            actionButton(ns("load_analyses"), i18n$t("Load Selected Analyses"), 
                        class = "btn btn-primary btn-block", icon = icon("download"))
          )
        ),
        
        br(),
        
        # Selected Analyses Preview
        uiOutput(ns("selected_analyses_preview"))
      ),
      
      # Desirability Settings Box
      box(
        title = i18n$t("Desirability Settings"),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        
        uiOutput(ns("desirability_settings_ui"))
      ),
      
      # Factor Range Settings Box
      box(
        title = i18n$t("Factor Range Settings"),
        width = NULL,
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        fluidRow(
          column(12,
            checkboxInput(ns("use_default_ranges"), 
                         i18n$t("Use Default Factor Ranges"), 
                         value = TRUE)
          )
        ),
        
        conditionalPanel(
          condition = sprintf("!input.%s", ns("use_default_ranges")),
          uiOutput(ns("factor_range_ui"))
        )
      ),
      
      # Global Parameters Box
      box(
        title = i18n$t("Global Parameters"),
        width = NULL,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        fluidRow(
          column(6,
            radioButtons(ns("dtype"), i18n$t("Factor Type:"),
                        choices = c("Coded" = "coded", "Uncoded" = "uncoded"),
                        selected = defaults$dtype %||% "coded",
                        inline = TRUE)
          ),
          column(6,
            radioButtons(ns("modbase"), i18n$t("Model Base:"),
                        choices = c("Initial" = "initial", "Final" = "final"),
                        selected = defaults$modbase %||% "final",
                        inline = TRUE)
          )
        ),
        
        fluidRow(
          column(6,
            selectInput(ns("optmet"), i18n$t("Optimization Method:"),
                       choices = c("NLopt" = "nlopt", "Optim" = "optim"),
                       selected = defaults$optmet %||% "nlopt")
          ),
          column(6,
            selectInput(ns("kmed"), i18n$t("Clustering:"),
                       choices = c("None" = "NA", "Auto" = "auto", "2" = "2", "3" = "3", 
                                  "4" = "4", "5" = "5"),
                       selected = "NA")
          )
        ),
        
        fluidRow(
          column(6,
            numericInput(ns("spts_random"), i18n$t("Random Starting Points:"),
                        value = 100, min = 10, max = 1000, step = 10)
          ),
          column(6,
            numericInput(ns("spts_data"), i18n$t("Data Starting Points:"),
                        value = 10, min = 1, max = 100, step = 1)
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
        
        # Tab 1 - Summary
        tabPanel(
          title = i18n$t("Summary"),
          value = "summary",

          # Export Results (top) - only show when calculation is completed
          conditionalPanel(
            condition = "output.show_export_results",
            ns = ns,
            fluidRow(
              column(12,
                style = "display: flex; justify-content: center;",
                downloadButton(ns("export_all"), i18n$t("Export Results"), class = "btn btn-default", style = "margin-left:5px;")
              )
            ),
            br()
          ),

          fluidRow(
            column(12,
              box(
                title = i18n$t("Factor Limits"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                div(style='overflow-x:auto', DT::dataTableOutput(ns("factor_limits_table")))
              )
            ),
            column(12,
              box(
                title = i18n$t("Response Limits"),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
               div(style='overflow-x:auto', DT::dataTableOutput(ns("response_limits_table")))
              )
            )
          ),
          
          fluidRow(
            column(12,
              box(
                title = i18n$t("Model Summaries"),
                width = NULL,
                status = "info",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("model_summaries_table"))
              )
            )
          )
        ),
        
        # Tab 2 - Optimization Results
        tabPanel(
          title = i18n$t("Optimization Results"),
          value = "optimization",
          
          box(
            title = i18n$t("Optimal Solutions"),
            width = NULL,
            status = "success",
            solidHeader = TRUE,
            DT::dataTableOutput(ns("unique_solutions_table")),
            br(),
            downloadButton(ns("download_solutions"), i18n$t("Download Solutions"), 
                          class = "btn btn-primary")
          ),
          
          box(
            title = i18n$t("All Optimization Outputs"),
            width = NULL,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput(ns("output_data_table"))
          ),
          
          box(
            title = i18n$t("Original Data with Desirabilities"),
            width = NULL,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput(ns("orig_data_table"))
          )
        )
      )
    )
  )
}
