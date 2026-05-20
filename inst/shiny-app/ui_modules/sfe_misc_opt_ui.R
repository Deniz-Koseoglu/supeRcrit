miscibility_optimization_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  tagList(
    # Add custom JavaScript for resetting inputs
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('resetInput', function(message) {
          var input = $('#' + message.id);
          if (input.length > 0) {
            input.val(message.value).trigger('change'); // Update value and trigger change event
          }
        });
      "))
    ),
    fluidRow(
      column(
        width = 4,
        # Top buttons row
        fluidRow(
          column(6, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
          column(6, actionButton(ns("optimize_miscibility"), i18n$t("Optimize Miscibility"), class = "btn btn-primary btn-block", style = "color: white;"))
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Miscibility Optimization")),
            div(uiOutput(ns("sfe_misc_opt_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          shinydashboardPlus::accordion(
            id = ns("opt_accordion"),

            # Solute Selection Section
            accordionItem(
              title = i18n$t("Solute Selection"),
              value = "solute_selection",
              collapsed = FALSE,
              # Load Examples and Load from Disk buttons
              div(
                style = "margin-bottom: 10px;",
                actionButton(ns("load_example_data"), i18n$t("Load Examples"),
                  icon = icon("flask"),
                  class = "btn btn-info btn-sm"
                ),
                actionButton(ns("load_from_disk"), i18n$t("Load from Disk"),
                  icon = icon("folder-open"),
                  class = "btn btn-default btn-sm",
                  style = "margin-left: 5px;"
                )
              ),
              
              uiOutput(ns("saved_calculations_list_ui"))
            ),

            # Co-Solvent Selection Section
            accordionItem(
              title = i18n$t("Co-Solvent Selection"),
              value = "cosolvent_selection",
              collapsed = TRUE,
              div(id = ns("modifier_selection_wrapper"), uiOutput(ns("modifier_selection_ui"))),
              # Solvent Blends Section (inside Co-Solvent Selection)
              box(
                title = i18n$t("Solvent Blends (Optional)"),
                status = "primary",
                collapsible = TRUE,
                collapsed = TRUE,
                solidHeader = TRUE,
                width = NULL,
                # Blend Builder
                h5(i18n$t("Build a Blend")),
                div(id = ns("blend_solvent_selector_wrapper"), uiOutput(ns("blend_solvent_selector_ui"))),
                div(
                  style = "display: flex; gap: 6px;",
                  div(style = "flex: 1;", actionButton(ns("add_to_blend"), i18n$t("Add to Blend"), class = "btn btn-primary btn-block", style = "color: white;")),
                  div(style = "flex: 1;", actionButton(ns("equalize_blend"), i18n$t("Equalize"), class = "btn btn-success btn-block", style = "color: white;", disabled = "disabled"))
                ),
                hr(),

                # Current Blend Being Built
                h5(i18n$t("Current Blend Composition:")),
                uiOutput(ns("current_blend_ui")), # Dynamic UI for fraction inputs

                fluidRow(
                  column(
                    6,
                    div(id = ns("clear_blend_wrapper"), actionButton(ns("clear_blend"), i18n$t("Clear"), class = "btn btn-primary btn-block", style = "color: white;"))
                  ),
                  column(
                    6,
                    div(id = ns("save_blend_wrapper"), actionButton(ns("save_blend"), i18n$t("Add Blend"), class = "btn btn-primary btn-block", style = "color: white;"))
                  )
                ),
                hr(),

                # Added Blends List
                h5(i18n$t("Added Blends:")),
                uiOutput(ns("saved_blends_ui")) # Dynamic list of Added Blends
              )
            ),

            # Process Conditions Section
            accordionItem(
              title = i18n$t("Process Conditions"),
              value = "process_conditions",
              collapsed = TRUE,
              div(id = ns("vfrac_input_wrapper"), uiOutput(ns("vfrac_input_ui"))),
              fluidRow(
                column(6, div(id = ns("pressure_min_wrapper"), uiOutput(ns("pressure_min_ui")))),
                column(6, div(id = ns("temperature_min_wrapper"), uiOutput(ns("temperature_min_ui"))))
              ),
              fluidRow(
                column(6, div(id = ns("pressure_max_wrapper"), uiOutput(ns("pressure_max_ui")))),
                column(6, div(id = ns("temperature_max_wrapper"), uiOutput(ns("temperature_max_ui"))))
              ),
              fluidRow(
                column(6, div(id = ns("pressure_step_wrapper"), uiOutput(ns("pressure_step_ui")))),
                column(6, div(id = ns("temperature_step_wrapper"), uiOutput(ns("temperature_step_ui"))))
              ),
              hr(),
              h5(tags$b(i18n$t("Best Modifier Table Display"))),
              div(id = ns("table_display_mode_wrapper"), 
                uiOutput(ns("table_display_mode_ui"))
              )
            )
          )
        )
      ),
      column(
        width = 8,
        # Best Modifier Table - hidden until calculation is performed
        shinyjs::hidden(
          div(
            id = ns("best_modifier_table_wrapper"),
            fluidRow(
              box(
                title = i18n$t("Best Modifier Table"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                div(
                  style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
                  uiOutput(ns("best_table_view_toggle_ui")),
                  uiOutput(ns("best_table_legend_ui"))
                ),
                div(
                  style = "overflow-x: auto;",
                  DT::dataTableOutput(ns("best_modifier_table"))
                )
              )
            )
          )
        ),
        fluidRow(
          tabBox(
            id = ns("results_tabs"),
            width = 12,
            tabPanel(
              title = i18n$t("Summary"),
              # Placeholder when no results
              conditionalPanel(
                condition = "!output.has_results",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("chart-line", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Select the parameters and run optimization to see results."))
                )
              ),
              # Actual content when results are available
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
                div(
                  id = ns("export_button_wrapper"),
                  fluidRow(
                    column(12,
                      style = "display: flex; justify-content: center; margin-bottom: 15px;",
                      downloadButton(ns("export_results"),
                        i18n$t("Export Results"),
                        class = "btn btn-success btn-lg",
                        style = "margin-left:5px;"
                      )
                    )
                  )
                ),
                fluidRow(
                  # Value Boxes
                  valueBoxOutput(ns("best_modifier_vb"), width = 4),
                  valueBoxOutput(ns("max_me_vb"), width = 4),
                  valueBoxOutput(ns("num_modifiers_vb"), width = 4)
                ),
                DT::dataTableOutput(ns("modifier_ranking_table"))
              )
            ),
            tabPanel(
              title = i18n$t("Co-Solvent Comparison"),
              conditionalPanel(
                condition = "!output.has_results",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("table", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Select the parameters and run optimization to see results."))
                )
              ),
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
                DT::dataTableOutput(ns("miscibility_comparison_table"))
              )
            ),
            tabPanel(
              title = i18n$t("Miscibility Enhancement"),
              conditionalPanel(
                condition = "!output.has_results",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("chart-bar", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Select the parameters and run optimization to see results."))
                )
              ),
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
                fluidRow(
                  column(
                    width = 12,
                    selectInput(ns("miscibility_enhancement_solvent_selection"), i18n$t("Select Co-Solvent"), choices = NULL)
                  ),
                  column(
                    width = 12,
                    DT::dataTableOutput(ns("miscibility_enhancement_table"))
                  )
                )
              )
            ),
            tabPanel(
              title = i18n$t("Detailed Results"),
              conditionalPanel(
                condition = "!output.has_results",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("list-alt", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Select the parameters and run optimization to see results."))
                )
              ),
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
                DT::dataTableOutput(ns("miscibility_results_table"))
              )
            )
          )
        )
      )
    )
  )
}
