miscomp_ui <- function(id, defaults, i18n) {
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
          column(6, actionButton(ns("run_miscomp"), i18n$t("Run Comparison"), class = "btn btn-primary btn-block", style = "color: white;"))
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Miscibility Comparison")),
            div(uiOutput(ns("sfe_misc_comp_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          shinydashboardPlus::accordion(
            id = ns("comp_accordion"),

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

              # Select Solutes from Saved Calculations

              uiOutput(ns("saved_calculations_list_ui")),
              h5(i18n$t("Selected Solutes:")),
              uiOutput(ns("selected_solutes_ui"))
            ),

            # Co-Solvent Selection Section
            accordionItem(
              title = i18n$t("Co-Solvent Selection"),
              value = "cosolvent_selection",
              collapsed = TRUE,
              # Radio button to choose modifier type
              uiOutput(ns("modifier_type_ui")),
              # Pure Modifier Selection (shown when modifier_type == "pure")
              conditionalPanel(
                condition = sprintf("input['%s'] == 'pure'", ns("modifier_type")),
                div(id = ns("modifier_selection_wrapper"), uiOutput(ns("modifier_selection_ui")))
              ),
              # Custom Blend Builder (shown when modifier_type == "blend")
              conditionalPanel(
                condition = sprintf("input['%s'] == 'blend'", ns("modifier_type")),
                uiOutput(ns("custom_blend_box_ui"))
              )
            ),

            # Process Conditions Section
            accordionItem(
              title = i18n$t("Process Conditions"),
              value = "process_conditions",
              collapsed = TRUE,
              div(id = ns("pres_input_wrapper"), uiOutput(ns("pres_input_ui"))),
              div(id = ns("temp_wrapper"), uiOutput(ns("temp_ui"))),
              div(id = ns("vfrac_input_wrapper"), uiOutput(ns("vfrac_input_ui")))
            ),

            # Advanced Plot Options Section
            accordionItem(
              title = i18n$t("Advanced Plot Options"),
              value = "advanced_options",
              collapsed = TRUE,
              div(id = ns("plt_title_wrapper"), checkboxInput(ns("plt_title"), i18n$t("Show Plot Titles"), value = defaults$plt_title)),
              div(id = ns("use_custom_colors_wrapper"), checkboxInput(ns("use_custom_colors"), i18n$t("Use Custom Colors"), value = defaults$use_custom_colors)),
              conditionalPanel(
                condition = paste0("input['", ns("use_custom_colors"), "']"),
                fluidRow(
                  column(4, colourInput(ns("col_one"), i18n$t("Color 1"), value = defaults$col_one)),
                  column(4, colourInput(ns("col_two"), i18n$t("Color 2"), value = defaults$col_two)),
                  column(4, colourInput(ns("col_three"), i18n$t("Color 3"), value = defaults$col_three))
                ),
                fluidRow(
                  column(4, colourInput(ns("col_four"), i18n$t("Color 4"), value = defaults$col_four)),
                  column(4, colourInput(ns("col_five"), i18n$t("Color 5"), value = defaults$col_five)),
                  column(4, colourInput(ns("col_six"), i18n$t("Color 6"), value = defaults$col_six))
                )
              )
            )
          )
        )
      ),
      column(
        width = 8,
        fluidRow(
          tabBox(
            id = ns("results_tabs"),
            width = 12,
            tabPanel(
              title = i18n$t("Plots"),
              # Placeholder when no results
              conditionalPanel(
                condition = "!output.has_results",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("chart-line", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Select the parameters and run comparison to see results."))
                )
              ),
              # Actual content when results are available
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
                # Export buttons row
                div(
                  style = "margin-bottom: 15px; display: flex; gap: 10px; justify-content: center;",
                  downloadButton(ns("export_current_plot"), i18n$t("Export Current Plot"), class = "btn btn-success btn-lg"),
                  downloadButton(ns("export_all_plots"), i18n$t("Export All Plots"), class = "btn btn-success btn-lg")
                ),
                uiOutput(ns("plots_tabs_ui")) # Dynamic tabs for plots
              )
            ),
            tabPanel(
              title = i18n$t("Miscibility Data"),
              # Placeholder when no results
              conditionalPanel(
                condition = "!output.has_results",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("table", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Select the parameters and run comparison to see results."))
                )
              ),
              # Actual content when results are available
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
                DT::dataTableOutput(ns("miscibility_data_table"))
              )
            )
          )
        )
      )
    )
  )
}
