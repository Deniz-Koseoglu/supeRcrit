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
        # Top buttons row: Reset | Run Comparison | Export ▾ (PDF = all graphs, CSV = data). The Export
        # dropdown sits to the RIGHT of the primary action and is a disabled placeholder until a comparison
        # has been run (output.has_results), mirroring the Cost Analysis / Co-Solvent Selection modules.
        fluidRow(
          column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
          column(4, actionButton(ns("run_miscomp"), i18n$t("Run Comparison"), class = "btn btn-primary btn-block", style = "color: white;")),
          column(4,
            conditionalPanel(
              condition = "!output.has_results", ns = ns,
              tags$button(class = "btn btn-success dropdown-toggle", disabled = NA,
                style = "color:white; opacity:0.65; width:100%;",
                icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret"))),
            conditionalPanel(
              condition = "output.has_results", ns = ns,
              div(class = "btn-group", style = "width:100%;",
                tags$button(type = "button", class = "btn btn-success dropdown-toggle",
                  `data-toggle` = "dropdown", `aria-haspopup` = "true", `aria-expanded` = "false",
                  style = "color:white; width:100%;",
                  icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret")),
                tags$ul(class = "dropdown-menu dropdown-menu-right", style = "min-width:100%;",
                  tags$li(downloadLink(ns("export_comp_pdf"), tagList(icon("file-pdf"), " ", i18n$t("Export PDF")),
                    style = "padding:5px 15px; display:block;")),
                  tags$li(downloadLink(ns("export_comp_csv"), tagList(icon("file-csv"), " ", i18n$t("Export CSV")),
                    style = "padding:5px 15px; display:block;"))))))
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Co-Solvent Comparison")),
            div(uiOutput(ns("sfe_misc_comp_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          # 15px horizontal margin = the accordion panels' own padding, so this top selector aligns
          # with the accordion content below it.
          div(style = "margin: 2px 15px 10px 15px;", uiOutput(ns("comp_method_ui"))),
          shinydashboardPlus::accordion(
            id = ns("comp_accordion"),

            # Solute Selection Section
            accordionItem(
              title = i18n$t("Solute Selection"),
              value = "solute_selection",
              collapsed = FALSE,
              # Load Examples, Load from Disk and Load from Database buttons (one line)
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
                ),
                uiOutput(ns("db_load_btn"), inline = TRUE)
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
              conditionalPanel(
                condition = sprintf("input['%s'] != 'solubility'", ns("comp_method")),
                div(id = ns("vfrac_input_wrapper"), uiOutput(ns("vfrac_input_ui")))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'solubility'", ns("comp_method")),
                uiOutput(ns("comp_engine_ui")),
                uiOutput(ns("engine_coverage_ui")),
                div(id = ns("comp_molfrac_wrapper"), uiOutput(ns("comp_molfrac_ui"))),
                div(id = ns("comp_unit_wrapper"), uiOutput(ns("comp_unit_ui")))
              )
            ),

            # Advanced Options Section
            accordionItem(
              title = i18n$t("Advanced Options"),
              value = "advanced_options",
              collapsed = TRUE,
              # Parallel-evaluation toggle (Solubility mode only; server-rendered so the tooltip translates live).
              uiOutput(ns("sol_parallel_ui")),
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
              # Actual content when results are available. The Export buttons now sit INLINE on the right
              # of each plot tab's selector row (rendered in plots_tabs_ui), so no separate button row here.
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
                uiOutput(ns("plots_tabs_ui")) # Dynamic tabs for plots (with inline export buttons)
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
