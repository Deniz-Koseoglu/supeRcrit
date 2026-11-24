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
      ")),
      # Greyed, non-interactive appearance for the manual solvent/blend inputs while the Automatic
      # Solvent Finder checkbox is on; plus centre the text in every table cell in this module.
      tags$style(HTML(sprintf(
        ".disabled-section{opacity:0.5;pointer-events:none;} [id^='%s'] table.dataTable th,[id^='%s'] table.dataTable td{text-align:center;}",
        ns(""), ns("")))
      )
    ),
    fluidRow(
      column(
        width = 4,
        # Top buttons row
        fluidRow(
          column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
          # Contextual button: "Run Selection" (primary) normally; a red "Kill Evaluation" while a
          # background solubility scan runs. Two static buttons toggled by the sol_running flag (no
          # re-render, so the Run observer never spuriously re-fires).
          column(4,
            conditionalPanel(condition = "!output.sol_running", ns = ns,
              actionButton(ns("optimize_miscibility"), i18n$t("Run Selection"),
                class = "btn btn-primary btn-block", style = "color: white;")),
            conditionalPanel(condition = "output.sol_running", ns = ns,
              actionButton(ns("kill_run"), i18n$t("Kill Evaluation"), icon = icon("stop"),
                class = "btn btn-danger btn-block",
                style = "color:white; background-color:#dc3545; border-color:#c82333; font-weight:600;"))),
          # Export dropdown (PDF / CSV) to the RIGHT of Run Selection, active only once a run has produced
          # results (contextual). PDF via cosolv_export.R; CSV via the existing export_results handler.
          column(4,
            conditionalPanel(condition = "!output.has_results", ns = ns,
              tags$button(class = "btn btn-success dropdown-toggle", disabled = NA,
                style = "color:white; opacity:0.65; width:100%;",
                icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret"))),
            conditionalPanel(condition = "output.has_results", ns = ns,
              div(class = "btn-group", style = "width:100%;",
                tags$button(type = "button", class = "btn btn-success dropdown-toggle",
                  `data-toggle` = "dropdown", `aria-haspopup` = "true", `aria-expanded` = "false",
                  style = "color:white; width:100%;",
                  icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret")),
                tags$ul(class = "dropdown-menu dropdown-menu-right", style = "min-width:100%;",
                  tags$li(downloadLink(ns("export_pdf_report"), tagList(icon("file-pdf"), " ", i18n$t("Export PDF")),
                    style = "padding:5px 15px; display:block;")),
                  tags$li(downloadLink(ns("export_results"), tagList(icon("file-csv"), " ", i18n$t("Export CSV")),
                    style = "padding:5px 15px; display:block;"))
                ))
          ))
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Co-Solvent Selection")),
            div(uiOutput(ns("sfe_misc_opt_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          # 15px horizontal margin = the accordion panels' own padding, so this top selector aligns
          # with the accordion content below it.
          div(style = "margin: 2px 15px 10px 15px;", uiOutput(ns("opt_method_ui"))),
          shinydashboardPlus::accordion(
            id = ns("opt_accordion"),

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
              
              uiOutput(ns("saved_calculations_list_ui"))
            ),

            # Co-Solvent Selection Section
            accordionItem(
              title = i18n$t("Co-Solvent Selection"),
              value = "cosolvent_selection",
              collapsed = TRUE,
              # Automatic Solvent Finder. Explanatory text lives in an i18n-safe help popover (input_help)
              # — a title="" attribute cannot hold a translated string (i18n$t() returns a <span> tag).
              div(style = "margin-bottom:8px; padding:6px 10px; background:#f6fbf3; border:1px solid #c3e6cb; border-radius:5px;",
                checkboxInput(ns("auto_finder"),
                  tags$span(tags$b(i18n$t("Automatic Solvent Finder (screen all solvents)")),
                    input_help(i18n$t("Screens every candidate co-solvent for the loaded solute and ranks them (by Hansen miscibility distance in HSP mode, or by predicted scCO2 solubility enhancement in Solubility mode). Tick \"Also screen enhancing blends\" to additionally recommend two-solvent blends."),
                               title = i18n$t("Automatic Solvent Finder"), buttonLabel = i18n$t("OK"))),
                  value = FALSE),
                # Enhancing-blend screening — offered whenever the Auto Finder runs a blend-capable model:
                # HSP (volume-weighted Hansen-distance blending) and the CSP-2021 / openCOSMO-RS-Phi EoS
                # engines (genuine n-component solves). NOT for Makarov ML (no blend-mixing rule). Opt-in.
                conditionalPanel(
                  condition = sprintf("input['%s'] == true && (input['%s'] == 'hsp' || (input['%s'] == 'solubility' && (input['%s'] == 'csp' || input['%s'] == 'ocrsphi')))",
                                      ns("auto_finder"), ns("opt_method"), ns("opt_method"), ns("sol_engine"), ns("sol_engine")),
                  checkboxInput(ns("screen_blends"),
                    tags$span(tags$b(i18n$t("Also screen enhancing blends")),
                      input_help(i18n$t("Also screens two-solvent enhancing blends of the top-ranked single co-solvents. HSP mode uses volume-weighted Hansen-distance blending; the CSP-2021 / openCOSMO-RS-Phi EoS engines evaluate each blend directly as a true n-component solve (slower). Recommendations appear in the Recommended Enhancing Blends tab. Not available for the Makarov ML model, which has no blend-mixing rule."),
                                 title = i18n$t("Screen enhancing blends"), buttonLabel = i18n$t("OK"))),
                    value = FALSE))
              ),
              div(id = ns("modifier_selection_wrapper"), uiOutput(ns("modifier_selection_ui"))),
              # Solvent Blends Section — disabled (greyed, non-interactive) when Auto Finder is on.
              div(id = ns("blends_box_wrapper"),
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
              )  # /blends_box_wrapper
            ),

            # Process Conditions Section
            accordionItem(
              title = i18n$t("Process Conditions"),
              value = "process_conditions",
              collapsed = TRUE,
              conditionalPanel(
                condition = sprintf("input['%s'] != 'solubility'", ns("opt_method")),
                div(id = ns("vfrac_input_wrapper"), uiOutput(ns("vfrac_input_ui")))
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'solubility'", ns("opt_method")),
                div(id = ns("sol_molfrac_wrapper"), uiOutput(ns("sol_molfrac_ui")))
              ),
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
              # Solubility Unit is now inline with the Solubility Engine selector (in sol_molfrac_ui);
              # sol_unit_ui renders nothing but is kept so its outputOptions binding stays valid.
              div(id = ns("sol_unit_wrapper"), uiOutput(ns("sol_unit_ui"))),
              hr(),
              h5(tags$b(i18n$t("Best Modifier Table Display"))),
              div(id = ns("table_display_mode_wrapper"),
                uiOutput(ns("table_display_mode_ui"))
              )
            )
            # (Automatic Solvent Finder is now the checkbox in the Co-Solvent Selection section above.)
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
                  # (Export moved to the Export dropdown beside Run Selection — same download handler.)
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
              # Actual content when results are available (Export Results button moved to the Best
              # Modifier Table window, inline with the Show ME Values toggle).
              conditionalPanel(
                condition = "output.has_results",
                ns = ns,
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
              title = i18n$t("Recommended Enhancing Blends"),
              # Blends are available for HSP and for Solubility + Makarov (ML). The EoS engines
              # (CSP-2021 / openCOSMO-RS-Phi) evaluate a single co-solvent only -> show a note instead.
              conditionalPanel(
                condition = "!output.blends_supported",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("circle-info", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Blends are not available with the Makarov ML model")),
                  p(i18n$t("The Makarov ML model evaluates a single co-solvent at a time (ranked in the Summary tab). For enhancing-blend recommendations use HSP (Miscibility) mode or a COSMO-SAC-Phi EoS engine (CSP-2021 / openCOSMO-RS-Phi) — all genuine blend-capable models."))
                )
              ),
              # Blends supported but nothing produced yet (neither HSP finder nor an EoS blend screen): prompt.
              conditionalPanel(
                condition = "output.blends_supported && !output.has_finder_blends && !output.has_eos_blends",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("wand-magic-sparkles", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("No enhancing-blend recommendations yet")),
                  p(i18n$t("Tick the \"Automatic Solvent Finder\" checkbox (Co-Solvent Selection), then tick \"Also screen enhancing blends\", and run. Works in HSP (Miscibility) mode and in Solubility mode with a COSMO-SAC-Phi EoS engine (CSP-2021 / openCOSMO-RS-Phi)."))
                )
              ),
              # HSP finder blends.
              conditionalPanel(
                condition = "output.blends_supported && output.has_finder_blends",
                ns = ns,
                uiOutput(ns("finder_summary_ui")),
                div(style = "overflow-x: auto;", DT::dataTableOutput(ns("finder_blends_table")))
              ),
              # EoS (CSP / openCOSMO-RS-Phi) genuine n-component blends.
              conditionalPanel(
                condition = "output.blends_supported && output.has_eos_blends",
                ns = ns,
                div(style = "overflow-x: auto;", DT::dataTableOutput(ns("eos_blends_table")))
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
            )
            # (Detailed Results tab removed — it duplicated the per-condition data already in
            #  Miscibility Enhancement and the Summary table.)
          )
        )
      )
    )
  )
}
