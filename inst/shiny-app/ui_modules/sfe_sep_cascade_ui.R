sep_cascade_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  tagList(
    # Slightly larger base font for this module's result panels (the plots/tables read too small).
    # .ck-flush zeroes a checkbox's Bootstrap margins so it can be bottom-aligned with an input box
    # via flexbox (no hard-coded pixel offsets).
    tags$style(HTML(paste0(
      "#", ns("results_tabs"), " .tab-content{font-size:14px;}",
      "#", ns("results_tabs"), " table.dataTable{font-size:13px;}",
      " .ck-flush .form-group{margin:0 !important;} .ck-flush .checkbox{margin:0 !important;}",
      " .cascade-units-row .form-group{margin-bottom:0 !important;}",
      # Zero the co-solvent input's Bootstrap .form-group bottom margin (15px) so the left column's bottom
      # edge equals the VISIBLE input box bottom — otherwise the bottom-aligned "Use co-solvent" checkbox
      # sits ~15px too low (centred on the invisible margin instead of the box).
      " .cosolv-field .form-group{margin-bottom:0 !important;}"))),
    fluidRow(
      column(
        width = 4,
        fluidRow(
          column(6, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
          column(6, actionButton(ns("run_cascade"), i18n$t("Run"), class = "btn btn-primary btn-block", style = "color: white;"))
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Solubility & Separator Cascade")),
            div(uiOutput(ns("sfe_sep_cascade_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          # 15px horizontal margin matches the accordion panels' padding so this top selector lines up
          # with the accordion content below it (not a random offset — it is the panels' own padding).
          div(style = "margin: 2px 15px 0 15px;", uiOutput(ns("sol_model_ui"))),
          uiOutput(ns("sol_model_advice_ui")),
          # CSP-model management: DB size + "Add compound to CSP" (shown only when CSP is selected).
          uiOutput(ns("csp_manage_ui")),
          # openCOSMO-RS-Phi database-size note (shown only when that engine is selected).
          uiOutput(ns("ocrsphi_manage_ui")),
          div(style = "margin-bottom: 10px;"),
          shinydashboardPlus::accordion(
            id = ns("cascade_accordion"),

            # --- 1. Solute Selection (session import + inline database loader) ---
            accordionItem(
              title = i18n$t("Solute Selection"),
              value = "solute_selection",
              collapsed = FALSE,
              div(
                style = "margin-bottom: 10px; display:flex; flex-wrap:wrap; gap:5px;",
                actionButton(ns("load_example_data"), i18n$t("Load Examples"),
                  icon = icon("flask"), class = "btn btn-info btn-sm"),
                actionButton(ns("load_from_disk"), i18n$t("Load from Disk"),
                  icon = icon("folder-open"), class = "btn btn-default btn-sm"),
                # Inline database loader (opens a modal), replacing the old "Add from Database" accordion.
                actionButton(ns("open_db_search"), i18n$t("Load from Database"),
                  icon = icon("database"), class = "btn btn-success btn-sm", style = "color:white;")
              ),
              uiOutput(ns("saved_calculations_list_ui")),
              h5(i18n$t("Selected Solutes:")),
              uiOutput(ns("selected_solutes_ui"))
            ),

            # --- 2. Solubility-curve conditions ---
            accordionItem(
              title = i18n$t("Solubility Conditions"),
              value = "sol_conditions",
              collapsed = TRUE,
              div(id = ns("curve_temps_wrapper"), uiOutput(ns("curve_temps_ui"))),
              div(id = ns("curve_pres_wrapper"), uiOutput(ns("curve_pres_ui"))),
              selectInput(ns("eos"),
                label = tags$span(i18n$t("Density EoS"), " ",
                  input_help(i18n$t("Equation of state used to compute the CO₂ density that converts mole-fraction solubility to g/L and g/kg. Span-Wagner is the reference EoS; Bender is a faster alternative."),
                    title = i18n$t("Density EoS"), buttonLabel = i18n$t("OK"))),
                # Proper names - not translated (and keeping i18n$t out of setNames() names avoids the
                # shiny.i18n multi-match footgun that breaks choice vectors).
                choices = c("Span-Wagner" = "spanwagner", "Bender" = "bender"),
                selected = defaults$eos %||% "spanwagner"),
            ),

            # --- 2b. Co-Solvent(s) — its own collapsible section ---
            accordionItem(
              title = i18n$t("Co-Solvent(s)"),
              value = "cosolvent",
              collapsed = TRUE,
              # Header row: the "Total co-solvent" input (left, wide, so the title never wraps) + a
              # right-aligned "Use co-solvent" checkbox vertically CENTRED on the input box beside it. The
              # outer flex bottom-aligns the columns, so a flushed (zero-margin) checkbox wrapper of the input
              # box's height (BS3 .form-control = 34px) with align-items:center lands the checkbox on the
              # input's mid-line. The checkbox is disabled (info note below) for models that cannot represent
              # a co-solvent (Makarov, Experimental).
              # margin-bottom restores the spacing to the content below (Modifier Type): the input's own
              # .form-group margin is zeroed (.cosolv-field) so the checkbox can centre on the box, so the
              # gap lives on the row instead — external to the flex, it doesn't affect the centring.
              div(style = "display:flex; gap:18px; align-items:flex-end; flex-wrap:wrap; margin-bottom:15px;",
                div(class = "cosolv-field", style = "flex:1 1 260px;", uiOutput(ns("cosolvent_field_ui"))),
                div(class = "ck-flush", style = "display:flex; align-items:center; height:34px;",
                  checkboxInput(ns("use_cosolvent"), i18n$t("Use co-solvent"), value = FALSE))
              ),
              uiOutput(ns("cosolvent_block_info_ui")),
              # The co-solvent selection appears only once "Use co-solvent" is ticked: Modifier Type
              # (Pure / Custom Blend), then either the pure-modifier picker or the Custom Blend builder —
              # the exact same components as the Co-Solvent Comparison module.
              conditionalPanel(
                condition = "input.use_cosolvent == true", ns = ns,
                uiOutput(ns("modifier_type_ui")),
                conditionalPanel("input.modifier_type != 'blend'", ns = ns,
                  uiOutput(ns("modifier_selection_ui"))),
                conditionalPanel("input.modifier_type == 'blend'", ns = ns,
                  uiOutput(ns("custom_blend_box_ui"))),
                uiOutput(ns("cosolvent_misc_ui"))
              )
            ),

            # --- 3. Separator cascade (with automatic optimization) ---
            accordionItem(
              title = i18n$t("Separator Cascade"),
              value = "cascade",
              collapsed = TRUE,
              # Single-value settings, two per row (50/50) each with its range badge.
              fluidRow(
                column(6, div(id = ns("extract_temp_wrapper"), uiOutput(ns("extract_temp_ui")))),
                column(6, div(id = ns("extract_pres_wrapper"), uiOutput(ns("extract_pres_ui"))))
              ),
              fluidRow(
                column(6, div(id = ns("co2_flow_wrapper"), uiOutput(ns("co2_flow_ui")))),
                column(6, div(id = ns("recovery_target_wrapper"), uiOutput(ns("recovery_target_ui"))))
              ),
              fluidRow(
                column(6, div(id = ns("n_stages_wrapper"), uiOutput(ns("n_stages_ui")))),
                column(6, div(id = ns("recovery_pressure_wrapper"), uiOutput(ns("recovery_pressure_ui"))))
              ),
              # Separator pressures / temperatures. Each input carries its "Optimize" checkbox inline on
              # the right (rendered inside the badge by range_badge_multi_input, so it is vertically
              # centred on the input box); ticking it greys the input. Optimize is on by default.
              div(id = ns("sep_pres_wrapper"), uiOutput(ns("sep_pres_ui"))),
              div(id = ns("sep_temp_wrapper"), uiOutput(ns("sep_temp_ui"))),
              # Per-solute FEED LOADING (separated from the settings above by a rule): each selected solute
              # inline with its saturation badge, loading input and a red delete button.
              tags$hr(style = "margin:12px 0;"),
              # ---- Solute Loading and Deposition — its own sub-heading. The long usage note is now a hover
              # tooltip on this title (title=), and the estimate-loading / D₀ magic wands live inline with the
              # "g/kg CO₂" and "D₀" column headers inside feed_loadings_ui.
              # Title and the "Model co-deposition" toggle sit inline on one row (no wrap): the checkbox's
              # default Bootstrap margins are zeroed via .codep-inline so it baselines with the h5 heading.
              tags$style(HTML(".codep-inline .checkbox, .codep-inline .form-group{margin:0;} .codep-inline .checkbox label{font-weight:400;}")),
              div(style = "margin:2px 0 10px; display:flex; align-items:center; gap:16px; flex-wrap:nowrap;",
                h5(i18n$t("Solute Loading and Deposition"), style = "margin:0; font-weight:700; cursor:help; white-space:nowrap; flex:0 0 auto;",
                  title = i18n$t("Enter each solute's feed loading (g/kg CO₂) below. The optimizer reports where each solute begins to precipitate (onset) and the highest separator pressure recovering the target fraction, at its feed loading. Separator pressure/temperature are auto-found when \"Optimize\" is ticked, or entered manually; separators apply in the listed (decreasing) pressure order.")),
                div(class = "codep-inline", style = "flex:0 0 auto; white-space:nowrap;",
                  checkboxInput(ns("codep_enable"),
                    tagList(i18n$t("Model co-deposition"),
                      input_help(i18n$t("When on, a dilute solute that never reaches its own saturation is still partly trapped in the bulk (oil) deposit that precipitates at each stage, per a reference partition D₀ = γ(CO₂)/γ(oil). The density dependence is added automatically from the solute's own solubility curve — no calibration to separator data. Enter D₀ per solute in the column that appears, or auto-estimate it from structure."),
                      title = i18n$t("Co-deposition (entrainment)"))),
                    value = FALSE, width = "auto"))),
              uiOutput(ns("feed_loadings_ui"))
            )
          )
        )
      ),
      column(
        width = 8,
        tabBox(
          id = ns("results_tabs"),
          width = 12,
          # ---- Solubility Curves (interactive) + the solubility data table beneath it ----
          tabPanel(
            title = i18n$t("Solubility Curves"),
            conditionalPanel(
              condition = "!output.has_results", ns = ns,
              div(style = "text-align: center; padding: 50px; color: #888;",
                icon("chart-line", style = "font-size: 48px; margin-bottom: 15px;"),
                h4(i18n$t("Output data will appear here")),
                p(i18n$t("Select solutes and run to see results.")))
            ),
            conditionalPanel(
              condition = "output.has_results", ns = ns,
              # Units dropdown on the left, Export Plot inline on the right (white text on green).
              div(style = "display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:wrap; margin-bottom:12px;",
                div(uiOutput(ns("curve_basis_ui"))),
                downloadButton(ns("export_curve_plot"), i18n$t("Export Plot"),
                  class = "btn btn-success", style = "color:white;")),
              plotly::plotlyOutput(ns("solubility_plot"), height = "480px"),
              hr(),
              h4(style = "color:#3c8dbc; border-bottom:2px solid #3c8dbc; padding-bottom:5px; margin-top:6px;",
                i18n$t("Solubility Data")),
              div(style = "margin-bottom:8px;", uiOutput(ns("curve_solute_filter_ui"))),
              DT::dataTableOutput(ns("solubility_table"))
            )
          ),
          # ---- Optimization / Cascade Fractionation ----
          tabPanel(
            title = i18n$t("Cascade Fractionation"),
            conditionalPanel(
              condition = "!output.has_cascade", ns = ns,
              div(style = "text-align: center; padding: 50px; color: #888;",
                icon("layer-group", style = "font-size: 48px; margin-bottom: 15px;"),
                h4(i18n$t("Output data will appear here")),
                p(i18n$t("Set the cascade conditions and run to see fractionation.")))
            ),
            conditionalPanel(
              condition = "output.has_cascade", ns = ns,
              # Section title (larger — matches the section headers in Solute Characterization).
              h4(style = "color:#3c8dbc; border-bottom:2px solid #3c8dbc; padding-bottom:5px; margin:0 0 12px 0; font-weight:600;",
                icon("bullseye"), " ", i18n$t("Target-fractionation optimization")),
              # PLOT first: one combined figure per solute (recovery design curve + per-separator capture),
              # with the solute selector aligned to the Export buttons. "Export All Plots" only when >1 solute.
              fluidRow(
                column(5, div(class = "cascade-units-row", uiOutput(ns("cascade_solute_sel_ui")))),
                column(7, div(style = "display:flex; gap:8px; justify-content:flex-end; align-items:flex-end; height:100%;",
                  downloadButton(ns("export_current_plot"), i18n$t("Export Plot"),
                    class = "btn btn-success btn-sm", style = "color:white;"),
                  conditionalPanel(condition = "output.has_multiple_solutes", ns = ns,
                    downloadButton(ns("export_all_plots"), i18n$t("Export All Plots"),
                      class = "btn btn-success btn-sm", style = "color:white;"))))
              ),
              plotly::plotlyOutput(ns("combined_cascade_plot"), height = "460px"),
              hr(),
              # Precipitation onset & recovery table (Group-Contribution table-title styling, slightly smaller).
              tags$h5(style = "color:#605ca8; border-bottom:2px solid #605ca8; padding-bottom:4px; margin:2px 0 10px 0; font-weight:600; font-size:14px;",
                i18n$t("Precipitation onset & recovery")),
              DT::dataTableOutput(ns("onset_table")),
              tags$hr(style = "margin:16px 0;"),
              # Recovery per separator stage (%).
              tags$h5(style = "color:#00a65a; border-bottom:2px solid #00a65a; padding-bottom:4px; margin:2px 0 10px 0; font-weight:600; font-size:14px;",
                i18n$t("Recovery per separator stage (%)")),
              DT::dataTableOutput(ns("recovery_table"))
            )
          )
        )
      )
    )
  )
}
