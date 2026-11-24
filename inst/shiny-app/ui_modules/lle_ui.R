# Liquid-Liquid Extraction (LLE) solvent-screening UI module.
# Ranks / optimises candidate extraction solvents for target solute(s) via COSMO-SAC-dsp or the
# openCOSMO-RS-Phi EoS: distribution coefficient D, capacity, selectivity, real two-phase flash and
# mixed-solvent (blend) optimisation. Engines: csac_lle() / csac_lle_extract() / csac_lle_optimize_blend()
# and ocrsphi_lle_extract(). All long guidance lives in blue help icons (input_help) next to each field.
lle_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  # Small helper: a field label carrying a trailing blue help icon (keeps the long text off the form).
  lbl <- function(text, help, title = NULL)
    tagList(text, input_help(help, title = if (is.null(title)) text else title, buttonLabel = i18n$t("OK")))

  # GSK colour-dot render for the candidate-solvent selectize (set at creation so it survives choice
  # updates). Maps each solvent name to its sustainability-band hex; off-list additions render grey.
  .SOLV <- tryCatch(supeRcrit::lle_solvents(), error = function(e) NULL)
  .solv_render <- if (!is.null(.SOLV)) {
    band <- supeRcrit::lle_solvent_band(.SOLV$gsk)
    colmap_js <- paste0("{", paste0('"', gsub('"', '', .SOLV$name), '":"', band$colour, '"', collapse = ","), "}")
    I(sprintf(paste0(
      "(function(){ var M=%s;",
      " var dot=function(v){var c=M[v]||'#6c757d';return '<span style=\"display:inline-block;width:9px;height:9px;border-radius:50%%;background:'+c+';margin-right:7px;margin-left:2px;vertical-align:middle;\"></span>';};",
      " return { option: function(item,escape){return '<div style=\"padding-left:6px;\">'+dot(item.value)+escape(item.label)+'</div>';},",
      " item: function(item,escape){return '<div>'+dot(item.value)+escape(item.label)+'</div>';} }; })()"),
      colmap_js))
  } else NULL

  tagList(
    fluidRow(
      column(
        width = 4,
        # Action row: Reset | Run | Export (Export contextually enabled once a run has produced results —
        # same pattern/placement as the other modules; Load Example moved into the Setup accordion below).
        fluidRow(
          column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
          column(4,
            # Run swaps to a red Kill button while ANY cancellable background run (screen / flash / optimize)
            # is in flight. Rendered SERVER-SIDE (uiOutput + renderUI) — the same robust mechanism the
            # Fractionation Column module uses for its Cancel button, so the swap never depends on a
            # client-side conditionalPanel boolean reaching the browser.
            uiOutput(ns("lle_run_button"))),
          column(4,
            conditionalPanel(condition = "!output.has_any_results", ns = ns,
              tags$button(class = "btn btn-success dropdown-toggle", disabled = NA,
                style = "color:white; opacity:0.65; width:100%;",
                icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret"))),
            conditionalPanel(condition = "output.has_any_results", ns = ns,
              div(class = "btn-group", style = "width:100%;",
                tags$button(type = "button", class = "btn btn-success dropdown-toggle",
                  `data-toggle` = "dropdown", `aria-haspopup` = "true", `aria-expanded` = "false",
                  style = "color:white; width:100%;",
                  icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret")),
                tags$ul(class = "dropdown-menu dropdown-menu-right", style = "min-width:100%;",
                  tags$li(downloadLink(ns("export_pdf"), tagList(icon("file-pdf"), " ", i18n$t("Export PDF")),
                    style = "padding:5px 15px; display:block;")),
                  tags$li(downloadLink(ns("export_csv"), tagList(icon("file-csv"), " ", i18n$t("Export CSV")),
                    style = "padding:5px 15px; display:block;"))))))
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Liquid-Liquid Extraction")),
            div(uiOutput(ns("lle_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          shinydashboardPlus::accordion(
            id = ns("lle_accordion"),

            # --- Setup, feed phase & operating conditions ---
            accordionItem(
              title = i18n$t("Setup & Feed Phase"),
              value = "setup_feed",
              collapsed = FALSE,

              # Load a built-in industry example (same button style as the other modules).
              div(style = "margin-bottom: 10px;",
                actionButton(ns("load_example"), i18n$t("Load Example"),
                  icon = icon("flask"), class = "btn btn-info btn-sm")),

              # Thermodynamic engine (dropdown) + a small info badge showing coverage.
              selectInput(ns("engine"),
                lbl(i18n$t("Thermodynamic engine"),
                    i18n$t("COSMO-SAC-dsp equates activity coefficients from bundled sigma-profiles — broad and fast, and the only engine for Quick screen and Optimize blend. openCOSMO-RS-Phi is a rigorous equation-of-state COSMO model that equates fugacities at a fixed pressure (more accurate, slower, individually-profiled compounds) and is used for the Two-phase flash. CS25 is the 2025 COSMO-SAC reparametrisation — the most accurate for water/organic mutual solubilities, Two-phase flash only, and limited to the compounds in its built-in database."),
                    title = i18n$t("Thermodynamic engine")),
                # CS25 appears only where the local bridge (Java + model install) is detected.
                choices = c(c("COSMO-SAC-dsp" = "csac", "openCOSMO-RS-Phi (EoS)" = "ocrsphi"),
                            if (exists("cs25_available") && isTRUE(cs25_available()))
                              c("CS25" = "cs25")),
                selected = "csac", width = "100%"),
              uiOutput(ns("engine_info")),

              # Pressure only matters for the EoS engine.
              conditionalPanel(condition = "input.engine == 'ocrsphi'", ns = ns,
                uiOutput(ns("pressure_ui"))),

              # Analysis mode (dropdown).
              selectInput(ns("mode"),
                lbl(i18n$t("Analysis mode"),
                    i18n$t("Quick screen ranks many solvents fast from infinite-dilution activity coefficients (gamma-infinity). Two-phase flash solves the real liquid-liquid equilibrium (Michelsen stability + isoactivity): it splits the feed into two phases, gives each solute's true distribution, the mutual solvent solubility, and can scan a temperature range. Optimize blend tests BINARY co-solvent mixtures — every pair of the top candidate solvents is optimised over its composition and temperature — and ranks them by selectivity or distribution."),
                    title = i18n$t("Analysis mode")),
                choices = c("Quick screen (rank solvents)" = "screen",
                            "Two-phase flash (rigorous split + T scan)" = "flash",
                            "Optimize blend (best binary co-solvent)" = "optimize"),
                selected = "screen", width = "100%"),

              # Compound database (COSMO-SAC only) + info badge.
              conditionalPanel(condition = "input.engine == 'csac'", ns = ns,
                selectInput(ns("database"),
                  lbl(i18n$t("Compound database"),
                      i18n$t("Curated: 2,259 named, CAS-tagged compounds (DMol3 profiles). CHAOS: ~53,000 compounds (Gond et al. 2026, omegaB97X-D) for broad / designer coverage — 6,805 carry a resolved common name + CAS; the rest are addressed by SMILES (a new SMILES is estimated on the fly by FastSigma). The two databases use different quantum chemistry and cannot be mixed in one ranking."),
                      title = i18n$t("Compound database")),
                  choices = c("Curated (named + CAS)" = "ud", "CHAOS (broad coverage)" = "chaos"),
                  selected = "chaos", width = "100%"),
                uiOutput(ns("db_info"))),

              # Feed / diluent phase (single), name/CAS-searchable. (Rendered before the solutes so the
              # per-solute list can reference it.)
              # Feed / diluent phase — label row with a right-aligned Browse-database button, then the picker
              # and a small "common solvents only" toggle (the full database carries many obscure compounds).
              tags$style(HTML(".lle-labelrow{display:flex;justify-content:space-between;align-items:flex-end;gap:8px;} .lle-labelrow>.lle-title{min-width:0;margin-bottom:5px;} .lle-labelrow .btn{white-space:nowrap;flex-shrink:0;margin-bottom:5px;} .lle-compact-check .form-group{margin:-2px 0 8px;} .lle-compact-check .checkbox{margin:0;} .lle-compact-check .checkbox label{display:flex;align-items:center;gap:6px;padding-left:0;font-weight:400;font-size:13px;} .lle-compact-check .checkbox input[type=checkbox]{position:static;margin:0;}")),
              div(class = "lle-labelrow",
                tags$label(class = "control-label lle-title",
                  lbl(i18n$t("Feed / diluent phase"),
                      i18n$t("The phase the solute starts in (the liquid to extract from) — commonly water or an oil/organic diluent. A good extraction solvent must be largely immiscible with this feed so two liquid phases form."),
                      title = i18n$t("Feed / diluent phase"))),
                div(style = "display:flex; gap:6px; flex-shrink:0;",
                  actionButton(ns("clear_feed"), tagList(icon("times"), i18n$t("Clear")),
                    class = "btn btn-default btn-xs", title = i18n$t("Clear the feed selection")),
                  actionButton(ns("browse_feed"), tagList(icon("table"), i18n$t("Browse database")),
                    class = "btn btn-default btn-xs"))),
              selectizeInput(ns("feed"), label = NULL, choices = NULL, width = "100%",
                options = list(placeholder = i18n$t("Search name or CAS — type to filter"))),
              div(class = "lle-compact-check",
                checkboxInput(ns("feed_common"), i18n$t("Show common solvents only"), value = TRUE)),

              # Temperature — a single value for screen; a constrained scan (range slider + step) for
              # flash/optimize. The "T" symbol is italicised throughout.
              conditionalPanel(condition = "input.mode == 'screen'", ns = ns,
                uiOutput(ns("temp_ui"))),
              conditionalPanel(condition = "input.mode == 'flash' || input.mode == 'optimize'", ns = ns,
                # Temperature scan as explicit Min / Max / Step numeric inputs (range checked at run time).
                tags$label(class = "control-label",
                  lbl(i18n$t("Temperature range (°C)"),
                      i18n$t("The temperature window the flash / optimizer scans. Distribution and mutual solubility both shift with temperature, so scanning finds the best operating point."),
                      title = i18n$t("Temperature range"))),
                # Min / Max / Step as range-badge inputs (rendered server-side so each badge turns red when its
                # value leaves the valid window — same mechanism as the single-temperature input and sfe_misc_opt).
                div(style = "display:flex; gap:10px; align-items:flex-start;",
                  div(style = "flex:1; min-width:0;", uiOutput(ns("temp_lo_ui"))),
                  div(style = "flex:1; min-width:0;", uiOutput(ns("temp_hi_ui"))),
                  div(style = "flex:1; min-width:0;", uiOutput(ns("temp_step_ui")))),
                # Solvent : feed — value + unit toggle (mole ratio / mass ratio / volume %), always optimised.
                uiOutput(ns("sf_ui")))
            ),

            # --- Target solute(s) + per-solute concentrations (own accordion) ---
            accordionItem(
              title = i18n$t("Target Solute(s)"),
              value = "target_solutes",
              collapsed = FALSE,

              # Target solute(s): ONE tag-based, name/CAS-searchable multi-select to ADD solutes. The first
              # is the target; further tags are co-extracted solutes for the separation selectivity. Each
              # selected solute then gets its own concentration input in the per-solute list below. The
              # Browse-database button is right-justified on the label row.
              div(class = "lle-labelrow",
                tags$label(class = "control-label lle-title",
                  lbl(i18n$t("Target solute(s)"),
                      i18n$t("Search by name or CAS and add one or more solutes, or use Browse database to pick from a table. The first is the target to extract; any further solutes are co-extracted species — the separation selectivity is the target's distribution over theirs. Set each solute's own concentration in the list below. In CHAOS you may also type/paste a SMILES for an off-list compound (FastSigma estimates its profile)."),
                      title = i18n$t("Target solute(s)"))),
                actionButton(ns("browse_solutes"), tagList(icon("table"), i18n$t("Browse database")),
                  class = "btn btn-default btn-xs")),
              selectizeInput(ns("solutes"), label = NULL,
                choices = NULL, multiple = TRUE, width = "100%",
                options = list(placeholder = i18n$t("Search name or CAS, or Browse the database"))),
              # FastSigma guidance: off-database solutes can be entered by SMILES and estimated by FastSigma,
              # but only against the CHAOS set (FastSigma is trained on CHAOS; it cannot mix with the curated
              # DMol3 profiles). The note switches on the selected database.
              uiOutput(ns("fastsigma_note")),
              # Per-solute concentration list (each solute's own value) + a unit toggle (mol·mol⁻¹ / g·L⁻¹).
              # Hidden for Quick screen: it ranks by infinite-dilution gamma ratios, so the concentration
              # does not enter the result — it only matters for the finite-concentration flash / optimize.
              conditionalPanel(condition = "input.mode != 'screen'", ns = ns,
                uiOutput(ns("solute_conc_ui")))
            ),

            # --- Candidate solvents (explicit list, GSK-rated) ---
            accordionItem(
              title = i18n$t("Candidate Solvents"),
              value = "solvents",
              collapsed = FALSE,

              # Candidate source: hand-pick from the curated list, or AMS-style auto-search over a broad
              # vetted solvent library (Tier 2) / the whole openCOSMO-RS-Phi set (Tier 1). Hidden for CS25,
              # which only offers the hand-pick option (no point showing a single-choice dropdown).
              conditionalPanel(condition = "input.engine != 'cs25'", ns = ns,
                selectInput(ns("cand_source"),
                  lbl(i18n$t("Candidate source"),
                      i18n$t("Choose solvents lets you hand-pick from the curated GSK library. Extended solvent library auto-searches a vetted set of ~190 common extraction/process solvents across every chemical class — the AMS-style 'search the whole solvent database' — automatically restricted to those liquid at your temperature. (openCOSMO-RS-Phi additionally offers its entire profiled set.) Auto modes apply to Quick screen and Two-phase flash; Optimize blend always uses a hand-picked set."),
                      title = i18n$t("Candidate source")),
                  choices = c("Choose solvents (curated list)" = "pick",
                              "Auto: extended solvent library" = "extended"),
                  selected = "pick", width = "100%")),
              uiOutput(ns("cand_note")),

              # Manual picker (shown only when hand-picking).
              conditionalPanel(condition = "input.cand_source == 'pick'", ns = ns,
                # Explicit multi-select of candidate solvents from the curated GSK library (add as many as you
                # like). In Optimize blend mode these are the solvents mixed together (choose 2 or more).
                selectizeInput(ns("candidate_solvents"),
                  lbl(i18n$t("Candidate solvents"),
                      i18n$t("Add as many candidate extraction solvents as you like from the curated library (grounded in GSK's solvent sustainability guide). Each option is colour-coded by sustainability band in the dropdown — green Recommended through red Highly Hazardous (see the legend below). In Optimize blend mode these are the solvents blended together (pick at least two). You may also type a solvent not on the list."),
                      title = i18n$t("Candidate solvents")),
                  choices = NULL, multiple = TRUE, width = "100%",
                  options = c(list(placeholder = i18n$t("Add candidate solvents — colour shows GSK rating"),
                                   create = TRUE, plugins = list("remove_button")),
                              if (!is.null(.solv_render)) list(render = .solv_render) else NULL)),
                # Compact colour legend (static) — the per-solvent detail lives in the dropdown itself.
                div(style = "font-size:11px; color:#888; margin:-4px 0 10px;",
                  i18n$t("GSK rating: "),
                  tags$span(style = "color:#2e7d32;", HTML("&#9679; "), i18n$t("Recommended")), HTML("&nbsp; "),
                  tags$span(style = "color:#f9a825;", HTML("&#9679; "), i18n$t("Some issues")), HTML("&nbsp; "),
                  tags$span(style = "color:#e65100;", HTML("&#9679; "), i18n$t("Hazardous")), HTML("&nbsp; "),
                  tags$span(style = "color:#b71c1c;", HTML("&#9679; "), i18n$t("Highly hazardous"))),
                # Live feed↔solvent miscibility check (LLE needs an IMMISCIBLE pair to form two phases).
                uiOutput(ns("miscibility_ui"))),

              checkboxInput(ns("dsp"),
                lbl(i18n$t("Include dispersion term (recommended)"),
                    i18n$t("Adds the 2014 COSMO-SAC-dsp dispersion contribution. Recommended: it improves activity coefficients for non-polar and aromatic systems. Automatically skipped for any compound lacking a dispersion parameter."),
                    title = i18n$t("Dispersion term")),
                value = TRUE)
            ),

            # --- Optimization settings (Optimize blend mode) ---
            accordionItem(
              title = i18n$t("Optimization Settings"),
              value = "opt_settings",
              collapsed = FALSE,
              conditionalPanel(condition = "input.mode == 'optimize'", ns = ns,
                selectInput(ns("objective"),
                  lbl(i18n$t("Optimize for"),
                      i18n$t("Selectivity maximises the target solute's distribution relative to a co-extracted solute (needs at least two solutes). Target distribution maximises the target solute's own distribution coefficient into the extract phase."),
                      title = i18n$t("Optimization objective")),
                  choices = c("Separation selectivity" = "selectivity",
                              "Target distribution coefficient" = "distribution"),
                  selected = "selectivity", width = "100%"),
                uiOutput(ns("multistart_ui")),
                # Warm-start: a plain inline checkbox (label beside the box) on its own row, with the help
                # icon alongside — avoids the fragile label-above-a-numeric-input alignment.
                div(style = "display:flex; align-items:center; gap:4px; margin:2px 0 12px;",
                  tags$div(class = "lle-warmstart-box",
                    checkboxInput(ns("warmstart"), tagList(i18n$t("Warm-start over "), tags$em("T")), value = TRUE)),
                  input_help(i18n$t("Seeds each temperature's optimisation from the previous temperature's optimum, so the search tracks the best recipe smoothly across the scan instead of restarting cold."),
                    title = i18n$t("Warm-start"), buttonLabel = i18n$t("OK"))),
                tags$style(HTML(".lle-warmstart-box .checkbox{margin:0;} .lle-warmstart-box .form-group{margin:0;}")),
                selectInput(ns("opt_method"),
                  lbl(i18n$t("Objective evaluation"),
                      i18n$t("Fast scores each trial blend from infinite-dilution gamma ratios (AMS-style, ~1 s; the winner is still verified with one full flash). Rigorous re-flashes every trial blend at finite concentration — slower but exact."),
                      title = i18n$t("Objective evaluation")),
                  choices = c("Fast (gamma-infinity ratios)" = "gamma_inf",
                              "Rigorous (full two-phase flash)" = "flash"),
                  selected = "gamma_inf", width = "100%")),
              conditionalPanel(condition = "input.mode != 'optimize'", ns = ns,
                helpText(i18n$t("These settings apply to the Optimize blend analysis mode.")))
            )
          )
        )
      ),
      column(
        width = 8,
        # Let long value-box values (blend recipes, long solvent names) wrap/shrink instead of overflowing
        # the box — shinydashboard sets white-space:nowrap + 38px on .small-box h3 by default.
        tags$style(HTML(".lle-results .small-box h3{white-space:normal;word-break:break-word;overflow-wrap:anywhere;font-size:24px;line-height:1.15;} .lle-results .small-box .inner{overflow:hidden;}")),
        div(class = "lle-results",
        fluidRow(
          tabBox(
            id = ns("results_tabs"),
            width = 12,
            # --- Ranked Solvents (screen mode): ranking table + bar chart + solute sigma-profile, combined ---
            tabPanel(
              title = i18n$t("Ranked Solvents"),
              conditionalPanel(
                condition = "!output.has_results", ns = ns,
                div(style = "text-align: center; padding: 50px; color: #888;",
                  icon("droplet", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Choose a solute, feed phase and candidate solvents, then run the screening.")))
              ),
              conditionalPanel(
                condition = "output.has_results", ns = ns,
                fluidRow(
                  valueBoxOutput(ns("best_solvent_vb"), width = 4),
                  valueBoxOutput(ns("best_d_vb"), width = 4),
                  valueBoxOutput(ns("n_candidates_vb"), width = 4)
                ),
                div(style = "overflow-x: auto;", DT::dataTableOutput(ns("ranking_table"))),
                tags$hr(),
                # Solute σ-profile (the distribution-coefficient bar was removed — the ranked table conveys D).
                div(style = "display:flex; align-items:flex-start; justify-content:space-between; gap:10px;",
                  tags$label(class = "control-label",
                    lbl(i18n$t("Solute σ-profile"),
                        i18n$t("COSMO-SAC σ-profile of the selected solute. When the compound's structure is known, a SMILES-based FastSigma estimate is overlaid for comparison; the engine itself uses the experimental (database) profile unless an off-database SMILES is entered, in which case the FastSigma estimate is used."),
                        title = i18n$t("Solute σ-profile"))),
                  downloadButton(ns("dl_sigma_plot"), i18n$t("Download plot"),
                    class = "btn btn-success btn-sm", style = "color:white; white-space:nowrap;")),
                plotOutput(ns("sigma_plot"), height = "430px")
              )
            ),
            # --- Two-phase flash / optimize results (flash mode): the Distribution-vs-T plot is merged in here ---
            tabPanel(
              title = i18n$t("Two-Phase Optimize"),
              conditionalPanel(
                condition = "!output.has_flash", ns = ns,
                div(style = "text-align: center; padding: 50px; color: #888;",
                  icon("layer-group", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Switch to Two-phase flash mode, choose the solute(s) to separate, then run.")))
              ),
              conditionalPanel(
                condition = "output.has_flash", ns = ns,
                fluidRow(
                  valueBoxOutput(ns("flash_best_vb"), width = 4),
                  valueBoxOutput(ns("flash_sel_vb"), width = 4),
                  valueBoxOutput(ns("flash_n_vb"), width = 4)
                ),
                div(style = "overflow-x: auto;", DT::dataTableOutput(ns("flash_table"))),
                tags$hr(),
                # Distribution / selectivity vs temperature across the scan (was its own tab).
                div(style = "display:flex; align-items:flex-start; justify-content:space-between; gap:10px;",
                  tags$label(class = "control-label",
                    lbl(i18n$t("Distribution vs temperature"),
                        i18n$t("How each candidate solvent's distribution coefficient (or separation selectivity, with two solutes) changes across the temperature scan."),
                        title = i18n$t("Distribution vs temperature"))),
                  downloadButton(ns("dl_flash_plot"), i18n$t("Download plot"),
                    class = "btn btn-success btn-sm", style = "color:white; white-space:nowrap;")),
                plotly::plotlyOutput(ns("flash_plot"), height = "480px"),
                tags$hr(),
                h4(i18n$t("Phase compositions (best solvent / temperature)")),
                helpText(i18n$t("Mole-fraction composition of the two equilibrium liquid phases for the top-ranked solvent and temperature, and each component's distribution D = x(extract) / x(raffinate).")),
                div(style = "overflow-x: auto;", DT::dataTableOutput(ns("flash_phase_table")))
              )
            ),
            # --- Blend-composition optimizer results (optimize mode) ---
            tabPanel(
              title = i18n$t("Blend Optimizer"),
              conditionalPanel(
                condition = "!output.has_opt", ns = ns,
                div(style = "text-align: center; padding: 50px; color: #888;",
                  icon("flask-vial", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Output data will appear here")),
                  p(i18n$t("Switch to Optimize blend mode, choose 2 or more solvents to mix, then run.")))
              ),
              conditionalPanel(
                condition = "output.has_opt", ns = ns,
                fluidRow(
                  valueBoxOutput(ns("opt_recipe_vb"), width = 5),
                  valueBoxOutput(ns("opt_value_vb"), width = 4),
                  valueBoxOutput(ns("opt_temp_vb"), width = 3)
                ),
                h4(i18n$t("Optimal solvent / blend composition")),
                div(style = "overflow-x: auto;", DT::dataTableOutput(ns("opt_recipe_table"))),
                br(),
                h4(i18n$t("Candidate solvents & blends (ranked)")),
                helpText(i18n$t("Only binary mixtures are considered (a pure solvent or one co-solvent): every pair of the top candidate solvents is optimised over its composition and temperature, then ranked by the objective.")),
                div(style = "overflow-x: auto;", DT::dataTableOutput(ns("opt_trace_table")))
              )
            )
          )
        )
        )
      )
    )
  )
}
