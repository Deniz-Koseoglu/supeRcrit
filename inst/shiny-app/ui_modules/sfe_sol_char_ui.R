solute_characterization_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  tagList(
    # Molecular Descriptors table: first (Property) column shrinks to fit its labels without wrapping;
    # the Value column takes the remaining width. Combined with table-layout:auto this re-flows on
    # language change with no fixed pixel widths.
    tags$style(HTML(paste0(
      ".mol-desc-table{table-layout:auto !important;}",
      ".mol-desc-table td:first-child, .mol-desc-table th:first-child{white-space:nowrap;width:1%;}"))),
    fluidRow(
      column(
        width = 4,
        # Top buttons row
        fluidRow(
          style = "display: flex; align-items: stretch;",
          column(4, style = "display: flex; align-items: center;",
            actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")
          ),
          column(4, style = "display: flex; align-items: center;",
            # Split button for Calculate with Compare in dropdown
            div(
              class = "btn-group btn-block",
              actionButton(
                ns("characterize_solute"), 
                i18n$t("Calculate"), 
                class = "btn-primary",
                style = "color: white; width: calc(100% - 30px); border-top-right-radius: 0; border-bottom-right-radius: 0;"
              ),
              tags$button(
                type = "button",
                class = "btn btn-primary dropdown-toggle",
                style = "color: white; width: 30px; border-top-left-radius: 0; border-bottom-left-radius: 0; padding-left: 8px; padding-right: 8px;",
                `data-toggle` = "dropdown",
                `aria-haspopup` = "true",
                `aria-expanded` = "false",
                tags$span(class = "caret")
              ),
              tags$ul(
                class = "dropdown-menu",
                style = "min-width: 100%;",
                tags$li(
                  actionLink(ns("compare_gcm"), i18n$t("Compare"), style = "padding: 5px 15px; display: block;")
                )
              )
            )
          ),
          column(4, style = "display: flex; align-items: center;",
            div(style = "width: 100%;",
              # Export dropdown, consistent with the other modules. The menu exports PDF / CSV and also keeps
              # "Save to Disk" (which persists a CHARACTERIZED solute — input selections + resolved Tm / Hvap /
              # Chrastil coefficients — for downstream modules to import). Before results: a disabled placeholder.
              conditionalPanel(condition = "!output.has_results", ns = ns,
                tags$button(class = "btn btn-success btn-block dropdown-toggle", disabled = NA,
                  style = "color: white; opacity: 0.65; width:100%; margin:0;", icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret"))),
              conditionalPanel(condition = "output.has_results", ns = ns,
                # Match rp_export_dropdown_col EXACTLY (plain btn-group + width:100%/display:block + buttons
                # width:100%/margin:0) so Export lines up with the flex-centred Reset / Calculate columns.
                div(class = "btn-group", style = "width:100%; display:block;",
                  tags$button(type = "button", class = "btn btn-success btn-block dropdown-toggle",
                    style = "color: white; width:100%; margin:0;",
                    `data-toggle` = "dropdown", `aria-haspopup` = "true", `aria-expanded` = "false",
                    icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret")),
                  tags$ul(class = "dropdown-menu dropdown-menu-right", style = "min-width: 100%;",
                    tags$li(downloadLink(ns("export_pdf"), tagList(icon("file-pdf"), " ", i18n$t("Export PDF")),
                      style = "padding: 5px 15px; display: block;")),
                    tags$li(downloadLink(ns("export_csv"), tagList(icon("file-csv"), " ", i18n$t("Export CSV")),
                      style = "padding: 5px 15px; display: block;")),
                    tags$li(class = "divider"),
                    tags$li(actionLink(ns("save_to_disk"), tagList(icon("floppy-disk"), " ", i18n$t("Save to Disk")),
                      style = "padding: 5px 15px; display: block;"))
                  )
                )
              )
            )
          )
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Solute Characterization")),
            div(uiOutput(ns("sfe_sol_char_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          shinydashboardPlus::accordion(
            id = ns("accordion"),
            accordionItem(
              title = i18n$t("Solute Identification"),
              value = "solute_id",
              collapsed = FALSE,
              div(style = "margin-bottom: 8px; display: flex; gap: 8px;",
                actionButton(ns("open_db_modal"), i18n$t("Load or Add Database Entry"),
                  icon = icon("database"), class = "btn btn-success btn-sm", style = "color:white;")),
              fluidRow(
                column(6, div(id = ns("cas_input_wrapper"), uiOutput(ns("cas_input_ui")))),
                column(6, div(id = ns("name_input_wrapper"), uiOutput(ns("name_input_ui"))))
              ),
              # SMILES with its own activation toggle (no longer gated behind a "Specify SMILES" checkbox).
              div(id = ns("smiles_input_wrapper"), uiOutput(ns("smiles_input_ui"))),
              # auto-notify when the entered compound has an experimental DB record
              uiOutput(ns("db_notify_ui")),
              # COSMO scCO₂ solubility engine (CSP-2021 / openCOSMO-RS-Phi) database-coverage indicator,
              # with on-demand structural-similarity suggestions when the solute is absent.
              uiOutput(ns("cosmo_avail_ui"))
            ),
            accordionItem(
              title = i18n$t("Solute Characterization (Estimators)"),
              value = "characterize",
              collapsed = TRUE,

              # === Phase-Transition Properties: boiling point (Tb), melting point (Tm), heat of vaporization ===
              tags$h5(style = "color:#d35400; border-bottom:2px solid #d35400; padding-bottom:4px; margin:2px 0 10px 0; font-weight:600;",
                icon("icicles"), " ", i18n$t("Phase-Transition Properties")),
              div(style = "display: flex; gap: 10px; align-items: flex-start;",
                div(id = ns("gcm_tb_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_tb_ui"))),
                div(id = ns("gcm_tb_gorder_wrapper"), style = "width: 160px;", uiOutput(ns("gcm_tb_gorder_ui"))),
                shinyjs::hidden(
                  div(
                    id = ns("tb_manual_div"),
                    style = "width: 160px;",
                    uiOutput(ns("tb_manual_input_ui"))
                  )
                )
              ),
              div(style = "display: flex; gap: 10px; align-items: flex-start; margin-top: 6px;",
                div(id = ns("gcm_tm_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_tm_ui"))),
                shinyjs::hidden(div(id = ns("tm_manual_div"), style = "width: 160px;", uiOutput(ns("tm_manual_input_ui"))))
              ),
              div(style = "display: flex; gap: 10px; align-items: flex-start;",
                div(id = ns("gcm_hvap_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_hvap_ui"))),
                shinyjs::hidden(div(id = ns("hvap_manual_div"), style = "width: 160px;", uiOutput(ns("hvap_manual_input_ui"))))
              ),
              # Fusion enthalpy (ΔHfus) via the Chickos-Acree GCM — disabled until a melting point is chosen.
              div(style = "display: flex; gap: 10px; align-items: flex-start; margin-top: 6px;",
                div(id = ns("gcm_hfus_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_hfus_ui"))),
                shinyjs::hidden(div(id = ns("hfus_manual_div"), style = "width: 160px;", uiOutput(ns("hfus_manual_input_ui"))))
              ),
              tags$hr(style = "margin:14px 0;"),

              # === Critical Parameters (Tc, Pc, Vc) ===
              tags$h5(style = "color:#f39c12; border-bottom:2px solid #f39c12; padding-bottom:4px; margin:2px 0 10px 0; font-weight:600;",
                icon("temperature-high"), " ", i18n$t("Critical Parameters")),
              div(style = "display: flex; gap: 10px; align-items: flex-start;",
                div(id = ns("gcm_crit_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_crit_ui"))),
                div(id = ns("gcm_crit_gorder_wrapper"), style = "width: 160px;", uiOutput(ns("gcm_crit_gorder_ui")))
              ),
              shinyjs::hidden(div(id = ns("crit_manual_div"), uiOutput(ns("crit_manual_input_ui")))),
              tags$hr(style = "margin:14px 0;"),

              # === Hansen Solubility Parameters ===
              tags$h5(style = "color:#605ca8; border-bottom:2px solid #605ca8; padding-bottom:4px; margin:2px 0 10px 0; font-weight:600;",
                icon("water"), " ", i18n$t("Hansen Solubility Parameters")),
              div(style = "display: flex; gap: 10px; align-items: flex-start;",
                div(id = ns("gcm_hsp_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_hsp_ui"))),
                div(id = ns("gcm_hsp_gorder_wrapper"), style = "width: 160px;", uiOutput(ns("gcm_hsp_gorder_ui")))
              ),
              shinyjs::hidden(div(id = ns("hsp_manual_div"), uiOutput(ns("hsp_manual_input_ui")))),
              tags$hr(style = "margin:14px 0;"),

              # === Other Parameters: Van der Waals volume (Makarov ML scCO₂ solubility removed) ===
              tags$h5(style = "color:#00a65a; border-bottom:2px solid #00a65a; padding-bottom:4px; margin:2px 0 10px 0; font-weight:600;",
                icon("cube"), " ", i18n$t("Other Parameters")),
              div(id = ns("gcm_vdw_wrapper"), uiOutput(ns("gcm_vdw_ui"))),
              tags$hr(style = "margin:14px 0;"),

              # === Chrastil Solubility Parameters (experimental / manual input only; no GCM estimate) ===
              tags$h5(style = "color:#16a085; border-bottom:2px solid #16a085; padding-bottom:4px; margin:2px 0 10px 0; font-weight:600;",
                icon("tint"), " ", i18n$t("Chrastil Solubility Parameters")),
              div(id = ns("gcm_chrastil_wrapper"), uiOutput(ns("gcm_chrastil_ui"))),
              shinyjs::hidden(div(id = ns("chrastil_manual_div"), uiOutput(ns("chrastil_manual_input_ui"))))
            ),
            accordionItem(
              title = i18n$t("Additional Parameters"),
              value = "additional_params",
              collapsed = TRUE,
              uiOutput(ns("additional_params_ui"))
            ),
            accordionItem(
              title = i18n$t("GCM Comparison"),
              value = "gcm_comp",
              collapsed = TRUE,
              div(id = ns("gcm_comp_gorder_wrapper"), uiOutput(ns("gcm_comp_gorder_ui")))
            )
          )
        )
      ),
      column(
        width = 8,
        tabBox(
          id = ns("sol_char_tabs"),
          width = NULL,
          tabPanel(
            title = i18n$t("Solute Characterization Results"),
            value = "solute_results",
            # Placeholder when no results
            conditionalPanel(
              condition = "!output.has_results",
              ns = ns,
              div(
                style = "text-align: center; padding: 50px; color: #888;",
                icon("flask", style = "font-size: 48px; margin-bottom: 15px;"),
                h4(i18n$t("Output data will appear here")),
                p(i18n$t("Select the parameters and run characterization to see results."))
              )
            ),
            # Actual content when results are available
            conditionalPanel(
              condition = "output.has_results",
              ns = ns,
              fluidRow(
                column(
                  12,
                  uiOutput(ns("solute_details"))
                )
              )
            )
          ),
          tabPanel(
            title = i18n$t("Predicted Parameters"),
            value = "predicted_params",
            # Placeholder when no results
            conditionalPanel(
              condition = "!output.has_results",
              ns = ns,
              div(
                style = "text-align: center; padding: 50px; color: #888;",
                icon("table", style = "font-size: 48px; margin-bottom: 15px;"),
                h4(i18n$t("Output data will appear here")),
                p(i18n$t("Select the parameters and run characterization to see results."))
              )
            ),
            conditionalPanel(
              condition = "output.has_results",
              ns = ns,
              DT::dataTableOutput(ns("predicted_params_table"))
            )
          ),
          tabPanel(
            title = i18n$t("Group Contributions"),
            value = "group_contributions",
            # Placeholder when no results
            conditionalPanel(
              condition = "!output.has_results",
              ns = ns,
              div(
                style = "text-align: center; padding: 50px; color: #888;",
                icon("list-alt", style = "font-size: 48px; margin-bottom: 15px;"),
                h4(i18n$t("Output data will appear here")),
                p(i18n$t("Select the parameters and run characterization to see results."))
              )
            ),
            conditionalPanel(
              condition = "output.has_results",
              ns = ns,
              uiOutput(ns("group_contributions_table"))
            )
          ),
          tabPanel(
            title = i18n$t("Global Validation"),
            value = "validation_results",
            # Predicted-vs-experimental parity for each GCM method, all experimental test sets merged by
            # property. Points are classified (Valid / High error / Missing atoms / Out of domain) and only
            # Valid drives the best-fit; flagged series toggle from the legend and never enter the fit.
            # Validation mode selector: Group Contribution methods vs Machine-Learning predictors (item 6).
            # Validation-mode label inline with the toggle (label + ":"), buttons in green (btn-success,
            # matching the Export buttons) and a slightly larger font. The toggle is wrapped so its
            # default Bootstrap .form-group bottom-margin and empty label are zeroed — that is what lets
            # align-items:center line the text up with the buttons WITHOUT any hard-coded pixel offsets.
            div(style = "margin: 6px 0 12px 0; display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
              tags$strong(style = "font-size:15px;", i18n$t("Validation mode"), ":"),
              div(class = "val-mode-toggle",
                shinyWidgets::radioGroupButtons(ns("val_mode"), label = NULL,
                  # usei18n makes i18n$t() return a <span> tag, so labels must go through choiceNames
                  # (which accepts tags) — NOT setNames, whose names must be plain-length-1 strings.
                  choiceNames = list(i18n$t("Group Contribution"), i18n$t("Machine Learning")),
                  choiceValues = c("gc", "ml"),
                  selected = "gc", status = "success", size = "normal")),
              tags$style(HTML(
                ".val-mode-toggle .form-group{margin-bottom:0 !important;}",
                ".val-mode-toggle .control-label{display:none !important;}",
                ".val-mode-toggle{display:flex;}"))),

            # ===================== Group-Contribution mode =====================
            conditionalPanel(condition = "input.val_mode == 'gc'", ns = ns,
              conditionalPanel(
                condition = "!output.has_validation",
                ns = ns,
                div(
                  style = "text-align: center; padding: 50px; color: #888;",
                  icon("chart-line", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("Validation data unavailable")),
                  p(i18n$t("The bundled GCM validation dataset could not be loaded."))
                )
              ),
              conditionalPanel(
                condition = "output.has_validation",
                ns = ns,
                p(style = "color:#555; margin-top:8px;",
                  i18n$t("Predicted vs. experimental parity for each estimation method, with all experimental test sets merged by property. Select a plot:")),
                # Plot selectors (property / method / references), left-aligned and compact (item 4)
                div(style = "display:flex; gap:12px; flex-wrap:wrap; align-items:flex-end; margin-bottom:4px;",
                  div(style = "min-width:240px;", uiOutput(ns("val_property_ui"))),
                  div(style = "min-width:210px;", uiOutput(ns("val_method_ui"))),
                  div(style = "min-width:240px;", uiOutput(ns("val_sources_ui")))
                ),
                div(style = "margin-bottom: 6px; color:#666; font-size:12px;",
                  i18n$t("Best-fit uses only Valid (in-domain) points; flagged points are shown for context (toggle via the legend) but never enter the fit.")),
                # Export buttons above the plot, styled like the Fractionation Column submodule (item 8b)
                div(style = "display:flex; gap:8px; margin:4px 0 6px 0;",
                  downloadButton(ns("val_export_plot"), i18n$t("Export Plot"), icon = icon("image"), class = "btn btn-success btn-sm", style = "color:white;"),
                  downloadButton(ns("val_export_data"), i18n$t("Export Data"), icon = icon("table"), class = "btn btn-success btn-sm", style = "color:white;")),
                uiOutput(ns("val_fit_eqn")),                       # best-fit equation + R² (replaces badges, item 2)
                plotly::plotlyOutput(ns("val_parity_plot"), height = "500px"),
                p(style = "color:#999; font-size:12px; font-style:italic; margin-top:6px;",
                  i18n$t("Validation strategy: these are fixed, pre-published group-contribution methods evaluated against external experimental benchmarks — not trained models, so there is no train/test split. \"Valid\" points show in-domain accuracy on the benchmark set; generalization follows from each method's published design. (Trained machine-learning predictors are validated separately, on held-out data, under the Machine Learning tab.)")),
                div(style = "margin-top: 6px;", uiOutput(ns("val_references_panel"))),
                div(style = "margin-top: 8px;", uiOutput(ns("val_method_notes"))),
                hr(),
                # Title styled like the other modules' table titles (item 8a)
                h4(style = "color:#3c8dbc; border-bottom:2px solid #3c8dbc; padding-bottom:5px; margin-bottom:12px;",
                   i18n$t("All methods for this property (summary)")),
                DT::dataTableOutput(ns("val_summary_table"))
              )
            ),

            # ===================== Machine-Learning mode (held-out parity scatter) =====================
            conditionalPanel(condition = "input.val_mode == 'ml'", ns = ns,
              conditionalPanel(condition = "!output.has_ml_val", ns = ns,
                div(style = "text-align: center; padding: 50px; color: #888;",
                  icon("robot", style = "font-size: 48px; margin-bottom: 15px;"),
                  h4(i18n$t("ML validation data unavailable")),
                  p(i18n$t("The bundled machine-learning validation dataset could not be loaded.")))
              ),
              conditionalPanel(condition = "output.has_ml_val", ns = ns,
                h4(style = "color:#605ca8; border-bottom:2px solid #605ca8; padding-bottom:5px; margin:8px 0 12px 0;",
                   icon("robot"), " ", i18n$t("Machine-Learning Model Validation")),
                p(style = "color:#555;",
                  i18n$t("Held-out predicted vs. experimental values for each machine-learning predictor (points the model never saw during training). Select a model:")),
                div(style = "display:flex; gap:12px; flex-wrap:wrap; align-items:flex-end; margin-bottom:6px;",
                  div(style = "min-width:320px;", uiOutput(ns("ml_val_prop_ui")))),
                uiOutput(ns("ml_val_fit_eqn")),
                plotly::plotlyOutput(ns("ml_val_scatter"), height = "500px"),
                p(style = "color:#999; font-size:12px; font-style:italic; margin-top:6px;",
                  i18n$t("Solubility: leave-solute-out (every test solute is entirely unseen). Melting point & heat of vaporization: 10% random hold-out. These are honest generalization estimates, not training-set fits.")),
                hr(),
                h4(style = "color:#605ca8; border-bottom:2px solid #605ca8; padding-bottom:5px; margin-bottom:12px;",
                   i18n$t("All ML models (summary)")),
                DT::dataTableOutput(ns("ml_val_table"))
              )
            )
          ),
          # GCM Comparison is the rightmost (last) tab.
          tabPanel(
            title = i18n$t("GCM Comparison"),
            value = "gcm_comparison",
            # Placeholder when no comparison results
            conditionalPanel(
              condition = "!output.has_gcm_comparison",
              ns = ns,
              div(
                style = "text-align: center; padding: 50px; color: #888;",
                icon("balance-scale", style = "font-size: 48px; margin-bottom: 15px;"),
                h4(i18n$t("Output data will appear here")),
                p(i18n$t("Select the parameters and run comparison to see results."))
              )
            ),
            conditionalPanel(
              condition = "output.has_gcm_comparison",
              ns = ns,
              uiOutput(ns("gcm_comparison_title")),
              DT::dataTableOutput(ns("gcm_comparison_table"))
            )
          )
        )
      )
    )
  )
}
