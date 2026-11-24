# Process Simulation - Components & Parameters UI Module
# Builds a GC-EOS / GCA-EOS component: SMARTS group decomposition, GCM-estimated
# (or manual) critical temperature and boiling point, critical-diameter (dc)
# regression, and pure vapour pressure at one or more temperatures. On a
# successful analysis the component (groups + Tc + dc) is added to the session
# automatically; "Save to Disk" persists it. Mirrors Solute Characterization.
eos_components_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # ====== INPUTS (left) ===============================================
      column(
        width = 5,
        # Top buttons row: Reset | Analyze | Save to Disk (green) | Export (CSV/PDF dropdown)
        fluidRow(
          style = "display: flex; align-items: stretch;",
          column(3, style = "display: flex; align-items: center;",
            actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")
          ),
          column(3, style = "display: flex; align-items: center;",
            actionButton(ns("analyze"), i18n$t("Analyze"),
              icon = icon("atom"), class = "btn-primary btn-block", style = "color: white;")
          ),
          column(3, style = "display: flex; align-items: center;",
            # Native hover tooltip on the WRAPPER DIV — a disabled <button> suppresses its own title tooltip.
            with_tooltip(div(id = ns("save_wrapper"), style = "width: 100%;",
              shinyjs::disabled(actionButton(ns("save_to_disk"), i18n$t("Save to Disk"), class = "btn-success btn-block", style = "color: white;"))),
              "Save this component (groups, Tc and dc) to a JSON file on disk so it persists across sessions and can be reloaded into the flash, column and phase-behaviour tools.")
          ),
          rp_export_dropdown_col(ns, i18n, "has_results", width = 3,
            extra_style = "display: flex; align-items: center;")
        ),
        br(),
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Component Builder")),
            div(uiOutput(ns("eos_components_HELP"))),
            div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
          ),
          status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE,

          shinydashboardPlus::accordion(
            id = ns("comp_accordion"),

            # === Component Identification (model + load + identification / picker) ===
            accordionItem(
              title = i18n$t("Component Identification"), value = "identification", collapsed = FALSE,

              # --- Model + Load buttons (buttons inline with the EOS Model field, bottom-aligned to it) --------
              # align-items:flex-end + the button group's margin-bottom:15px matches the selectInput's .form-group
              # bottom margin, so the small buttons sit on the model field's baseline (the flex-end gotcha fix).
              div(style = "display: flex; align-items: flex-end; gap: 8px; flex-wrap: wrap;",
                div(style = "flex: 1; min-width: 220px; max-width: 480px;", div(id = ns("model_wrapper"), uiOutput(ns("model_ui")))),
                div(style = "display: flex; gap: 8px; margin-bottom: 15px;",
                  actionButton(ns("open_example_modal"), i18n$t("Load Example"),
                    icon = icon("flask"), class = "btn btn-info btn-sm"),
                  actionButton(ns("open_db_modal"), i18n$t("Load from Database"),
                    icon = icon("database"), class = "btn btn-success btn-sm", style = "color:white;"))
              ),
              tags$hr(style = "margin: 8px 0;"),

              # --- Decompose mode: CAS | Display Name | SMILES --------------
              conditionalPanel(
                condition = "input.model != 'diaz2005' && input.model != 'ganan2011'", ns = ns,
                uiOutput(ns("identification_ui")),
                # auto-notify when the entered compound has an experimental DB record
                uiOutput(ns("db_notify_ui"))
              ),
              # --- Picker mode (diaz2005 / ganan2011 terpene sets) ---------
              conditionalPanel(
                condition = "input.model == 'diaz2005' || input.model == 'ganan2011'", ns = ns,
                h5(i18n$t("Supported molecule")),
                uiOutput(ns("picker_ui"))
              )
            ),

            # === Critical Properties (Tc, Tb, dc) ===
            accordionItem(
              title = i18n$t("Critical Properties"), value = "critical_props", collapsed = FALSE,

              conditionalPanel(
                condition = "input.model != 'diaz2005' && input.model != 'ganan2011'", ns = ns,

                # --- Critical temperature & boiling point ------------------
                div(style = "display: flex; gap: 10px; align-items: flex-start;",
                  div(id = ns("tc_method_wrapper"), style = "flex: 1;", uiOutput(ns("tc_method_ui"))),
                  div(id = ns("tc_side_wrapper"), style = "width: 185px;", uiOutput(ns("tc_side_ui")))
                ),
                div(style = "display: flex; gap: 10px; align-items: flex-start;",
                  div(id = ns("tb_method_wrapper"), style = "flex: 1;", uiOutput(ns("tb_method_ui"))),
                  div(id = ns("tb_side_wrapper"), style = "width: 185px;", uiOutput(ns("tb_side_ui")))
                ),

                # --- Critical diameter (dc) source (title merged into the Calculation Method label) ---
                div(id = ns("dc_method_wrapper"), uiOutput(ns("dc_method_ui"))),
                div(id = ns("dc_vle_wrapper"), uiOutput(ns("dc_vle_ui")))
              ),
              # --- Picker mode: properties come from the published set -----
              conditionalPanel(
                condition = "input.model == 'diaz2005' || input.model == 'ganan2011'", ns = ns,
                p(style = "color:#888; font-style:italic; margin:6px 0;",
                  i18n$t("For the selected parameter set, the critical temperature, boiling point and critical diameter are taken from published literature values."))
              )
            ),

            # === Vapour-pressure evaluation temperatures (tag input) ===
            accordionItem(
              title = tags$span(HTML(paste0(i18n$t("Vapor Pressure"), " (<i>P</i><sub>sat</sub>)"))),
              value = "vapor_pressure", collapsed = TRUE,
              div(id = ns("teval_wrapper"), uiOutput(ns("teval_ui")))
            )
          )
        )
      ),

      # ====== RESULTS (right, wide) =======================================
      column(
        width = 7,
        box(
          title = i18n$t("Results"),
          status = "success", solidHeader = TRUE, width = NULL,
          conditionalPanel(
            condition = "!output.has_results", ns = ns,
            div(style = "text-align: center; padding: 50px; color: #888;",
              icon("atom", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Component analysis will appear here")),
              p(i18n$t("Enter a CAS number or SMILES (or pick an example) and run the analysis.")))
          ),
          conditionalPanel(
            condition = "output.has_results", ns = ns,
            uiOutput(ns("results"))
          )
        )
      )
    )
  )
}
