solute_characterization_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  tagList(
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
            actionButton(ns("save_to_disk"), i18n$t("Save to Disk"), class = "btn-success btn-block", style = "color: white;")
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
              fluidRow(
                column(6, div(id = ns("cas_input_wrapper"), uiOutput(ns("cas_input_ui")))),
                column(6, div(id = ns("name_input_wrapper"), uiOutput(ns("name_input_ui"))))
              ),
              div(id = ns("specify_smiles_wrapper"), uiOutput(ns("specify_smiles_ui"))),
              shinyjs::hidden(
                div(
                  id = ns("smiles_div"),
                  div(id = ns("smiles_input_wrapper"), uiOutput(ns("smiles_input_ui")))
                )
              )
            ),
            accordionItem(
              title = i18n$t("Solute Characterization (GCM)"),
              value = "characterize",
              collapsed = TRUE,
              h4(i18n$t("GCM Method Selection")),
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
              div(style = "display: flex; gap: 10px; align-items: flex-start;",
                div(id = ns("gcm_crit_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_crit_ui"))),
                div(id = ns("gcm_crit_gorder_wrapper"), style = "width: 160px;", uiOutput(ns("gcm_crit_gorder_ui")))
              ),
              div(style = "display: flex; gap: 10px; align-items: flex-start;",
                div(id = ns("gcm_hsp_wrapper"), style = "flex: 1;", uiOutput(ns("gcm_hsp_ui"))),
                div(id = ns("gcm_hsp_gorder_wrapper"), style = "width: 160px;", uiOutput(ns("gcm_hsp_gorder_ui")))
              ),
              div(id = ns("gcm_vdw_wrapper"), uiOutput(ns("gcm_vdw_ui"))),
              div(id = ns("gcm_simplicity_wrapper"), uiOutput(ns("gcm_simplicity_ui")))
            ),
            accordionItem(
              title = i18n$t("GCM Comparison"),
              value = "gcm_comp",
              collapsed = TRUE,
              div(id = ns("gcm_comp_gorder_wrapper"), uiOutput(ns("gcm_comp_gorder_ui"))),
              div(id = ns("gcm_comp_simplicity_wrapper"), uiOutput(ns("gcm_comp_simplicity_ui")))
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
              ),
              fluidRow(
                column(
                  12,
                  fluidRow(
                    column(4, actionButton(ns("show_hsp_vis"), i18n$t("Show HSP Visualization"), class = "btn-primary", style = "color: white;display:inline-block;float:left;")),
                    column(4, actionButton(ns("show_critical_vis"), i18n$t("Show Critical Visualization"), class = "btn-primary", style = "color: white;display:inline-block;float:left;")),
                    column(4, actionButton(ns("show_tb_vis"), HTML(paste0(i18n$t("Show"), " <i>T</i><sub><i>b</i></sub> ", i18n$t("Visualization"))), class = "btn-primary", style = "color: white;display:inline-block;float:left;"))
                  )
                )
              )
            )
          ),
          tabPanel(
            title = i18n$t("Predicted Parameters (HSP and Critical)"),
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
