solute_characterization_ui <- function(id, defaults, i18n) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          title = i18n$t("Solute Information"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
      
          fluidRow(
            column(6, div(id = ns("cas_input_wrapper"), textInput(ns("cas_input"), i18n$t("CAS Number"), value = defaults$cas_input))),
            column(6, div(id = ns("name_input_wrapper"), textInput(ns("name_input"), i18n$t("Solute Name"), value = defaults$name_input)))
          ),
             div(id = ns("specify_smiles_wrapper"), checkboxInput(ns("specify_smiles"), i18n$t("Specify SMILES"), value = FALSE)),
          shinyjs::hidden(
            div(
              id = ns("smiles_div"),
              div(id = ns("smiles_input_wrapper"), textInput(ns("smiles_input"), i18n$t("SMILES String"), value = defaults$smiles_input))
            )
          )
        ),
        shinydashboardPlus::accordion(
          id = ns("accordion"),
          accordionItem(
            title = i18n$t("Characterizing Solute (GCM)"),
            value = "characterize",
            collapsed = TRUE,
            actionButton(ns("sfe_solute_char_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
            h4(i18n$t("GCM Method Selection")),
            div(id = ns("gcm_tb_wrapper"), selectInput(ns("gcm_tb"), i18n$t("Boiling Point (Tb) Method:"),
              choices = c("None" = "none", "JR", "JR_corr", "SB", "SB_corr", "NL04"),
              selected = defaults$gcm_tb %||% "none"
            )),
            div(id = ns("gcm_crit_wrapper"), selectInput(ns("gcm_crit"), i18n$t("Critical Parameters Method:"),
              choices = c("None" = "none", "JR", "NL07", "NL07_robust"),
              selected = defaults$gcm_crit %||% "none"
            )),
            div(id = ns("gcm_hsp_wrapper"), selectInput(ns("gcm_hsp"), i18n$t("Hansen Solubility Parameters (HSP) Method:"),
              choices = c("None" = "none", "SP08", "SP12"),
              selected = defaults$gcm_hsp %||% "none"
            )),
            div(id = ns("gcm_vdw_wrapper"), selectInput(ns("gcm_vdw"), i18n$t("Van der Waals Volume Method:"),
              choices = c("None" = "none", "ZHAO", "BND", "SLON"),
              selected = "none"
            )),
            div(id = ns("gcm_simplicity_wrapper"), selectInput(ns("gcm_simplicity"), i18n$t("Fragmentation Simplicity:"),
              choices = c("auto", "simple", "normal", "complex"),
              selected = "auto"
            )),
            div(id = ns("gcm_gorder_wrapper"), numericInput(ns("gcm_gorder"), i18n$t("Maximum Group Order:"),
              value = 0, min = 0
            )),
            div(id = ns("characterize_solute_wrapper"), actionButton(ns("characterize_solute"), i18n$t("Characterize Solute (GCM)"), class = "btn-primary"))
          ),
          accordionItem(
            title = i18n$t("GCM Comparison"),
            value = "gcm_comp",
            collapsed = TRUE,
              actionButton(ns("sfe_solute_char_comp_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
            div(id = ns("gcm_comp_gorder_wrapper"), numericInput(ns("gcm_comp_gorder"), i18n$t("Maximum Group Order:"),
              value = 0, min = 0
            )),
            div(id = ns("gcm_comp_simplicity_wrapper"), selectInput(ns("gcm_comp_simplicity"), i18n$t("Fragmentation Simplicity:"),
              choices = c("auto", "simple", "normal", "complex"),
              selected = "auto"
            )),
            div(id = ns("compare_gcm_wrapper"), actionButton(ns("compare_gcm"), i18n$t("Compare GCM"), class = "btn-primary"))
          )
        )
      ),
      column(
        width = 8,
        tabBox(
          width = NULL,
          tabPanel(
            title = i18n$t("Solute Characterization Results"),
            fluidRow(
              column(
                12,
                uiOutput(ns("solute_details"))
              )
            ),
            fluidRow(
              column(
                12,
                # plotlyOutput(ns("molecular_plot"),
                #   height = "800px"

                # )
                fluidRow(
                  column(4, actionButton(ns("show_hsp_vis"), i18n$t("Show HSP Visualization"), class = "btn-primary")),
                  column(4, actionButton(ns("show_critical_vis"), i18n$t("Show Critical Visualization"), class = "btn-primary")),
                  column(4, actionButton(ns("show_tb_vis"), i18n$t("Show Tb Visualization"), class = "btn-primary"))
                )
              )
            )
          ),
          tabPanel(
            title = i18n$t("Predicted Parameters (HSP & Critical)"),
            DT::dataTableOutput(ns("predicted_params_table"))
          ),
          tabPanel(
            title = i18n$t("Group Contributions"),
            uiOutput(ns("group_contributions_table"))
          ),
          tabPanel(
            title = i18n$t("GCM Comparison"),
            DT::dataTableOutput(ns("gcm_comparison_table"))
          )
        )
      )
    )
  )
}
