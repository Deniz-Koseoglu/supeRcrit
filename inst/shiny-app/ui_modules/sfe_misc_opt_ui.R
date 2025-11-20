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
        box(
          title = i18n$t("Optimization Parameters"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          # Load Saved Calculation accordion
          box(
            title = i18n$t("Lastest Solute Characterizations"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE, # veya TRUE
            width = NULL,
            actionButton(ns("sfe_misc_opt_latest_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
            uiOutput(ns("saved_calculations_list_ui")),
            div(id = ns("load_selected_calculation_wrapper"), actionButton(
              ns("load_selected_calculation"),
              i18n$t("Load Selected Calculation"),
              class = "btn-primary btn-block"
            ))
          ),
       
          
          # GCM Method Selection
          box(
            title = i18n$t("Characterizing Solute (GCM)"),
            collapsible = TRUE,
            status = "info",
             solidHeader = TRUE,
            collapsed = TRUE,
            width=NULL,
             actionButton(ns("sfe_misc_opt_gcm_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
            h4(i18n$t("GCM Method Selection")),
            div(id = ns("gcm_tb_wrapper"), selectInput(ns("gcm_tb"), i18n$t("Boiling Point (Tb) Method:"),
              choices = c("None" = "none", "JR", "JR_corr", "SB", "SB_corr", "NL04"),
              selected = "JR"
            )),
            div(id = ns("gcm_crit_wrapper"), selectInput(ns("gcm_crit"), i18n$t("Critical Parameters Method:"),
              choices = c("None" = "none", "JR", "NL07", "NL07_robust"),
              selected = "NL07"
            )),
            div(id = ns("gcm_hsp_wrapper"), selectInput(ns("gcm_hsp"), i18n$t("Hansen Solubility Parameters (HSP) Method:"),
              choices = c("None" = "none", "SP08", "SP12"),
              selected = "SP08"
            )),
            div(id = ns("gcm_simplicity_wrapper"), selectInput(ns("gcm_simplicity"), i18n$t("Fragmentation Simplicity:"),
              choices = c("auto", "simple", "normal", "complex"),
              selected = "auto"
            )),
            div(id = ns("gcm_gorder_wrapper"), numericInput(ns("gcm_gorder"), i18n$t("Maximum Group Order:"),
              value = 0, min = 0
            ))
          ),
          # Solute Information Box
          box(
            title = i18n$t("Solute Information"),
            status = "success",
            width = NULL,
             actionButton(ns("sfe_misc_opt_solute_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
            br(),
            fluidRow(
              column(6, div(id = ns("cas_input_wrapper"), textInput(ns("cas_input"), i18n$t("CAS Number"), value = defaults$cas_input))),
              column(6, div(id = ns("name_input_wrapper"), textInput(ns("name_input"), i18n$t("Solute Name"), value = defaults$name_input)))
            ),
            div(id = ns("specify_smiles_wrapper"), checkboxInput(ns("specify_smiles"), i18n$t("Specify SMILES"), value = FALSE)),
            shinyjs::hidden(
              div(
                id = ns("smiles_div"),
                div(id = ns("smiles_input_wrapper"), textInput(ns("smiles_input"), i18n$t("SMILES String"), value = ""))
              )
            )
          ),
       
        
          h4(i18n$t("Co-Solvent Selection")),
          div(id = ns("modifier_selection_wrapper"), pickerInput(
            inputId = ns("modifier_selection"),
            label = i18n$t("Select Modifier(s)"),
            choices = c(
              "Acetone", "Benzene", "Cyclohexane", "DiethylEther",
              "Ethanol", "Heptane", "Hexane", "Methanol", "MethylOleate",
              "Toluene", "PXylene", "OXylene", "Water"
            ), # Example choices
            selected = c("Methanol", "Ethanol","Acetone"),
            multiple = TRUE,
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          )),
          # Solvent Blends Section
          box(
            title = i18n$t("Solvent Blends (Optional)"),
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,
            solidHeader = TRUE,
            width = NULL,
             actionButton(ns("sfe_misc_opt_solvent_blend_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
            
            # Blend Builder
            h5(i18n$t("Build a Blend:")),
            div(id = ns("blend_solvent_selector_wrapper"), selectInput(
              ns("blend_solvent_selector"),
              i18n$t("Select Solvent to Add:"),
              choices = c(
                "Acetone", "Benzene", "Cyclohexane", "DiethylEther",
                "Ethanol", "Heptane", "Hexane", "Methanol", "MethylOleate",
                "Toluene", "PXylene", "OXylene", "Water"
              ),
              selected = NULL
            )),
            div(id = ns("add_to_blend_wrapper"), actionButton(ns("add_to_blend"), i18n$t("Add to Blend"), class = "btn-sm btn-info")),

            hr(),

            # Current Blend Being Built
            h5(i18n$t("Current Blend Composition:")),
            uiOutput(ns("current_blend_ui")),  # Dynamic UI for fraction inputs

            fluidRow(
              column(6,
                div(id = ns("clear_blend_wrapper"), actionButton(ns("clear_blend"), i18n$t("Clear"), class = "btn-sm btn-warning btn-block"))
              ),
              column(6,
                div(id = ns("save_blend_wrapper"), actionButton(ns("save_blend"), i18n$t("Save Blend"), class = "btn-sm btn-success btn-block"))
              )
            ),
            
            hr(),
            
            # Saved Blends List
            h5(i18n$t("Saved Blends:")),
            uiOutput(ns("saved_blends_ui"))  # Dynamic list of saved blends
          ),
           actionButton(ns("sfe_misc_opt_pressure_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
          h4(i18n$t("Process Conditions")),
          div(id = ns("vfrac_input_wrapper"), numericInput(ns("vfrac_input"), i18n$t("Co-Solvent Volume Fraction (%)"), value = defaults$vfrac * 100, min = 0.1, max = 99.9)),
          fluidRow(
            column(6, div(id = ns("pressure_min_wrapper"), numericInput(ns("pressure_min"), i18n$t("Min Pressure (bar)"), value = defaults$pres_min, min = 75, max = 1000))),
            column(6, div(id = ns("pressure_max_wrapper"), numericInput(ns("pressure_max"), i18n$t("Max Pressure (bar)"), value = defaults$pres_max, min = 75, max = 1000)))
          ),
          fluidRow(
            column(6, div(id = ns("temperature_min_wrapper"), numericInput(ns("temperature_min"), i18n$t("Min Temperature (Celsius)"), value = defaults$temp_min, min = 31, max = 200))),
            column(6, div(id = ns("temperature_max_wrapper"), numericInput(ns("temperature_max"), i18n$t("Max Temperature (Celsius)"), value = defaults$temp_max, min = 31, max = 200)))
          ),
          div(id = ns("optimize_miscibility_wrapper"), actionButton(ns("optimize_miscibility"), i18n$t("Optimize Miscibility"), class = "btn-success"))
        )
      ),
      column(
        width = 8,
        fluidRow(
          box(
            title = i18n$t("Best Modifier Table"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput(ns("best_modifier_table"))
          )
        ),
        # Export button - shows only when results exist
        conditionalPanel(
          condition = "output.show_export_button",
          ns = ns,
          fluidRow(
            column(12,
              style = "display: flex; justify-content: center;",
              downloadButton(ns("export_results"), 
                             i18n$t("Export Results"), 
                             class = "btn btn-default",
                             style="margin-left:5px;")
            )
          ),
          br()
        ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              title = i18n$t("Summary"),
              fluidRow(
                # Value Boxes
                valueBoxOutput(ns("best_modifier_vb"), width = 3),
                valueBoxOutput(ns("max_me_vb"), width = 3),
                valueBoxOutput(ns("num_conditions_vb"), width = 3),
                valueBoxOutput(ns("num_modifiers_vb"), width = 3)
              ),
              DT::dataTableOutput(ns("modifier_ranking_table"))
            ),
            tabPanel(
              title = i18n$t("Co-Solvent Comparison"),
              DT::dataTableOutput(ns("miscibility_comparison_table"))
            ),
            tabPanel(
              title = i18n$t("Miscibility Enhancement"),
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
            ),
            tabPanel(
              title = i18n$t("Detailed Results"),
              DT::dataTableOutput(ns("miscibility_results_table"))
            )
          )
        )
      )
    )
  )
}
