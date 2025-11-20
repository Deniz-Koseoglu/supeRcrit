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
        box(
          title = i18n$t("Comparison Parameters"),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,

          # Solute Selection Section
          box(
            title = i18n$t("Solute Selectionx"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = NULL,

            # Load from Saved Calculations
            box(
              title = i18n$t("Select Solutes from Saved Characterizations"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              actionButton(ns("sfe_misc_comp_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              ),
            
              uiOutput(ns("saved_calculations_list_ui")),
              div(id = ns("load_selected_calculations_wrapper"), actionButton(
                ns("load_selected_calculations"),
                i18n$t("Load Selected Solutes"),
                class = "btn-primary btn-block"
              ))
            ),

            # Manual Solute Entry
            box(
              title = i18n$t("Manual Solute Entry"),
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              actionButton(ns("sfe_misc_comp_manual_help"), "",
                icon = icon("question-circle"),
                class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
              ),
         
              fluidRow(
                column(6, div(id = ns("manual_cas_wrapper"), textInput(ns("manual_cas"), i18n$t("CAS")))),
                column(6, div(id = ns("manual_name_wrapper"), textInput(ns("manual_name"), i18n$t("Name"))))
              ),
              fluidRow(
                column(12, div(id = ns("manual_smiles_wrapper"), textInput(ns("manual_smiles"), i18n$t("SMILES"))))
              ),
              fluidRow(
                column(12,
                  align = "center",
                  div(id = ns("add_manual_solute_wrapper"), actionButton(ns("add_manual_solute"), i18n$t("Add"), class = "btn-sm btn-success"))
                )
              ),
              hr(),
              h5(i18n$t("Added Solutes:")),
              uiOutput(ns("added_solutes_ui"))
            )
          ),

          # GCM Method Selection (Copied from sfe_mod)
          box(
            title = i18n$t("Characterizing Solute (GCM)"),
            collapsible = TRUE,
            status = "info",
            solidHeader = TRUE,
            collapsed = TRUE,
            width = NULL,
            actionButton(ns("sfe_misc_comp_gcm_help"), "",
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
          h4(i18n$t("Co-Solvent Selection")),
          div(id = ns("modifier_selection_wrapper"), pickerInput(
            inputId = ns("modifier_selection"),
            label = i18n$t("Select Modifier(s)"),
            choices = c(
              "Acetone", "Benzene", "Cyclohexane", "DiethylEther",
              "Ethanol", "Heptane", "Hexane", "Methanol", "MethylOleate",
              "Toluene", "PXylene", "OXylene", "Water"
            ), # Example choices
            selected = "Ethanol", # Default to Ethanol for miscomp
            multiple = FALSE, # miscomp typically uses one modifier
            options = list(`actions-box` = TRUE, `live-search` = TRUE)
          )),
          # Solvent Blends Section (Copied from sfe_mod)
          box(
            title = i18n$t("Solvent Blends (Optional)"),
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,
            solidHeader = TRUE,
            width = NULL,
            actionButton(ns("sfe_misc_comp_blend_help"), "",
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
            uiOutput(ns("current_blend_ui")), # Dynamic UI for fraction inputs

            fluidRow(
              column(
                6,
                div(id = ns("clear_blend_wrapper"), actionButton(ns("clear_blend"), i18n$t("Clear"), class = "btn-sm btn-warning btn-block"))
              ),
              column(
                6,
                div(id = ns("save_blend_wrapper"), actionButton(ns("save_blend"), i18n$t("Save Blend"), class = "btn-sm btn-success btn-block"))
              )
            ),
            hr(),

            # Saved Blends List
            h5(i18n$t("Saved Blends:")),
            uiOutput(ns("saved_blends_ui")) # Dynamic list of saved blends
          ),
          h4(i18n$t("Process Conditions")),
          actionButton(ns("sfe_misc_comp_process_help"), "",
            icon = icon("question-circle"),
            class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
          ),
          div(id = ns("pres_input_wrapper"), textInput(ns("pres_input"), i18n$t("Pressures (bar, comma-separated)"),
            value = "100,200,300,400,500,600"
          )),
          div(id = ns("pres_comp_wrapper"), selectInput(ns("pres_comp"), i18n$t("Comparative Pressure"), choices = NULL)),
          div(id = ns("temp_wrapper"), numericInput(ns("temp"), i18n$t("Temperature (°C)"), value = 40, min = 40, max = 70)),
          div(id = ns("vfrac_input_wrapper"), textInput(ns("vfrac_input"), i18n$t("Volume Fractions (comma-separated)"),
            value = "0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40"
          )),

          # Advanced Plot Options
          box(
            title = i18n$t("Advanced Plot Options"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = NULL,
            actionButton(ns("sfe_misc_comp_plot_help"), "",
              icon = icon("question-circle"),
              class = "btn-info btn-xs", style = "float: right; margin-top: -5px;"
            ),
            div(id = ns("plt_title_wrapper"), checkboxInput(ns("plt_title"), i18n$t("Show Plot Titles"), value = TRUE)),
            div(id = ns("use_custom_colors_wrapper"), checkboxInput(ns("use_custom_colors"), i18n$t("Use Custom Colors"))),
            conditionalPanel(
              condition = paste0("input['", ns("use_custom_colors"), "']"),
              fluidRow(
                column(4, colourInput(ns("col_one"), i18n$t("Color 1"), value = "#E41A1C")),
                column(4, colourInput(ns("col_two"), i18n$t("Color 2"), value = "#377EB8")),
                column(4, colourInput(ns("col_three"), i18n$t("Color 3"), value = "#4DAF4A"))
              ),
              fluidRow(
                column(4, colourInput(ns("col_four"), i18n$t("Color 4"), value = "#984EA3")),
                column(4, colourInput(ns("col_five"), i18n$t("Color 5"), value = "#FF7F00")),
                column(4, colourInput(ns("col_six"), i18n$t("Color 6"), value = "#FFFF33"))
              )
            )
          ),
          div(id = ns("run_miscomp_wrapper"), actionButton(ns("run_miscomp"), i18n$t("Run Miscibility Comparison"), class = "btn-success"))
        )
      ),
      column(
        width = 8,
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              title = i18n$t("Plots"),
              uiOutput(ns("plots_tabs_ui")) # Dynamic tabs for plots
            ),
            tabPanel(
              title = i18n$t("Miscibility Data"),
              DT::dataTableOutput(ns("miscibility_data_table"))
            )
          )
        )
      )
    )
  )
}
