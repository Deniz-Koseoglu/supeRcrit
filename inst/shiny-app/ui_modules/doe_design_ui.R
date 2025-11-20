# DOE Design UI Module
doe_design_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    # actionButton(ns("open_settings"), i18n$t("Config Data"), icon = icon("cog"), class = "btn btn-default"),
    column(
      width = 5,
      fluidRow(
        column(4, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(4, actionButton(ns("calculate"), i18n$t("Design"), class = "btn btn-info btn-block")),
        column(4, actionButton(ns("save_design"), i18n$t("Save Design"), class = "btn btn-default btn-block"))
        
 
      ),
      br(),
      box(
        title = div(
          i18n$t("Design Parameters"),
        
       
        ),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        # Design Type Selection
        fluidRow(
          column(12,   actionButton(ns("doe_design_help"), "", icon = icon("question-circle"),
                       class = "btn-info btn-xs", style = "float: right; margin-top: -5px;")),
          column(12, radioButtons(ns("design_type"), i18n$t("Select Design Type:"),
            choices = c(
              "Box-Behnken Design (BBD)" = "bbd",
              "Central Composite Design (CCD)" = "ccd",
              "Full Factorial Design (FFD)" = "ffd",
              "Fractional Factorial Design (FRFD)" = "frfd",
              "Taguchi Method (TM)" = "tm"
            ),
            selected = defaults$design_type
          ))
        ),

        # Common Parameters
        fluidRow(
          column(6, numericInput(ns("factors"), i18n$t("Number of Factors"), value = defaults$factors, min = 2, max = 5)),
          column(6, uiOutput(ns("center_points_input")))
        ),

        # Randomize Option
        fluidRow(
          column(12, checkboxInput(ns("randomize"), i18n$t("Randomize Run Order"), value = defaults$randomize))
        ),

        # Design-specific warnings/info
        uiOutput(ns("design_requirements")),

        # Factor Names and Limits (Dynamic)
        uiOutput(ns("factor_inputs")),

        # Design-specific parameters
        uiOutput(ns("design_specific_params"))
      )
    ),

    # Right Panel - Results
    column(
      width = 7,
      tabBox(
        width = NULL,
        height = "calc(100vh - 100px)",
        # Tab 1 - Design Matrix
        tabPanel(
          title = i18n$t("Design Matrix"),
          value = "design_matrix",
          # Export button (üstte) - only show when design is generated
          conditionalPanel(
            condition = "output.show_export_button",
            ns = ns,
            fluidRow(
              column(12,
                style = "display: flex; justify-content: center;",
                downloadButton(ns("export_design"), i18n$t("Export Design"), class = "btn btn-default",style="margin-left:5px;")
              )
            ),
            br()
          ),
          # Description alert (en üstte)
          box(
            title = i18n$t("Design Description"),
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            htmlOutput(ns("design_description"))
          ),
          fluidRow(
            column(
              12,
              box(
                title = tagList(
                  i18n$t("Experimental Design Matrix"),
                  # Matrix name (başlık altında)
                  uiOutput(ns("design_matrix_name"))
                ),
                width = NULL,
                status = "success",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("design_table"))
            
        
              )
            )
          )
        ),

        # Tab 2 - Confounding Pattern
        tabPanel(
          title = i18n$t("Confounding Pattern"),
          value = "confounding_pattern",
          # ConditionalPanel - sadece frfd için
          conditionalPanel(
            condition = "output.has_confounding_pattern",
            ns = ns,
            box(
              title = i18n$t("Confounding Pattern"),
              width = NULL,
              status = "success",
              solidHeader = TRUE,
              DT::dataTableOutput(ns("confounding_table"))
            )
          ),
          # frfd değilse mesaj göster
          conditionalPanel(
            condition = "!output.has_confounding_pattern",
            ns = ns,
            div(class = "alert alert-info",
              i18n$t("Confounding pattern is only available for Fractional Factorial Designs.")
            )
          )
        )
      )
    )
  )
}

# Save Design Modal UI
doe_design_save_modal_ui <- function(id, i18n) {
  ns <- NS(id)
  modalDialog(
    title = i18n$t("Save Design"),
    textInput(ns("design_name"), i18n$t("Enter a name for your design:"), value = ""),
    uiOutput(ns("design_name_preview")),
    footer = tagList(
      modalButton(i18n$t("Cancel")),
      actionButton(ns("confirm_save_design"), i18n$t("Save"))
    )
  )
}
