# DOE Design UI Module
doe_design_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    # actionButton(ns("open_settings"), i18n$t("Config Data"), icon = icon("cog"), class = "btn btn-default"),
    column(
      width = 5,
      # Action row: Reset | Generate | Save | Export (Export to the right of Save, a success dropdown with
      # CSV + PDF, enabled once a design exists — same convention as the finished modules).
      fluidRow(
        column(3, actionButton(ns("reset"), i18n$t("Reset"), class = "btn btn-default btn-block")),
        column(3, actionButton(ns("calculate"), i18n$t("Generate"), class = "btn btn-primary btn-block", style = "color: white;")),
        # Native hover tooltip on the WRAPPER DIV (not the button): a disabled <button> suppresses pointer
        # events so its OWN title never shows — the wrapping div still does (and works once enabled too).
        # with_tooltip stores the English source + re-translates the browser `title` live via help_i18n.js.
        column(3, with_tooltip(div(id = ns("save_design_wrapper"),
          shinyjs::disabled(actionButton(ns("save_design"), i18n$t("Save"), class = "btn btn-default btn-block"))),
          "Save this design to the current session so you can reload or compare it later. It is NOT written to a file on disk.")),
        column(3,
          conditionalPanel(condition = "!output.show_export_button", ns = ns,
            tags$button(class = "btn btn-success dropdown-toggle", disabled = NA,
              style = "color:white; opacity:0.65; width:100%;",
              icon("download"), " ", i18n$t("Export"), " ", tags$span(class = "caret"))),
          conditionalPanel(condition = "output.show_export_button", ns = ns,
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
          span(i18n$t("DOE Design")),
          div(uiOutput(ns("doe_design_HELP"))),
          div(style = "position: absolute; right: 40px;", uiOutput(ns("accordion_toggle_btn")))
        ),
        width = NULL,
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,

        # Accordion for parameter groups
        shinydashboardPlus::accordion(
          id = ns("param_accordion"),

          # Design Settings Section
          accordionItem(
            title = div(i18n$t("Design Settings")),
            value = "design_settings",
            collapsed = FALSE,
            
            # Design Type Selection
            fluidRow(
              column(12, uiOutput(ns("design_type_ui")))
            ),

            # Common Parameters
            fluidRow(
              column(6, uiOutput(ns("factors_input"))),
              column(6, uiOutput(ns("center_points_input")))
            ),

            # Design-specific parameters (BEFORE Randomize)
            uiOutput(ns("design_specific_params")),

            # Randomize Option
            fluidRow(
              column(12, uiOutput(ns("randomize_ui")))
            ),
            
            # Design requirements info (for ALL designs)
            uiOutput(ns("design_requirements"))
          ),
          
          # Factor Settings Section
          accordionItem(
            title = div(i18n$t("Factor Settings")),
            value = "factor_settings",
            collapsed = FALSE,

            # Factor Names and Limits (Dynamic)
            uiOutput(ns("factor_inputs"))
          )
        )
      )
    ),

    # Right Panel - Results
    column(
      width = 7,
      tabBox(
        id = ns("results_tabs"),
        width = NULL,
        height = "calc(100vh - 100px)",
        # Tab 1 - Design Matrix
        tabPanel(
          title = i18n$t("Design Matrix"),
          value = "design_matrix",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.show_export_button",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("table", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and generate a design to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.show_export_button",
            ns = ns,
            # (Export moved to the action row at the top; the design results start here.)
            # Description alert (top)
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
                    # Matrix name (below title)
                    uiOutput(ns("design_matrix_name"))
                  ),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  DT::dataTableOutput(ns("design_table"))
                )
              )
            )
          )
        ),

        # Tab 2 - Confounding Pattern
        tabPanel(
          title = i18n$t("Confounding Pattern"),
          value = "confounding_pattern",
          # Placeholder when no results
          conditionalPanel(
            condition = "!output.show_export_button",
            ns = ns,
            div(
              style = "text-align: center; padding: 50px; color: #888;",
              icon("project-diagram", style = "font-size: 48px; margin-bottom: 15px;"),
              h4(i18n$t("Output data will appear here")),
              p(i18n$t("Select the parameters and generate a design to see results."))
            )
          ),
          # Actual content when results are available
          conditionalPanel(
            condition = "output.show_export_button",
            ns = ns,
            # ConditionalPanel - only for frfd
            conditionalPanel(
              condition = "output.has_confounding_pattern",
              ns = ns,
              box(
                title = i18n$t("Confounding Pattern"),
                width = NULL,
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput(ns("confounding_table"))
              )
            ),
            # Show message if not frfd
            conditionalPanel(
              condition = "!output.has_confounding_pattern",
              ns = ns,
              div(
                class = "alert alert-info",
                i18n$t("Confounding pattern is only available for Fractional Factorial Designs.")
              )
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
    div(class = "alert alert-info", style = "padding:8px 12px; font-size:13px; margin-bottom:12px;",
      icon("info-circle"), " ",
      i18n$t("Save this design to the current session so you can reload or compare it later. It is NOT written to a file on disk.")),
    textInput(ns("design_name"), i18n$t("Enter a name for your design"), value = ""),
    uiOutput(ns("design_name_preview")),
    footer = tagList(
      modalButton(i18n$t("Cancel")),
      actionButton(ns("confirm_save_design"), i18n$t("Save"))
    )
  )
}
