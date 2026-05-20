# Kinetic Auxiliary Tools UI Module
# Unified density calculator for CO2 (Bender EoS), Ethanol (Poling et al. +
# Linear Blend Rule), and Water (IAPWS-95). Inputs box on the left in a
# 2-row grid layout (each cell 4 of 12 cols so contents never wrap);
# Results box on the right.
kinetic_aux_tool_ui <- function(id, defaults, i18n) {
  ns <- NS(id)

  tagList(
    # Module-local CSS: keep labels and badges on one line.
    tags$head(tags$style(HTML(sprintf(
      "
      #%1$s_inputs .control-label { white-space: nowrap; }
      ",
      id
    )))),

    fluidRow(
      # ====== INPUTS BOX (left, wide) =====================================
      column(
        width = 8,
        box(
          title = div(
            style = "display: flex; align-items: center;",
            span(i18n$t("Density Calculator")),
            div(uiOutput(ns("kinetic_aux_tool_density_HELP")))
          ),
          status = "success",
          solidHeader = TRUE,
          width = NULL,

          div(
            id = paste0(id, "_inputs"),

            # --- Row 1: Substance | Output Units | Calculate ----------
            fluidRow(
              column(
                4,
                div(id = ns("substance_wrapper"), uiOutput(ns("substance_ui")))
              ),
              column(
                4,
                div(id = ns("units_wrapper"), uiOutput(ns("units_ui")))
              ),
              column(
                4,
                div(
                  id = ns("calc_wrapper"),
                  class = "form-group",
                  style = "margin-top: 25px;",
                  div(
                    style = "display: flex; gap: 6px;",
                    div(style = "flex: 1;",
                      actionButton(
                        ns("calc"),
                        i18n$t("Calculate Density"),
                        icon = icon("calculator"),
                        class = "btn btn-primary btn-block",
                        style = "color: white !important;"
                      )
                    ),
                    div(style = "flex: 0 0 auto;",
                      actionButton(
                        ns("reset"),
                        i18n$t("Reset"),
                        icon = icon("undo"),
                        class = "btn btn-default"
                      )
                    )
                  )
                )
              )
            ),

            # --- Row 2: Pressure | Temperature | CO2 Volume Fraction ----
            fluidRow(
              column(
                4,
                div(id = ns("pres_wrapper"), uiOutput(ns("pres_ui")))
              ),
              column(
                4,
                div(id = ns("temp_wrapper"), uiOutput(ns("temp_ui")))
              ),
              column(
                4,
                div(id = ns("co2_frac_wrapper"), uiOutput(ns("co2_frac_ui")))
              )
            )
          )
        )
      ),

      # ====== RESULTS BOX (right, narrow) =================================
      column(
        width = 4,
        box(
          title = i18n$t("Results"),
          status = "success",
          solidHeader = TRUE,
          width = NULL,
          uiOutput(ns("results"))
        )
      )
    )
  )
}
