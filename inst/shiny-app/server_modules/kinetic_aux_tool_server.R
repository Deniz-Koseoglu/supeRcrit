# Kinetic Auxiliary Tools Server Module
# Unified density calculator: substance picker drives the inputs, ranges and
# units; one Calculate button dispatches to bendens / etoh_dens / h2o_dens.
kinetic_aux_tool_server <- function(input, output, session, defaults, i18n, tablang) {



  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # =========================================================================
  # Substance metadata (single source of truth for ranges).
  # Ranges from documented valid ranges of bendens (CO2 Bender EoS),
  # h2o_dens (IAPWS-95 subcritical), and etoh_dens (Poling et al. + Linear
  # Blend Rule). Ethanol temp upper bound 240 degC is below the critical
  # temperature (~241 degC) where the Poling correlation becomes ill-defined.
  # =========================================================================
  substance_meta <- list(
    co2 = list(
      pres = list(min = 1, max = 1000, default = 300),
      temp = list(min = -50, max = 300, default = 45)
    ),
    etoh = list(
      pres = list(min = 1, max = 1000, default = 300),  # only used when CO2 fraction > 0
      temp = list(min = 0, max = 240, default = 55)
    ),
    h2o = list(
      pres = list(min = 1, max = 220, default = 40),
      temp = list(min = 0, max = 374, default = 200)
    )
  )

  # Track whether pressure is in active use by the current calculator.
  pres_used <- reactive({
    sub <- input$substance %||% "co2"
    co2f <- input$co2_frac %||% 0
    if (sub != "etoh") return(TRUE)
    isTRUE(co2f > 0)
  })

  # CO2 fraction only used by the Ethanol calculator.
  co2_frac_used <- reactive({
    (input$substance %||% "co2") == "etoh"
  })

  # =========================================================================
  # Dynamic UI
  # =========================================================================

  output$substance_ui <- renderUI({
    selectInput(
      ns("substance"),
      tags$span(i18n_r()$t("Substance"),
        input_help(i18n_r()$t("Pick the substance whose density you want to calculate: CO\u2082 (Bender EoS), Ethanol (Poling et al. with Linear Blend Rule for CO\u2082 mixtures), or Water (IAPWS-95)."),
                   title = i18n_r()$t("Substance"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("co2", "etoh", "h2o"),
        c(i18n_r()$t("CO\u2082"),
          i18n_r()$t("Ethanol"),
          i18n_r()$t("Water"))
      ),
      selected = isolate(input$substance) %||% "co2"
    )
  })
  outputOptions(output, "substance_ui", suspendWhenHidden = FALSE)

  # Output units: compact selectInput so it lines up with the radio.
  output$units_ui <- renderUI({
    selectInput(
      ns("units"),
      tags$span(i18n_r()$t("Output Units"),
        input_help(i18n_r()$t("Output units for all three calculators. Choose g/L or g/mL."),
                   title = i18n_r()$t("Output Units"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("g/L", "g/mL"),
                         c(i18n_r()$t("g/L"), i18n_r()$t("g/mL"))),
      selected = isolate(input$units) %||% "g/L"
    )
  })
  outputOptions(output, "units_ui", suspendWhenHidden = FALSE)

  # Pressure: numericInput with range badge in label. Rendered with the
  # disabled attribute baked in for the initial state on substance change;
  # the shinyjs observer below handles flips during slider/numeric changes.
  output$pres_ui <- renderUI({
    req(input$substance)
    rng <- substance_meta[[input$substance]]$pres
    cur <- isolate(input$pres)
    if (is.null(cur) || is.na(cur)) cur <- rng$default
    is_used <- isolate(pres_used())

    pres_input <- numericInput(
      ns("pres"), label = NULL, value = cur,
      min = rng$min, max = rng$max, step = 1
    )
    if (!is_used) pres_input <- shinyjs::disabled(pres_input)

    tags$div(
      tags$label(
        i18n_r()$t("Pressure (bar)"),
        input_help(i18n_r()$t("Set the pressure in bar. The valid range adapts to the selected substance. Pressure is not used for pure ethanol, but is needed when a CO\u2082 fraction is set so the CO\u2082 density can be computed for the Linear Blend Rule."),
                   title = i18n_r()$t("Pressure"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          paste0(rng$min, "\u2013", rng$max),
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0(i18n_r()$t("Valid range:"), " ", rng$min, "\u2013", rng$max, " ", i18n_r()$t("bar"))
        )
      ),
      pres_input
    )
  })
  outputOptions(output, "pres_ui", suspendWhenHidden = FALSE)

  # Temperature: numericInput with range badge.
  output$temp_ui <- renderUI({
    req(input$substance)
    rng <- substance_meta[[input$substance]]$temp
    cur <- isolate(input$temp)
    if (is.null(cur) || is.na(cur)) cur <- rng$default

    tags$div(
      tags$label(
        i18n_r()$t("Temperature (\u00b0C)"),
        input_help(i18n_r()$t("Set the temperature in degrees Celsius. The valid range adapts to the selected substance."),
                   title = i18n_r()$t("Temperature"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          paste0(rng$min, "\u2013", rng$max),
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0(i18n_r()$t("Valid range:"), " ", rng$min, "\u2013", rng$max, " \u00b0C")
        )
      ),
      numericInput(ns("temp"), label = NULL, value = cur,
                   min = rng$min, max = rng$max, step = 1)
    )
  })
  outputOptions(output, "temp_ui", suspendWhenHidden = FALSE)

  # CO2 volume fraction: numericInput (was slider) with range badge and
  # auto-correct observer (see below). 0-0.99 is the validated range.
  output$co2_frac_ui <- renderUI({
    cur <- isolate(input$co2_frac)
    if (is.null(cur) || is.na(cur)) cur <- 0
    is_used <- isolate(co2_frac_used())

    co2_frac_input <- numericInput(
      ns("co2_frac"), label = NULL, value = cur,
      min = 0, max = 0.99, step = 0.01
    )
    if (!is_used) co2_frac_input <- shinyjs::disabled(co2_frac_input)

    tags$div(
      tags$label(
        i18n_r()$t("CO\u2082 Volume Fraction"),
        input_help(i18n_r()$t("When Ethanol is selected, this field sets the CO\u2082 volume fraction (0\u20130.99). Leave at 0 for pure ethanol; any value above 0 triggers the Linear Blend Rule mixture calculation."),
                   title = i18n_r()$t("CO\u2082 Volume Fraction"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "0\u20130.99",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0(i18n_r()$t("Valid range:"), " 0\u20130.99")
        )
      ),
      co2_frac_input
    )
  })
  outputOptions(output, "co2_frac_ui", suspendWhenHidden = FALSE)

  # =========================================================================
  # Enable/disable Pressure based on whether the active calculator uses it.
  # =========================================================================
  observe({
    if (pres_used()) {
      shinyjs::enable("pres")
    } else {
      shinyjs::disable("pres")
    }
  })

  # CO2 Volume Fraction is enabled only when Ethanol is selected.
  observe({
    if (co2_frac_used()) {
      shinyjs::enable("co2_frac")
    } else {
      shinyjs::disable("co2_frac")
    }
  })

  # =========================================================================
  # When the substance changes, snap Pressure and Temperature to the new
  # substance's defaults (each calculator has very different valid ranges)
  # and clear any stale results from the previous substance.
  # =========================================================================
  observeEvent(input$substance, {
    rng_p <- substance_meta[[input$substance]]$pres
    rng_t <- substance_meta[[input$substance]]$temp
    updateNumericInput(session, "pres", value = rng_p$default,
                       min = rng_p$min, max = rng_p$max)
    updateNumericInput(session, "temp", value = rng_t$default,
                       min = rng_t$min, max = rng_t$max)
    output$results <- renderUI({
      div(
        style = "text-align: center; padding: 50px; color: #888;",
        icon("chart-area", style = "font-size: 48px; margin-bottom: 15px;"),
        h4(i18n$t("Output data will appear here")),
        p(i18n$t("Select the parameters and run the calculation to see results."))
      )
    })
  }, ignoreInit = TRUE)

  # =========================================================================
  # Auto-correct observers: clamp out-of-range numeric inputs and notify.
  # =========================================================================
  observe({
    req(input$substance, input$pres)
    rng <- substance_meta[[input$substance]]$pres
    val <- input$pres
    if (!is.null(val) && !is.na(val) && (val < rng$min || val > rng$max)) {
      clamped <- max(rng$min, min(rng$max, val))
      showNotification(
        sprintf("%s %s\u2013%s %s.",
                i18n$t("Pressure was adjusted to the valid range"),
                rng$min, rng$max, i18n$t("bar")),
        type = "warning"
      )
      updateNumericInput(session, "pres", value = clamped)
    }
  })

  observe({
    req(input$substance, input$temp)
    rng <- substance_meta[[input$substance]]$temp
    val <- input$temp
    if (!is.null(val) && !is.na(val) && (val < rng$min || val > rng$max)) {
      clamped <- max(rng$min, min(rng$max, val))
      showNotification(
        sprintf("%s %s\u2013%s \u00b0C.",
                i18n$t("Temperature was adjusted to the valid range"),
                rng$min, rng$max),
        type = "warning"
      )
      updateNumericInput(session, "temp", value = clamped)
    }
  })

  # CO2 fraction auto-correct to [0, 0.99].
  observeEvent(input$co2_frac, {
    val <- input$co2_frac
    if (!is.null(val) && !is.na(val) && (val < 0 || val > 0.99)) {
      clamped <- max(0, min(0.99, val))
      showNotification(
        i18n$t("CO\u2082 volume fraction was adjusted to the valid range (0\u20130.99)."),
        type = "warning"
      )
      updateNumericInput(session, "co2_frac", value = clamped)
    }
  }, ignoreInit = TRUE)

  # =========================================================================
  # Helpers
  # =========================================================================

  # Convert density from g/L to the user-selected output unit.
  to_user_units <- function(rho_g_per_L) {
    if (is.null(rho_g_per_L) || is.na(rho_g_per_L)) return(NA_real_)
    units_sel <- input$units %||% "g/L"
    if (units_sel == "g/mL") rho_g_per_L / 1000 else rho_g_per_L
  }

  # Format a number at up to 3 dp with trailing zeroes stripped.
  fmt_num <- function(x, digits = 3) {
    if (is.null(x) || is.na(x)) return("\u2014")
    s <- formatC(round(x, digits), format = "f", digits = digits)
    sub("\\.?0+$", "", s)
  }

  # =========================================================================
  # Calculate Density
  # =========================================================================
  observeEvent(input$calc, {
    req(input$substance, input$temp, input$units)
    sub <- input$substance
    units_sel <- input$units

    withProgress(message = i18n$t("Calculating density..."), {

      if (sub == "co2") {
        req(input$pres)
        results <- tryCatch(
          bendens(pres = input$pres, temp = input$temp, units = units_sel),
          error = function(e) {
            showNotification(
              paste(i18n$t("Error calculating CO\u2082 density:"), e$message),
              type = "error", duration = 5
            )
            NULL
          }
        )
        if (!is.null(results)) {
          rho_val <- unname(results["rho"])
          ent_val <- unname(results["ent"])
          output$results <- renderUI({
            tagList(
              h5(tags$strong(i18n_r()$t("CO\u2082 Density Results"))),
              tags$p(
                tags$strong(
                  i18n_r()$t("Density"), " (",
                  HTML("&rho;"),
                  "):"
                ),
                " ", fmt_num(rho_val), " ", i18n_r()$t(units_sel)
              ),
              tags$p(
                tags$strong(
                  i18n_r()$t("Specific Enthalpy"), " (",
                  HTML("h<sub>spec</sub>"),
                  "):"
                ),
                " ", fmt_num(ent_val), " ", i18n_r()$t("kJ/kg")
              )
            )
          })
        }

      } else if (sub == "etoh") {
        co2f <- input$co2_frac %||% 0
        # etoh_dens internally needs CO2 density in g/L for the Linear Blend
        # Rule. If the user wants a mixture (co2_frac > 0), compute that CO2
        # density on the fly from the current P/T via the Bender EoS.
        co2_rho_g_per_L <- 0
        if (isTRUE(co2f > 0)) {
          req(input$pres)
          co2_rho_g_per_L <- tryCatch(
            unname(bendens(pres = input$pres, temp = input$temp, units = "g/L")["rho"]),
            error = function(e) {
              showNotification(
                paste(i18n$t("Could not compute CO\u2082 density for the mixture:"), e$message),
                type = "error", duration = 5
              )
              NA_real_
            }
          )
          if (is.na(co2_rho_g_per_L)) return()
        }

        results <- tryCatch(
          etoh_dens(temp = input$temp, co2_frac = co2f, co2_rho = co2_rho_g_per_L),
          error = function(e) {
            showNotification(
              paste(i18n$t("Error calculating ethanol density:"), e$message),
              type = "error", duration = 5
            )
            NULL
          }
        )
        if (!is.null(results)) {
          etoh_disp <- to_user_units(unname(results["etoh"]))
          mix_disp  <- to_user_units(unname(results["co2_etoh"]))
          show_mixture <- isTRUE(co2f > 0) && !is.na(mix_disp)
          output$results <- renderUI({
            tagList(
              h5(tags$strong(i18n_r()$t("Ethanol Density Results"))),
              tags$p(
                tags$strong(
                  i18n_r()$t("Pure Ethanol Density"), " (",
                  HTML("&rho;"),
                  "):"
                ),
                " ", fmt_num(etoh_disp), " ", i18n_r()$t(units_sel)
              ),
              if (show_mixture) {
                tagList(
                  tags$p(
                    tags$strong(
                      i18n_r()$t("CO\u2082\u2013Ethanol Mixture Density"), " (",
                      HTML("&rho;"),
                      "):"
                    ),
                    " ", fmt_num(mix_disp), " ", i18n_r()$t(units_sel)
                  ),
                  tags$p(
                    style = "font-size: 11px; color: #6c757d;",
                    sprintf(
                      "%s %s %s, %s %s.",
                      i18n_r()$t("Mixture density computed via the Linear Blend Rule using a CO\u2082 density of"),
                      fmt_num(to_user_units(co2_rho_g_per_L)),
                      i18n_r()$t(units_sel),
                      i18n_r()$t("at"),
                      paste0(input$pres, " ", i18n_r()$t("bar"), " / ", input$temp, " \u00b0C")
                    )
                  )
                )
              }
            )
          })
        }

      } else if (sub == "h2o") {
        req(input$pres)
        # h2o_dens validates the subcritical region itself.
        results <- tryCatch(
          h2o_dens(temp = input$temp, pres = input$pres),
          error = function(e) {
            showNotification(
              paste(i18n$t("Error calculating water density:"), e$message),
              type = "error", duration = 5
            )
            NULL
          }
        )
        if (!is.null(results)) {
          rho_disp <- to_user_units(unname(results))
          output$results <- renderUI({
            tagList(
              h5(tags$strong(i18n_r()$t("Water Density Results"))),
              tags$p(
                tags$strong(
                  i18n_r()$t("Water Density"), " (",
                  HTML("&rho;"),
                  "):"
                ),
                " ", fmt_num(rho_disp), " ", i18n_r()$t(units_sel)
              )
            )
          })
        }
      }
    })
  })

  # Help button: substance-specific help bundle.
  output$kinetic_aux_tool_density_HELP <- renderUI({
    sub <- input$substance %||% "co2"
    content_key <- switch(sub,
      "co2"  = "kinetic_aux_tool_co2_density_help_en",
      "etoh" = "kinetic_aux_tool_etoh_density_help_en",
      "h2o"  = "kinetic_aux_tool_h2o_density_help_en",
      "kinetic_aux_tool_co2_density_help_en"
    )
    helper(
      shiny_tag = "",
      buttonLabel = "OK",
      content = i18n_r()$t(content_key),
      type = "markdown",
      size = "l",
      style = "color:white; font-size:20px; vertical-align:middle ; margin-top: 0; margin-right: 3%;"
    )
  })

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "kinetic_aux_tool_density_HELP", suspendWhenHidden = FALSE)

  # Reset button handler
  observeEvent(input$reset, {
    sub <- input$substance %||% "co2"
    rng_p <- substance_meta[[sub]]$pres
    rng_t <- substance_meta[[sub]]$temp
    updateNumericInput(session, "pres", value = rng_p$default)
    updateNumericInput(session, "temp", value = rng_t$default)
    updateNumericInput(session, "co2_frac", value = 0)
    updateSelectInput(session, "units", selected = "g/L")
    output$results <- renderUI({
      div(
        style = "text-align: center; padding: 50px; color: #888;",
        icon("chart-area", style = "font-size: 48px; margin-bottom: 15px;"),
        h4(i18n$t("Output data will appear here")),
        p(i18n$t("Select the parameters and run the calculation to see results."))
      )
    })
  })

}
