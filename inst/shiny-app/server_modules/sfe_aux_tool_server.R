auxiliary_tools_server <- function(input, output, session, defaults, i18n, tablang, sfe_rv) {



  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Solvent values (programmatic names) and display keys (human-readable for translation)
  solvent_values <- c("Acetone", "Benzene", "CarbonDioxide", "Cyclohexane", "DiethylEther",
    "Ethanol", "Heptane", "Hexane", "Hydrogen", "Methanol", "MethylOleate",
    "Toluene", "PXylene", "OXylene", "Water")
  solvent_display_keys <- c("Acetone", "Benzene", "Carbon Dioxide", "Cyclohexane", "Diethyl Ether",
    "Ethanol", "Heptane", "Hexane", "Hydrogen", "Methanol", "Methyl Oleate",
    "Toluene", "p-Xylene", "o-Xylene", "Water")

  # Helper: translate solvent name
  translate_solvent <- function(name, i18n_fn) {
    key_map <- setNames(solvent_display_keys, solvent_values)
    if (name %in% names(key_map)) return(i18n_fn$t(key_map[[name]]))
    return(name)
  }

  # GCM method code to readable name mapping
  gcm_method_codes <- c("JR_corr", "JR", "SB_corr", "SB", "NL07_robust", "NL07", "NL04",
    "HKR_STW", "HKR_SIM", "SP08", "SP12", "ZHAO", "BND", "SLON")
  gcm_method_display_keys <- c(
    "Joback-Reid corrected", "Joback-Reid",
    "Stein-Brown corrected", "Stein-Brown",
    "Nannoolal 2007 robust", "Nannoolal 2007", "Nannoolal 2004",
    "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous",
    "Stefanis-Panayiotou 2008", "Stefanis-Panayiotou 2012",
    "Zhao 2003", "Bondi 1964", "Slonimskii 1970"
  )

  # Helper: translate GCM method code
  translate_gcm_method <- function(code, i18n_fn) {
    key_map <- setNames(gcm_method_display_keys, gcm_method_codes)
    if (code %in% names(key_map)) return(i18n_fn$t(key_map[[code]]))
    return(code)
  }

  # iscrit_gen method code to readable name mapping (from source documentation)
  iscrit_method_codes <- c("KAY", "LI", "FECP", "HECP", "TANG1", "TANG2", "TANG3", "TANG4")
  iscrit_method_display_keys <- c(
    "Kay (1938)", "Li (1971)", "First Ext. Chueh-Prausnitz (FECP)",
    "He et al. (2017)", "Tang et al. v1 (2025)", "Tang et al. v2 (2025)",
    "Tang et al. v3 (2025)", "Tang et al. v4 (2025)"
  )

  # Helper: translate iscrit method code
  translate_iscrit_method <- function(code, i18n_fn) {
    key_map <- setNames(iscrit_method_display_keys, iscrit_method_codes)
    if (code %in% names(key_map)) return(i18n_fn$t(key_map[[code]]))
    return(code)
  }

  # Helper: translate iscrit_etoh statement
  translate_etoh_statement <- function(statement, i18n_fn) {
    if (is.null(statement) || length(statement) == 0) return("")
    
    # Handle no reference provided case (check first element if vector)
    if (any(grepl("no reference pressure", statement, fixed = TRUE))) {
      return(i18n_fn$t("The physical state of the system was not determined since no reference pressure and temperature were provided."))
    }
    
    # Translate each statement in the vector
    translated <- sapply(statement, function(s) {
      # Skip empty strings or newlines
      s <- trimws(s)
      if (s == "" || s == "\n") return("")
      
      # Extract values using regex
      method_match <- regmatches(s, regexpr("Based on the (Chueh-Prausnitz|Redlich-Kister) method", s))
      if (length(method_match) == 0 || method_match == "") return(s)
      
      method_name <- gsub("Based on the | method", "", method_match)
      
      # Extract Pc, Tc values
      pc_val <- regmatches(s, regexpr("Pc = [0-9.]+", s))
      tc_val <- regmatches(s, regexpr("Tc = [0-9.]+", s))
      if (length(pc_val) == 0 || length(tc_val) == 0) return(s)
      pc_num <- gsub("Pc = ", "", pc_val)
      tc_num <- gsub("Tc = ", "", tc_val)
      
      # Extract conditions
      mpa_val <- regmatches(s, regexpr("At [0-9.]+ MPa", s))
      k_val <- regmatches(s, regexpr("and [0-9.]+ K", s))
      if (length(mpa_val) == 0 || length(k_val) == 0) return(s)
      mpa_num <- gsub("At | MPa", "", mpa_val)
      k_num <- gsub("and | K", "", k_val)
      
      bar_val <- regmatches(s, regexpr("[0-9]+ bar", s))
      c_val <- regmatches(s, regexpr("and [0-9.-]+ C\\)", s))
      if (length(bar_val) == 0 || length(c_val) == 0) return(s)
      bar_num <- gsub(" bar", "", bar_val)
      c_num <- gsub("and | C\\)", "", c_val)
      
      # Determine state
      is_supercrit <- grepl("is supercritical\\.", s)
      state_text <- if (is_supercrit) i18n_fn$t("supercritical") else i18n_fn$t("NOT supercritical")
      
      # Build translated statement using template with placeholder
      # "Based on the {method} method" -> use gsub to replace {method}
      method_phrase <- gsub("\\{method\\}", method_name, i18n_fn$t("Based on the {method} method"))
      
      # Helper for Pc/Tc with subscript
      pc_html <- "<i>P</i><sub>c</sub>"
      tc_html <- "<i>T</i><sub>c</sub>"
      
      # State text: bold
      state_html <- if (is_supercrit) {
        paste0(i18n_fn$t("the mixture physical state is"), " <strong>", i18n_fn$t("supercritical"), "</strong>.")
      } else {
        paste0(i18n_fn$t("the mixture physical state is"), " <strong>", i18n_fn$t("NOT supercritical"), "</strong>.")
      }
      
      paste0(
        method_phrase, ", ", pc_html, " = <strong>", pc_num,
        " MPa</strong> ", i18n_fn$t("and"), " ", tc_html, " = <strong>", tc_num, " K</strong>. ",
        i18n_fn$t("At"), " <strong>", mpa_num, " MPa</strong> ", i18n_fn$t("and"), " <strong>", k_num, " K</strong> (<strong>",
        bar_num, " bar</strong> ", i18n_fn$t("and"), " <strong>", c_num, " \u00B0C</strong>), ",
        state_html
      )
    }, USE.NAMES = FALSE)
    
    # Filter out empty strings and join
    translated <- translated[translated != ""]
    paste(translated, collapse = "\n")
  }

  # Helper: translate iscrit_gen statement
  translate_gen_statement <- function(statement, i18n_fn) {
    if (is.null(statement) || length(statement) == 0 || statement == "") return("")
    
    # Handle no reference provided case
    if (any(grepl("no reference pressure", statement, fixed = TRUE))) {
      return(i18n_fn$t("The physical state of the system was not determined since no reference pressure and temperature were provided."))
    }
    
    # Extract values from "Global averages: Pc = X ± Y MPa, Tc = Z ± W K. At ..."
    # Pc values
    pc_match <- regmatches(statement, regexpr("Pc = [0-9.]+ \u00b1 [0-9.]+", statement))
    if (length(pc_match) == 0 || pc_match == "") return(statement)
    pc_parts <- strsplit(gsub("Pc = ", "", pc_match), " \u00b1 ")[[1]]
    
    # Tc values  
    tc_match <- regmatches(statement, regexpr("Tc = [0-9.]+ \u00b1 [0-9.]+", statement))
    if (length(tc_match) == 0 || tc_match == "") return(statement)
    tc_parts <- strsplit(gsub("Tc = ", "", tc_match), " \u00b1 ")[[1]]
    
    # Conditions
    mpa_val <- regmatches(statement, regexpr("At [0-9.]+ MPa", statement))
    k_val <- regmatches(statement, regexpr("and [0-9.]+ K", statement))
    if (length(mpa_val) == 0 || length(k_val) == 0) return(statement)
    mpa_num <- gsub("At | MPa", "", mpa_val)
    k_num <- gsub("and | K", "", k_val)
    
    bar_val <- regmatches(statement, regexpr("[0-9]+ bar", statement))
    c_val <- regmatches(statement, regexpr("and [0-9.-]+ C\\)", statement))
    if (length(bar_val) == 0 || length(c_val) == 0) return(statement)
    bar_num <- gsub(" bar", "", bar_val)
    c_num <- gsub("and | C\\)", "", c_val)
    
    # Helper for Pc/Tc with subscript
    pc_html <- "<i>P</i><sub>c</sub>"
    tc_html <- "<i>T</i><sub>c</sub>"
    
    # Determine state
    is_supercrit <- grepl("is supercritical\\.", statement)
    state_html <- if (is_supercrit[1]) {
      paste0(i18n_fn$t("the mixture physical state is"), " <strong>", i18n_fn$t("supercritical"), "</strong>.")
    } else {
      paste0(i18n_fn$t("the mixture physical state is"), " <strong>", i18n_fn$t("NOT supercritical"), "</strong>.")
    }
    
    # Build translated statement with HTML formatting
    paste0(
      i18n_fn$t("Global averages"), ": ", pc_html, " = <strong>", pc_parts[1], " \u00B1 ", pc_parts[2],
      " MPa</strong>, ", tc_html, " = <strong>", tc_parts[1], " \u00B1 ", tc_parts[2], " K</strong>. ",
      i18n_fn$t("At"), " <strong>", mpa_num, " MPa</strong> ", i18n_fn$t("and"), " <strong>", k_num, " K</strong> (<strong>",
      bar_num, " bar</strong> ", i18n_fn$t("and"), " <strong>", c_num, " \u00B0C</strong>), ",
      state_html
    )
  }

  # Helper: create numericInput with inline range badge (for narrow inputs)
  # This is a specialized variant that keeps badge inline with label text
  range_badge_input_inline <- function(input_id, label_text, value, min_val, max_val,
                                       step = NA, range_text = NULL, tooltip = NULL) {
    badge_text <- if (!is.null(range_text)) range_text else paste0(min_val, "\u2013", max_val)
    badge_title <- if (!is.null(tooltip)) tooltip else paste0(min_val, " \u2013 ", max_val)
    badge_color <- "#6c757d"
    if (!is.null(value) && !is.na(value) && (value < min_val || value > max_val)) {
      badge_color <- "#dc3545"
    }
    args <- list(inputId = ns(input_id), label = NULL, value = value)
    if (!is.na(step)) args$step <- step
    tags$div(
      tags$label(
        class = "control-label",
        tags$span(label_text),
        tags$span(
          badge_text,
          style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ",
            badge_color, "; color: white; margin-left: 6px; font-weight: normal;"),
          title = badge_title
        )
      ),
      do.call(numericInput, args)
    )
  }

  # Local wrapper for range_badge_input that uses module's ns and i18n_r
  # Calls the global version from utils/general_helpers.R
  local_range_badge_input <- function(input_id, label_text, value, min_val, max_val,
                                      step = NA, range_text = NULL, tooltip = NULL) {
    range_badge_input(ns, i18n_r, input_id, label_text, value, min_val, max_val,
                      step = step, range_text = range_text, tooltip = tooltip,
                      help_content = NULL, help_title = NULL,
                      include_minmax = FALSE)
  }

  # Track whether results exist for conditional panels
  rv_has_crit_results <- reactiveVal(FALSE)
  rv_has_demo_results <- reactiveVal(FALSE)

  output$has_crit_results <- reactive({ rv_has_crit_results() })
  outputOptions(output, "has_crit_results", suspendWhenHidden = FALSE)

  output$has_demo_results <- reactive({ rv_has_demo_results() })
  outputOptions(output, "has_demo_results", suspendWhenHidden = FALSE)

  # Reactive values for general solvent mixture (iscrit_gen)
  rv_gen_solvents <- reactiveVal(list("CO₂", "Ethanol")) # Start with 2 solvents
  rv_gen_fractions <- reactiveVal(c(0.5, 0.5)) # Corresponding fractions

  # Display available solvents data
  output$show_solv_table <- DT::renderDataTable({
    suppressWarnings(solvents_data <- show_solv())

    # Translate solvent names
    solvents_data$Solvent <- sapply(solvents_data$Solvent, function(s) translate_solvent(s, i18n_r()))

    # Remove Abbreviation column (redundant)
    solvents_data <- solvents_data[, !names(solvents_data) %in% "Abbreviation"]

    # Short column names (for display) - with proper HTML formatting
    # Using italics for variables, subscripts where needed, translated units
    cols_prettynames_short <- c(
      i18n_r()$t("Solvent"),
      i18n_r()$t("CAS"),
      paste0("<i>M</i><sub>W</sub> (", i18n_r()$t("g/mol"), ")"),
      paste0("<i>M</i><sub>V</sub> (", i18n_r()$t("mL/mol"), ")"),
      paste0("<i>\u03C1</i> (", i18n_r()$t("g/mL"), ")"),
      paste0("<i>\u03B4</i><sub>D</sub> (", i18n_r()$t("MPa"), "<sup>\u00BD</sup>)"),
      paste0("<i>\u03B4</i><sub>P</sub> (", i18n_r()$t("MPa"), "<sup>\u00BD</sup>)"),
      paste0("<i>\u03B4</i><sub>H</sub> (", i18n_r()$t("MPa"), "<sup>\u00BD</sup>)"),
      paste0("<i>T</i><sub>b</sub> (K)"),
      paste0("<i>T</i><sub>tp</sub> (K)"),
      paste0("<i>T</i><sub>c</sub> (K)"),
      paste0("<i>P</i><sub>c</sub> (", i18n_r()$t("MPa"), ")"),
      paste0("<i>V</i><sub>c</sub> (", i18n_r()$t("L/mol"), ")"),
      paste0("<i>\u03C1</i><sub>c</sub> (", i18n_r()$t("mol/L"), ")"),
      "<i>\u03C9</i>"
    )

    # Full column names (for tooltips)
    cols_prettynames_full <- c(
      i18n_r()$t("Solvent Name"),
      i18n_r()$t("CAS Number"),
      i18n_r()$t("Molecular Weight"),
      i18n_r()$t("Molar Volume"),
      i18n_r()$t("Density"),
      i18n_r()$t("Hansen Parameter: Dispersion"),
      i18n_r()$t("Hansen Parameter: Polar"),
      i18n_r()$t("Hansen Parameter: Hydrogen Bonding"),
      i18n_r()$t("Boiling Point"),
      i18n_r()$t("Triple Point"),
      i18n_r()$t("Critical Temperature"),
      i18n_r()$t("Critical Pressure"),
      i18n_r()$t("Critical Volume"),
      i18n_r()$t("Critical Density"),
      i18n_r()$t("Acentric Factor")
    )

    # Create HTML tooltips
    add_prettynames <- sprintf(
      "<span title='%s'>%s</span>",
      cols_prettynames_full, cols_prettynames_short
    )

    DT::datatable(
      solvents_data,
      extensions = "Buttons",
      colnames = add_prettynames,
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        dom = "Bfrtip",
        language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "solvents_database")
      ),
      escape = FALSE
    )
  })

  # Display GCM method selection chart
  output$show_gcm_table <- DT::renderDataTable({
    suppressWarnings(gcm_data <- show_gcm())
    # Replace "Y" with heavy checkmark
    gcm_data[gcm_data == "Y"] <- "\u2714"

    # Translate method names in the Method column
    gcm_data[[1]] <- sapply(gcm_data[[1]], function(m) translate_gcm_method(m, i18n_r()))

    # Short column names (for display) - with proper HTML formatting and translated units
    cols_prettynames_short <- c(
      i18n_r()$t("Method"),
      i18n_r()$t("Order"),
      "<i>T</i><sub>b</sub> (K)",
      "<i>T</i><sub>c</sub> (K)",
      paste0("<i>P</i><sub>c</sub> (", i18n_r()$t("MPa"), ")"),
      paste0("<i>V</i><sub>c</sub> (", i18n_r()$t("L/mol"), ")"),
      paste0("<i>\u03B4</i><sub>D</sub> (", i18n_r()$t("MPa"), "<sup>\u00BD</sup>)"),
      paste0("<i>\u03B4</i><sub>P</sub> (", i18n_r()$t("MPa"), "<sup>\u00BD</sup>)"),
      paste0("<i>\u03B4</i><sub>H</sub> (", i18n_r()$t("MPa"), "<sup>\u00BD</sup>)"),
      "<i>\u03B4</i><sub>P</sub> (low)",
      "<i>\u03B4</i><sub>H</sub> (low)",
      paste0("<i>V</i><sub>DW</sub> (", i18n_r()$t("cm³/mol"), ")"),
      paste0("<i>A</i><sub>VDW</sub> (", i18n_r()$t("cm²/mol"), ")"),
      "<i>\u03C9</i>",
      paste0("<i>V</i><sub>m</sub> (", i18n_r()$t("mL/mol"), ")")
    )

    # Full column names (for tooltips)
    cols_prettynames_full <- c(
      i18n_r()$t("Group Contribution Method"),
      i18n_r()$t("Method Order"),
      i18n_r()$t("Boiling Point"),
      i18n_r()$t("Critical Temperature"),
      i18n_r()$t("Critical Pressure"),
      i18n_r()$t("Critical Volume"),
      i18n_r()$t("Dispersion Parameter"),
      i18n_r()$t("Polar Parameter"),
      i18n_r()$t("H-Bonding Parameter"),
      i18n_r()$t("Low Polar Parameter"),
      i18n_r()$t("Low H-Bonding Parameter"),
      i18n_r()$t("Van der Waals Volume"),
      i18n_r()$t("Van der Waals Area"),
      i18n_r()$t("Acentric Factor"),
      i18n_r()$t("Molar Volume")
    )

    # Create HTML tooltips
    add_prettynames <- sprintf(
      "<span title='%s'>%s</span>",
      cols_prettynames_full, cols_prettynames_short
    )

    DT::datatable(
      gcm_data,
      extensions = "Buttons",
      colnames = add_prettynames,
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        dom = "Bfrtip",
        language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "gcm_method_selection")
      ),
      escape = FALSE
    )
  })

  # Get all available solvents for dynamic dropdowns
  all_solvents <- reactive({
    suppressWarnings(show_solv()$Solvent)
  })

  # Dynamic UI for CO2 molar fraction with inline range badge
  output$etoh_co2_frac_ui <- renderUI({
    cur <- if (is.null(input$etoh_co2_frac)) defaults$etoh_co2_frac else input$etoh_co2_frac
    sliderInput(ns("etoh_co2_frac"),
      tags$span(i18n_r()$t("CO\u2082 Molar Fraction"),
        input_help(i18n_r()$t("Molar fraction of CO2 in the binary CO2-Ethanol mixture (0 to 1). A value of 0 is pure ethanol, 1 is pure CO2."),
                   title = i18n_r()$t("CO2 Molar Fraction"), buttonLabel = i18n_r()$t("OK"))),
      min = 0, max = 1, value = cur, step = 0.01
    )
  })
  outputOptions(output, "etoh_co2_frac_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for etoh temperature
  output$etoh_temp_ui <- renderUI({
    cur <- if (is.null(input$etoh_temp)) defaults$etoh_temp else input$etoh_temp
    numericInput(ns("etoh_temp"), paste0(i18n_r()$t("Temperature"), " (\u00B0C)"), value = cur)
  })
  outputOptions(output, "etoh_temp_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for gen temperature
  output$gen_temp_ui <- renderUI({
    cur <- if (is.null(input$gen_temp)) defaults$gen_temp else input$gen_temp
    numericInput(ns("gen_temp"), paste0(i18n_r()$t("Temperature"), " (\u00B0C)"), value = cur)
  })
  outputOptions(output, "gen_temp_ui", suspendWhenHidden = FALSE)

  # Shared temperature UI with range badge and auto-correction
  output$shared_temp_ui <- renderUI({
    cur <- if (!is.null(input$shared_temp)) input$shared_temp else defaults$etoh_temp
    if (!is.null(cur) && !is.na(cur) && (cur < -20 || cur > 200)) {
      if (!is.null(input$shared_temp)) showNotification(i18n$t("Temperature was adjusted to the valid range (-20 to 200 °C)."), type = "warning")
      cur <- max(-20, min(200, cur))
    }
    tags$div(
      tags$label(
        paste0(i18n_r()$t("Temperature"), " (\u00B0C)"),
        input_help(i18n_r()$t("Temperature at which to evaluate the mixture critical parameters (-20 to 200 \u00B0C)."),
                   title = i18n_r()$t("Temperature"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("-20\u2013200",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0(i18n_r()$t("Valid range:"), " -20 \u2013 200 \u00B0C")
        )
      ),
      numericInput(ns("shared_temp"), label = NULL, value = cur)
    )
  })
  outputOptions(output, "shared_temp_ui", suspendWhenHidden = FALSE)

  # Shared pressure UI with range badge and auto-correction
  output$shared_pres_ui <- renderUI({
    cur <- if (!is.null(input$shared_pres)) input$shared_pres else defaults$etoh_pres
    if (!is.null(cur) && !is.na(cur) && (cur < 1 || cur > 1000)) {
      if (!is.null(input$shared_pres)) showNotification(i18n$t("Pressure was adjusted to the valid range (1 to 1000 bar)."), type = "warning")
      cur <- max(1, min(1000, cur))
    }
    tags$div(
      tags$label(
        paste0(i18n_r()$t("Pressure"), " (bar)"),
        input_help(i18n_r()$t("Pressure at which to evaluate the mixture critical parameters (1-1000 bar)."),
                   title = i18n_r()$t("Pressure"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("1\u20131000",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0(i18n_r()$t("Valid range:"), " 1 \u2013 1000 bar")
        )
      ),
      numericInput(ns("shared_pres"), label = NULL, value = cur)
    )
  })
  outputOptions(output, "shared_pres_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for etoh method radioButtons with translated labels
  output$etoh_method_ui <- renderUI({
    selectInput(ns("etoh_method"),
      tags$span(i18n_r()$t("Method"),
        input_help(i18n_r()$t("Method for estimating critical parameters of the CO2-Ethanol mixture. Chueh-Prausnitz is valid across the full range but overestimates at high CO2 fractions. Redlich-Kister corrects this overestimation. Both runs both methods for comparison."),
                   title = i18n_r()$t("Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("both", "chueh", "redlich"),
        c(i18n_r()$t("Both"), "Chueh-Prausnitz", "Redlich-Kister")
      ),
      selected = if (!is.null(input$etoh_method)) input$etoh_method else defaults$etoh_method
    )
  })
  outputOptions(output, "etoh_method_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for gen_tc_method selectInput with readable method names
  output$gen_tc_method_ui <- renderUI({
    tc_values <- c("all", "KAY", "LI", "FECP", "HECP", "TANG1", "TANG2", "TANG3", "TANG4")
    tc_names <- c(
      i18n_r()$t("All"),
      i18n_r()$t("Kay (1938)"),
      i18n_r()$t("Li (1971)"),
      i18n_r()$t("First Ext. Chueh-Prausnitz (FECP)"),
      i18n_r()$t("He et al. (2017)"),
      i18n_r()$t("Tang et al. v1 (2025)"),
      i18n_r()$t("Tang et al. v2 (2025)"),
      i18n_r()$t("Tang et al. v3 (2025)"),
      i18n_r()$t("Tang et al. v4 (2025)")
    )
    selectInput(ns("gen_tc_method"),
      tags$span(i18n_r()$t("Critical Temperature Method"),
        input_help(i18n_r()$t("Method for estimating the critical temperature of the mixture. Kay (1938) uses molar averaging. Li (1971) accounts for molecular size differences. He et al. (2017) and Tang et al. (2025) are newer empirical correlations."),
                   title = i18n_r()$t("Critical Temperature Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(tc_values, tc_names),
      selected = "all"
    )
  })
  outputOptions(output, "gen_tc_method_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for gen_pc_method selectInput with readable method names
  output$gen_pc_method_ui <- renderUI({
    pc_values <- c("all", "KAY", "LI", "HECP", "TANG1", "TANG2", "TANG3", "TANG4")
    pc_names <- c(
      i18n_r()$t("All"),
      i18n_r()$t("Kay (1938)"),
      i18n_r()$t("Li (1971)"),
      i18n_r()$t("He et al. (2017)"),
      i18n_r()$t("Tang et al. v1 (2025)"),
      i18n_r()$t("Tang et al. v2 (2025)"),
      i18n_r()$t("Tang et al. v3 (2025)"),
      i18n_r()$t("Tang et al. v4 (2025)")
    )
    selectInput(ns("gen_pc_method"),
      tags$span(i18n_r()$t("Critical Pressure Method"),
        input_help(i18n_r()$t("Method for estimating the critical pressure of the mixture. Same approaches as for critical temperature, except the First Extended Chueh-Prausnitz method is not available for pressure."),
                   title = i18n_r()$t("Critical Pressure Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(pc_values, pc_names),
      selected = "all"
    )
  })
  outputOptions(output, "gen_pc_method_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for general solvent inputs
  output$gen_solvent_inputs <- renderUI({
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()
    num_solvents <- length(solvents)

    input_elements <- lapply(seq_len(num_solvents), function(i) {
      current_solvent <- solvents[[i]]
      current_fraction <- fractions[[i]]

      # Filter choices for dropdown: exclude already selected solvents
      available_choices <- setdiff(all_solvents(), solvents[-i])
      translated_choices <- sapply(available_choices, function(s) translate_solvent(s, i18n_r()))

      # Badge color for fraction
      badge_color <- "#6c757d"
      if (!is.null(current_fraction) && !is.na(current_fraction) && (current_fraction < 0 || current_fraction > 1)) {
        badge_color <- "#dc3545"
      }

      div(
        style = "display: flex; align-items: flex-end; gap: 6px; margin-bottom: 5px;",
        div(
          style = "flex: 1;",
          selectInput(ns(paste0("gen_solv_", i)), i18n_r()$t(paste("Solvent", i)),
            choices = setNames(available_choices, translated_choices),
            selected = current_solvent,
            width = "100%"
          )
        ),
        div(
          style = "width: 100px;",
          tags$div(
            tags$label(
              i18n_r()$t("Fraction"),
              class = "control-label",
              style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
              tags$span("0\u20131",
                style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ",
                  badge_color, "; color: white; margin-left: auto; font-weight: normal;")
              )
            ),
            numericInput(ns(paste0("gen_frac_", i)), label = NULL,
              value = current_fraction, step = 0.01, width = "100%"
            )
          )
        ),
        if (i == 3) {
          div(
            style = "margin-bottom: 15px;",
            actionButton(ns(paste0("remove_solv_", i)), label = NULL,
              icon = icon("times"),
              class = "btn btn-default btn-sm"
            )
          )
        }
      )
    })

    # Add button for 3rd solvent if less than 3 solvents are selected
    if (num_solvents < 3) {
      input_elements <- c(input_elements, list(
        actionButton(ns("add_solvent"), i18n_r()$t("Add Solvent"), class = "btn btn-primary", style = "color: white;")
      ))
    }

    # Total fraction display
    input_elements <- c(input_elements, list(
      uiOutput(ns("gen_total_fraction_display"))
    ))

    tagList(input_elements)
  })

  # Reactive display of total fraction, equalize button, and fraction units toggle
  output$gen_total_fraction_display <- renderUI({
    solvents <- rv_gen_solvents()

    # Get current values from inputs
    actual_fractions <- as.numeric(lapply(seq_along(solvents), function(i) {
      val <- input[[paste0("gen_frac_", i)]]
      if (is.null(val)) 0 else val
    }))

    total_fraction <- sum(actual_fractions)
    total_bg <- if (abs(total_fraction - 1) < 0.001) "#28a745" else "#dc3545"
    
    cur_units <- input$gen_units %||% "mol"

    div(
      style = "display: flex; align-items: center; gap: 8px; margin-top: 5px;",
      p(
        style = "margin-bottom: 0;",
        strong(i18n$t("Total:")), " ",
        span(
          sprintf("%.2f", total_fraction),
          style = paste0(
            "background-color:", total_bg, "; color: white; font-weight: bold; ",
            "padding: 2px 8px; border-radius: 4px;"
          )
        )
      ),
      actionButton(ns("equalize_fractions"), i18n$t("Equalize"),
        class = "btn btn-success", style = "color: white;"
      ),
      span(style = "border-left: 1px solid #dee2e6; height: 28px;"),
      div(
        style = "display: flex; align-items: center; gap: 4px; margin-bottom: 0;",
        tags$span(style = "font-weight: bold; margin-right: 2px;", i18n$t("Fraction units:")),
        tags$label(
          class = "btn btn-default", style = paste0("margin-bottom: 0; padding: 4px 10px; cursor: pointer;",
            if (cur_units == "mol") " background-color: #337ab7; color: white; border-color: #337ab7;" else ""),
          `for` = ns("gen_units_mol"),
          i18n$t("Mole")
        ),
        tags$input(type = "radio", name = ns("gen_units"), id = ns("gen_units_mol"),
          value = "mol", checked = if (cur_units == "mol") "checked" else NULL,
          style = "display: none;",
          onchange = paste0("Shiny.setInputValue('", ns("gen_units"), "', 'mol')")
        ),
        tags$label(
          class = "btn btn-default", style = paste0("margin-bottom: 0; padding: 4px 10px; cursor: pointer;",
            if (cur_units == "mass") " background-color: #337ab7; color: white; border-color: #337ab7;" else ""),
          `for` = ns("gen_units_mass"),
          i18n$t("Mass")
        ),
        tags$input(type = "radio", name = ns("gen_units"), id = ns("gen_units_mass"),
          value = "mass", checked = if (cur_units == "mass") "checked" else NULL,
          style = "display: none;",
          onchange = paste0("Shiny.setInputValue('", ns("gen_units"), "', 'mass')")
        )
      )
    )
  })

  # Add solvent button logic
  observeEvent(input$add_solvent, {
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()
    if (length(solvents) < 3) {
      # Find a solvent not already in the list
      available_solvents <- setdiff(all_solvents(), solvents)
      new_solvent <- if (length(available_solvents) > 0) available_solvents[1] else ""

      rv_gen_solvents(c(solvents, new_solvent))
      rv_gen_fractions(c(fractions, 0)) # Add with default fraction 0
    }
  })

  # Equalize fractions
  observeEvent(input$equalize_fractions, {
    solvents <- rv_gen_solvents()
    n <- length(solvents)
    req(n >= 2)
    equal_frac <- round(1 / n, 4)
    rv_gen_fractions(rep(equal_frac, n))
    for (i in seq_len(n)) {
      updateNumericInput(session, paste0("gen_frac_", i), value = equal_frac)
    }
  })

  # Remove solvent button logic (for the 3rd solvent)
  observeEvent(input$remove_solv_3, {
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()
    if (length(solvents) == 3) {
      rv_gen_solvents(solvents[-3])
      rv_gen_fractions(fractions[-3])
    }
  })

  # Handle CO₂-Ethanol Critical Parameters calculation
  observeEvent(input$calc_etoh_crit, {
    req(input$etoh_co2_frac, input$etoh_pres, input$etoh_temp, input$etoh_method)

    withProgress(message = i18n$t("Calculating CO₂-Ethanol Critical Parameters..."), {
      results <- tryCatch(
        {
          iscrit_etoh(
            fracs = input$etoh_co2_frac,
            pres = input$etoh_pres,
            temp = input$etoh_temp,
            units = "mol", # iscrit_etoh uses mol fraction
            method = input$etoh_method
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error calculating CO₂-Ethanol critical params:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(results)) {
        output$iscrit_etoh_results <- renderUI({
          # Translate statement
          statement_text <- translate_etoh_statement(results$statement, i18n_r())

          # Extract results - results$results is a list with method names as keys
          method_results <- results$results

          output_elements <- list(
            h5(strong(i18n_r()$t("Result:"))),
            p(style = "white-space: pre-wrap; line-height: 1.8;", HTML(statement_text))
          )

          # Add results for each method
          for (method_name in names(method_results)) {
            method_data <- method_results[[method_name]]
            # Translate method name (CHUEH -> Chueh-Prausnitz, REDLICH -> Redlich-Kister)
            display_name <- switch(method_name,
              "CHUEH" = "Chueh-Prausnitz",
              "REDLICH" = "Redlich-Kister",
              method_name
            )
            # Use template with placeholder for proper word order: "{method} Method Results"
            method_results_text <- gsub("\\{method\\}", display_name, i18n_r()$t("{method} Method Results"))
            output_elements <- c(output_elements, list(
              hr(),
              h5(method_results_text),
              p(HTML(paste0("<strong>", i18n_r()$t("Critical Temperature (K)"), "</strong> ")), sprintf("%.2f", method_data["Tc"])),
              p(HTML(paste0("<strong>", i18n_r()$t("Critical Pressure (MPa)"), "</strong> ")), sprintf("%.2f", method_data["Pc"]))
            ))
          }

          do.call(tagList, output_elements)
        })
      }
    })
  })

  # Handle General Solvent Mixtures Critical Parameters calculation
  observeEvent(input$calc_gen_crit, {
    solvents <- rv_gen_solvents()
    fractions <- rv_gen_fractions()

    # Actual values from inputs at calculation time
    actual_solvents <- lapply(seq_along(solvents), function(i) {
      input[[paste0("gen_solv_", i)]]
    })

    actual_fractions <- as.numeric(lapply(seq_along(fractions), function(i) {
      val <- input[[paste0("gen_frac_", i)]]
      if (is.null(val)) 0 else val
    }))

    req(length(actual_solvents) >= 2, length(actual_solvents) <= 3) # Ensure 2 or 3 solvents
    req(all(!is.na(actual_solvents)), all(actual_solvents != "")) # Ensure solvents are selected
    req(all(!is.na(actual_fractions)), all(actual_fractions >= 0)) # Ensure fractions are valid numbers

    # Ensure fractions sum to 1 (or close to 1 due to floating point)
    if (abs(sum(actual_fractions) - 1) > 0.001) {
      showNotification(i18n$t("Fractions must sum to 1."), type = "error")
      return(NULL)
    }

    # Check for duplicate solvents
    if (length(unique(actual_solvents)) != length(actual_solvents)) {
      showNotification(i18n$t("Duplicate solvents are not allowed."), type = "error")
      return(NULL)
    }

    withProgress(message = i18n$t("Calculating General Critical Parameters..."), {
      results <- tryCatch(
        {
          iscrit_gen(
            solv = unlist(actual_solvents),
            fracs = unlist(actual_fractions),
            pres = input$gen_pres,
            temp = input$gen_temp,
            units = input$gen_units %||% "mol",
            tc = input$gen_tc_method,
            pc = input$gen_pc_method
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error calculating general critical params:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(results)) {
        output$iscrit_gen_results <- renderUI({
          # Translate statement
          statement_text <- translate_gen_statement(results$statement, i18n_r())

          tagList(
            h5(strong(i18n_r()$t("Result:"))),
            p(style = "white-space: pre-wrap; line-height: 1.8;", HTML(statement_text)),
            DT::renderDataTable({
              req(!is.null(results$results_df))

              # Translate method names in results_df
              results_df <- results$results_df
              results_df$Method <- sapply(results_df$Method, function(m) translate_iscrit_method(m, i18n_r()))

              # Column names translation
              add_prettynames <- c(
                i18n_r()$t("Method"),
                i18n_r()$t("Critical Temperature (K)"),
                i18n_r()$t("Critical Pressure (MPa)")
              )

              dt <- DT::datatable(
                results_df,
                extensions = "Buttons",
                colnames = add_prettynames,
                options = list(
                  dom = "Bfrtip",
                  scrollX = TRUE,
                  language = tablang(),
                  buttons = create_dt_export_buttons(i18n_r, "general_critical_parameters")
                ),
                rownames = FALSE
              )
              numeric_columns <- names(results_df)[sapply(results_df, is.numeric)]
              if (length(numeric_columns) > 0) {
                dt <- dt %>% DT::formatRound(columns = numeric_columns, digits = 2)
              }
              dt
            })
          )
        })
      }
    })
  })

  # Reset Critical Parameters
  observeEvent(input$reset_crit, {
    rv_has_crit_results(FALSE)
    rv_has_demo_results(FALSE)
    # Reset inputs to defaults
    updateSliderInput(session, "etoh_co2_frac", value = defaults$etoh_co2_frac)
    showNotification(i18n$t("Results cleared."), type = "message")
  })

  # Unified Calculate Critical Parameters handler
  observeEvent(input$calc_crit, {
    active_tab <- input$mixture_type_tabs
    req(active_tab)
    
    if (active_tab == "co2_etoh") {
      # CO2-Ethanol mode
      req(input$etoh_co2_frac, input$shared_pres, input$shared_temp, input$etoh_method)
      
      withProgress(message = i18n$t("Calculating CO₂-EtOH Critical Parameters..."), {
        results <- tryCatch({
          iscrit_etoh(
            fracs = input$etoh_co2_frac,
            pres = input$shared_pres,
            temp = input$shared_temp,
            units = "mol",
            method = input$etoh_method
          )
        }, error = function(e) {
          showNotification(paste(i18n$t("Error calculating critical params:"), e$message), type = "error")
          NULL
        })
        
        if (!is.null(results)) {
          rv_has_crit_results(TRUE)
          
          # Capture values at computation time
          co2_frac_val <- input$etoh_co2_frac
          
          output$crit_results <- renderUI({
            # Build mixture description inside renderUI for translation reactivity
            etoh_frac_val <- round(1 - co2_frac_val, 2)
            mixture_desc <- paste0(
              translate_solvent("CarbonDioxide", i18n_r()), " : ",
              translate_solvent("Ethanol", i18n_r()), " = ",
              co2_frac_val, " : ", etoh_frac_val,
              " (", i18n_r()$t("mole fraction"), ")"
            )
            
            statement_text <- translate_etoh_statement(results$statement, i18n_r())
            
            method_names <- names(results$results)
            
            # Build method result cards
            method_cards <- lapply(method_names, function(method_name) {
              method_data <- results$results[[method_name]]
              display_name <- switch(method_name,
                "CHUEH" = "Chueh-Prausnitz",
                "REDLICH" = "Redlich-Kister",
                method_name
              )
              method_results_text <- gsub("\\{method\\}", display_name, i18n_r()$t("{method} Method Results"))
              div(
                style = "padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
                tags$h5(method_results_text, style = "margin-top: 0;"),
                tags$p(style = "margin-bottom: 4px;", HTML(paste0("<strong>", i18n_r()$t("Critical Temperature (K)"), "</strong> ")), sprintf("%.2f", method_data["Tc"])),
                tags$p(style = "margin-bottom: 0;", HTML(paste0("<strong>", i18n_r()$t("Critical Pressure (MPa)"), "</strong> ")), sprintf("%.2f", method_data["Pc"]))
              )
            })
            
            # Side-by-side if multiple methods, single column otherwise
            if (length(method_cards) > 1) {
              results_row <- fluidRow(
                lapply(seq_along(method_cards), function(idx) {
                  card_style <- if (idx < length(method_cards)) "border-right: 1px solid #dee2e6; padding-right: 15px;" else ""
                  column(width = floor(12 / length(method_cards)), div(style = card_style, method_cards[[idx]]))
                })
              )
            } else {
              results_row <- method_cards[[1]]
            }
            
            tagList(
              p(strong(i18n_r()$t("Mixture:")), mixture_desc),
              h5(strong(i18n_r()$t("Result:"))),
              p(style = "white-space: pre-wrap; line-height: 1.8;", HTML(statement_text)),
              hr(),
              results_row
            )
          })
        }
      })
      
    } else if (active_tab == "gen_mixture") {
      # General mixture mode
      solvents <- rv_gen_solvents()
      fractions <- rv_gen_fractions()
      
      actual_solvents <- lapply(seq_along(solvents), function(i) input[[paste0("gen_solv_", i)]])
      actual_fractions <- as.numeric(lapply(seq_along(fractions), function(i) {
        val <- input[[paste0("gen_frac_", i)]]
        if (is.null(val)) 0 else val
      }))
      
      req(length(actual_solvents) >= 2, length(actual_solvents) <= 3)
      req(all(!is.na(actual_solvents)), all(actual_solvents != ""))
      req(all(!is.na(actual_fractions)), all(actual_fractions >= 0))
      
      if (abs(sum(actual_fractions) - 1) > 0.001) {
        showNotification(i18n$t("Fractions must sum to 1."), type = "error")
        return(NULL)
      }
      
      if (length(unique(actual_solvents)) != length(actual_solvents)) {
        showNotification(i18n$t("Duplicate solvents are not allowed."), type = "error")
        return(NULL)
      }
      
      withProgress(message = i18n$t("Calculating Critical Parameters..."), {
        results <- tryCatch({
          iscrit_gen(
            solv = unlist(actual_solvents),
            fracs = unlist(actual_fractions),
            pres = input$shared_pres,
            temp = input$shared_temp,
            units = input$gen_units %||% "mol",
            tc = input$gen_tc_method,
            pc = input$gen_pc_method
          )
        }, error = function(e) {
          showNotification(paste(i18n$t("Error calculating critical params:"), e$message), type = "error")
          NULL
        })
        
        if (!is.null(results)) {
          rv_has_crit_results(TRUE)
          
          # Capture values at computation time
          comp_solvents <- unlist(actual_solvents)
          comp_fractions <- unlist(actual_fractions)
          comp_units <- input$gen_units %||% "mol"
          
          output$crit_results <- renderUI({
            # Build mixture description inside renderUI for translation reactivity
            units_label <- if (comp_units == "mol") i18n_r()$t("mole fraction") else i18n_r()$t("mass fraction")
            translated_solvents <- sapply(comp_solvents, function(s) translate_solvent(s, i18n_r()))
            mixture_desc <- paste0(
              paste(translated_solvents, collapse = " : "), " = ",
              paste(round(comp_fractions, 4), collapse = " : "),
              " (", units_label, ")"
            )
            
            statement_text <- translate_gen_statement(results$statement, i18n_r())
            
            tagList(
              p(strong(i18n_r()$t("Mixture:")), mixture_desc),
              h5(strong(i18n_r()$t("Result:"))),
              p(style = "white-space: pre-wrap; line-height: 1.8;", HTML(statement_text)),
              DT::renderDataTable({
                req(!is.null(results$results_df))
                results_df <- results$results_df
                results_df$Method <- sapply(results_df$Method, function(m) translate_iscrit_method(m, i18n_r()))
                add_prettynames <- c(
                  i18n_r()$t("Method"),
                  i18n_r()$t("Critical Temperature (K)"),
                  i18n_r()$t("Critical Pressure (MPa)")
                )
                dt <- DT::datatable(
                  results_df,
                  extensions = "Buttons",
                  colnames = add_prettynames,
                  options = list(
                    dom = "Bfrtip",
                    scrollX = TRUE,
                    language = tablang(),
                    buttons = create_dt_export_buttons(i18n_r, "critical_parameters")
                  ),
                  rownames = FALSE
                )
                numeric_columns <- names(results_df)[sapply(results_df, is.numeric)]
                if (length(numeric_columns) > 0) dt <- dt %>% DT::formatRound(columns = numeric_columns, digits = 2)
                dt
              })
            )
          })
        }
      })
    }
  })

  # Handle iscrit_demo execution (integrated into etoh tab)
  observeEvent(input$run_etoh_demo, {
    withProgress(message = i18n$t("Running CO₂-Ethanol Demo..."), {
      demo_results <- tryCatch(
        {
          iscrit_demo()
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error running demo:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(demo_results)) {
        # Render Tc plot separately
        output$iscrit_etoh_demo_tc_plot <- renderPlot({
          plot <- demo_results$plots$Tc

          # Translate method names for legend labels
          method_labels <- c(
            "Chueh-Prausnitz" = "Chueh-Prausnitz",
            "Experimental" = i18n_r()$t("Experimental"),
            "Redlich-Kister" = "Redlich-Kister"
          )

          # Apply translations and override scales
          plot <- translate_plot_labels(
            plot,
            i18n_r(),
            x = "CO₂ mole fraction",
            y = "Critical Temperature (K)",
            colour = "Method: ",
            linetype = "Method: "
          ) +
            ggplot2::scale_colour_manual(
              values = c("darkred", "darkorange", "darkorange"),
              labels = method_labels
            ) +
            ggplot2::scale_linetype_manual(
              values = c(4, 1, 1),
              labels = method_labels
            )

          plot
        })

        # Render Pc plot separately
        output$iscrit_etoh_demo_pc_plot <- renderPlot({
          plot <- demo_results$plots$Pc

          # Translate method names for legend labels
          method_labels <- c(
            "Chueh-Prausnitz" = "Chueh-Prausnitz",
            "Experimental" = i18n_r()$t("Experimental"),
            "Redlich-Kister" = "Redlich-Kister"
          )

          # Apply translations and override scales
          plot <- translate_plot_labels(
            plot,
            i18n_r(),
            x = "CO₂ mole fraction",
            y = "Critical Pressure (MPa)",
            colour = "Method: ",
            linetype = "Method: "
          ) +
            ggplot2::scale_colour_manual(
              values = c("darkred", "darkorange", "darkorange"),
              labels = method_labels
            ) +
            ggplot2::scale_linetype_manual(
              values = c(4, 1, 1),
              labels = method_labels
            )

          plot
        })

        # Render data table
        output$iscrit_etoh_demo_table <- DT::renderDataTable({
          # Column names translation
          add_prettynames <- c(
            i18n_r()$t("Method"),
            i18n_r()$t("Fraction"),
            i18n_r()$t("Critical Temperature (K)"),
            i18n_r()$t("Critical Pressure (MPa)")
          )

          # Method values translation (exclude abbreviations)
          method_translations <- c(
            "Experimental" = i18n_r()$t("Experimental")
          )

          # Apply translations
          dt_data <- demo_results$data
          dt_data$Method <- my_mapvalues(dt_data$Method, names(method_translations), method_translations, warn_missing = FALSE)

          dt <- DT::datatable(
            dt_data,
            extensions = "Buttons",
            colnames = add_prettynames,
            options = list(
              scrollX = TRUE,
              paging = FALSE,
              dom = "Bfrtip",
              language = tablang(),
              buttons = create_dt_export_buttons(i18n_r, "co2_ethanol_demo")
            ),
            rownames = FALSE
          )
          numeric_columns <- names(dt_data)[sapply(dt_data, is.numeric)]
          if (length(numeric_columns) > 0) {
            dt <- dt %>% DT::formatRound(columns = numeric_columns, digits = 2)
          }
          dt
        })

        # Render UI container
        rv_has_demo_results(TRUE)
        output$iscrit_etoh_demo_results <- renderUI({
          tagList(
            h4(i18n_r()$t("Critical Temperature Plot")),
            plotOutput(ns("iscrit_etoh_demo_tc_plot")),
            hr(),
            h4(i18n_r()$t("Critical Pressure Plot")),
            plotOutput(ns("iscrit_etoh_demo_pc_plot")),
            hr(),
            h4(i18n_r()$t("Data Table")),
            DT::dataTableOutput(ns("iscrit_etoh_demo_table"))
          )
        })
      }
    })
  })

  # Helper renderUI outputs
  output$sfe_aux_tool_crit_params_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_aux_tool_crit_params_help_en")
  })

  output$sfe_aux_tool_etoh_crit_params_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_aux_tool_etoh_crit_params_help_en")
  })

  output$sfe_aux_tool_gen_crit_params_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_aux_tool_gen_crit_params_help_en")
  })

  output$sfe_aux_tool_solvents_db_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_aux_tool_solvents_db_help_en")
  })

  output$sfe_aux_tool_gcm_chart_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_aux_tool_gcm_chart_help_en")
  })

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "gen_solvent_inputs", suspendWhenHidden = FALSE)
  outputOptions(output, "gen_total_fraction_display", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_aux_tool_crit_params_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_aux_tool_etoh_crit_params_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_aux_tool_gcm_chart_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_aux_tool_gen_crit_params_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_aux_tool_solvents_db_HELP", suspendWhenHidden = FALSE)

}
