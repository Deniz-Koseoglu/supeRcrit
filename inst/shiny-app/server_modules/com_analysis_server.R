# COM Analysis Server Module
com_analysis_server <- function(input, output, session, defaults, i18n, tablang) {






  # Load required libraries
  library(scales)
  library(dplyr)

  # helper for creating namespaced ids inside this module
  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Load Example button: rendered server-side so i18n_r()$t() returns a plain
  # string (rather than a shiny.i18n <span> wrapper) for the title attribute.
  # See note above about i18n_r()$t() behaviour with/without an active session.
  output$load_example_data_btn <- renderUI({
    create_load_example_btn(ns, i18n_r)
  })

  # Helper function to translate COM results for export
  translate_com_results <- function(com_result, translator) {
    if (is.null(com_result)) return(com_result)
    
    # Helper to ensure UTF-8 encoding
    ensure_utf8 <- function(x) {
      if (is.character(x)) {
        Encoding(x) <- "UTF-8"
      }
      x
    }
    
    # Translation mapping for descriptions
    desc_translations <- c(
      # Production Overview
      "Extraction cycles per month" = ensure_utf8(translator$t("Extraction cycles per month")),
      "S/M mass ratio (CO2)" = ensure_utf8(translator$t("S/M mass ratio (CO2)")),
      "S/M mass ratio (co-solvent)" = ensure_utf8(translator$t("S/M mass ratio (co-solvent)")),
      "S/F mass ratio (CO2)" = ensure_utf8(translator$t("S/F mass ratio (CO2)")),
      "S/F mass ratio (co-solvent)" = ensure_utf8(translator$t("S/F mass ratio (co-solvent)")),
      "Monthly operating time" = ensure_utf8(translator$t("Monthly operating time")),
      "Monthly feed processed" = ensure_utf8(translator$t("Monthly feed processed")),
      "Extract produced (monthly)" = ensure_utf8(translator$t("Extract produced (monthly)")),
      "--- Extract produced (monthly) by response ---" = ensure_utf8(translator$t("--- Extract produced (monthly) by response ---")),
      "  Total" = ensure_utf8(paste0("  ", translator$t("Total"))),
      
      # CRM
      "Main solvent loss per cycle" = ensure_utf8(translator$t("Main solvent loss per cycle")),
      "Co-solvent loss per batch" = ensure_utf8(translator$t("Co-solvent loss per batch")),
      "CO2 loss per month" = ensure_utf8(translator$t("CO2 loss per month")),
      "Co-solvent loss per month" = ensure_utf8(translator$t("Co-solvent loss per month")),
      "Raw material requirement (monthly)" = ensure_utf8(translator$t("Raw material requirement (monthly)")),
      "Main solvent requirement (monthly)" = ensure_utf8(translator$t("Main solvent requirement (monthly)")),
      "Co-solvent requirement (monthly)" = ensure_utf8(translator$t("Co-solvent requirement (monthly)")),
      "Raw material cost (monthly)" = ensure_utf8(translator$t("Raw material cost (monthly)")),
      "Main solvent cost (monthly)" = ensure_utf8(translator$t("Main solvent cost (monthly)")),
      "Co-solvent cost (monthly)" = ensure_utf8(translator$t("Co-solvent cost (monthly)")),
      "Cost of Raw Materials (CRM)" = ensure_utf8(translator$t("Cost of Raw Materials (CRM)")),
      "Auxiliary material cost (monthly)" = ensure_utf8(translator$t("Auxiliary material cost (monthly)")),
      
      # CUT
      "Main power consumption (monthly)" = ensure_utf8(translator$t("Main power consumption (monthly)")),
      "Main power cost (monthly)" = ensure_utf8(translator$t("Main power cost (monthly)")),
      "Drying power consumption (monthly)" = ensure_utf8(translator$t("Drying power consumption (monthly)")),
      "Drying power cost (monthly)" = ensure_utf8(translator$t("Drying power cost (monthly)")),
      "Compression power consumption (monthly)" = ensure_utf8(translator$t("Compression power consumption (monthly)")),
      "Compression power cost (monthly)" = ensure_utf8(translator$t("Compression power cost (monthly)")),
      "Evaporation power consumption (monthly)" = ensure_utf8(translator$t("Evaporation power consumption (monthly)")),
      "Evaporation power cost (monthly)" = ensure_utf8(translator$t("Evaporation power cost (monthly)")),
      "Total electricity consumption (monthly)" = ensure_utf8(translator$t("Total electricity consumption (monthly)")),
      "Total electricity cost (monthly)" = ensure_utf8(translator$t("Total electricity cost (monthly)")),
      "Cost of Utilities (CUT)" = ensure_utf8(translator$t("Cost of Utilities (CUT)")),
      
      # Financial
      "Cost of Labor (COL)" = ensure_utf8(translator$t("Cost of Labor (COL)")),
      "Fixed Costs (FMC)" = ensure_utf8(translator$t("Fixed Costs (FMC)")),
      "Cost of Manufacturing (COM)" = ensure_utf8(translator$t("Cost of Manufacturing (COM)")),
      "Specific extract cost" = ensure_utf8(translator$t("Specific extract cost")),
      "Sales volume (monthly)" = ensure_utf8(translator$t("Sales volume (monthly)")),
      "Monthly gross profit (pre-tax)" = ensure_utf8(translator$t("Monthly gross profit (pre-tax)")),
      "Monthly profit (after tax)" = ensure_utf8(translator$t("Monthly profit (after tax)")),
      "Profit margin" = ensure_utf8(translator$t("Profit margin")),
      "Payback period" = ensure_utf8(translator$t("Payback period")),
      "Weighted average price" = ensure_utf8(translator$t("Weighted average price")),
      
      # Per-response
      "Sale price" = ensure_utf8(translator$t("Sale price")),
      "Dilution factor" = ensure_utf8(translator$t("Dilution factor")),
      "Cost share (proportional)" = ensure_utf8(translator$t("Cost share (proportional)")),
      "Gross profit" = ensure_utf8(translator$t("Gross profit")),
      "Contribution to payback" = ensure_utf8(translator$t("Contribution to payback"))
    )
    
    # Unit translations
    unit_translations <- c(
      "none" = ensure_utf8(translator$t("none")),
      "kg/cycle" = ensure_utf8(translator$t("kg/cycle")),
      "kg/month" = ensure_utf8(translator$t("kg/month")),
      "USD/month" = ensure_utf8(translator$t("USD/month")),
      "USD/kg" = ensure_utf8(translator$t("USD/kg")),
      "kWh/month" = ensure_utf8(translator$t("kWh/month")),
      "hours" = ensure_utf8(translator$t("hours")),
      "years" = ensure_utf8(translator$t("years")),
      "percent" = ensure_utf8(translator$t("percent")),
      "%" = "%"
    )
    
    # Column header translations
    col_translations <- c(
      "Description" = ensure_utf8(translator$t("Description")),
      "Value" = ensure_utf8(translator$t("Value")),
      "Unit" = ensure_utf8(translator$t("Unit"))
    )
    
    # Helper to translate a table
    translate_table <- function(tbl) {
      if (is.null(tbl)) return(tbl)
      
      # Translate descriptions
      if ("Description" %in% colnames(tbl)) {
        tbl$Description <- sapply(tbl$Description, function(d) {
          # Check for exact match first
          if (d %in% names(desc_translations)) {
            return(desc_translations[[d]])
          }
          # Check for indented response names (keep as-is, they're user-defined)
          if (grepl("^  ", d) && !d %in% c("  Total")) {
            return(d)
          }
          return(d)
        })
      }
      
      # Translate units
      if ("Unit" %in% colnames(tbl)) {
        tbl$Unit <- sapply(tbl$Unit, function(u) {
          if (u %in% names(unit_translations)) {
            return(unit_translations[[u]])
          }
          return(u)
        })
      }
      
      # Translate column names
      colnames(tbl) <- sapply(colnames(tbl), function(cn) {
        if (cn %in% names(col_translations)) {
          return(col_translations[[cn]])
        }
        return(cn)
      })
      
      return(tbl)
    }
    
    # Create a deep copy to avoid modifying the original
    translated <- com_result
    
    # Translate tables
    if (!is.null(translated$tables)) {
      for (tbl_name in names(translated$tables)) {
        if (tbl_name == "per_response" && !is.null(translated$tables$per_response)) {
          # Handle nested per-response tables
          for (resp_name in names(translated$tables$per_response)) {
            translated$tables$per_response[[resp_name]] <- translate_table(translated$tables$per_response[[resp_name]])
          }
        } else {
          translated$tables[[tbl_name]] <- translate_table(translated$tables[[tbl_name]])
        }
      }
    }
    
    # Translate input parameters table
    if (!is.null(translated$input)) {
      # Translate column headers (capitalize first letter)
      input_col_translations <- c(
        "parameter" = translator$t("Parameter"),
        "value" = translator$t("Value"),
        "units" = translator$t("Units"),
        "type" = translator$t("Type"),
        "description" = translator$t("Description")
      )
      colnames(translated$input) <- sapply(colnames(translated$input), function(cn) {
        if (cn %in% names(input_col_translations)) {
          return(input_col_translations[[cn]])
        }
        return(cn)
      })
      
      # Translate descriptions if column exists (check translated name too)
      desc_col <- if ("description" %in% colnames(translated$input)) "description" else translator$t("Description")
      if (desc_col %in% colnames(translated$input)) {
        # Input parameter description translations
        input_desc_translations <- c(
          # General Parameters
          "Extraction vessel volume" = translator$t("Extraction vessel volume"),
          "Material loading per batch" = translator$t("Material loading per batch"),
          "Feed flow rate" = translator$t("Feed flow rate"),
          "Feed density" = translator$t("Feed density"),
          "Extraction pressure" = translator$t("Extraction pressure"),
          "Extraction temperature" = translator$t("Extraction temperature"),
          "Main solvent flow (CO2 or water; mL/min or g/min)" = translator$t("Main solvent flow (CO2 or water; mL/min or g/min)"),
          "Additional batch processing time (changeover etc.)" = translator$t("Additional batch processing time (changeover etc.)"),
          "Dilution factor of the crude extract (if any)" = translator$t("Dilution factor of the crude extract (if any)"),
          "Sale price of the final extract" = translator$t("Sale price of the final extract"),
          "Co-solvent flow (EtOH or CO2; mL/min or g/min)" = translator$t("Co-solvent flow (EtOH or CO2; mL/min or g/min)"),
          "Flow measurement pressure" = translator$t("Flow measurement pressure"),
          "Flow measurement temperature" = translator$t("Flow measurement temperature"),
          # CRM Parameters
          "Material bed height" = translator$t("Material bed height"),
          "Extraction vessel internal diameter" = translator$t("Extraction vessel internal diameter"),
          "Price of extractable material" = translator$t("Price of extractable material"),
          "Price of the main solvent (CO2 or H2O)" = translator$t("Price of the main solvent (CO2 or H2O)"),
          "Price of co-solvent (EtOH or CO2)" = translator$t("Price of co-solvent (EtOH or CO2)"),
          "CO2 recovery line pressure" = translator$t("CO2 recovery line pressure"),
          "CO2 recovery line temperature" = translator$t("CO2 recovery line temperature"),
          "Final CO2 separator temperature" = translator$t("Final CO2 separator temperature"),
          "Fraction of collected co-solvent lost during post-processing" = translator$t("Fraction of collected co-solvent lost during post-processing"),
          # CUT Parameters
          "Main system power usage" = translator$t("Main system power usage"),
          "Price of electricity" = translator$t("Price of electricity"),
          "Power usage for drying (pre-processing)" = translator$t("Power usage for drying (pre-processing)"),
          "Power usage for comminution (pre-processing)" = translator$t("Power usage for comminution (pre-processing)"),
          "Power usage for evaporation (post-processing)" = translator$t("Power usage for evaporation (post-processing)"),
          "Hourly drying capacity (pre-processing)" = translator$t("Hourly drying capacity (pre-processing)"),
          "Hourly comminution capacity (pre-processing)" = translator$t("Hourly comminution capacity (pre-processing)"),
          "Hourly evaporation capacity (post-processing)" = translator$t("Hourly evaporation capacity (post-processing)"),
          "Additional auxiliary power usage" = translator$t("Additional auxiliary power usage"),
          # COL Parameters
          "Number of operator staff required" = translator$t("Number of operator staff required"),
          "Number of hours per work shift" = translator$t("Number of hours per work shift"),
          "Number of work shifts per calendar day" = translator$t("Number of work shifts per calendar day"),
          "Average monthly wage per operator" = translator$t("Average monthly wage per operator"),
          "Number of work days per month (28 days)" = translator$t("Number of work days per month (28 days)"),
          # FMC Parameters
          "Capital expenditure invested" = translator$t("Capital expenditure invested"),
          "Monthly maintenance costs" = translator$t("Monthly maintenance costs"),
          "Other monthly costs" = translator$t("Other monthly costs"),
          "Depreciation (as CAPEX fraction per year)" = translator$t("Depreciation (as CAPEX fraction per year)"),
          "Tax rate (for net profit calculation)" = translator$t("Tax rate (for net profit calculation)")
        )
        
        translated$input[[desc_col]] <- sapply(translated$input[[desc_col]], function(d) {
          if (d %in% names(input_desc_translations)) {
            return(input_desc_translations[[d]])
          }
          # Fallback to translator$t
          trans <- translator$t(d)
          if (trans != d) return(trans)
          return(d)
        })
      }
      
      # Translate units column
      units_col <- if ("units" %in% colnames(translated$input)) "units" else translator$t("Units")
      if (units_col %in% colnames(translated$input)) {
        input_unit_translations <- c(
          "L" = translator$t("L"),
          "kg" = translator$t("kg"),
          "bar" = translator$t("bar"),
          "°C" = translator$t("°C"),
          "degC" = translator$t("°C"),
          "mL/min" = translator$t("mL/min"),
          "g/min" = translator$t("g/min"),
          "min" = translator$t("min"),
          "none" = translator$t("none"),
          "USD/kg" = translator$t("USD/kg"),
          "USD" = translator$t("USD"),
          "USD/month" = translator$t("USD/month"),
          "USD/kWh" = translator$t("USD/kWh"),
          "kW" = translator$t("kW"),
          "kWh" = translator$t("kWh"),
          "kg/h" = translator$t("kg/h"),
          "L/h" = translator$t("L/h"),
          "cm" = translator$t("cm"),
          "shifts/day" = translator$t("shifts/day"),
          "h/shift" = translator$t("h/shift"),
          "hours" = translator$t("hours"),
          "people" = translator$t("people"),
          "days/month" = translator$t("days/month"),
          "%" = "%",
          "%/year" = translator$t("dimensionless"),
          "dimensionless" = translator$t("dimensionless"),
          "percent" = translator$t("percent")
        )
        
        translated$input[[units_col]] <- sapply(translated$input[[units_col]], function(u) {
          if (u %in% names(input_unit_translations)) {
            return(input_unit_translations[[u]])
          }
          # Fallback to translator$t
          trans <- translator$t(u)
          if (trans != u) return(trans)
          return(u)
        })
      }
    }
    
    # Translate performance table column names
    if (!is.null(translated$tables) && !is.null(translated$tables$performance)) {
      perf_col_translations <- c(
        "Time (min)" = translator$t("Time (min)"),
        "Yield (%)" = translator$t("Yield (%)"),
        "Monthly Profit ($)" = translator$t("Monthly Profit ($)"),
        "Margin (%)" = translator$t("Margin (%)"),
        "Payback (years)" = translator$t("Payback (years)"),
        "Specific Cost ($/kg)" = translator$t("Specific Cost ($/kg)")
      )
      current_cols <- colnames(translated$tables$performance)
      new_cols <- sapply(current_cols, function(cn) {
        if (cn %in% names(perf_col_translations)) {
          return(perf_col_translations[[cn]])
        }
        return(cn)
      })
      colnames(translated$tables$performance) <- new_cols
    }
    
    # Translate extra results names (fci, col)
    if (!is.null(translated$extra)) {
      extra_translations <- c(
        "fci" = translator$t("FMC (Fixed Costs)"),
        "col" = translator$t("COL (Cost of Labor)")
      )
      current_names <- names(translated$extra)
      new_names <- sapply(current_names, function(n) {
        if (n %in% names(extra_translations)) {
          return(extra_translations[[n]])
        }
        return(n)
      })
      names(translated$extra) <- new_names
    }
    
    return(translated)
  }



  # Enable helper buttons
  # observe_helpers(withMathJax = TRUE)




  # Reactive values for storing results and input data
  results <- reactiveValues(
    com_result = NULL,
    simple_data = NULL,
    detailed_data = NULL,
    input_data = NULL,
    is_multi_value = FALSE,
    selected_simple_data = NULL,
    payback_filter_notified = FALSE
  )

  # Reactive values for auxiliary materials
  aux_materials <- reactiveValues(
    materials = list()  # List of material IDs
  )
  
  # Reactive values for additional responses (up to 3)
  additional_responses <- reactiveValues(
    responses = list()  # List of response IDs
  )

  # Reactive value to store example data and edited preview data
  example_data <- reactiveValues(data = NULL)
  edited_preview_data <- reactiveValues(data = NULL)
  manual_input_data <- reactiveValues(data = NULL)  # Persists across input type changes
  csv_data <- reactiveValues(data = NULL)  # Store CSV data separately

  # Clear edited_preview_data when switching input types to avoid stale data
  # This prevents mismatches between the UI condition check and table render data
  observeEvent(input$input_type, {
    edited_preview_data$data <- NULL
  }, ignoreInit = TRUE)

  # Load CSV data when a new file is uploaded
  observeEvent(input$csv_file, {
    req(input$csv_file)
    tryCatch({
      raw_data <- read.csv(input$csv_file$datapath, header = TRUE, stringsAsFactors = FALSE,
                          na.strings = c("", "NA", "N/A"), strip.white = TRUE)
      # Remove completely empty columns
      csv_data$data <- raw_data[, !sapply(raw_data, function(x) all(is.na(x) | x == ""))]
    }, error = function(e) {
      showNotification(paste(i18n$t("Error loading CSV:"), e$message), type = "error")
      csv_data$data <- NULL
    })
  })

  # Helper function to create range-limited numeric input observer
  # Clamps value to [min_val, max_val] and shows notification if clamped
  create_range_observer <- function(input_id, min_val, max_val, label) {
    observeEvent(input[[input_id]], {
      # Skip range checking for fields disabled in CC-SFE mode
      cc <- (input$comode %||% "sfe") == "cc_sfe"
      if (cc && input_id %in% c("sept", "extime", "id", "bh")) return()
      
      val <- input[[input_id]]
      if (is.null(val) || is.na(val)) return()
      
      clamped <- FALSE
      new_val <- val
      
      if (val < min_val) {
        new_val <- min_val
        clamped <- TRUE
      } else if (val > max_val) {
        new_val <- max_val
        clamped <- TRUE
      }
      
      if (clamped) {
        updateNumericInput(session, input_id, value = new_val)
        showNotification(
          sprintf(i18n$t("%s must be between %s and %s"), label, min_val, max_val),
          type = "warning",
          duration = 3
        )
      }
    }, ignoreInit = TRUE)
  }
  
  # Range-limited inputs for General Parameters
  create_range_observer("pres", 1, 1000, i18n$t("Pressure"))
  create_range_observer("temp", -50, 300, i18n$t("Temperature"))
  
  # Range-limited inputs for Recovery/Separator
  create_range_observer("recp", 1, 1000, i18n$t("Recovery Pressure"))
  create_range_observer("rect", -50, 300, i18n$t("Recovery Temperature"))
  create_range_observer("sept", -50, 300, i18n$t("Separator Temperature"))
  
  # Range-limited inputs for Flow Measurement Parameters
  create_range_observer("flowpar_pres", 1, 1000, i18n$t("Flow Measurement Pressure"))
  create_range_observer("flowpar_temp", -50, 300, i18n$t("Flow Measurement Temperature"))
  
  # Range-limited inputs for Other Parameters
  create_range_observer("cosol_loss", 0, 1, i18n$t("Co-solvent Loss"))
  create_range_observer("depr", 0, 1, i18n$t("Depreciation Rate (per year)"))
  create_range_observer("taxrate", 0, 100, i18n$t("Tax Rate"))
  
  # Dynamic UI for Pressure input with range info button
  # Dynamic UI for Load / Feed Flow (changes label for CC-SFE)
  # Store values across renderUI re-renders
  stored_load <- reactiveVal(NULL)
  stored_extime <- reactiveVal(NULL)
  stored_id <- reactiveVal(NULL)
  stored_bh <- reactiveVal(NULL)
  stored_sept <- reactiveVal(NULL)
  feed_density <- reactiveVal(1.0)  # Feed density in g/mL for CC-SFE
  main_flow_mass <- reactiveVal(FALSE)  # FALSE = mL/min, TRUE = g/min
  feed_flow_mass <- reactiveVal(FALSE)  # FALSE = mL/min, TRUE = g/min

  # Preserve values before renderUI destroys them
  observe({
    for(pair in list(
      list(input$load, stored_load),
      list(input$extime, stored_extime),
      list(input$id, stored_id),
      list(input$bh, stored_bh),
      list(input$sept, stored_sept)
    )) {
      val <- pair[[1]]
      setter <- pair[[2]]
      if(!is.null(val) && length(val) == 1 && !is.na(val) && !identical(val, "") && is.numeric(val)) {
        setter(val)
      }
    }
  })

  # Toggle observers for flow unit badges
  observeEvent(input$toggle_main_flow, { main_flow_mass(!main_flow_mass()) })
  observeEvent(input$toggle_feed_flow, { feed_flow_mass(!feed_flow_mass()) })

  output$load_input_ui <- renderUI({
    cc <- (input$comode %||% "sfe") == "cc_sfe"
    cur <- stored_load() %||% defaults$load %||% 0
    if(is.na(cur)) cur <- defaults$load %||% 0
    if(cc) {
      feed_unit <- if(feed_flow_mass()) "g/min" else "mL/min"
      tags$div(
        tags$label(
          tags$span(
            paste0(i18n_r()$t("Feed Flow"), " (", feed_unit, ")"),
            input_help(i18n_r()$t("Flow rate of the liquid feed material. In counter-current extraction, both the feed and CO2 flow continuously. Click the unit badge to toggle. Set feed density via the density badge."),
                       title = i18n_r()$t("Feed Flow"), buttonLabel = i18n_r()$t("OK"))),
          class = "control-label",
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          tags$span(style = "display: flex; gap: 4px; margin-left: auto;",
            tags$span(feed_unit,
              style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; cursor: pointer; background-color: ",
                if(feed_flow_mass()) "#17a2b8" else "#28a745", "; color: white; font-weight: normal;"),
              onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', Math.random());", ns("toggle_feed_flow"))),
            tags$span(
              paste0(feed_density(), " g/mL"),
              style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #17a2b8; color: white; font-weight: normal; cursor: pointer;",
              title = i18n_r()$t("Feed density (g/mL). Click to change."),
              onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', Math.random())", ns("change_feed_density")))
          )
        ),
        numericInput(ns("load"), label = NULL, value = cur, min = 0)
      )
    } else {
      numericInput(ns("load"),
        tags$span(i18n_r()$t("Load (kg)"),
          input_help(i18n_r()$t("Mass of raw material loaded into the extractor per cycle, in kilograms."),
                     title = i18n_r()$t("Load"), buttonLabel = i18n_r()$t("OK"))),
        value = cur, min = 0)
    }
  })
  outputOptions(output, "load_input_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for Time (min) - disabled for CC-SFE
  output$single_time_ui <- renderUI({
    cc <- (input$comode %||% "sfe") == "cc_sfe"
    cur <- isolate(input$single_time) %||% defaults$single_time %||% 180
    tagList(
      numericInput(ns("single_time"),
        tags$span(i18n_r()$t("Time (min)"),
          input_help(i18n_r()$t("Extraction time in minutes for the single-point calculation. Not used in CC-SFE cost calculations."),
                     title = i18n_r()$t("Time"), buttonLabel = i18n_r()$t("OK"))),
        value = cur, min = 0),
      if(cc) tags$script(HTML(sprintf("
        (function(){ var el=document.getElementById('%s'); if(el){ el.disabled=true; el.style.backgroundColor='#e9ecef'; } })();
      ", ns("single_time"))))
    )
  })

  # Feed density change dialog
  observeEvent(input$change_feed_density, {
    showModal(modalDialog(
      title = i18n_r()$t("Set Feed Density"),
      numericInput(ns("feed_density_input"), i18n_r()$t("Feed density (g/mL)"),
                   value = feed_density(), min = 0.1, max = 5, step = 0.01),
      footer = tagList(
        modalButton(i18n_r()$t("Cancel")),
        actionButton(ns("apply_feed_density"), i18n_r()$t("Apply"), class = "btn-primary")
      ),
      size = "s"
    ))
  })
  observeEvent(input$apply_feed_density, {
    val <- input$feed_density_input
    if(!is.null(val) && !is.na(val) && val > 0) {
      feed_density(val)
      removeModal()
    }
  })

  # Dynamic UI for Main Solvent Flow
  output$flow_input_ui <- renderUI({
    cur <- isolate(input$flow) %||% defaults$flow
    if(is.na(cur)) cur <- defaults$flow %||% 0
    flow_unit <- if(main_flow_mass()) "g/min" else "mL/min"
    tags$div(
      tags$label(
        tags$span(
          paste0(i18n_r()$t("Main Solvent Flow"), " (", flow_unit, ")"),
          input_help(i18n_r()$t("Flow rate of the main solvent (CO2 for SFE/CC-SFE, H2O for SWE). Click the unit badge to toggle between mL/min and g/min."),
                     title = i18n_r()$t("Solvent Flow"), buttonLabel = i18n_r()$t("OK"))),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(flow_unit,
          style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; cursor: pointer; background-color: ",
            if(main_flow_mass()) "#17a2b8" else "#28a745", "; color: white; margin-left: auto; font-weight: normal;"),
          onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('%s', Math.random());", ns("toggle_main_flow")))
      ),
      numericInput(ns("flow"), label = NULL, value = cur, min = 0)
    )
  })
  outputOptions(output, "flow_input_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for Cycle Overhead Time (disabled for CC-SFE)
  output$extime_input_ui <- renderUI({
    cc <- (input$comode %||% "sfe") == "cc_sfe"
    cur <- stored_extime() %||% defaults$extime %||% 0
    if(is.na(cur)) cur <- defaults$extime %||% 0
    tagList(
      if(cc) {
        tagList(
          numericInput(ns("extime"),
            tags$span(i18n_r()$t("Cycle Overhead Time (min)"),
              input_help(i18n_r()$t("Extra time per cycle for loading, unloading, pressurization and depressurization. Not applicable for CC-SFE."),
                         title = i18n_r()$t("Overhead Time"), buttonLabel = i18n_r()$t("OK"))),
            value = cur, min = 0),
          tags$script(HTML(sprintf("
        (function(){ var el=document.getElementById('%s'); if(el){ el.disabled=true; el.style.backgroundColor='#e9ecef'; el.dataset.savedValue=el.value; el.value=''; el.placeholder='%s'; el.type='text'; } })();
      ", ns("extime"), i18n_r()$t("Not needed for CC-SFE"))))
        )
      } else {
        numericInput(ns("extime"),
          tags$span(i18n_r()$t("Cycle Overhead Time (min)"),
            input_help(i18n_r()$t("Extra time per cycle for loading, unloading, pressurization and depressurization. Not applicable for CC-SFE."),
                       title = i18n_r()$t("Overhead Time"), buttonLabel = i18n_r()$t("OK"))),
          value = cur, min = 0)
      }
    )
  })
  outputOptions(output, "extime_input_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for Inner Extractor Diameter (disabled for CC-SFE)
  output$id_input_ui <- renderUI({
    cc <- (input$comode %||% "sfe") == "cc_sfe"
    cur <- stored_id() %||% defaults$id %||% 0
    if(is.na(cur)) cur <- defaults$id %||% 0
    tagList(
      if(cc) {
        tagList(
          numericInput(ns("id"),
            tags$span(i18n_r()$t("Inner Extractor Diameter (cm)"),
              input_help(i18n_r()$t("Inner diameter of the extraction vessel in centimeters. Not applicable for CC-SFE."),
                         title = i18n_r()$t("Extractor Diameter"), buttonLabel = i18n_r()$t("OK"))),
            value = cur, min = 0),
          tags$script(HTML(sprintf("
        (function(){ var el=document.getElementById('%s'); if(el){ el.disabled=true; el.style.backgroundColor='#e9ecef'; el.dataset.savedValue=el.value; el.value=''; el.placeholder='%s'; el.type='text'; } })();
      ", ns("id"), i18n_r()$t("Not needed for CC-SFE"))))
        )
      } else {
        numericInput(ns("id"),
          tags$span(i18n_r()$t("Inner Extractor Diameter (cm)"),
            input_help(i18n_r()$t("Inner diameter of the extraction vessel in centimeters. Used with vessel volume to calculate the bed height."),
                       title = i18n_r()$t("Extractor Diameter"), buttonLabel = i18n_r()$t("OK"))),
          value = cur, min = 0)
      }
    )
  })
  outputOptions(output, "id_input_ui", suspendWhenHidden = FALSE)

  output$pres_input_ui <- renderUI({
    current_value <- if (is.null(input$pres)) defaults$pres else input$pres
    
    tags$div(
      tags$label(
        i18n$t("Pressure (bar)"),
        input_help(i18n_r()$t("Operating pressure of the supercritical extraction vessel. Used to calculate CO2 density and equipment cost scaling."),
                   title = i18n_r()$t("Pressure"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "1–1000",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: 1 to 1000 bar")
        )
      ),
      numericInput(ns("pres"), NULL, value = current_value, min = 1, max = 1000)
    )
  })
  outputOptions(output, "pres_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Temperature input with range info button
  output$temp_input_ui <- renderUI({
    current_value <- if (is.null(input$temp)) defaults$temp else input$temp
    
    tags$div(
      tags$label(
        i18n$t("Temperature (°C)"),
        input_help(i18n_r()$t("Operating temperature of the extraction vessel. Used alongside pressure to calculate solvent density."),
                   title = i18n_r()$t("Temperature"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "-50–300",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: -50 to 300 °C")
        )
      ),
      numericInput(ns("temp"), NULL, value = current_value, min = -50, max = 300)
    )
  })
  outputOptions(output, "temp_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Recovery Pressure input with range info
    output$recp_input_ui <- renderUI({
    current_value <- if (is.null(input$recp)) defaults$recp else input$recp
    
    tags$div(
      tags$label(
        i18n$t("Recovery Pressure (bar)"),
        input_help(i18n_r()$t("Pressure in the separator vessel. Used for flow measurement calibration in auto mode."),
                   title = i18n_r()$t("Recovery Pressure"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("1–1000",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: 1 to 1000 bar"))
      ),
      numericInput(ns("recp"), NULL, value = current_value, min = 1, max = 1000)
    )
  })
  outputOptions(output, "recp_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Recovery Temperature input with range info
    output$rect_input_ui <- renderUI({
    current_value <- if (is.null(input$rect)) defaults$rect else input$rect
    
    tags$div(
      tags$label(
        i18n$t("Recovery Temperature (°C)"),
        input_help(i18n_r()$t("Temperature in the separator vessel. Used for flow measurement calibration in auto mode."),
                   title = i18n_r()$t("Recovery Temperature"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("-50–300",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: -50 to 300 °C"))
      ),
      numericInput(ns("rect"), NULL, value = current_value, min = -50, max = 300)
    )
  })
  outputOptions(output, "rect_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Separator Temperature input with range info
  output$sept_input_ui <- renderUI({
    cc <- (input$comode %||% "sfe") == "cc_sfe"
    current_value <- stored_sept() %||% defaults$sept
    
    tagList(
      numericInput(ns("sept"),
        tags$span(i18n$t("Separator Temperature (°C)"),
          input_help(i18n_r()$t("Temperature of the CO2 after separation, before recompression. Not applicable for CC-SFE."),
                     title = i18n_r()$t("Separation Temperature"), buttonLabel = i18n_r()$t("OK"))),
        value = current_value, min = -50, max = 300),
      if(cc) tags$script(HTML(sprintf("
        (function(){ var el=document.getElementById('%s'); if(el){ el.disabled=true; el.style.backgroundColor='#e9ecef'; el.dataset.savedValue=el.value; el.value=''; el.placeholder='%s'; el.type='text'; } })();
      ", ns("sept"), i18n_r()$t("Not needed for CC-SFE"))))
    )
  })
  outputOptions(output, "sept_input_ui", suspendWhenHidden = FALSE)
  
  output$comode_ui <- renderUI({
    input_mode <- input$input_type %||% "single"

    # CC-SFE is only available in Single Value mode
    if(input_mode == "single") {
      choice_names <- list(
        tags$span(tags$strong("SFE"), " \u2014 ", i18n_r()$t("Supercritical Fluid Extraction (CO\u2082 as main solvent)")),
        tags$span(tags$strong("CC-SFE"), " \u2014 ", i18n_r()$t("Counter-Current CO\u2082 Extraction (liquid feed)")),
        tags$span(tags$strong("SWE"), " \u2014 ", i18n_r()$t("Subcritical Water Extraction (H\u2082O as main solvent)"))
      )
      choice_values <- c("sfe", "cc_sfe", "swe")
    } else {
      choice_names <- list(
        tags$span(tags$strong("SFE"), " \u2014 ", i18n_r()$t("Supercritical Fluid Extraction (CO\u2082 as main solvent)")),
        tags$span(tags$strong("SWE"), " \u2014 ", i18n_r()$t("Subcritical Water Extraction (H\u2082O as main solvent)"))
      )
      choice_values <- c("sfe", "swe")
    }

    # If CC-SFE was selected but we're no longer in single mode, fall back to SFE
    current <- isolate(input$comode) %||% defaults$comode
    if(!current %in% choice_values) current <- "sfe"

    tagList(
      tags$label(
        i18n_r()$t("Process Type"),
        input_help(i18n_r()$t("Choose the extraction process type. SFE uses supercritical CO2 as the main solvent with optional ethanol as co-solvent. CC-SFE is counter-current supercritical CO2 extraction for liquid feeds. SWE uses subcritical water as the main solvent with optional CO2 as co-solvent."),
                   title = i18n_r()$t("Process Type"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label", style = "font-weight: bold;"
      ),
      radioButtons(ns("comode"), NULL,
        choiceNames = choice_names,
        choiceValues = choice_values,
        selected = current
      ),
      tags$small(
        class = "text-muted",
        style = "display: block; margin-top: -10px; margin-bottom: 15px;",
        i18n_r()$t("Affects solvent density calculations and co-solvent options (EtOH for SFE/CC-SFE, CO\u2082 for SWE).")
      )
    )
  })
  outputOptions(output, "comode_ui", suspendWhenHidden = FALSE)

  # Notify user when CC-SFE is forced off by switching away from Single Value mode
  observeEvent(input$input_type, {
    if((input$input_type %||% "single") != "single" && (input$comode %||% "sfe") == "cc_sfe") {
      showNotification(
        i18n$t("CC-SFE mode is only available in Single Value mode. Process type has been switched to SFE."),
        type = "warning", duration = 6
      )
    }
  }, ignoreInit = TRUE)

  # CC-SFE mode changes are handled by renderUIs that read input$comode directly.

  output$use_coefs_ui <- renderUI({
    tags$span(
      checkboxInput(ns("use_coefs"), i18n_r()$t("Use Turton et al. (1998) coefficients"),
                    value = isolate(input$use_coefs) %||% defaults$use_coefs),
      title = i18n_r()$t("When enabled, the equipment purchase cost (CAPEX) is estimated automatically from vessel dimensions and operating pressure using the Turton et al. (1998) pressure vessel correlation.")
    )
  })
  outputOptions(output, "use_coefs_ui", suspendWhenHidden = FALSE)

  output$flowpar_auto_ui <- renderUI({
    tags$span(
      checkboxInput(ns("flowpar_auto"), i18n_r()$t("Auto (use recovery line conditions)"),
                    value = isTRUE(isolate(input$flowpar_auto) %||% defaults$flowpar_auto)),
      title = i18n_r()$t("When enabled, the flow measurement pressure and temperature are automatically set to the recovery line conditions.")
    )
  })
  outputOptions(output, "flowpar_auto_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for Flow Measurement Pressure input with range info
    output$flowpar_pres_input_ui <- renderUI({
    current_value <- if (is.null(input$flowpar_pres)) defaults$flowpar_pres else input$flowpar_pres
    
    tags$div(
      tags$label(
        i18n$t("Pressure (bar)"),
        input_help(i18n_r()$t("Pressure at which the CO2 flow meter was calibrated. Used to convert volumetric flow to mass flow."),
                   title = i18n_r()$t("Flow Meas. Pressure"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("1–1000",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: 1 to 1000 bar"))
      ),
      numericInput(ns("flowpar_pres"), NULL, value = current_value, min = 1, max = 1000)
    )
  })
  outputOptions(output, "flowpar_pres_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Flow Measurement Temperature input with range info
    output$flowpar_temp_input_ui <- renderUI({
    current_value <- if (is.null(input$flowpar_temp)) defaults$flowpar_temp else input$flowpar_temp
    
    tags$div(
      tags$label(
        i18n$t("Temperature (°C)"),
        input_help(i18n_r()$t("Temperature at which the CO2 flow meter was calibrated. Used to convert volumetric flow to mass flow."),
                   title = i18n_r()$t("Flow Meas. Temperature"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span("-50–300",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: -50 to 300 °C"))
      ),
      numericInput(ns("flowpar_temp"), NULL, value = current_value, min = -50, max = 300)
    )
  })
  outputOptions(output, "flowpar_temp_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Co-solvent Loss input with range info
  output$cosol_loss_input_ui <- renderUI({
    current_value <- if (is.null(input$cosol_loss)) defaults$cosol_loss else input$cosol_loss
    
    tags$div(
      tags$label(
        i18n$t("Co-solvent Loss (fraction)"),
        input_help(i18n_r()$t("Fraction of co-solvent lost per extraction cycle (0 to 1). For example, 0.05 means 5% of co-solvent is not recovered."),
                   title = i18n_r()$t("Co-Solvent Loss"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "0–1",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: 0 to 1")
        )
      ),
      numericInput(ns("cosol_loss"), NULL, value = current_value, min = 0, max = 1, step = 0.01)
    )
  })
  outputOptions(output, "cosol_loss_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Main Solvent Loss Fraction (CC-SFE only)
  output$msol_loss_frac_input_ui <- renderUI({
    cc <- (input$comode %||% "sfe") == "cc_sfe"
    current_value <- if (is.null(input$msol_loss_frac)) (defaults$msol_loss_frac %||% 0.01) else input$msol_loss_frac
    if (is.na(current_value)) current_value <- 0.01
    
    tags$div(
      tags$label(
        tags$span(i18n$t("CO\u2082 Loss Fraction"),
          input_help(i18n_r()$t("Fraction of CO2 lost per recirculation pass in counter-current extraction (0 to 1). Accounts for system leakage, valve losses, and incomplete recovery. Only applicable for CC-SFE mode."),
                     title = i18n_r()$t("CO2 Loss Fraction"), buttonLabel = i18n_r()$t("OK"))),
        class = "control-label",
        style = paste0("display: flex; align-items: center; justify-content: space-between; width: 100%;",
                        if(!cc) " color: #999;" else ""),
        if(cc) tags$span(
          "0\u20131",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: 0 to 1")
        )
      ),
      if(cc) {
        numericInput(ns("msol_loss_frac"), NULL, value = current_value, min = 0, max = 1, step = 0.01)
      } else {
        tagList(
          numericInput(ns("msol_loss_frac"), NULL, value = 0.01, min = 0, max = 1, step = 0.01),
          tags$script(HTML(sprintf("
            (function(){ var el=document.getElementById('%s'); if(el){ el.disabled=true; el.style.backgroundColor='#e9ecef'; el.dataset.savedValue=el.value; el.value=''; el.placeholder='%s'; el.type='text'; } })();
          ", ns("msol_loss_frac"), i18n$t("CC-SFE only"))))
        )
      }
    )
  })
  outputOptions(output, "msol_loss_frac_input_ui", suspendWhenHidden = FALSE)

  # Dynamic UI for Depreciation Rate input with range info
  output$depr_input_ui <- renderUI({
    current_value <- if (is.null(input$depr)) defaults$depr else input$depr
    
    tags$div(
      tags$label(
        i18n$t("Depreciation Rate (per year)"),
        input_help(i18n_r()$t("Annual depreciation rate expressed as a fraction of the total capital expenditure (CAPEX). For example, 0.1 means 10% of CAPEX is depreciated per year."),
                   title = i18n_r()$t("Depreciation Rate"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "0–1",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: 0 to 1")
        )
      ),
      numericInput(ns("depr"), NULL, value = current_value, min = 0, max = 1, step = 0.01)
    )
  })
  outputOptions(output, "depr_input_ui", suspendWhenHidden = FALSE)
  
  # Dynamic UI for Tax Rate input with range info
  output$taxrate_input_ui <- renderUI({
    current_value <- if (is.null(input$taxrate)) defaults$taxrate else input$taxrate
    
    tags$div(
      tags$label(
        i18n$t("Tax Rate (%)"),
        input_help(i18n_r()$t("Corporate income tax rate as a percentage. Applied to profits when calculating the payback period."),
                   title = i18n_r()$t("Tax Rate"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "0–100",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = i18n$t("Valid range: 0 to 100%")
        )
      ),
      numericInput(ns("taxrate"), NULL, value = current_value, min = 0, max = 100, step = 1)
    )
  })
  outputOptions(output, "taxrate_input_ui", suspendWhenHidden = FALSE)
  
  # Reactive to calculate theoretical bed height from extractor volume and inner diameter
  # Cylinder formula: V = π * r² * h, so h = V / (π * r²) = 4V / (π * d²)
  # V in liters (1L = 1000 cm³), d in cm, result h in cm
  theoretical_bed_height <- reactive({
    volex <- input$volex
    id <- input$id
    
    if (is.null(volex) || is.null(id) || is.na(volex) || is.na(id) || volex <= 0 || id <= 0) {
      return(NULL)
    }
    
    # Convert volume from L to cm³
    vol_cm3 <- volex * 1000
    
    # Calculate height: h = 4V / (π * d²)
    height <- (4 * vol_cm3) / (pi * id^2)
    
    round(height, 1)
  })
  
  # Dynamic UI for Bed Height input with calculated range badge
  output$bh_input_ui <- renderUI({
    cc <- (input$comode %||% "sfe") == "cc_sfe"
    current_value <- stored_bh() %||% defaults$bh
    theo_height <- theoretical_bed_height()
    
    if(cc) {
      tagList(
        numericInput(ns("bh"),
          tags$span(i18n$t("Bed Height (cm)"),
            input_help(i18n_r()$t("Height of the packed material bed inside the extractor. Not applicable for CC-SFE."),
                       title = i18n_r()$t("Bed Height"), buttonLabel = i18n_r()$t("OK"))),
          value = current_value, min = 0),
        tags$script(HTML(sprintf("
          (function(){ var el=document.getElementById('%s'); if(el){ el.disabled=true; el.style.backgroundColor='#e9ecef'; el.dataset.savedValue=el.value; el.value=''; el.placeholder='%s'; el.type='text'; } })();
        ", ns("bh"), i18n_r()$t("Not needed for CC-SFE"))))
      )
    } else {
      # Range badge
      if (!is.null(theo_height) && theo_height > 0) {
        range_text <- paste0("0\u2013", theo_height)
        range_title <- sprintf(i18n$t("Theoretical max: %s cm (based on extractor volume and diameter)"), theo_height)
        badge_color <- if (!is.null(current_value) && !is.na(current_value) && current_value > theo_height) "#ffc107" else "#6c757d"
      } else {
        range_text <- "\u2013"
        range_title <- i18n$t("Set extractor volume and inner diameter to see valid range")
        badge_color <- "#6c757d"
      }
      tags$div(
        tags$label(
          tags$span(i18n$t("Bed Height (cm)"),
            input_help(i18n_r()$t("Height of the packed material bed inside the extractor. Calculated automatically from vessel volume and diameter, or enter manually."),
                       title = i18n_r()$t("Bed Height"), buttonLabel = i18n_r()$t("OK"))),
          class = "control-label",
          style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          tags$span(range_text,
            style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ", badge_color, "; color: white; margin-left: auto; font-weight: normal;"),
            title = range_title)
        ),
        numericInput(ns("bh"), NULL, value = current_value, min = 0)
      )
    }
  })
  outputOptions(output, "bh_input_ui", suspendWhenHidden = FALSE)
  
  # Soft warning observer for bed height - warns but doesn't prevent
  observeEvent(input$bh, {
    bh <- input$bh
    theo_height <- theoretical_bed_height()
    
    if (is.null(bh) || is.na(bh) || is.null(theo_height)) return()
    
    if (bh > theo_height) {
      showNotification(
        sprintf(i18n$t("Bed height (%s cm) exceeds theoretical maximum (%s cm) based on extractor geometry"), bh, theo_height),
        type = "warning",
        duration = 5
      )
    }
  }, ignoreInit = TRUE)

  # Render dynamic auxiliary materials UI
  output$aux_materials_ui <- renderUI({
    material_ids <- aux_materials$materials
    
    if (length(material_ids) == 0) {
      return(tags$p(i18n$t("No auxiliary materials added."), style = "color: #888; font-style: italic; margin: 5px 0;"))
    }
    
    lapply(material_ids, function(mat_id) {
      # Get current values if they exist, otherwise use defaults
      current_name <- isolate(input[[paste0("aux_name_", mat_id)]]) %||% paste0(i18n$t("Material"), " ", mat_id)
      current_price <- isolate(input[[paste0("aux_price_", mat_id)]]) %||% 1
      current_fraction <- isolate(input[[paste0("aux_fraction_", mat_id)]]) %||% 0.1
      
      div(
        id = ns(paste0("aux_mat_row_", mat_id)),
        style = "background-color: #f9f9f9; padding: 8px 10px; margin-bottom: 8px; border-radius: 5px; border: 1px solid #ddd;",
        fluidRow(
          style = "margin-left: -8px; margin-right: -8px;",
          column(4, style = "padding-left: 8px; padding-right: 4px;", 
            textInput(ns(paste0("aux_name_", mat_id)),
              tags$span(i18n$t("Material Name"),
                if (mat_id == material_ids[1]) input_help(i18n_r()$t("Name of the auxiliary material (e.g. filter paper, adsorbent, packing material)."),
                           title = i18n_r()$t("Material"), buttonLabel = i18n_r()$t("OK"))),
              value = current_name)),
          column(3, style = "padding-left: 4px; padding-right: 4px;",
            numericInput(ns(paste0("aux_price_", mat_id)),
              tags$span(i18n$t("Price (USD/kg)"),
                if (mat_id == material_ids[1]) input_help(i18n_r()$t("Purchase price of the auxiliary material in US dollars per kilogram."),
                           title = i18n_r()$t("Aux. Price"), buttonLabel = i18n_r()$t("OK"))),
              value = current_price, min = 0, step = 0.1)),
          column(4, style = "padding-left: 4px; padding-right: 4px;",
            numericInput(ns(paste0("aux_fraction_", mat_id)),
              tags$span(i18n$t("Fraction (per kg extract)"),
                if (mat_id == material_ids[1]) input_help(i18n_r()$t("Amount of this auxiliary material consumed per kilogram of extract produced (kg auxiliary / kg extract). For example, if you use 0.5 kg of filter paper to produce 1 kg of extract, enter 0.5. This value is multiplied by the extract yield and the material price to calculate the auxiliary material cost per cycle."),
                           title = i18n_r()$t("Usage Fraction"), buttonLabel = i18n_r()$t("OK"))),
              value = current_fraction, min = 0, max = 10, step = 0.01)),
          column(1, style = "padding-left: 4px; padding-right: 8px;",
            div(style = "margin-top: 25px;",
              actionButton(ns(paste0("remove_aux_", mat_id)), "", icon = icon("trash"), class = "btn-danger btn-sm")
            )
          )
        )
      )
    })
  })

  # Add auxiliary material
  observeEvent(input$add_aux_material, {
    # Find the next available ID (smallest positive integer not in use)
    existing_ids <- unlist(aux_materials$materials)
    if (length(existing_ids) == 0) {
      new_id <- 1
    } else {
      # Find the first gap or use max + 1
      all_possible <- seq_len(max(existing_ids) + 1)
      available <- setdiff(all_possible, existing_ids)
      new_id <- min(available)
    }
    aux_materials$materials <- c(aux_materials$materials, new_id)
  })

  # Remove auxiliary material - observe all remove buttons
 observe({
    material_ids <- aux_materials$materials
    lapply(material_ids, function(mat_id) {
      observeEvent(input[[paste0("remove_aux_", mat_id)]], {
        aux_materials$materials <- aux_materials$materials[aux_materials$materials != mat_id]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ============================================================
  # ADDITIONAL RESPONSES UI AND EVENT HANDLERS
  # ============================================================
  
  # Helper function to get all available columns (without exclusions - used for dropdown choices)
  get_all_data_columns <- reactive({
    cols <- c()
    
    if (input$input_type == "csv" && !is.null(csv_data$data)) {
      cols <- names(csv_data$data)
    } else if (input$input_type == "text" && !is.null(edited_preview_data$data)) {
      cols <- names(edited_preview_data$data)
    } else if (input$input_type != "single" && !is.null(example_data$data)) {
      cols <- names(example_data$data)
    }
    
    return(cols)
  })
  
  # Render additional responses UI
  output$additional_responses_ui <- renderUI({
    response_ids <- additional_responses$responses
    
    if (length(response_ids) == 0) {
      return(tags$p(i18n$t("No additional responses added."), style = "color: #888; font-style: italic; margin: 5px 0;"))
    }
    
    # Get all columns for selection
    all_cols <- get_all_data_columns()
    is_single_mode <- input$input_type == "single"
    
    # Exclude time column
    time_col <- input$time_column
    if (!is.null(time_col) && time_col != "" && time_col %in% all_cols) {
      all_cols <- setdiff(all_cols, time_col)
    }
    
    # Exclude primary yield column
    yield_col <- input$yield_column
    if (!is.null(yield_col) && yield_col != "" && yield_col %in% all_cols) {
      all_cols <- setdiff(all_cols, yield_col)
    }
    
    lapply(response_ids, function(resp_id) {
      # For this response's dropdown, exclude columns used by OTHER additional responses
      available_cols <- all_cols
      for (other_id in response_ids) {
        if (other_id != resp_id) {
          used_col <- input[[paste0("add_resp_column_", other_id)]]
          if (!is.null(used_col) && used_col != "" && used_col %in% available_cols) {
            available_cols <- setdiff(available_cols, used_col)
          }
        }
      }
      
      # Get current selection if it exists
      current_col_selection <- isolate(input[[paste0("add_resp_column_", resp_id)]])
      # Include current selection in available_cols if it's valid (so it can remain selected)
      if (!is.null(current_col_selection) && current_col_selection != "" && 
          current_col_selection %in% get_all_data_columns()) {
        available_cols <- union(available_cols, current_col_selection)
      }
      
      selected_col <- if (!is.null(current_col_selection) && current_col_selection %in% available_cols) {
        current_col_selection
      } else {
        ""  # Start with no selection
      }
      
      # Get current values for all inputs, use defaults if not set
      current_name <- isolate(input[[paste0("add_resp_name_", resp_id)]]) %||% paste0(i18n$t("Response"), " ", resp_id + 1)
      current_unit <- isolate(input[[paste0("add_resp_unit_", resp_id)]]) %||% "percent"
      current_dilfac <- isolate(input[[paste0("add_resp_dilfac_", resp_id)]]) %||% 1
      current_pr_sale <- isolate(input[[paste0("add_resp_pr_sale_", resp_id)]]) %||% 50
      current_value <- isolate(input[[paste0("add_resp_value_", resp_id)]]) %||% 0
      
      div(
        id = ns(paste0("add_resp_row_", resp_id)),
        style = "background-color: #f0f8ff; padding: 10px; margin-bottom: 10px; border-radius: 5px; border: 1px solid #b8d4e8;",
        fluidRow(
          style = "margin: 0;",
          column(12, 
            div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
              tags$label(paste0(i18n$t("Additional Response"), " ", resp_id), style = "font-weight: bold; margin: 0;"),
              actionButton(ns(paste0("remove_add_resp_", resp_id)), "", icon = icon("times"), 
                          class = "btn-danger btn-xs", style = "padding: 2px 6px;")
            )
          )
        ),
        fluidRow(
          style = "margin: 0 -5px;",
          # Column selector for CSV/text mode (serves as name), OR name input for single mode
          if (is_single_mode) {
            column(4, style = "padding: 0 5px;",
              textInput(ns(paste0("add_resp_name_", resp_id)),
              tags$span(i18n$t("Response Name"),
                if (resp_id == response_ids[1]) input_help(i18n_r()$t("Descriptive name for this additional response. Shown in results tables."),
                           title = i18n_r()$t("Response Name"), buttonLabel = i18n_r()$t("OK"))), 
                       value = current_name)
            )
          } else {
            column(4, style = "padding: 0 5px;",
              selectInput(ns(paste0("add_resp_column_", resp_id)),
              tags$span(i18n$t("Response Column"),
                if (resp_id == response_ids[1]) input_help(i18n_r()$t("Select the data column for this additional response. Each additional response contributes to combined revenue."),
                           title = i18n_r()$t("Response Column"), buttonLabel = i18n_r()$t("OK"))),
                choices = c(setNames("", i18n$t("Select...")), setNames(available_cols, available_cols)),
                selected = selected_col
              )
            )
          },
          column(4, style = "padding: 0 5px;",
            selectInput(ns(paste0("add_resp_unit_", resp_id)),
              tags$span(i18n$t("Response Unit"),
                if (resp_id == response_ids[1]) input_help(i18n_r()$t("Unit of this additional response. Same options as the primary response."),
                           title = i18n_r()$t("Unit"), buttonLabel = i18n_r()$t("OK"))),
              choices = c("%" = "percent", "g" = "g", "kg" = "kg"),
              selected = current_unit
            )
          ),
          column(4, style = "padding: 0 5px;",
            numericInput(ns(paste0("add_resp_dilfac_", resp_id)),
              tags$span(i18n$t("Dilution Factor"),
                if (resp_id == response_ids[1]) input_help(i18n_r()$t("Factor by which the extract was diluted during analysis. The measured response is multiplied by this factor to obtain the true yield."),
                           title = i18n_r()$t("Dilution Factor"), buttonLabel = i18n_r()$t("OK"))), 
                        value = current_dilfac, min = 0, step = 0.1)
          )
        ),
        fluidRow(
          style = "margin: 0 -5px;",
          column(4, style = "padding: 0 5px;",
            numericInput(ns(paste0("add_resp_pr_sale_", resp_id)),
              tags$span(i18n$t("Sales Price (USD/kg)"),
                if (resp_id == response_ids[1]) input_help(i18n_r()$t("Selling price of this extract in US dollars per kilogram. Used to calculate revenue from this additional response."),
                           title = i18n_r()$t("Sales Price"), buttonLabel = i18n_r()$t("OK"))), 
                        value = current_pr_sale, min = 0, step = 1)
          ),
          # Value input for single mode only
          if (is_single_mode) {
            column(4, style = "padding: 0 5px;",
              numericInput(ns(paste0("add_resp_value_", resp_id)),
                tags$span(i18n$t("Response Value"),
                  if (resp_id == response_ids[1]) input_help(i18n_r()$t("Measured yield or response value for this additional response at the same extraction time as the primary response."),
                             title = i18n_r()$t("Response Value"), buttonLabel = i18n_r()$t("OK"))),
                          value = current_value, min = 0, step = 0.01)
            )
          }
        )
      )
    })
  })
  
  # Render add response button (disabled if 3 responses already added)
  output$add_response_button_ui <- renderUI({
    n_responses <- length(additional_responses$responses)
    
    if (n_responses >= 3) {
      return(tags$p(
        i18n$t("Maximum of 3 additional responses reached."), 
        style = "color: #888; font-style: italic; margin: 5px 0;"
      ))
    }
    
    actionButton(
      ns("add_response"), 
      i18n$t("Add Response"), 
      icon = icon("plus"), 
      class = "btn-success btn-sm", 
      style = "margin-bottom: 10px;"
    )
  })
  
  # Add additional response
  observeEvent(input$add_response, {
    if (length(additional_responses$responses) >= 3) {
      showNotification(i18n$t("Maximum of 3 additional responses allowed."), type = "warning")
      return()
    }
    
    # Find the next available ID
    existing_ids <- unlist(additional_responses$responses)
    if (length(existing_ids) == 0) {
      new_id <- 1
    } else {
      all_possible <- seq_len(max(existing_ids) + 1)
      available <- setdiff(all_possible, existing_ids)
      new_id <- min(available)
    }
    additional_responses$responses <- c(additional_responses$responses, new_id)
  })
  
  # Remove additional response - observe all remove buttons
  observe({
    response_ids <- additional_responses$responses
    lapply(response_ids, function(resp_id) {
      observeEvent(input[[paste0("remove_add_resp_", resp_id)]], {
        additional_responses$responses <- additional_responses$responses[additional_responses$responses != resp_id]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  # ============================================================
  # MOISTURE CONTENT DYNAMIC UI (enabled only when mass units used)
  # ============================================================
  
  # Check if any response uses mass units (g or kg)
  any_mass_units <- reactive({
    # Check primary response (handle both single and csv mode input names)
    primary_unit <- if (input$input_type == "single") {
      input$response_unit_single
    } else {
      input$response_unit
    }
    if (!is.null(primary_unit) && primary_unit %in% c("g", "kg")) {
      return(TRUE)
    }
    
    # Check additional responses
    response_ids <- additional_responses$responses
    for (resp_id in response_ids) {
      unit <- input[[paste0("add_resp_unit_", resp_id)]]
      if (!is.null(unit) && unit %in% c("g", "kg")) {
        return(TRUE)
      }
    }
    
    return(FALSE)
  })
  
  # Render moisture content input (greyed out unless mass units are used)
  output$moisture_content_input_ui <- renderUI({
    enabled <- any_mass_units()
    
    tags$div(
      tags$label(
        tags$span(i18n$t("Raw Material Moisture (%)"),
          input_help(i18n_r()$t("Moisture content of the raw material as a percentage. Used to calculate dry mass for yield and drying costs. Only active when at least one response uses mass units."),
                     title = i18n_r()$t("Moisture"), buttonLabel = i18n_r()$t("OK"))),
        class = "control-label",
        style = paste0(
          "display: flex; align-items: center; justify-content: space-between; width: 100%;",
          if (!enabled) " color: #999;" else ""
        ),
        if (enabled) {
          tags$span(
            "0\u2013100",
            style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
            title = i18n$t("Valid range: 0 to 100%")
          )
        }
      ),
      numericInput(
        ns("moisture_content"), 
        NULL, 
        value = if (!is.null(input$moisture_content)) input$moisture_content else (defaults$moisture_content %||% 0), 
        min = 0, 
        max = 100,
        width = "100%"
      ),
      if (!enabled) {
        tags$script(HTML(sprintf("
          (function() {
            var el = document.getElementById('%s');
            el.dataset.savedValue = el.value;
            el.value = '';
            el.disabled = true;
            el.style.backgroundColor = '#e9ecef';
            el.placeholder = '%s';
            el.type = 'text';
          })();
        ", ns("moisture_content"), i18n$t("Requires mass unit"))))
      } else {
        tags$script(HTML(sprintf("
          (function() {
            var el = document.getElementById('%s');
            if (el.type === 'text') el.type = 'number';
            el.disabled = false;
            el.style.backgroundColor = '';
            el.placeholder = '';
            if (el.dataset.savedValue && !el.value) {
              el.value = el.dataset.savedValue;
              $(el).trigger('change');
            }
          })();
        ", ns("moisture_content"))))
      }
    )
  })
  outputOptions(output, "moisture_content_input_ui", suspendWhenHidden = FALSE)

  output$com_analysis_HELP <- renderUI({
    create_help_modal(i18n_r, "com_analysis_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "param_accordion")
  })

  # Render dynamic COM equation display based on use_coefs checkbox
  output$com_equation_display <- renderUI({
    use_coefs <- if (is.null(input$use_coefs)) FALSE else input$use_coefs
    
    if (use_coefs) {
      # Turton et al. (1998) equation with coefficients
      div(
        class = "well well-sm",
        style = "background-color: #f8f9fa; border: 1px solid #dee2e6; margin-top: 10px; padding: 15px;",
        tags$div(
          style = "text-align: center;",
          withMathJax(),
          helpText("$$COM = 0.304 \\times FMC + 2.73 \\times COL + 1.23 \\times (CRM + CUT)$$")
        ),
        tags$small(
          class = "text-muted",
          style = "display: block; margin-top: 5px; text-align: center;",
          i18n$t("Turton et al. (1998) formula with empirical coefficients")
        )
      )
    } else {
      # Custom coefficients with inline inputs
      # Get current values or defaults (with explicit fallback to 1)
      coef_fci <- if (!is.null(input$coef_fci)) as.numeric(input$coef_fci) else if (!is.null(defaults$coef_fci)) defaults$coef_fci else 1
      coef_col <- if (!is.null(input$coef_col)) as.numeric(input$coef_col) else if (!is.null(defaults$coef_col)) defaults$coef_col else 1
      coef_crm <- if (!is.null(input$coef_crm)) as.numeric(input$coef_crm) else if (!is.null(defaults$coef_crm)) defaults$coef_crm else 1
      coef_cut <- if (!is.null(input$coef_cut)) as.numeric(input$coef_cut) else if (!is.null(defaults$coef_cut)) defaults$coef_cut else 1
      
      # Inline input style
      input_style <- "width: 55px; height: 26px; padding: 2px 5px; text-align: center; font-size: 13px; display: inline-block; margin: 0 2px; border: 1px solid #ced4da; border-radius: 4px;"
      
      div(
        class = "well well-sm",
        style = "background-color: #f8f9fa; border: 1px solid #dee2e6; margin-top: 10px; padding: 15px;",
        tags$div(
          style = "text-align: center; font-size: 16px; line-height: 2.2;",
          tags$span("COM = ", style = "font-style: italic;"),
          tags$input(type = "number", id = ns("coef_fci"), value = coef_fci, step = 0.01, min = 0, style = input_style),
          tags$span(" × FMC + ", style = "font-style: italic;"),
          tags$input(type = "number", id = ns("coef_col"), value = coef_col, step = 0.01, min = 0, style = input_style),
          tags$span(" × COL + ", style = "font-style: italic;"),
          tags$input(type = "number", id = ns("coef_crm"), value = coef_crm, step = 0.01, min = 0, style = input_style),
          tags$span(" × CRM + ", style = "font-style: italic;"),
          tags$input(type = "number", id = ns("coef_cut"), value = coef_cut, step = 0.01, min = 0, style = input_style),
          tags$span(" × CUT", style = "font-style: italic;")
        ),
        tags$small(
          class = "text-muted",
          style = "display: block; margin-top: 8px; text-align: center;",
          i18n$t("Custom coefficients (default: 1 for simple sum)")
        )
      )
    }
  })

  # Reactive to check if export all results button should be visible
  output$show_export_all_results <- reactive({
    !is.null(results$com_result)
  })
  outputOptions(output, "show_export_all_results", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
 observe({
    has_results <- !is.null(results$com_result)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
    }
  })

  # -- Settings now handled by global settings system -------------------------
  # (Settings modal removed - use global settings button in header)

  # Helper function to convert text input to numeric or NA
  to_numeric_or_na <- function(x) {
    if (is.null(x) || x == "" || is.na(x)) {
      return(NA_real_)
    } else {
      return(as.numeric(x))
    }
  }

  # Reactive for base data (from file upload, example, or default empty table for manual)
  base_com_data <- reactive({
    req(input$input_type)
    data <- NULL

    if (input$input_type == "csv") {
      if (!is.null(csv_data$data)) {
        # Use stored CSV data
        data <- csv_data$data
      } else if (!is.null(example_data$data)) {
        # Example data loaded via button
        data <- example_data$data
      }
      # If no file uploaded and no example data, return NULL (placeholder will be shown)
    } else if (input$input_type == "text") {
      # Manual input mode - use persistent manual data, then default empty table
      # NOTE: Do NOT fall back to example_data$data - that belongs to CSV mode
      if (!is.null(manual_input_data$data)) {
        data <- manual_input_data$data
      } else {
        # Default empty 8-column data for manual input (6 rows)
        # Extra columns allow pasting data with more columns without crashing
        data <- data.frame(
          time = rep(NA_real_, 6),
          yield = rep(NA_real_, 6),
          Col3 = rep(NA_real_, 6),
          Col4 = rep(NA_real_, 6),
          Col5 = rep(NA_real_, 6),
          Col6 = rep(NA_real_, 6),
          Col7 = rep(NA_real_, 6),
          Col8 = rep(NA_real_, 6),
          stringsAsFactors = FALSE
        )
      }
    }
    data
  })

  # Reactive for display data (includes NA rows for preview table)
  # This is used for calculations, not for rendering the table
  display_com_data <- reactive({
    # If user has edited the preview table, use that data
    if (!is.null(edited_preview_data$data)) {
      return(edited_preview_data$data)
    }
    # Otherwise use base data
    base_com_data()
  })
  
  # Reactive specifically for triggering table re-render
  # Uses display_com_data() so column changes (additions/renames) show immediately
  table_render_data <- reactive({
    display_com_data()
  })

  # Reactive for final COM data (filters NA rows for calculations)
  com_data <- reactive({
    data <- display_com_data()
    
    # Remove rows that are completely NA for calculations
    if (!is.null(data) && nrow(data) > 0) {
      complete_rows <- apply(data, 1, function(row) !all(is.na(row)))
      data <- data[complete_rows, , drop = FALSE]
      if (nrow(data) == 0) data <- NULL
    }
    data
  })

  # Render Data Preview section (shown in CSV and text modes)
  output$data_preview_ui <- renderUI({
    data <- display_com_data()
    calc_data <- com_data()  # Data for calculations (NA rows filtered)
    has_valid_data <- !is.null(calc_data) && nrow(calc_data) > 0
    
    if (!is.null(data) && nrow(data) > 0) {
      # Create download button with conditional disabled state
      download_btn <- downloadButton(ns("download_current_data"), i18n$t("Download"),
        icon = icon("download"),
        class = "btn btn-outline-secondary btn-sm",
        style = "height: 34px; padding: 5px 10px;"
      )
      
      # Disable button if no valid data
      if (!has_valid_data) {
        download_btn <- shinyjs::disabled(download_btn)
      }
      
      # Create clear button
      clear_btn <- actionButton(ns("clear_data"), i18n$t("Clear"),
        icon = icon("trash-alt"),
        class = "btn btn-outline-secondary btn-sm",
        style = "height: 34px; padding: 5px 10px;",
        title = i18n$t("Clear current data")
      )
      
      tagList(
        # Controls row: Clear, Download, separator, Rename
        div(
          style = "display: flex; align-items: center; justify-content: flex-start; margin-bottom: 8px; gap: 8px;",
          # Clear and Download buttons
          clear_btn,
          download_btn,
          # Separator
          tags$span(style = "border-left: 1px solid #ccc; height: 20px; margin-left: 4px; margin-right: 4px;"),
          # Rename column controls
          uiOutput(ns("rename_col_controls_ui"))
        ),
        fluidRow(
          column(12, rhandsontable::rHandsontableOutput(ns("input_data_preview")))
        )
      )
    } else {
      # Placeholder when no data is loaded - message depends on input mode
      placeholder_msg <- if (input$input_type == "csv") {
        i18n$t("Upload a CSV file to preview data.")
      } else {
        i18n$t("Click in the table to start entering data.")
      }
      
      div(
        style = "text-align: center; padding: 20px; color: #888; background-color: #f9f9f9; border: 1px dashed #ddd; border-radius: 4px;",
        icon("table", style = "font-size: 24px; margin-bottom: 8px; display: block;"),
        tags$span(placeholder_msg)
      )
    }
  })

  # Separate renderUI for rename controls - reacts to display_com_data() changes
  output$rename_col_controls_ui <- renderUI({
    data <- display_com_data()
    if (is.null(data) || ncol(data) < 1) return(NULL)
    
    current_cols <- colnames(data)
    
    # Truncate long column names for display (keep full name as value)
    max_display_len <- 12
    display_names <- sapply(current_cols, function(name) {
      if (nchar(name) > max_display_len) {
        paste0(substr(name, 1, max_display_len - 1), "…")
      } else {
        name
      }
    })
    choices <- setNames(current_cols, display_names)
    
    div(
      style = "display: flex; align-items: center; gap: 5px; flex: 1;",
      tags$span(style = "font-size: 12px; color: #666; white-space: nowrap;", i18n$t("Rename")),
      selectInput(
        ns("rename_col_select"),
        label = NULL,
        choices = choices,
        selected = current_cols[1],
        width = "100px"
      ) |> tagAppendAttributes(style = "margin-bottom: 0; flex-shrink: 0;"),
      tags$input(
        id = ns("rename_col_newname"),
        type = "text",
        class = "form-control form-control-sm",
        style = "flex: 1; min-width: 80px; height: 34px; font-size: 12px;",
        placeholder = i18n$t("New name")
      ),
      actionButton(
        ns("rename_col_btn"),
        label = NULL,
        icon = icon("pen"),
        class = "btn btn-outline-secondary btn-sm",
        style = "height: 34px; padding: 2px 8px; flex-shrink: 0;",
        title = i18n$t("Rename column")
      )
    )
  })

  # Observer for column rename
  observeEvent(input$rename_col_btn, {
    old_name <- input$rename_col_select
    new_name <- trimws(input$rename_col_newname)
    
    if (is.null(old_name) || old_name == "" || is.null(new_name) || new_name == "") {
      showNotification(i18n$t("Please select a column and enter a new name."), type = "warning")
      return()
    }
    
    # Get current display data
    data <- display_com_data()
    
    if (is.null(data)) return()
    
    # Check if old name exists
    if (!old_name %in% colnames(data)) {
      showNotification(i18n$t("Column not found."), type = "error")
      return()
    }
    
    # Check if new name already exists
    if (new_name %in% colnames(data) && new_name != old_name) {
      showNotification(i18n$t("A column with this name already exists."), type = "error")
      return()
    }
    
    # Rename column
    col_idx <- which(colnames(data) == old_name)
    colnames(data)[col_idx] <- new_name
    edited_preview_data$data <- data
    
    # Clear the new name input
    shinyjs::runjs(sprintf("$('#%s').val('');", ns("rename_col_newname")))
    
    showNotification(sprintf(i18n$t("Column renamed: %s → %s"), old_name, new_name), type = "message")
  })

  # Display input data preview with rhandsontable (editable)
  # Uses table_render_data() to only re-render when base data changes, not on every edit
  output$input_data_preview <- rhandsontable::renderRHandsontable({
    create_editable_hot(table_render_data(), max_height = 200)
  })

  # Update edited data when user modifies the preview table
  observeEvent(input$input_data_preview, {
    if (!is.null(input$input_data_preview)) {
      tryCatch({
        new_data <- rhandsontable::hot_to_r(input$input_data_preview)
        
        # Check for valid data
        if (is.null(new_data) || !is.data.frame(new_data)) {
          return()
        }
        
        # Fix column names - replace NA, empty, or generic names with sensible defaults
        col_names <- colnames(new_data)
        for (i in seq_along(col_names)) {
          if (is.na(col_names[i]) || col_names[i] == "" || col_names[i] == " ") {
            col_names[i] <- paste0("Column", i)
          }
        }
        # Ensure unique names
        col_names <- make.unique(col_names, sep = "_")
        colnames(new_data) <- col_names
        
        edited_preview_data$data <- new_data
        
        # Also update the underlying data source for the current mode
        # This ensures data persists and is isolated between modes
        if (!is.null(input$input_type)) {
          if (input$input_type == "text") {
            manual_input_data$data <- new_data
          } else if (input$input_type == "csv") {
            # Update whichever CSV source is active
            if (!is.null(csv_data$data)) {
              csv_data$data <- new_data
            } else if (!is.null(example_data$data)) {
              example_data$data <- new_data
            }
          }
        }
      }, error = function(e) {
        # Silently ignore rhandsontable internal errors (genColHeaders, afterChange)
      })
    }
  }, ignoreInit = TRUE)

  # Observer for loading example data
  observeEvent(input$load_example_data, {
    default_path <- system.file("extdata", "gui_com_input-test-data-0.csv", package = "supeRcrit")
    if (file.exists(default_path)) {
      tryCatch({
        raw_data <- read.csv(default_path, stringsAsFactors = FALSE)
        # Rename response to yield if present
        if ("response" %in% colnames(raw_data)) {
          colnames(raw_data)[colnames(raw_data) == "response"] <- "yield"
        }
        
        if (input$input_type == "text") {
          # Manual input mode - store in manual_input_data
          manual_input_data$data <- raw_data
          edited_preview_data$data <- NULL
        } else if (input$input_type == "csv") {
          # CSV mode - clear csv_data first, then set example_data
          csv_data$data <- NULL
          edited_preview_data$data <- NULL
          example_data$data <- raw_data
        }
        
        showNotification(i18n$t("Example data loaded successfully!"), type = "message")
      }, error = function(e) {
        showNotification(paste(i18n$t("Error loading example:"), e$message), type = "error")
      })
    } else {
      showNotification(i18n$t("Example data file not found."), type = "error")
    }
  })

  # Reset example data when user uploads a file
  observeEvent(input$csv_file, {
    example_data$data <- NULL
    edited_preview_data$data <- NULL
  })

  # Clear data button - removes all data and resets preview
  observeEvent(input$clear_data, {
    if (input$input_type == "text") {
      # In manual mode, revert to empty starting table with 8 columns
      # (matches the default structure in base_com_data for pasting multi-column data)
      # NOTE: Do NOT clear example_data$data here - it belongs to CSV mode
      manual_input_data$data <- NULL
      edited_preview_data$data <- data.frame(
        time = rep(NA_real_, 6),
        yield = rep(NA_real_, 6),
        Col3 = rep(NA_real_, 6),
        Col4 = rep(NA_real_, 6),
        Col5 = rep(NA_real_, 6),
        Col6 = rep(NA_real_, 6),
        Col7 = rep(NA_real_, 6),
        Col8 = rep(NA_real_, 6),
        stringsAsFactors = FALSE
      )
    } else {
      example_data$data <- NULL
      edited_preview_data$data <- NULL
      csv_data$data <- NULL  # Clear stored CSV data
      shinyjs::reset("csv_file")
    }
    showNotification(i18n$t("Data cleared."), type = "message")
  })

  # Enable/disable Load Example button based on input type
  observe({
    if (input$input_type == "single") {
      shinyjs::disable("load_example_data")
    } else {
      shinyjs::enable("load_example_data")
    }
  })

  # Enable/disable Reload button based on whether a CSV file is selected
  observe({
    if (!is.null(input$csv_file) && !is.null(input$csv_file$datapath)) {
      shinyjs::enable("reload_csv")
    } else {
      shinyjs::disable("reload_csv")
    }
  })

  # Reload CSV button handler
  observeEvent(input$reload_csv, {
    req(input$csv_file)
    
    tryCatch({
      raw_data <- read.csv(input$csv_file$datapath, header = TRUE, stringsAsFactors = FALSE,
                          na.strings = c("", "NA", "N/A"), strip.white = TRUE)
      # Remove completely empty columns
      data <- raw_data[, !sapply(raw_data, function(x) all(is.na(x) | x == ""))]
      
      # Update csv_data and reset edited data
      csv_data$data <- data
      edited_preview_data$data <- NULL
      example_data$data <- NULL
      
      # Update column selections
      column_names <- names(data)
      updateSelectInput(session, "time_column",
        choices = column_names,
        selected = column_names[1]
      )
      default_yield <- if (length(column_names) >= 2) column_names[2] else column_names[1]
      updateSelectInput(session, "yield_column",
        choices = column_names,
        selected = default_yield
      )
      
      showNotification(i18n$t("CSV file reloaded successfully."), type = "message")
    }, error = function(e) {
      showNotification(paste(i18n$t("Error reloading CSV:"), e$message), type = "error")
    })
  })

  # Download current data
  output$download_current_data <- downloadHandler(
    filename = function() {
      paste0("com_data_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec"), ".csv")
    },
    content = function(file) {
      data <- com_data()  # Use filtered data (no NA rows)
      if (!is.null(data) && nrow(data) > 0) {
        write.csv(data, file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
  )

  # Prepare input data for calcom - uses com_data() with proper column mapping
  prepare_input_data <- function() {
    # Handle single value mode separately
    if (input$input_type == "single") {
      req(input$single_yield)
      cc <- (input$comode %||% "sfe") == "cc_sfe"
      time_val <- if(cc) 1 else { req(input$single_time); input$single_time }
      return(data.frame(time = time_val, yield = input$single_yield))
    }
    
    # For CSV and text modes, get data from com_data()
    data <- com_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Use selected columns for both CSV and Manual Input modes
    time_col <- input$time_column
    yield_col <- input$yield_column
    
    if (!is.null(time_col) && !is.null(yield_col) && 
        time_col %in% colnames(data) && yield_col %in% colnames(data)) {
      
      # Start with time and yield columns
      result <- data.frame(
        time = as.numeric(data[[time_col]]),
        yield = as.numeric(data[[yield_col]])
      )
      
      # Add any additional response columns that are selected
      response_ids <- additional_responses$responses
      if (length(response_ids) > 0) {
        for (resp_id in response_ids) {
          add_col <- input[[paste0("add_resp_column_", resp_id)]]
          if (!is.null(add_col) && add_col != "" && add_col %in% colnames(data)) {
            # Add the column with its original name
            result[[add_col]] <- as.numeric(data[[add_col]])
          }
        }
      }
      
      # Remove rows with NA in time/yield (core columns)
      result <- result[complete.cases(result[, c("time", "yield")]), ]
      if (nrow(result) > 0) return(result)
    }
    
    # Fallback: use first two columns as time and yield
    if (ncol(data) >= 2) {
      result <- data.frame(
        time = as.numeric(data[[1]]),
        yield = as.numeric(data[[2]])
      )
      # Remove rows with NA
      result <- result[complete.cases(result), ]
      if (nrow(result) > 0) return(result)
    }
    
    return(NULL)
  }

  # Update results$input_data when data changes
  observe({
    results$input_data <- prepare_input_data()
    results$is_multi_value <- !is.null(results$input_data) && nrow(results$input_data) > 1
  })

  # Dynamic UI for time column selection - based on current data
  output$time_column_ui <- renderUI({
    data <- display_com_data()
    calc_data <- com_data()  # Filtered data (no NA rows)
    
    # Don't show selector if no valid data - show message here only (yield will be empty)
    if (is.null(calc_data) || nrow(calc_data) == 0 || ncol(calc_data) < 2) {
      return(
        div(
          style = "color: #888; font-style: italic; padding: 8px 0; white-space: nowrap;",
          icon("info-circle"),
          " ", i18n$t("Load data to select variables")
        )
      )
    }
    
    all_cols <- colnames(data)
    current_selection <- isolate(input$time_column)
    
    # Keep current selection if valid (including ""), otherwise default
    selected <- if (!is.null(current_selection) && (current_selection == "" || current_selection %in% all_cols)) {
      current_selection
    } else if ("time" %in% all_cols) {
      "time"
    } else if (length(all_cols) > 0) {
      all_cols[1]
    } else {
      ""
    }
    
    tags$div(
      tags$label(
        i18n$t("Select Time Column"),
        input_help(i18n_r()$t("Column containing extraction time values (minutes). Used as the independent variable for the cost curve."),
                   title = i18n_r()$t("Time Column"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("clear_time_column"), i18n$t("Clear"),
          style = "font-size: 11px; padding: 1px 6px; border-radius: 3px; text-decoration: none; background-color: #dc3545; color: white; margin-left: auto;",
          title = i18n$t("Clear selection")
        )
      ),
      selectInput(ns("time_column"), NULL, choices = c("None" = "", setNames(all_cols, all_cols)), selected = selected)
    )
  })

  # Dynamic UI for yield column selection - based on current data
  output$yield_column_ui <- renderUI({
    data <- display_com_data()
    calc_data <- com_data()  # Filtered data (no NA rows)
    
    # Don't show anything if no valid data (message shown in time column UI)
    if (is.null(calc_data) || nrow(calc_data) == 0 || ncol(calc_data) < 2) {
      return(NULL)
    }
    
    all_cols <- colnames(data)
    
    # Exclude time column from choices
    time_col <- input$time_column
    available_cols <- all_cols
    if (!is.null(time_col) && time_col != "" && time_col %in% available_cols) {
      available_cols <- setdiff(available_cols, time_col)
    }
    
    # Exclude columns used by additional responses
    response_ids <- additional_responses$responses
    for (resp_id in response_ids) {
      used_col <- input[[paste0("add_resp_column_", resp_id)]]
      if (!is.null(used_col) && used_col != "" && used_col %in% available_cols) {
        available_cols <- setdiff(available_cols, used_col)
      }
    }
    
    current_selection <- isolate(input$yield_column)
    
    # Keep current selection if valid (including ""), otherwise default
    selected <- if (!is.null(current_selection) && (current_selection == "" || current_selection %in% available_cols)) {
      current_selection
    } else if ("yield" %in% available_cols) {
      "yield"
    } else if (length(available_cols) >= 1) {
      available_cols[1]
    } else {
      ""
    }
    
    tags$div(
      tags$label(
        i18n$t("Select Response Column"),
        input_help(i18n_r()$t("Column containing the primary response/yield data. This is the main extract whose cost of manufacturing will be calculated."),
                   title = i18n_r()$t("Response Column"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        actionLink(ns("clear_yield_column"), i18n$t("Clear"),
          style = "font-size: 11px; padding: 1px 6px; border-radius: 3px; text-decoration: none; background-color: #dc3545; color: white; margin-left: auto;",
          title = i18n$t("Clear selection")
        )
      ),
      selectInput(ns("yield_column"), NULL, choices = c("None" = "", setNames(available_cols, available_cols)), selected = selected)
    )
  })

  # Dynamic UI for primary response section (CSV/text mode) - shows column selectors and parameters only when data is loaded
  output$primary_response_ui <- renderUI({
    calc_data <- com_data()
    
    # If no data, show message
    if (is.null(calc_data) || nrow(calc_data) == 0 || ncol(calc_data) < 2) {
      return(
        div(
          style = "color: #888; font-style: italic; padding: 8px 0; white-space: nowrap;",
          icon("info-circle"),
          " ", i18n$t("Load data to select variables")
        )
      )
    }
    
    # Data is loaded - show column selectors and response parameters
    tagList(
      fluidRow(
        column(6, div(id = ns("time_column_wrapper"), uiOutput(ns("time_column_ui")))),
        column(6, div(id = ns("primary_yield_column_wrapper"), uiOutput(ns("yield_column_ui"))))
      ),
      uiOutput(ns("column_validation_message_general")),
      fluidRow(
        column(4, 
          selectInput(ns("response_unit"),
          tags$span(i18n$t("Response Unit"),
            input_help(i18n_r()$t("Unit of the primary response data. Percent for yield relative to raw material, grams or kilograms for absolute mass."),
                       title = i18n_r()$t("Response Unit"), buttonLabel = i18n_r()$t("OK"))),
            choices = c("%" = "percent", "g" = "g", "kg" = "kg"),
            selected = defaults$response_unit %||% "percent"
          )
        ),
        column(4,
          numericInput(ns("dilfac"),
          tags$span(i18n$t("Dilution Factor"),
            input_help(i18n_r()$t("Factor by which the extract was diluted during analysis. The measured response is multiplied by this factor to obtain the true yield."),
                       title = i18n_r()$t("Dilution Factor"), buttonLabel = i18n_r()$t("OK"))), value = defaults$dilfac, min = 0)
        ),
        column(4,
          numericInput(ns("pr_sale"),
          tags$span(i18n$t("Sales Price (USD/kg)"),
            input_help(i18n_r()$t("Selling price of this extract in US dollars per kilogram. Used to calculate revenue and payback period."),
                       title = i18n_r()$t("Sales Price"), buttonLabel = i18n_r()$t("OK"))), value = defaults$pr_sale, min = 0)
        )
      )
    )
  })
  
  # Dynamic UI for additional responses section - only shown when data is loaded (for CSV/text mode) or always for single mode
  output$additional_responses_section_ui <- renderUI({
    # For single mode, always show
    if (input$input_type == "single") {
      return(
        tagList(
          tags$hr(style = "margin: 15px 0; border-top: 1px dashed #ccc;"),
          tags$p(tags$strong(i18n$t("Additional Responses")), style = "margin-top: 10px; margin-bottom: 5px; font-size: 14px; color: #333;"),
          tags$small(
            class = "text-muted",
            style = "display: block; margin-bottom: 10px;",
            i18n$t("Add up to 3 additional responses to calculate combined revenue")
          ),
          div(id = ns("additional_responses_container"),
            uiOutput(ns("additional_responses_ui"))
          ),
          fluidRow(
            column(12,
              uiOutput(ns("add_response_button_ui"))
            )
          )
        )
      )
    }
    
    # For CSV/text mode, only show if data is loaded
    calc_data <- com_data()
    if (is.null(calc_data) || nrow(calc_data) == 0 || ncol(calc_data) < 2) {
      return(NULL)
    }
    
    tagList(
      tags$hr(style = "margin: 15px 0; border-top: 1px dashed #ccc;"),
      tags$p(tags$strong(i18n$t("Additional Responses")), style = "margin-top: 10px; margin-bottom: 5px; font-size: 14px; color: #333;"),
      tags$small(
        class = "text-muted",
        style = "display: block; margin-bottom: 10px;",
        i18n$t("Add up to 3 additional responses to calculate combined revenue")
      ),
      div(id = ns("additional_responses_container"),
        uiOutput(ns("additional_responses_ui"))
      ),
      fluidRow(
        column(12,
          uiOutput(ns("add_response_button_ui"))
        )
      )
    )
  })

  # Clear button observers for variable selectors
  observeEvent(input$clear_time_column, {
    updateSelectInput(session, "time_column", selected = "")
  })
  
  observeEvent(input$clear_yield_column, {
    updateSelectInput(session, "yield_column", selected = "")
  })

  # Reset edited data when user uploads a new CSV file
  observeEvent(input$csv_file, {
    example_data$data <- NULL
    edited_preview_data$data <- NULL
  })

  # Observer to toggle flowpar pressure/temperature inputs based on auto checkbox
  observeEvent(input$flowpar_auto, {
    if (isTRUE(input$flowpar_auto)) {
      shinyjs::disable("flowpar_pres")
      shinyjs::disable("flowpar_temp")
    } else {
      shinyjs::enable("flowpar_pres")
      shinyjs::enable("flowpar_temp")
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Validation message for duplicate column selections (general - for all modes)
  output$column_validation_message_general <- renderUI({
    calc_data <- com_data()  # Use filtered data (NA rows removed)
    
    time_col <- input$time_column
    yield_col <- input$yield_column
    
    # Check if any selection is empty
    time_empty <- is.null(time_col) || time_col == ""
    yield_empty <- is.null(yield_col) || yield_col == ""
    
    # Check if same column is selected for both
    same_column <- !time_empty && !yield_empty && time_col == yield_col
    
    # Handle wrapper borders for empty selections
    time_wrapper_id <- ns("time_column_wrapper")
    yield_wrapper_id <- ns("yield_column_wrapper")
    
    # No valid data (NULL or empty after filtering NA rows) - clear borders
    if (is.null(calc_data) || nrow(calc_data) == 0) {
      # Clear borders when no data
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        time_wrapper_id
      ))
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        yield_wrapper_id
      ))
      return(NULL)
    }
    
    # Apply orange border for empty time column or same column selection
    if (time_empty || same_column) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '2px solid #ffc107', 'border-radius': '4px', 'padding': '5px'});",
        time_wrapper_id
      ))
    } else {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        time_wrapper_id
      ))
    }
    
    # Apply orange border for empty yield column or same column selection
    if (yield_empty || same_column) {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '2px solid #ffc107', 'border-radius': '4px', 'padding': '5px'});",
        yield_wrapper_id
      ))
    } else {
      shinyjs::runjs(sprintf(
        "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
        yield_wrapper_id
      ))
    }
    
    if (time_empty || yield_empty) {
      div(
        class = "alert alert-warning",
        style = "margin: 10px 0; padding: 10px;",
        icon("exclamation-triangle"),
        i18n$t(" Variable selection cannot be empty.")
      )
    } else if (same_column) {
      div(
        class = "alert alert-warning",
        style = "margin: 10px 0; padding: 10px;",
        icon("exclamation-triangle"),
        i18n$t(" Time and Response columns must be different.")
      )
    } else {
      NULL
    }
  })

  # Validation message for duplicate column selections (CSV mode - kept for compatibility)
  output$column_validation_message <- renderUI({
    NULL  # Now handled by column_validation_message_general
  })

  # Function to prepare parameters for calcom
  prepare_parameters <- function() {
    # Helper function to get input value or default
    # This handles the case where accordion sections haven't been opened yet
    get_input_or_default <- function(input_name) {
      val <- input[[input_name]]
      if (is.null(val) || identical(val, "") || identical(val, NA)) {
        # Return default value if input hasn't been rendered yet
        # or was cleared by JS (disabled fields return "" as character)
        defaults[[input_name]]
      } else {
        val
      }
    }
    
    # Helper function to safely create named vectors and debug
    safe_named_vector_debug <- function(param_name, ...) {
      values <- list(...)
      # Filter out NULL values before creating the vector
      non_null_values <- values[!sapply(values, is.null)]

      # Check if names are present for all non-NULL values
      if (length(non_null_values) > 0 && is.null(names(non_null_values))) {
        stop(paste("DEBUG ERROR: Parameter group '", param_name, "' has values without names."))
      }

      # Check for length mismatch between values and names
      if (length(non_null_values) != length(names(non_null_values))) {
        stop(paste(
          "DEBUG ERROR: Parameter group '", param_name,
          "' has a mismatch between number of values (", length(non_null_values),
          ") and number of names (", length(names(non_null_values)), ").",
          "Values:", paste(non_null_values, collapse = ", "),
          "Names:", paste(names(non_null_values), collapse = ", ")
        ))
      }

      # Create the vector
      result <- do.call(c, non_null_values)
      return(result)
    }

    # General parameters - ensure all required parameters are included
    gen_params <- safe_named_vector_debug("gen_params",
      volex = get_input_or_default("volex"),
      load = get_input_or_default("load"),
      pres = get_input_or_default("pres"),
      temp = get_input_or_default("temp"),
      flow = get_input_or_default("flow"),
      extime = get_input_or_default("extime"),
      dilfac = if (input$input_type == "single") get_input_or_default("dilfac_single") else get_input_or_default("dilfac"),
      pr_sale = if (input$input_type == "single") get_input_or_default("pr_sale_single") else get_input_or_default("pr_sale"),
      csol_flow = get_input_or_default("csol_flow")
    )

    # Calculation Mode parameters
    comode_val <- get_input_or_default("comode")
    if (is.null(comode_val) || is.na(comode_val)) comode_val <- "sfe"
    use_coefs_val <- if (is.null(input$use_coefs)) {
      if (is.null(defaults$use_coefs)) FALSE else as.logical(defaults$use_coefs)
    } else {
      as.logical(input$use_coefs)
    }
    
    # Custom coefficients (used when use_coefs is FALSE)
    coefs_val <- c(
      fci = if (!is.null(input$coef_fci)) as.numeric(input$coef_fci) else if (!is.null(defaults$coef_fci)) defaults$coef_fci else 1,
      col = if (!is.null(input$coef_col)) as.numeric(input$coef_col) else if (!is.null(defaults$coef_col)) defaults$coef_col else 1,
      crm = if (!is.null(input$coef_crm)) as.numeric(input$coef_crm) else if (!is.null(defaults$coef_crm)) defaults$coef_crm else 1,
      cut = if (!is.null(input$coef_cut)) as.numeric(input$coef_cut) else if (!is.null(defaults$coef_cut)) defaults$coef_cut else 1
    )

    # Cost of Raw Materials parameters
    crm_params <- safe_named_vector_debug("crm_params",
      bh = get_input_or_default("bh"),
      id = get_input_or_default("id"),
      pr_mat = get_input_or_default("pr_mat"),
      pr_msol = get_input_or_default("pr_msol"),
      pr_csol = get_input_or_default("pr_csol"),
      recp = get_input_or_default("recp"),
      rect = get_input_or_default("rect"),
      sept = get_input_or_default("sept")
    )

    # Cost of Utilities parameters
    cut_params <- safe_named_vector_debug("cut_params",
      pw_main = get_input_or_default("pw_main"),
      pr_kwh = get_input_or_default("pr_kwh"),
      pw_dry = get_input_or_default("pw_dry"),
      pw_com = get_input_or_default("pw_com"),
      pw_evap = get_input_or_default("pw_evap"),
      cap_dry = get_input_or_default("cap_dry"),
      cap_com = get_input_or_default("cap_com"),
      cap_evap = get_input_or_default("cap_evap")
    )

    # Cost of Labor parameters
    col_params <- safe_named_vector_debug("col_params",
      oper = get_input_or_default("oper"),
      whr = get_input_or_default("whr"),
      shifts = get_input_or_default("shifts"),
      wage = get_input_or_default("wage"),
      wdays = get_input_or_default("wdays")
    )

    # Fixed Costs parameters
    fci_params <- safe_named_vector_debug("fci_params",
      capex = get_input_or_default("capex"),
      maint = get_input_or_default("maint"),
      other = get_input_or_default("other"),
      depr = get_input_or_default("depr")
    )
    taxrate_val <- to_numeric_or_na(get_input_or_default("taxrate"))

    # Other parameters - collect dynamic auxiliary materials
    material_ids <- aux_materials$materials
    if (length(material_ids) > 0) {
      auxpr_vals <- sapply(material_ids, function(mat_id) {
        val <- input[[paste0("aux_price_", mat_id)]]
        if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
      })
      auxfr_vals <- sapply(material_ids, function(mat_id) {
        val <- input[[paste0("aux_fraction_", mat_id)]]
        if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
      })
      aux_names <- sapply(material_ids, function(mat_id) {
        name_val <- input[[paste0("aux_name_", mat_id)]]
        if (is.null(name_val) || name_val == "") paste0("material_", mat_id) else gsub("[^A-Za-z0-9_]", "_", name_val)
      })
      names(auxpr_vals) <- aux_names
      names(auxfr_vals) <- aux_names
      
      # Filter out NA values
      valid_idx <- !is.na(auxpr_vals) & !is.na(auxfr_vals)
      if (any(valid_idx)) {
        auxpr_params <- auxpr_vals[valid_idx]
        auxfr_params <- auxfr_vals[valid_idx]
      } else {
        auxpr_params <- NA
        auxfr_params <- NA
      }
    } else {
      auxpr_params <- NA
      auxfr_params <- NA
    }

    # Handle cosol_loss
    cosol_loss_input <- get_input_or_default("cosol_loss")
    cosol_loss_val <- if (!is.null(cosol_loss_input) && !is.na(cosol_loss_input)) {
      as.numeric(cosol_loss_input)
    } else {
      0.05 # Hardcode default if still NULL
    }

    # Handle msol_loss_frac (CC-SFE only)
    msol_loss_frac_input <- get_input_or_default("msol_loss_frac")
    msol_loss_frac_val <- if (!is.null(msol_loss_frac_input) && !is.na(msol_loss_frac_input) &&
                              is.numeric(msol_loss_frac_input)) {
      as.numeric(msol_loss_frac_input)
    } else {
      0.01
    }

    # Mass flow flags: main solvent, co-solvent (always volumetric), feed (CC-SFE)
    mass_flow_val <- c(main_flow_mass(), FALSE, feed_flow_mass())

    # Handle flowpar - now with separate pressure/temperature inputs and auto checkbox
    flowpar_val <- "auto" # Default
    flowpar_auto_input <- get_input_or_default("flowpar_auto")
    flowpar_auto <- if (is.null(flowpar_auto_input)) TRUE else as.logical(flowpar_auto_input)
    
    if (!flowpar_auto) {
      # Use the manually specified pressure and temperature
      flowpar_pres_input <- get_input_or_default("flowpar_pres")
      flowpar_pres <- if (!is.null(flowpar_pres_input) && !is.na(flowpar_pres_input)) {
        as.numeric(flowpar_pres_input)
      } else {
        60 # Default pressure
      }
      flowpar_temp_input <- get_input_or_default("flowpar_temp")
      flowpar_temp <- if (!is.null(flowpar_temp_input) && !is.na(flowpar_temp_input)) {
        as.numeric(flowpar_temp_input)
      } else {
        10 # Default temperature
      }
      flowpar_val <- list(c(flowpar_pres, flowpar_temp))
    }

    # Handle response_unit and moisture_content
    response_unit_val <- if (input$input_type == "single") {
      get_input_or_default("response_unit_single")
    } else {
      get_input_or_default("response_unit")
    }
    if (is.null(response_unit_val)) response_unit_val <- "percent"
    
    moisture_content_val <- get_input_or_default("moisture_content")
    if (is.null(moisture_content_val) || identical(moisture_content_val, "") ||
        (length(moisture_content_val) == 1 && is.na(moisture_content_val))) moisture_content_val <- 0
    moisture_content_val <- as.numeric(moisture_content_val)
    if (is.na(moisture_content_val)) moisture_content_val <- 0
    
    # Handle response_name - use column name for CSV/text modes, text input for single mode
    if (input$input_type == "single") {
      response_name_val <- get_input_or_default("response_name")
      if (is.null(response_name_val) || response_name_val == "") response_name_val <- "Primary"
    } else {
      # Use the yield column name as the response name
      response_name_val <- input$yield_column
      if (is.null(response_name_val) || response_name_val == "") response_name_val <- "Primary"
    }
    
    # Handle additional_responses
    additional_resp_params <- NULL
    response_ids <- additional_responses$responses
    
    if (length(response_ids) > 0) {
      additional_resp_params <- lapply(response_ids, function(resp_id) {
        # For CSV/text mode, use column name as response name; for single mode use text input
        if (input$input_type == "single") {
          name_val <- input[[paste0("add_resp_name_", resp_id)]]
          if (is.null(name_val) || name_val == "") {
            name_val <- paste0("Response_", resp_id + 1)
          }
        } else {
          # Use the column name as the response name
          name_val <- input[[paste0("add_resp_column_", resp_id)]]
          if (is.null(name_val) || name_val == "") {
            name_val <- paste0("Response_", resp_id + 1)
          }
        }
        
        # Get response unit
        unit_val <- input[[paste0("add_resp_unit_", resp_id)]]
        if (is.null(unit_val)) unit_val <- "percent"
        
        # Get dilution factor
        dilfac_val <- input[[paste0("add_resp_dilfac_", resp_id)]]
        if (is.null(dilfac_val) || is.na(dilfac_val)) dilfac_val <- 1
        dilfac_val <- as.numeric(dilfac_val)
        
        # Get sale price
        pr_sale_val <- input[[paste0("add_resp_pr_sale_", resp_id)]]
        if (is.null(pr_sale_val) || is.na(pr_sale_val)) {
          stop(paste0("Sale price for additional response '", name_val, "' is required!"))
        }
        pr_sale_val <- as.numeric(pr_sale_val)
        
        # Build the response specification
        resp_spec <- list(
          name = name_val,
          pr_sale = pr_sale_val,
          response_unit = unit_val,
          dilfac = dilfac_val
        )
        
        # For single input mode, get value; for CSV/text mode, get column
        if (input$input_type == "single") {
          value_val <- input[[paste0("add_resp_value_", resp_id)]]
          if (is.null(value_val) || is.na(value_val)) value_val <- 0
          resp_spec$value <- as.numeric(value_val)
        } else {
          col_val <- input[[paste0("add_resp_column_", resp_id)]]
          if (is.null(col_val) || col_val == "") {
            stop(paste0("Response column for additional response '", name_val, "' must be selected!"))
          }
          resp_spec$column <- col_val
        }
        
        return(resp_spec)
      })
    }

    return(list(
      gen = gen_params,
      crm = crm_params,
      cut = cut_params,
      col = col_params,
      fci = fci_params,
      auxpr = auxpr_params,
      auxfr = auxfr_params,
      cosol_loss = cosol_loss_val,
      msol_loss_frac = msol_loss_frac_val,
      mass_flow = mass_flow_val,
      feed_density = feed_density(),
      flowpar = flowpar_val,
      comode = comode_val,
      use_coefs = use_coefs_val,
      coefs = coefs_val,
      taxrate = taxrate_val,
      response_unit = response_unit_val,
      moisture_content = moisture_content_val,
      response_name = response_name_val,
      additional_responses = additional_resp_params
    ))
  }

  # Calculate button event
  observeEvent(input$calculate, {
    # Check if data is loaded (for non-single modes)
    if (input$input_type != "single") {
      calc_data <- com_data()
      if (is.null(calc_data) || nrow(calc_data) == 0) {
        showNotification(i18n$t("Please load data before calculating."), type = "error")
        return()
      }
      
      # Check for duplicate column selection
      time_col <- input$time_column
      yield_col <- input$yield_column
      if (!is.null(time_col) && !is.null(yield_col) && 
          time_col != "" && yield_col != "" && 
          time_col == yield_col) {
        showNotification(i18n$t("Time and Response columns must be different."), type = "error")
        return()
      }
    }
    
    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Calculating..."), value = 0, {
          # Prepare input data
          incProgress(0.2, detail = i18n$t("Preparing data..."))
          input_data <- prepare_input_data()

          # Prepare parameters
          incProgress(0.4, detail = i18n$t("Preparing parameters..."))
          params <- prepare_parameters()

          # Call calcom function
          incProgress(0.8, detail = i18n$t("Performing calculations..."))

          # Check if input_data is valid before proceeding
          if (is.null(input_data)) {
            stop(i18n$t("No input data available. Please provide valid input data before calculating."))
          }

          # Determine multi-value based on input data BEFORE calculation
          if (nrow(input_data) > 1) {
            results$is_multi_value <- TRUE
          } else {
            results$is_multi_value <- FALSE
          }

          if (results$is_multi_value) {
            com_result <- calcom(
              input = input_data,
              invars = c("time", "yield"),
              gen = params$gen,
              crm = params$crm,
              cut = params$cut,
              col = params$col,
              fci = params$fci,
              auxpr = params$auxpr,
              auxfr = params$auxfr,
              taxrate = params$taxrate, # Add taxrate as a direct parameter
              cosol_loss = params$cosol_loss,
              msol_loss_frac = params$msol_loss_frac,
              flowpar = params$flowpar, # Use the flowpar from params
              comode = params$comode,
              mass_flow = params$mass_flow,
              feed_density = params$feed_density,
              use_coefs = params$use_coefs,
              coefs = params$coefs,
              response_unit = params$response_unit,
              moisture_content = params$moisture_content,
              response_name = params$response_name,
              additional_responses = params$additional_responses,
              pltlab = "Time (min)",
              draw = TRUE # Ensure plots are generated for multi-value input
            )
          } else {
            # For single value, input needs to be a named vector
            com_result <- calcom(
              input = c(time = input_data$time[1], yield = input_data$yield[1]),
              invars = c("time", "yield"),
              gen = params$gen,
              crm = params$crm,
              cut = params$cut,
              col = params$col,
              fci = params$fci,
              auxpr = params$auxpr,
              auxfr = params$auxfr,
              taxrate = params$taxrate, # Add taxrate as a direct parameter
              cosol_loss = params$cosol_loss,
              msol_loss_frac = params$msol_loss_frac,
              flowpar = params$flowpar, # Use the flowpar from params
              comode = params$comode,
              mass_flow = params$mass_flow,
              feed_density = params$feed_density,
              use_coefs = params$use_coefs,
              coefs = params$coefs,
              response_unit = params$response_unit,
              moisture_content = params$moisture_content,
              response_name = params$response_name,
              additional_responses = params$additional_responses,
              pltlab = "Time (min)",
              draw = FALSE # No plots for single value input
            )
          }
          # Güzel görünümde call oluştur
          pretty_call <- substitute(
            calcom(
              input = input_val, invars = c("time", "yield"),
              pltlab = "Time (min)", gen = gen_val, crm = crm_val,
              auxpr = auxpr_val, auxfr = auxfr_val, cut = cut_val,
              col = col_val, fci = fci_val
            ),
            list(
              input_val = c(time = input_data$time[1], yield = input_data$yield[1]),
              gen_val = params$gen,
              crm_val = params$crm,
              auxpr_val = params$auxpr,
              auxfr_val = params$auxfr,
              cut_val = params$cut,
              col_val = params$col,
              fci_val = params$fci
            )
          )

          results$call <- format(com_result[["call"]])
          #  results$call <- deparse(com_result[["call"]], width.cutoff = 500)
          # results$call <- pretty_call

          # Store results
          results$com_result <- com_result
          results$simple_data <- com_result$simple_output
          results$detailed_data <- com_result$output
          results$params <- params # Store params in reactive values
          results$payback_filter_notified <- FALSE  # Reset notification flag for new calculation

          # Set is_multi_value flag based on input data
          if (nrow(input_data) > 1) {
            results$is_multi_value <- TRUE
          } else {
            results$is_multi_value <- FALSE
          }

          incProgress(1, detail = i18n$t("Completed!"))
        })

        # Show success message
        showNotification(i18n$t("Calculation completed!"), type = "message")
      },
      error = function(e) {
        # Show error message
        showNotification(paste(i18n$t("bug-Error occurred:"), e$message), type = "error")
        # browser()
      }
    )
  })

  # Reset button event
  observeEvent(input$reset, {
    # Reset all inputs to default values
    updateNumericInput(session, "volex", value = defaults$volex)
    updateNumericInput(session, "load", value = defaults$load)
    updateNumericInput(session, "pres", value = defaults$pres)
    updateNumericInput(session, "temp", value = defaults$temp)
    updateNumericInput(session, "flow", value = defaults$flow)
    updateNumericInput(session, "extime", value = defaults$extime)
    updateNumericInput(session, "dilfac", value = defaults$dilfac)
    updateNumericInput(session, "dilfac_single", value = defaults$dilfac)
    updateNumericInput(session, "pr_sale", value = defaults$pr_sale)
    updateNumericInput(session, "pr_sale_single", value = defaults$pr_sale)

    updateRadioButtons(session, "comode", selected = defaults$comode)
    updateCheckboxInput(session, "use_coefs", value = defaults$use_coefs)

    updateNumericInput(session, "bh", value = defaults$bh)
    updateNumericInput(session, "id", value = defaults$id)
    updateNumericInput(session, "pr_mat", value = defaults$pr_mat)
    updateNumericInput(session, "pr_msol", value = defaults$pr_msol)
    updateNumericInput(session, "pr_csol", value = defaults$pr_csol)
    updateNumericInput(session, "recp", value = defaults$recp)
    updateNumericInput(session, "rect", value = defaults$rect)
    updateNumericInput(session, "sept", value = defaults$sept)

    updateNumericInput(session, "pw_main", value = defaults$pw_main)
    updateNumericInput(session, "pr_kwh", value = defaults$pr_kwh)
    updateNumericInput(session, "pw_dry", value = defaults$pw_dry)
    updateNumericInput(session, "pw_com", value = defaults$pw_com)
    updateNumericInput(session, "pw_evap", value = defaults$pw_evap)
    updateNumericInput(session, "cap_dry", value = defaults$cap_dry)
    updateNumericInput(session, "cap_com", value = defaults$cap_com)
    updateNumericInput(session, "cap_evap", value = defaults$cap_evap)

    updateNumericInput(session, "oper", value = defaults$oper)
    updateNumericInput(session, "whr", value = defaults$whr)
    updateNumericInput(session, "shifts", value = defaults$shifts)
    updateNumericInput(session, "wage", value = defaults$wage)
    updateNumericInput(session, "wdays", value = defaults$wdays)

    updateNumericInput(session, "capex", value = defaults$capex)
    updateNumericInput(session, "maint", value = defaults$maint)
    updateNumericInput(session, "other", value = defaults$other)
    updateNumericInput(session, "depr", value = defaults$depr)
    updateNumericInput(session, "taxrate", value = defaults$taxrate)

    # Reset auxiliary materials
    aux_materials$materials <- list()
    
    # Reset additional responses
    additional_responses$responses <- list()
    
    # Reset response name
    updateTextInput(session, "response_name", value = defaults$response_name %||% "Primary")
    
    # Reset response unit and moisture content (both single and csv mode inputs)
    updateSelectInput(session, "response_unit", selected = defaults$response_unit %||% "percent")
    updateSelectInput(session, "response_unit_single", selected = defaults$response_unit %||% "percent")
    updateNumericInput(session, "moisture_content", value = defaults$moisture_content %||% 0)
    
    # Reset dilfac and pr_sale (both single and csv mode inputs)
    updateNumericInput(session, "dilfac", value = defaults$dilfac)
    updateNumericInput(session, "dilfac_single", value = defaults$dilfac)
    updateNumericInput(session, "pr_sale", value = defaults$pr_sale)
    updateNumericInput(session, "pr_sale_single", value = defaults$pr_sale)
    
    updateNumericInput(session, "cosol_loss", value = defaults$cosol_loss)
    updateTextInput(session, "flowpar", value = defaults$flowpar)
    updateNumericInput(session, "csol_flow", value = defaults$csol_flow)

    # Reset input data section
    updateNumericInput(session, "single_time", value = defaults$single_time)
    updateNumericInput(session, "single_yield", value = defaults$single_yield)

    # Clear all input data (same as BIC/TWS modules)
    example_data$data <- NULL
    edited_preview_data$data <- NULL
    shinyjs::reset("csv_file")
    
    # Clear variable selection wrapper borders
    shinyjs::runjs(sprintf(
      "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
      ns("time_column_wrapper")
    ))
    shinyjs::runjs(sprintf(
      "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
      ns("yield_column_wrapper")
    ))

    # Clear calculation results
    results$com_result <- NULL
    results$simple_data <- NULL
    results$detailed_data <- NULL
    results$input_data <- NULL
    results$is_multi_value <- FALSE

    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Observe results and update time sliders (both General and Detailed tabs)
  observeEvent(results$com_result, {
    if (results$is_multi_value && !is.null(results$simple_data)) {
      time_values <- results$simple_data$time
      if (length(time_values) > 0) {
        # Find the row with the maximum monthly_profit
        max_profit_row <- results$simple_data[which.max(results$simple_data$monthly_profit), ]

        # Calculate step as GCD of all time differences to ensure slider hits all actual data points
        # This handles irregular time spacing (e.g., 60, 120, 180, 240, 300, 360, 434, 480)
        time_diffs <- diff(sort(unique(time_values)))
        if (length(time_diffs) > 0) {
          # Calculate GCD of all differences
          gcd_func <- function(a, b) { if (b == 0) a else gcd_func(b, a %% b) }
          step_val <- Reduce(gcd_func, time_diffs)
          step_val <- max(1, step_val)  # Minimum step of 1
        } else {
          step_val <- 1
        }

        updateSliderInput(session, "time_slider",
          min = min(time_values),
          max = max(time_values),
          value = max_profit_row$time,
          step = step_val
        )
        updateSliderInput(session, "time_slider_detailed",
          min = min(time_values),
          max = max(time_values),
          value = max_profit_row$time,
          step = step_val
        )
      }
    }
  })
  
  # Sync sliders: when General tab slider changes, update Detailed tab slider
  observeEvent(input$time_slider, {
    if (!is.null(input$time_slider_detailed) && input$time_slider != input$time_slider_detailed) {
      updateSliderInput(session, "time_slider_detailed", value = input$time_slider)
    }
  }, ignoreInit = TRUE)
  
  # Sync sliders: when Detailed tab slider changes, update General tab slider
  observeEvent(input$time_slider_detailed, {
    if (!is.null(input$time_slider) && input$time_slider_detailed != input$time_slider) {
      updateSliderInput(session, "time_slider", value = input$time_slider_detailed)
    }
  }, ignoreInit = TRUE)

  # ---- Optimal time computation ----
  optimal_times <- reactive({
    req(results$simple_data, nrow(results$simple_data) > 1)
    sd <- results$simple_data

    opt <- list()

    # Minimum specific cost (lowest SC_per_kg)
    if("SC_per_kg" %in% names(sd) && any(!is.na(sd$SC_per_kg))) {
      idx <- which.min(sd$SC_per_kg)
      if(length(idx) > 0) {
        opt$min_cost <- list(
          time = sd$time[idx],
          value = round(sd$SC_per_kg[idx], 2)
        )
      }
    }

    # Minimum payback (shortest positive payback)
    if("payback_yr" %in% names(sd) && any(!is.na(sd$payback_yr))) {
      valid <- which(sd$payback_yr > 0 & !is.na(sd$payback_yr))
      if(length(valid) > 0) {
        idx <- valid[which.min(sd$payback_yr[valid])]
        opt$min_payback <- list(
          time = sd$time[idx],
          value = round(sd$payback_yr[idx], 2)
        )
      }
    }

    opt
  })

  # Helper to build the button row (shared between both slider locations)
  build_optimal_buttons <- function(btn_prefix) {
    opt <- optimal_times()
    if(is.null(opt) || length(opt) == 0) return(NULL)

    btn_style <- paste0(
      "font-size: 12px; padding: 3px 10px; margin-right: 6px; margin-bottom: 6px; ",
      "border-radius: 3px; font-weight: 600;"
    )

    btns <- list()

    if(!is.null(opt$min_cost)) {
      btns[[length(btns) + 1]] <- actionButton(
        ns(paste0(btn_prefix, "_goto_min_cost")),
        tagList(icon("arrow-right", lib = "font-awesome"),
                paste0(" ", i18n$t("Min. Cost"), " (", opt$min_cost$time, " min)")),
        style = paste0(btn_style, "background: #00a65a; border-color: #008d4c; color: white;"),
        class = "btn-xs"
      )
    }

    if(!is.null(opt$min_payback)) {
      btns[[length(btns) + 1]] <- actionButton(
        ns(paste0(btn_prefix, "_goto_min_payback")),
        tagList(icon("arrow-right", lib = "font-awesome"),
                paste0(" ", i18n$t("Min. Payback"), " (", opt$min_payback$time, " min)")),
        style = paste0(btn_style, "background: #3c8dbc; border-color: #367fa9; color: white;"),
        class = "btn-xs"
      )
    }

    if(length(btns) == 0) return(NULL)

    tags$div(
      style = "margin-top: -6px; margin-bottom: 8px;",
      tags$span(style = "font-size: 12px; color: #666; margin-right: 6px; font-weight: 600;",
                paste0(i18n$t("Jump to optimal time"), ":")),
      tagList(btns)
    )
  }

  # Render buttons for main slider
  output$optimal_time_buttons <- renderUI({
    build_optimal_buttons("main")
  })

  # Render buttons for detailed slider
  output$optimal_time_buttons_detailed <- renderUI({
    build_optimal_buttons("detail")
  })

  # ---- Snap-to-optimal observers ----
  # Main slider buttons
  observeEvent(input$main_goto_min_cost, {
    opt <- optimal_times()
    if(!is.null(opt$min_cost)) updateSliderInput(session, "time_slider", value = opt$min_cost$time)
  })
  observeEvent(input$main_goto_min_payback, {
    opt <- optimal_times()
    if(!is.null(opt$min_payback)) updateSliderInput(session, "time_slider", value = opt$min_payback$time)
  })
  # Detailed slider buttons
  observeEvent(input$detail_goto_min_cost, {
    opt <- optimal_times()
    if(!is.null(opt$min_cost)) updateSliderInput(session, "time_slider_detailed", value = opt$min_cost$time)
  })
  observeEvent(input$detail_goto_min_payback, {
    opt <- optimal_times()
    if(!is.null(opt$min_payback)) updateSliderInput(session, "time_slider_detailed", value = opt$min_payback$time)
  })

  # Reactive for selected time data (uses either slider, preferring the one that was last changed)
  selected_simple_data <- reactive({
    if (!is.null(results$com_result) && nrow(results$simple_data) > 0) {
      # Use time_slider as the primary (both are synced)
      slider_value <- input$time_slider
      if (is.null(slider_value)) slider_value <- input$time_slider_detailed
      if (is.null(slider_value)) return(results$simple_data[1, ])
      
      # Find the closest time value in simple_data to the slider value
      closest_time_idx <- which.min(abs(results$simple_data$time - slider_value))
      results$simple_data[closest_time_idx, ]
    } else {
      NULL
    }
  })

  # Reactive for selected detailed data
  selected_detailed_data <- reactive({
    req(results$detailed_data)

    if (results$is_multi_value) {
      req(input$time_slider)
      # Find the closest time column
      time_cols <- names(results$detailed_data)[grepl("_min$", names(results$detailed_data))]
      numeric_times <- as.numeric(gsub("_min$", "", time_cols))

      # Handle cases where numeric_times might be empty or contain NA/NaN
      if (length(numeric_times) == 0 || all(is.na(numeric_times))) {
        selected_col_name <- "60_min" # Fallback to default if no time columns found
      } else {
        closest_time_idx <- which.min(abs(numeric_times - input$time_slider))
        selected_col_name <- time_cols[closest_time_idx]
      }
    } else { # Single value case
      # Find the single _min column in detailed_data
      single_value_cols <- names(results$detailed_data)[grepl("_min$", names(results$detailed_data))]
      if (length(single_value_cols) == 1) {
        selected_col_name <- single_value_cols[1]
      } else {
        # Fallback if no single _min column found (should not happen with valid calcom output)
        selected_col_name <- "60_min" # Keep as a last resort fallback
      }
    }

    # Ensure the selected column exists, fallback to a safe default if not
    if (!selected_col_name %in% names(results$detailed_data)) {
      # Try to find any _min column if the selected one is missing
      available_min_cols <- names(results$detailed_data)[grepl("_min$", names(results$detailed_data))]
      if (length(available_min_cols) > 0) {
        selected_col_name <- available_min_cols[1] # Use the first available _min column
      } else {
        selected_col_name <- "60_min" # Absolute last resort fallback
      }
    }

    # Format the detailed data for display
    display_data <- results$detailed_data %>%
      select(description, !!sym(selected_col_name), units) %>% # Change order here
      rename(
        # "Parameter" = parameter,
        "Description" = description,
        "Value" = !!sym(selected_col_name), # Rename Value first
        "Unit" = units # Then rename Unit
      ) %>%
      mutate(Value = format(Value, big.mark = ",", digits = 2, scientific = FALSE)) # Format Value column
    return(display_data)
  })

  # Output to control conditionalPanel visibility for time slider
  output$has_multi_data <- reactive({
    # Only show slider if is_multi_value is TRUE AND simple_data has more than 1 row
    results$is_multi_value && !is.null(results$simple_data) && nrow(results$simple_data) > 1
  })
  outputOptions(output, "has_multi_data", suspendWhenHidden = FALSE)

  # Render Info Boxes
  output$extract_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      extract_value <- selected_simple_data()$extract_made
      infoBox(
        i18n$t("Extract produced *"),
        paste0(format(round(extract_value), big.mark = ","), " kg"),
        icon = icon("weight-hanging"),
        color = "blue"
      )
    } else {
      infoBox(i18n$t("Extract produced *"), "0 kg", icon = icon("weight-hanging"), color = "blue")
    }
  })

  output$profit_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      sc_value <- selected_simple_data()$monthly_profit
      infoBox(
        i18n$t("Profit (After-tax) *"),
        paste0(format(sc_value, digits = 2), " $"),
        icon = icon("dollar-sign"),
        color = "green"
      )
    } else {
      infoBox(i18n$t("Gross profit (After-tax) *"), "$0", icon = icon("dollar-sign"), color = "green")
    }
  })

  output$margin_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      margin_value <- selected_simple_data()$margin_percent
      infoBox(
        i18n$t("Profit Margin *"),
        paste0(format(margin_value, digits = 2), " %"),
        icon = icon("percentage"),
        color = "purple"
      )
    } else {
      infoBox(i18n$t("Profit Margin *"), "0 %", icon = icon("percentage"), color = "purple")
    }
  })

  output$payback_box <- renderInfoBox({
    if (!is.null(selected_simple_data())) {
      payback_value <- selected_simple_data()$payback_yr
      
      # Check if payback is negative (project not economically viable)
      if (!is.null(payback_value) && !is.na(payback_value) && payback_value < 0) {
        infoBox(
          i18n$t("Payback Period"),
          i18n$t("Not Viable"),
          subtitle = i18n$t("Negative profit margin"),
          icon = icon("exclamation-triangle"),
          color = "red"
        )
      } else {
        infoBox(
          i18n$t("Payback Period"),
          paste0(format(payback_value, digits = 2), i18n$t(" years")),
          icon = icon("clock"),
          color = "orange"
        )
      }
    } else {
      infoBox(i18n$t("Payback Period"), "0 years", icon = icon("clock"), color = "orange")
    }
  })

  # Render ROI Plot
  output$roi_plot <- renderPlotly({
    render_roi_plot(selected_simple_data(), input$capex, results$com_result, i18n)
  })

  output$roi_monthly_table <- DT::renderDataTable(
    {
      req(results$params) # Ensure params is available
      render_roi_monthly_table(results$detailed_data, results$is_multi_value, input$time_slider, input$capex, results$com_result, results$params$taxrate, i18n, tablang)
    },
    server = FALSE,
    options = list(rownames = FALSE)
  )
  # Export Monthly Data
  output$export_monthly <- download_roi_monthly_processed_data(
    prepare_roi_monthly_data(results$detailed_data, results$is_multi_value, input$time_slider, input$capex, results$com_result, results$params$taxrate, i18n)
  )

  # Render Detailed Table
  output$detailed_table <- DT::renderDataTable(
    {
      req(selected_detailed_data()) # Use the reactive detailed data

      # Use the detailed data directly (already formatted in selected_detailed_data)
      display_data <- selected_detailed_data()

      add_prettynames <- c(
        i18n_r()$t("Description"),
        i18n_r()$t("Value"),
        i18n_r()$t("Unit")
      )

      description_translations <- c(
        "Extraction cycles per month" = i18n_r()$t("Extraction cycles per month"),
        "Main solvent loss per cycle (CO2 or water)" = i18n_r()$t("Main solvent loss per cycle (CO2 or water)"),
        "Co-solvent loss per batch (EtOH or CO2)" = i18n_r()$t("Co-solvent loss per batch (EtOH or CO2)"),
        "CO2 loss per month" = i18n_r()$t("CO2 loss per month"),
        "Co-solvent loss per month" = i18n_r()$t("Co-solvent loss per month"),
        "S/M mass ratio (CO2)" = i18n_r()$t("S/M mass ratio (CO2)"),
        "S/M mass ratio (co-solvent)" = i18n_r()$t("S/M mass ratio (co-solvent)"),
        "S/F mass ratio (CO2)" = i18n_r()$t("S/F mass ratio (CO2)"),
        "S/F mass ratio (co-solvent)" = i18n_r()$t("S/F mass ratio (co-solvent)"),
        "Raw material requirement (monthly)" = i18n_r()$t("Raw material requirement (monthly)"),
        "Main solvent requirement (monthly)" = i18n_r()$t("Main solvent requirement (monthly)"),
        "Co-solvent requirement (monthly)" = i18n_r()$t("Co-solvent requirement (monthly)"),
        "Requirement of all auxiliary materials (monthly)" = i18n_r()$t("Requirement of all auxiliary materials (monthly)"),
        "Raw material cost (monthly)" = i18n_r()$t("Raw material cost (monthly)"),
        "Main solvent cost (monthly)" = i18n_r()$t("Main solvent cost (monthly)"),
        "Co-solvent cost (monthly)" = i18n_r()$t("Co-solvent cost (monthly)"),
        "Cost of all auxiliary materials (monthly)" = i18n_r()$t("Cost of all auxiliary materials (monthly)"),
        "Main power requirement (monthly)" = i18n_r()$t("Main power requirement (monthly)"),
        "Additional power requirement (monthly)" = i18n_r()$t("Additional power requirement (monthly)"),
        "Drying power requirement (monthly)" = i18n_r()$t("Drying power requirement (monthly)"),
        "Comminution power requirement (monthly)" = i18n_r()$t("Comminution power requirement (monthly)"),
        "Evaporation power requirement (monthly)" = i18n_r()$t("Evaporation power requirement (monthly)"),
        "Main power cost (monthly)" = i18n_r()$t("Main power cost (monthly)"),
        "Additional power cost (monthly)" = i18n_r()$t("Additional power cost (monthly)"),
        "Drying power cost (monthly)" = i18n_r()$t("Drying power cost (monthly)"),
        "Comminution power cost (monthly)" = i18n_r()$t("Comminution power cost (monthly)"),
        "Evaporation power cost (monthly)" = i18n_r()$t("Evaporation power cost (monthly)"),
        "Extract produced (monthly)" = i18n_r()$t("Extract produced (monthly)"),
        "Specific extract cost" = i18n_r()$t("Specific extract cost"),
        "Monthly gross profit (pre-tax)" = i18n_r()$t("Monthly gross profit (pre-tax)"),
        "Monthly profit (after tax)" = i18n_r()$t("Monthly profit (after tax)"),
        "Sales volume (monthly)" = i18n_r()$t("Sales volume (monthly)"),
        "Profit margin (monthly)" = i18n_r()$t("Profit margin (monthly)"),
        "Payback period" = i18n_r()$t("Payback period"),
        "Cost of Raw Materials" = i18n_r()$t("Cost of Raw Materials"),
        "Cost of Utilities" = i18n_r()$t("Cost of Utilities"),
        "Cost of Manufacturing" = i18n_r()$t("Cost of Manufacturing")
      )


      unit_translations <- c(
        "none" = i18n_r()$t("none"),
        "kg/cycle" = i18n_r()$t("kg/cycle"),
        "kg/month" = i18n_r()$t("kg/month"),
        "USD/month" = i18n_r()$t("USD/month"),
        "kWh/month" = i18n_r()$t("kWh/month"),
        "USD/kg" = i18n_r()$t("USD/kg"),
        "percent" = i18n_r()$t("percent"),
        "years" = i18n_r()$t("years")
      )
      dt_data <- display_data
      dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)
      dt_data$Unit <- my_mapvalues(dt_data$Unit, names(unit_translations), unit_translations, warn_missing = FALSE)

      DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 32,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_com_analysis_detailed_data")
        )
      )
    },
    server = FALSE,
    escape = FALSE
  )

  # Reactive for input parameters data
  input_params_data <- reactive({
    req(results$com_result)
    input_params <- results$com_result$input

    # Format the dataframe for display: Description, Value, Unit, Type
    display_data <- input_params %>%
      select(description, value, units, type) %>%
      rename(
        "Description" = description,
        "Value" = value,
        "Unit" = units,
        "Type" = type
      ) %>%
      mutate(Value = format(Value, big.mark = ",", digits = 4, scientific = FALSE)) # Format Value column with more decimals

    # Add flow measurement parameters from stored params
    if (!is.null(results$params)) {
      flowpar <- results$params$flowpar
      
      if (is.list(flowpar) && length(flowpar) >= 1) {
        # Manual flow parameters specified
        flowpar_pres <- flowpar[[1]][1]
        flowpar_temp <- flowpar[[1]][2]
      } else if (identical(flowpar, "auto")) {
        # Auto mode - get values from recovery line parameters or defaults
        crm <- results$params$crm
        if (!is.null(crm) && !is.na(crm[["recp"]]) && !is.na(crm[["rect"]])) {
          flowpar_pres <- crm[["recp"]]
          flowpar_temp <- crm[["rect"]]
        } else {
          # Defaults based on mode
          if (!is.null(results$params$comode) && results$params$comode == "swe") {
            flowpar_pres <- 1
            flowpar_temp <- 25
          } else {
            flowpar_pres <- 60
            flowpar_temp <- 10
          }
        }
      } else {
        # Fallback defaults
        flowpar_pres <- 60
        flowpar_temp <- 10
      }
      
      # Create rows for flow parameters
      flow_params_rows <- data.frame(
        Description = c("Flow measurement pressure", "Flow measurement temperature"),
        Value = c(format(flowpar_pres, big.mark = ",", digits = 4, scientific = FALSE),
                  format(flowpar_temp, big.mark = ",", digits = 4, scientific = FALSE)),
        Unit = c("bar", "°C"),
        Type = c("gen", "gen"),
        stringsAsFactors = FALSE
      )
      
      # Append flow parameters to display data
      display_data <- rbind(display_data, flow_params_rows)
    }

    return(display_data)
  })

  # Render Input Parameters Table
  output$input_params_table <- DT::renderDataTable(
    {
      req(input_params_data()) # Ensure data is available

      display_data <- input_params_data()

      # Translate column headers
      add_prettynames <- c(
        i18n_r()$t("Description"),
        i18n_r()$t("Value"),
        i18n_r()$t("Unit"),
        i18n_r()$t("Type")
      )

      # Translation mappings for Description column
      description_translations <- c(
        "Extraction vessel volume" = i18n_r()$t("Extraction vessel volume"),
        "Material loading per batch" = i18n_r()$t("Material loading per batch"),
        "Feed flow rate" = i18n_r()$t("Feed flow rate"),
        "Feed density" = i18n_r()$t("Feed density"),
        "Extraction pressure" = i18n_r()$t("Extraction pressure"),
        "Extraction temperature" = i18n_r()$t("Extraction temperature"),
        "Main solvent flow (CO2 or water; mL/min or g/min)" = i18n_r()$t("Main solvent flow (CO2 or water; mL/min or g/min)"),
        "Additional batch processing time (changeover etc.)" = i18n_r()$t("Additional batch processing time (changeover etc.)"),
        "Dilution factor of the crude extract (if any)" = i18n_r()$t("Dilution factor of the crude extract (if any)"),
        "Sale price of the final extract" = i18n_r()$t("Sale price of the final extract"),
        "Co-solvent flow (EtOH or CO2; mL/min or g/min)" = i18n_r()$t("Co-solvent flow (EtOH or CO2; mL/min or g/min)"),
        "Material bed height" = i18n_r()$t("Material bed height"),
        "Extraction vessel internal diameter" = i18n_r()$t("Extraction vessel internal diameter"),
        "Price of extractable material" = i18n_r()$t("Price of extractable material"),
        "Price of the main solvent (CO2 or H2O)" = i18n_r()$t("Price of the main solvent (CO2 or H2O)"),
        "Price of co-solvent (EtOH or CO2)" = i18n_r()$t("Price of co-solvent (EtOH or CO2)"),
        "CO2 recovery line pressure" = i18n_r()$t("CO2 recovery line pressure"),
        "CO2 recovery line temperature" = i18n_r()$t("CO2 recovery line temperature"),
        "Final CO2 separator temperature" = i18n_r()$t("Final CO2 separator temperature"),
        "Fraction of collected co-solvent lost during post-processing" = i18n_r()$t("Fraction of collected co-solvent lost during post-processing"),
        "Main system power usage" = i18n_r()$t("Main system power usage"),
        "Price of electricity" = i18n_r()$t("Price of electricity"),
        "Power usage for drying (pre-processing)" = i18n_r()$t("Power usage for drying (pre-processing)"),
        "Power usage for comminution (pre-processing)" = i18n_r()$t("Power usage for comminution (pre-processing)"),
        "Power usage for evaporation (post-processing)" = i18n_r()$t("Power usage for evaporation (post-processing)"),
        "Hourly drying capacity (pre-processing)" = i18n_r()$t("Hourly drying capacity (pre-processing)"),
        "Hourly comminution capacity (pre-processing)" = i18n_r()$t("Hourly comminution capacity (pre-processing)"),
        "Hourly evaporation capacity (post-processing)" = i18n_r()$t("Hourly evaporation capacity (post-processing)"),
        "Number of operator staff required" = i18n_r()$t("Number of operator staff required"),
        "Number of hours per work shift" = i18n_r()$t("Number of hours per work shift"),
        "Number of work shifts per calendar day" = i18n_r()$t("Number of work shifts per calendar day"),
        "Average monthly wage per operator" = i18n_r()$t("Average monthly wage per operator"),
        "Number of work days per month (28 days)" = i18n_r()$t("Number of work days per month (28 days)"),
        "Capital expenditure invested" = i18n_r()$t("Capital expenditure invested"),
        "Monthly maintenance costs" = i18n_r()$t("Monthly maintenance costs"),
        "Other monthly costs" = i18n_r()$t("Other monthly costs"),
        "Depreciation (as CAPEX fraction per year)" = i18n_r()$t("Depreciation (as CAPEX fraction per year)"),
        "Price of auxiliary material: oil" = i18n_r()$t("Price of auxiliary material: oil"),
        "Required fraction of auxiliary material: oil" = i18n_r()$t("Required fraction of auxiliary material: oil"),
        "Flow measurement pressure" = i18n_r()$t("Flow measurement pressure"),
        "Flow measurement temperature" = i18n_r()$t("Flow measurement temperature")
      )

      # Translation mappings for Unit column
      unit_translations <- c(
        "L" = i18n_r()$t("L"),
        "kg" = i18n_r()$t("kg"),
        "bar" = i18n_r()$t("bar"),
        "degC" = i18n_r()$t("°C"),
        "°C" = i18n_r()$t("°C"),
        "mL/min" = i18n_r()$t("mL/min"),
        "g/min" = i18n_r()$t("g/min"),
        "min" = i18n_r()$t("min"),
        "none" = i18n_r()$t("none"),
        "USD/kg" = i18n_r()$t("USD/kg"),
        "cm" = i18n_r()$t("cm"),
        "kW" = i18n_r()$t("kW"),
        "kWh" = i18n_r()$t("kWh"),
        "USD/kWh" = i18n_r()$t("USD/kWh"),
        "people" = i18n_r()$t("people"),
        "hours" = i18n_r()$t("hours"),
        "shifts/day" = i18n_r()$t("shifts/day"),
        "h/shift" = i18n_r()$t("h/shift"),
        "days/month" = i18n_r()$t("days/month"),
        "%/year" = i18n_r()$t("dimensionless"),
        "dimensionless" = i18n_r()$t("dimensionless"),
        "USD" = i18n_r()$t("USD"),
        "USD/month" = i18n_r()$t("USD/month"),
        "kg/h" = i18n_r()$t("kg/h"),
        "L/h" = i18n_r()$t("L/h")
      )

      # Translation mappings for Type column
      type_translations <- c(
        "general" = i18n_r()$t("general"),
        "gen" = i18n_r()$t("general"),
        "crm" = i18n_r()$t("crm"),
        "cut" = i18n_r()$t("cut"),
        "col" = i18n_r()$t("col"),
        "fci" = i18n_r()$t("fci"),
        "crm_aux" = i18n_r()$t("crm_aux")
      )

      # Apply translations to data
      dt_data <- display_data
      dt_data$Description <- my_mapvalues(dt_data$Description, names(description_translations), description_translations, warn_missing = FALSE)
      dt_data$Unit <- my_mapvalues(dt_data$Unit, names(unit_translations), unit_translations, warn_missing = FALSE)
      dt_data$Type <- my_mapvalues(dt_data$Type, names(type_translations), type_translations, warn_missing = FALSE)

      DT::datatable(
        dt_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 40,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_com_analysis_input_params")
        )
      )
    },
    server = FALSE
  )

  # ============================================================================
  # NEW: Enhanced Detailed Results Tab - Charts and Organized Tables
  # ============================================================================
  
  # Reactive for cost components data (CRM, CUT, COL, FMC)
  cost_components_data <- reactive({
    req(results$com_result)
    req(selected_simple_data())
    
    simple <- selected_simple_data()
    extra <- results$com_result$extra
    
    # Get COL and FMC from extra
    col_val <- as.numeric(extra["col"])
    fci_val <- as.numeric(extra["fci"])
    
    # Get CRM and CUT from simple_data
    crm_val <- as.numeric(simple$CRM)
    cut_val <- as.numeric(simple$CUT)
    com_val <- as.numeric(simple$COM)
    
    list(
      CRM = crm_val,
      CUT = cut_val,
      COL = col_val,
      FCI = fci_val,
      COM = com_val
    )
  })
  
  # PIE CHART - COM Breakdown
  output$com_pie_chart <- renderPlotly({
    req(cost_components_data())
    
    costs <- cost_components_data()
    
    # Short labels for legend, full names for tooltips
    labels <- c(
      i18n$t("CRM"),
      i18n$t("CUT"),
      i18n$t("COL"),
      i18n$t("FMC")
    )
    
    full_names <- c(
      i18n$t("Cost of Raw Materials"),
      i18n$t("Cost of Utilities"),
      i18n$t("Cost of Labour"),
      i18n$t("Fixed Costs")
    )
    
    values <- c(costs$CRM, costs$CUT, costs$COL, costs$FCI)
    
    # Filter out zero or NA values
    valid_idx <- !is.na(values) & values > 0
    labels <- labels[valid_idx]
    full_names <- full_names[valid_idx]
    values <- values[valid_idx]
    
    # Colors for each category
    colors <- c("#3498db", "#e74c3c", "#2ecc71", "#9b59b6")[valid_idx]
    
    # Calculate percentages for custom labels
    total <- sum(values)
    percentages <- round(values / total * 100, 1)
    
    # Create custom text labels with both absolute values and percentages
    custom_labels <- sapply(seq_along(values), function(i) {
      val_formatted <- if (values[i] >= 1000000) {
        paste0("$", round(values[i]/1000000, 1), "M")
      } else if (values[i] >= 1000) {
        paste0("$", round(values[i]/1000, 1), "K")
      } else {
        paste0("$", round(values[i]))
      }
      paste0(val_formatted, "\n(", percentages[i], "%)")
    })
    
    # Create pie chart with labels showing absolute values and percentages
    plot_ly(
      labels = labels,
      values = values,
      type = "pie",
      hole = 0.4,
      text = custom_labels,
      customdata = full_names,
      textinfo = "text",
      textposition = "inside",
      insidetextorientation = "horizontal",
      marker = list(colors = colors, line = list(color = "#FFFFFF", width = 2)),
      hovertemplate = paste0("<b>%{label}</b> - %{customdata}<br>",
                             i18n$t("Amount"), ": $%{value:,.0f}<br>",
                             i18n$t("Share"), ": %{percent}<extra></extra>")
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          x = 0.5, 
          xanchor = "center", 
          y = -0.1,
          font = list(size = 11)
        ),
        annotations = list(
          list(
            text = paste0("<b>", i18n$t("COM"), "</b><br>$", format(round(costs$COM), big.mark = ",")),
            x = 0.5, y = 0.5,
            font = list(size = 13, color = "#333"),
            showarrow = FALSE
          )
        ),
        margin = list(t = 20, b = 50, l = 20, r = 20)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # WATERFALL/BAR CHART - Cost Summary
  output$cost_waterfall_chart <- renderPlotly({
    req(cost_components_data())
    req(selected_simple_data())
    req(results$com_result)
    
    costs <- cost_components_data()
    simple <- selected_simple_data()
    com_result <- results$com_result
    
    # Check if we have multiple responses (response_details will exist)
    has_multiple_responses <- !is.null(com_result$response_details) && length(com_result$response_details) > 1
    
    # Data for waterfall-style bar chart
    base_categories <- c(
      i18n$t("CRM"),
      i18n$t("CUT"),
      i18n$t("COL"),
      i18n$t("FMC"),
      i18n$t("Total COM")
    )
    
    profit <- as.numeric(simple$monthly_profit)
    
    base_values <- c(
      -costs$CRM,
      -costs$CUT,
      -costs$COL,
      -costs$FCI,
      -costs$COM
    )
    
    base_colors <- c(
      "#3498db",  # CRM - blue
      "#e74c3c",  # CUT - red
      "#2ecc71",  # COL - green
      "#9b59b6",  # FMC - purple
      "#34495e"   # COM - dark gray
    )
    
    if (has_multiple_responses) {
      # Get response details - extract sales volumes for the selected time point
      response_details <- com_result$response_details
      
      # For multi-row data, we need to get the index of the selected row
      simple_output <- com_result$simple_output
      if (nrow(simple_output) > 1 && !is.null(results$selected_simple_data)) {
        # Find the row index based on time value
        selected_time <- results$selected_simple_data$time
        if (!is.null(selected_time)) {
          row_idx <- which(simple_output[, 1] == selected_time)[1]
          if (length(row_idx) == 0 || is.na(row_idx)) row_idx <- nrow(simple_output)
        } else {
          row_idx <- nrow(simple_output)
        }
      } else {
        row_idx <- 1
      }
      
      # Build stacked revenue data
      revenue_names <- c()
      revenue_values <- c()
      revenue_colors <- c("#27ae60", "#1abc9c", "#3498db", "#9b59b6")  # Different shades for up to 4 responses
      
      for (i in seq_along(response_details)) {
        resp <- response_details[[i]]
        resp_name <- resp$name
        # Get the sales volume for this response at the selected time point
        resp_svol <- if (length(resp$svol) > 1) resp$svol[row_idx] else resp$svol
        
        revenue_names <- c(revenue_names, resp_name)
        revenue_values <- c(revenue_values, resp_svol)
      }
      
      total_revenue <- sum(revenue_values, na.rm = TRUE)
      
      # Get tax rate from params
      taxrate <- results$params$taxrate
      has_tax <- !is.null(taxrate) && !is.na(taxrate) && taxrate > 0
      
      # Calculate gross profit (total revenue - COM)
      gross_profit <- total_revenue - costs$COM
      
      # Calculate net profit if tax rate is set
      if (has_tax) {
        net_profit <- gross_profit * (1 - taxrate / 100)
      }
      
      # Calculate profit contribution per response (proportional to revenue)
      profit_names <- c()
      profit_values_gross <- c()
      profit_values_net <- c()
      profit_colors <- c("#16a085", "#1abc9c", "#48c9b0", "#76d7c4")  # Teal shades for profit
      
      for (i in seq_along(response_details)) {
        resp_name <- response_details[[i]]$name
        resp_svol <- revenue_values[i]
        revenue_fraction <- if (total_revenue > 0) resp_svol / total_revenue else 0
        
        profit_names <- c(profit_names, resp_name)
        profit_values_gross <- c(profit_values_gross, gross_profit * revenue_fraction)
        if (has_tax) {
          profit_values_net <- c(profit_values_net, net_profit * revenue_fraction)
        }
      }
      
      # Calculate max value for determining text positioning
      max_abs_value <- max(c(abs(base_values), total_revenue, abs(gross_profit)), na.rm = TRUE)
      
      # Determine categories based on whether we have tax
      if (has_tax) {
        categories <- c(base_categories, i18n$t("Revenue"), i18n$t("Gross Profit"), i18n$t("Net Profit"))
      } else {
        categories <- c(base_categories, i18n$t("Revenue"), i18n$t("Gross Profit"))
      }
      
      # Determine text positions for cost bars based on relative size
      cost_text_positions <- ifelse(abs(base_values) / max_abs_value < 0.15, "outside", "inside")
      cost_text_colors <- ifelse(cost_text_positions == "inside", "white", "#333333")
      
      # Create traces for stacked bars
      plot <- plot_ly() %>%
        # Add cost bars (non-stacked)
        add_trace(
          x = base_categories,
          y = base_values,
          type = "bar",
          name = i18n$t("Costs"),
          marker = list(color = base_colors, line = list(color = "#FFFFFF", width = 1)),
          text = sapply(abs(base_values), function(x) {
            if (x >= 1000000) paste0("$", round(x/1000000, 1), "M")
            else if (x >= 1000) paste0("$", round(x/1000, 1), "K")
            else paste0("$", round(x))
          }),
          textposition = cost_text_positions,
          insidetextanchor = "middle",
          textfont = list(color = cost_text_colors, size = 10),
          hovertemplate = paste0("<b>%{x}</b><br>$%{y:,.0f}<extra></extra>"),
          showlegend = FALSE
        )
      
      # Add stacked revenue bars
      for (i in seq_along(revenue_names)) {
        plot <- plot %>%
          add_trace(
            x = i18n$t("Revenue"),
            y = revenue_values[i],
            type = "bar",
            name = revenue_names[i],
            marker = list(color = revenue_colors[min(i, length(revenue_colors))], line = list(color = "#FFFFFF", width = 1)),
            text = paste0(revenue_names[i], "\n$", format(round(revenue_values[i]), big.mark = ",")),
            textposition = if (revenue_values[i] / max_abs_value > 0.15) "inside" else "outside",
            insidetextanchor = "middle",
            textfont = list(color = if (revenue_values[i] / max_abs_value > 0.15) "white" else "#333", size = 9),
            hovertemplate = paste0("<b>", revenue_names[i], "</b><br>$%{y:,.0f}<extra></extra>")
          )
      }
      
      # Add stacked gross profit bars (per response)
      for (i in seq_along(profit_names)) {
        bar_color <- if (profit_values_gross[i] >= 0) profit_colors[min(i, length(profit_colors))] else "#c0392b"
        plot <- plot %>%
          add_trace(
            x = i18n$t("Gross Profit"),
            y = profit_values_gross[i],
            type = "bar",
            name = paste0(profit_names[i], " (", i18n$t("Gross"), ")"),
            marker = list(color = bar_color, line = list(color = "#FFFFFF", width = 1)),
            text = paste0(profit_names[i], "\n$", format(round(profit_values_gross[i]), big.mark = ",")),
            textposition = if (abs(profit_values_gross[i]) / max_abs_value > 0.15) "inside" else "outside",
            insidetextanchor = "middle",
            textfont = list(color = if (abs(profit_values_gross[i]) / max_abs_value > 0.15) "white" else "#333", size = 9),
            hovertemplate = paste0("<b>", profit_names[i], " (", i18n$t("Gross"), ")</b><br>$%{y:,.0f}<extra></extra>")
          )
      }
      
      # Add stacked net profit bars (per response) only if tax rate > 0
      if (has_tax) {
        net_profit_colors <- c("#2980b9", "#3498db", "#5dade2", "#85c1e9")  # Blue shades for net profit
        for (i in seq_along(profit_names)) {
          bar_color <- if (profit_values_net[i] >= 0) net_profit_colors[min(i, length(net_profit_colors))] else "#e74c3c"
          plot <- plot %>%
            add_trace(
              x = i18n$t("Net Profit"),
              y = profit_values_net[i],
              type = "bar",
              name = paste0(profit_names[i], " (", i18n$t("Net"), ")"),
              marker = list(color = bar_color, line = list(color = "#FFFFFF", width = 1)),
              text = paste0(profit_names[i], "\n$", format(round(profit_values_net[i]), big.mark = ",")),
              textposition = if (abs(profit_values_net[i]) / max_abs_value > 0.15) "inside" else "outside",
              insidetextanchor = "middle",
              textfont = list(color = if (abs(profit_values_net[i]) / max_abs_value > 0.15) "white" else "#333", size = 9),
              hovertemplate = paste0("<b>", profit_names[i], " (", i18n$t("Net"), ")</b><br>$%{y:,.0f}<extra></extra>")
            )
        }
      }
      
      plot <- plot %>%
        layout(
          barmode = "stack",
          xaxis = list(
            title = "",
            tickangle = -45,
            tickfont = list(size = 10),
            categoryorder = "array",
            categoryarray = categories
          ),
          yaxis = list(
            title = i18n$t("USD/month"),
            zeroline = TRUE,
            zerolinecolor = "#999",
            zerolinewidth = 2
          ),
          margin = list(t = 40, b = 60, l = 70, r = 30),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
      
      return(plot)
      
    } else {
      # Single response logic - also handle Gross/Net profit
      revenue <- as.numeric(simple$sales_volume)
      
      # Get tax rate from params
      taxrate <- results$params$taxrate
      has_tax <- !is.null(taxrate) && !is.na(taxrate) && taxrate > 0
      
      # Calculate gross profit (revenue - COM)
      gross_profit <- revenue - costs$COM
      
      # Build categories and values based on whether we have tax
      if (has_tax) {
        net_profit <- gross_profit * (1 - taxrate / 100)
        categories <- c(base_categories, i18n$t("Revenue"), i18n$t("Gross Profit"), i18n$t("Net Profit"))
        values <- c(base_values, revenue, gross_profit, net_profit)
        colors <- c(
          base_colors,
          "#27ae60",  # Revenue - green
          ifelse(gross_profit >= 0, "#16a085", "#c0392b"),  # Gross Profit - teal/red
          ifelse(net_profit >= 0, "#2980b9", "#e74c3c")  # Net Profit - blue/red
        )
      } else {
        categories <- c(base_categories, i18n$t("Revenue"), i18n$t("Gross Profit"))
        values <- c(base_values, revenue, gross_profit)
        colors <- c(
          base_colors,
          "#27ae60",  # Revenue - green
          ifelse(gross_profit >= 0, "#16a085", "#c0392b")  # Gross Profit - teal/red
        )
      }
      
      # Format labels for display (shorter format)
      text_labels <- sapply(abs(values), function(x) {
        if (x >= 1000000) {
          paste0("$", round(x/1000000, 1), "M")
        } else if (x >= 1000) {
          paste0("$", round(x/1000, 1), "K")
        } else {
          paste0("$", round(x))
        }
      })
      
      # Determine text position based on bar size relative to max
      max_abs_value <- max(abs(values), na.rm = TRUE)
      text_positions <- ifelse(abs(values) / max_abs_value < 0.15, "outside", "inside")
      text_colors <- ifelse(text_positions == "inside", "white", "#333333")
      
      plot_ly(
        x = categories,
        y = values,
        type = "bar",
        marker = list(color = colors, line = list(color = "#FFFFFF", width = 1)),
        text = text_labels,
        textposition = text_positions,
        insidetextanchor = "middle",
        textfont = list(color = text_colors, size = 11),
        hovertemplate = paste0("<b>%{x}</b><br>$%{y:,.0f}<extra></extra>")
      ) %>%
        layout(
          xaxis = list(
            title = "",
            tickangle = -45,
            tickfont = list(size = 10),
            categoryorder = "array",
            categoryarray = categories
          ),
          yaxis = list(
            title = i18n$t("USD/month"),
            zeroline = TRUE,
            zerolinecolor = "#999",
            zerolinewidth = 2
          ),
          margin = list(t = 40, b = 80, l = 70, r = 30),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # VALUE BOXES for cost components
  output$crm_value_box <- renderValueBox({
    req(cost_components_data())
    costs <- cost_components_data()
    pct <- if(costs$COM > 0) round(costs$CRM / costs$COM * 100, 1) else 0
    
    valueBox(
      value = paste0("$", format(round(costs$CRM), big.mark = ",")),
      subtitle = HTML(paste0(i18n$t("Raw Materials (CRM)"), "<br><small>", pct, "% ", i18n$t("of COM"), "</small>")),
      icon = icon("leaf"),
      color = "blue"
    )
  })
  
  output$cut_value_box <- renderValueBox({
    req(cost_components_data())
    costs <- cost_components_data()
    pct <- if(costs$COM > 0) round(costs$CUT / costs$COM * 100, 1) else 0
    
    valueBox(
      value = paste0("$", format(round(costs$CUT), big.mark = ",")),
      subtitle = HTML(paste0(i18n$t("Utilities (CUT)"), "<br><small>", pct, "% ", i18n$t("of COM"), "</small>")),
      icon = icon("bolt"),
      color = "red"
    )
  })
  
  output$col_value_box <- renderValueBox({
    req(cost_components_data())
    costs <- cost_components_data()
    pct <- if(costs$COM > 0) round(costs$COL / costs$COM * 100, 1) else 0
    
    valueBox(
      value = paste0("$", format(round(costs$COL), big.mark = ",")),
      subtitle = HTML(paste0(i18n$t("Labour (COL)"), "<br><small>", pct, "% ", i18n$t("of COM"), "</small>")),
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$fci_value_box <- renderValueBox({
    req(cost_components_data())
    costs <- cost_components_data()
    pct <- if(costs$COM > 0) round(costs$FCI / costs$COM * 100, 1) else 0
    
    valueBox(
      value = paste0("$", format(round(costs$FCI), big.mark = ",")),
      subtitle = HTML(paste0(i18n$t("Fixed Costs (FMC)"), "<br><small>", pct, "% ", i18n$t("of COM"), "</small>")),
      icon = icon("building"),
      color = "purple"
    )
  })
  
  # Helper function to create compact DataTable
  render_compact_dt <- function(data, colnames_vec) {
    DT::datatable(
      data,
      colnames = colnames_vec,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        language = tablang()
      ),
      rownames = FALSE,
      class = "compact stripe"
    )
  }
  
  # PRODUCTION OVERVIEW TABLE
  output$production_table <- DT::renderDataTable({
    req(selected_detailed_data())
    req(selected_simple_data())
    
    detailed <- selected_detailed_data()
    simple <- selected_simple_data()
    response_details <- results$com_result$response_details
    
    # Check if we have multiple responses
    has_multiple_responses <- !is.null(response_details) && length(response_details) > 1
    
    # Extract production-related rows (exclude "Extract produced" if multiple responses - we'll add breakdown instead)
    if (has_multiple_responses) {
      production_params <- c(
        "Extraction cycles per month",
        "Monthly operating time",
        "Monthly feed processed",
        "S/M mass ratio (CO2)",
        "S/M mass ratio (co-solvent)",
        "S/F mass ratio (CO2)",
        "S/F mass ratio (co-solvent)"
      )
    } else {
      production_params <- c(
        "Extraction cycles per month",
        "Monthly operating time",
        "Monthly feed processed",
        "S/M mass ratio (CO2)",
        "S/M mass ratio (co-solvent)",
        "S/F mass ratio (CO2)",
        "S/F mass ratio (co-solvent)",
        "Extract produced (monthly)"
      )
    }
    
    prod_data <- detailed %>%
      filter(Description %in% production_params)
    
    # Add translated descriptions
    desc_trans <- c(
      "Extraction cycles per month" = i18n$t("Extraction cycles per month"),
      "Monthly operating time" = i18n$t("Monthly operating time"),
      "Monthly feed processed" = i18n$t("Monthly feed processed"),
      "S/M mass ratio (CO2)" = i18n$t("S/M mass ratio (CO2)"),
      "S/M mass ratio (co-solvent)" = i18n$t("S/M mass ratio (co-solvent)"),
      "S/F mass ratio (CO2)" = i18n$t("S/F mass ratio (CO2)"),
      "S/F mass ratio (co-solvent)" = i18n$t("S/F mass ratio (co-solvent)"),
      "Extract produced (monthly)" = i18n$t("Extract produced (monthly)")
    )
    prod_data$Description <- my_mapvalues(prod_data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    unit_trans <- c(
      "none" = i18n$t("none"),
      "hours" = i18n$t("hours"),
      "kg" = i18n$t("kg"),
      "kg/month" = i18n$t("kg/month")
    )
    prod_data$Unit <- my_mapvalues(prod_data$Unit, names(unit_trans), unit_trans, warn_missing = FALSE)
    
    # Add per-response extract breakdown if multiple responses exist
    if (has_multiple_responses) {
      # Get current time index
      slider_value <- input$time_slider
      if (is.null(slider_value) && !is.null(input$time_slider_detailed)) {
        slider_value <- input$time_slider_detailed
      }
      row_idx <- 1
      if (!is.null(slider_value) && !is.null(results$simple_data)) {
        row_idx <- which.min(abs(results$simple_data$time - slider_value))
      }
      
      # Add separator row
      prod_data <- rbind(prod_data, data.frame(
        Description = paste0("--- ", i18n$t("Extract produced (monthly) by response"), " ---"),
        Value = "",
        Unit = "",
        stringsAsFactors = FALSE
      ))
      
      # Add per-response rows and calculate total
      total_extract <- 0
      for (i in seq_along(response_details)) {
        resp <- response_details[[i]]
        extract_val <- if (length(resp$extract) > 1) resp$extract[row_idx] else resp$extract
        total_extract <- total_extract + extract_val
        prod_data <- rbind(prod_data, data.frame(
          Description = paste0("  ", resp$name),
          Value = round(extract_val, 2),
          Unit = i18n$t("kg/month"),
          stringsAsFactors = FALSE
        ))
      }
      
      # Add Total row at the bottom
      prod_data <- rbind(prod_data, data.frame(
        Description = paste0("  ", i18n$t("Total")),
        Value = round(total_extract, 2),
        Unit = i18n$t("kg/month"),
        stringsAsFactors = FALSE
      ))
    }
    
    render_compact_dt(prod_data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # CRM TABLE - Raw Materials Details
  output$crm_table <- DT::renderDataTable({
    req(selected_detailed_data())
    
    detailed <- selected_detailed_data()
    
    # Extract CRM-related rows
    crm_params <- c(
      "Main solvent loss per cycle (CO2 or water)",
      "Co-solvent loss per batch (EtOH or CO2)",
      "CO2 loss per month",
      "Co-solvent loss per month",
      "Raw material requirement (monthly)",
      "Main solvent requirement (monthly)",
      "Co-solvent requirement (monthly)",
      "Requirement of all auxiliary materials (monthly)",
      "Raw material cost (monthly)",
      "Main solvent cost (monthly)",
      "Co-solvent cost (monthly)",
      "Cost of all auxiliary materials (monthly)",
      "Cost of Raw Materials"
    )
    
    crm_data <- detailed %>%
      filter(Description %in% crm_params)
    
    # Add translated descriptions
    desc_trans <- c(
      "Main solvent loss per cycle (CO2 or water)" = i18n$t("Main solvent loss per cycle (CO2 or water)"),
      "Co-solvent loss per batch (EtOH or CO2)" = i18n$t("Co-solvent loss per batch (EtOH or CO2)"),
      "CO2 loss per month" = i18n$t("CO2 loss per month"),
      "Co-solvent loss per month" = i18n$t("Co-solvent loss per month"),
      "Raw material requirement (monthly)" = i18n$t("Raw material requirement (monthly)"),
      "Main solvent requirement (monthly)" = i18n$t("Main solvent requirement (monthly)"),
      "Co-solvent requirement (monthly)" = i18n$t("Co-solvent requirement (monthly)"),
      "Requirement of all auxiliary materials (monthly)" = i18n$t("Requirement of all auxiliary materials (monthly)"),
      "Raw material cost (monthly)" = i18n$t("Raw material cost (monthly)"),
      "Main solvent cost (monthly)" = i18n$t("Main solvent cost (monthly)"),
      "Co-solvent cost (monthly)" = i18n$t("Co-solvent cost (monthly)"),
      "Cost of all auxiliary materials (monthly)" = i18n$t("Cost of all auxiliary materials (monthly)"),
      "Cost of Raw Materials" = i18n$t("Cost of Raw Materials")
    )
    crm_data$Description <- my_mapvalues(crm_data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    unit_trans <- c(
      "kg/cycle" = i18n$t("kg/cycle"),
      "kg/month" = i18n$t("kg/month"),
      "USD/month" = i18n$t("USD/month")
    )
    crm_data$Unit <- my_mapvalues(crm_data$Unit, names(unit_trans), unit_trans, warn_missing = FALSE)
    
    render_compact_dt(crm_data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # CUT TABLE - Utilities Details
  output$cut_table <- DT::renderDataTable({
    req(selected_detailed_data())
    
    detailed <- selected_detailed_data()
    
    # Extract CUT-related rows
    cut_params <- c(
      "Main power requirement (monthly)",
      "Additional power requirement (monthly)",
      "Drying power requirement (monthly)",
      "Comminution power requirement (monthly)",
      "Evaporation power requirement (monthly)",
      "Main power cost (monthly)",
      "Additional power cost (monthly)",
      "Drying power cost (monthly)",
      "Comminution power cost (monthly)",
      "Evaporation power cost (monthly)",
      "Cost of Utilities"
    )
    
    cut_data <- detailed %>%
      filter(Description %in% cut_params)
    
    # Add translated descriptions
    desc_trans <- c(
      "Main power requirement (monthly)" = i18n$t("Main power requirement (monthly)"),
      "Additional power requirement (monthly)" = i18n$t("Additional power requirement (monthly)"),
      "Drying power requirement (monthly)" = i18n$t("Drying power requirement (monthly)"),
      "Comminution power requirement (monthly)" = i18n$t("Comminution power requirement (monthly)"),
      "Evaporation power requirement (monthly)" = i18n$t("Evaporation power requirement (monthly)"),
      "Main power cost (monthly)" = i18n$t("Main power cost (monthly)"),
      "Additional power cost (monthly)" = i18n$t("Additional power cost (monthly)"),
      "Drying power cost (monthly)" = i18n$t("Drying power cost (monthly)"),
      "Comminution power cost (monthly)" = i18n$t("Comminution power cost (monthly)"),
      "Evaporation power cost (monthly)" = i18n$t("Evaporation power cost (monthly)"),
      "Cost of Utilities" = i18n$t("Cost of Utilities")
    )
    cut_data$Description <- my_mapvalues(cut_data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    unit_trans <- c(
      "kWh/month" = i18n$t("kWh/month"),
      "USD/month" = i18n$t("USD/month")
    )
    cut_data$Unit <- my_mapvalues(cut_data$Unit, names(unit_trans), unit_trans, warn_missing = FALSE)
    
    render_compact_dt(cut_data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # COL & FMC TABLE - Cost of Labour and Fixed Manufacturing Costs
  output$col_fmc_table <- DT::renderDataTable({
    req(results$com_result)
    req(results$params)
    
    params <- results$params
    col_params <- params$col
    fci_params <- params$fci
    
    # COL
    total_personnel <- col_params[["oper"]] * col_params[["shifts"]]
    COL_total <- col_params[["wage"]] * total_personnel
    
    # FMC
    base_fmc <- sum(c(fci_params[["maint"]], fci_params[["other"]]), na.rm = TRUE)
    depr <- fci_params[["depr"]]
    depr_monthly <- if (!is.na(depr)) (fci_params[["capex"]] * depr) / 12 else 0
    FMC_total <- base_fmc + depr_monthly
    
    col_fmc_data <- data.frame(
      Description = c(
        paste0(i18n$t("Cost of Labour (COL)"), " (", total_personnel, " ", i18n$t("people"), ")"),
        i18n$t("Fixed Manufacturing Costs (FMC)")
      ),
      Value = c(
        format(round(COL_total, 2), big.mark = ","),
        format(round(FMC_total, 2), big.mark = ",")
      ),
      Unit = c(
        i18n$t("USD/month"),
        i18n$t("USD/month")
      ),
      stringsAsFactors = FALSE
    )
    
    render_compact_dt(col_fmc_data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # FINANCIAL TABLE - Economic Results (Combined Summary)
  output$financial_table <- DT::renderDataTable({
    req(selected_detailed_data())
    req(results$com_result)
    
    detailed <- selected_detailed_data()
    simple <- selected_simple_data()
    response_details <- results$com_result$response_details
    
    # Get current time index for multi-value data
    slider_value <- input$time_slider
    if (is.null(slider_value) && !is.null(input$time_slider_detailed)) {
      slider_value <- input$time_slider_detailed
    }
    row_idx <- 1
    if (!is.null(slider_value) && !is.null(results$simple_data)) {
      row_idx <- which.min(abs(results$simple_data$time - slider_value))
    }
    
    # Extract financial rows
    financial_params <- c(
      "Specific extract cost",
      "Sales volume (monthly)",
      "Monthly gross profit (pre-tax)",
      "Monthly profit (after tax)",
      "Profit margin (monthly)",
      "Payback period",
      "Cost of Manufacturing"
    )
    
    fin_data <- detailed %>%
      filter(Description %in% financial_params)
    
    # Add translated descriptions
    desc_trans <- c(
      "Specific extract cost" = i18n$t("Specific extract cost"),
      "Sales volume (monthly)" = i18n$t("Sales volume (monthly)"),
      "Monthly gross profit (pre-tax)" = i18n$t("Monthly gross profit (pre-tax)"),
      "Monthly profit (after tax)" = i18n$t("Monthly profit (after tax)"),
      "Profit margin (monthly)" = i18n$t("Profit margin (monthly)"),
      "Payback period" = i18n$t("Payback period"),
      "Cost of Manufacturing" = i18n$t("Cost of Manufacturing")
    )
    fin_data$Description <- my_mapvalues(fin_data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    unit_trans <- c(
      "USD/kg" = i18n$t("USD/kg"),
      "USD/month" = i18n$t("USD/month"),
      "percent" = i18n$t("percent"),
      "years" = i18n$t("years")
    )
    fin_data$Unit <- my_mapvalues(fin_data$Unit, names(unit_trans), unit_trans, warn_missing = FALSE)
    
    # If multiple responses, add weighted average price
    if (!is.null(response_details) && length(response_details) > 1) {
      # Calculate totals
      total_extract <- sum(sapply(response_details, function(r) {
        if (length(r$extract) > 1) r$extract[row_idx] else r$extract
      }))
      total_revenue <- sum(sapply(response_details, function(r) {
        if (length(r$svol) > 1) r$svol[row_idx] else r$svol
      }))
      weighted_avg_price <- if (total_extract > 0) total_revenue / total_extract else 0
      
      # Add weighted average price row
      fin_data <- rbind(fin_data, data.frame(
        Description = i18n$t("Weighted average price"),
        Value = round(weighted_avg_price, 2),
        Unit = i18n$t("USD/kg"),
        stringsAsFactors = FALSE
      ))
    }
    
    render_compact_dt(fin_data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # PER-RESPONSE EXTRACT BREAKDOWN - now returns NULL (merged into production_table)
  output$per_response_extract_ui <- renderUI({
    return(NULL)  # Merged into production_table
  })
  
  # PER-RESPONSE FINANCIAL TABLES (shown when multiple responses exist)
  output$per_response_revenue_ui <- renderUI({
    req(results$com_result)
    
    response_details <- results$com_result$response_details
    if (is.null(response_details) || length(response_details) <= 1) {
      return(NULL)  # Only show for multiple responses
    }
    
    # Get current time index
    slider_value <- input$time_slider
    if (is.null(slider_value) && !is.null(input$time_slider_detailed)) {
      slider_value <- input$time_slider_detailed
    }
    row_idx <- 1
    if (!is.null(slider_value) && !is.null(results$simple_data)) {
      row_idx <- which.min(abs(results$simple_data$time - slider_value))
    }
    
    # Get COM from simple_data (SC_per_kg * total_extract = COM)
    simple <- selected_simple_data()
    total_extract <- sum(sapply(response_details, function(r) {
      if (length(r$extract) > 1) r$extract[row_idx] else r$extract
    }))
    total_revenue <- sum(sapply(response_details, function(r) {
      if (length(r$svol) > 1) r$svol[row_idx] else r$svol
    }))
    # Overall specific cost (same for all responses since it's a shared production process)
    overall_specific_cost <- if (!is.null(simple)) simple$SC_per_kg else 0
    com_total <- overall_specific_cost * total_extract
    
    # Calculate total gross profit for contribution to payback calculation
    total_gross_profit <- total_revenue - com_total
    
    # Build UI with a table for each response
    table_list <- lapply(seq_along(response_details), function(i) {
      resp <- response_details[[i]]
      extract_val <- if (length(resp$extract) > 1) resp$extract[row_idx] else resp$extract
      svol_val <- if (length(resp$svol) > 1) resp$svol[row_idx] else resp$svol
      
      # Calculate this response's share of COM (proportional to extract)
      extract_share <- if (total_extract > 0) extract_val / total_extract else 0
      resp_com <- com_total * extract_share
      resp_profit <- svol_val - resp_com
      resp_margin <- if (svol_val > 0) (resp_profit / svol_val) * 100 else 0
      
      # Calculate Sole Production Cost: what would the specific cost be if this response were the only one produced?
      # This is total COM divided by this response's extract alone
      sole_production_cost <- if (extract_val > 0) com_total / extract_val else 0
      
      # Calculate contribution to payback: percentage of total profit this response provides
      payback_contribution <- if (total_gross_profit > 0) (resp_profit / total_gross_profit) * 100 else 0
      
      # Build data frame for this response
      resp_data <- data.frame(
        Description = c(
          i18n$t("Extract produced (monthly)"),
          i18n$t("Sale price"),
          i18n$t("Sole production cost"),
          i18n$t("Sales volume (monthly)"),
          i18n$t("Cost share (proportional)"),
          i18n$t("Gross profit"),
          i18n$t("Profit margin"),
          i18n$t("Contribution to payback")
        ),
        Value = c(
          round(extract_val, 2),
          round(resp$pr_sale, 2),
          round(sole_production_cost, 2),
          round(svol_val, 2),
          round(resp_com, 2),
          round(resp_profit, 2),
          round(resp_margin, 2),
          round(payback_contribution, 1)
        ),
        Unit = c(
          i18n$t("kg/month"),
          i18n$t("USD/kg"),
          i18n$t("USD/kg"),
          i18n$t("USD/month"),
          i18n$t("USD/month"),
          i18n$t("USD/month"),
          i18n$t("percent"),
          i18n$t("percent")
        ),
        stringsAsFactors = FALSE
      )
      
      tagList(
        tags$hr(style = "margin: 15px 0; border-top: 1px dashed #ccc;"),
        tags$label(resp$name, style = "font-weight: bold; margin-bottom: 8px; display: block; font-size: 14px; color: #2c3e50;"),
        DT::renderDataTable({
          DT::datatable(
            resp_data,
            colnames = c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")),
            options = list(
              dom = "t",
              paging = FALSE,
              searching = FALSE,
              ordering = FALSE
            ),
            rownames = FALSE
          ) %>%
            formatRound(columns = "Value", digits = 2)
        }, server = FALSE)
      )
    })
    
    do.call(tagList, table_list)
  })

  # INPUT PARAMETERS TABLES - Organized by category
  # Helper for unit translations
  translate_units <- function(data, i18n) {
    unit_trans <- c(
      "L" = i18n$t("L"),
      "kg" = i18n$t("kg"),
      "bar" = i18n$t("bar"),
      "°C" = i18n$t("°C"),
      "degC" = i18n$t("°C"),
      "mL/min" = i18n$t("mL/min"),
      "g/min" = i18n$t("g/min"),
      "min" = i18n$t("min"),
      "none" = i18n$t("none"),
      "USD/kg" = i18n$t("USD/kg"),
      "USD" = i18n$t("USD"),
      "USD/month" = i18n$t("USD/month"),
      "USD/kWh" = i18n$t("USD/kWh"),
      "kW" = i18n$t("kW"),
      "kWh" = i18n$t("kWh"),
      "kg/h" = i18n$t("kg/h"),
      "L/h" = i18n$t("L/h"),
      "cm" = i18n$t("cm"),
      "shifts/day" = i18n$t("shifts/day"),
      "h/shift" = i18n$t("h/shift"),
      "hours" = i18n$t("hours"),
      "people" = i18n$t("people"),
      "days/month" = i18n$t("days/month"),
      "%/year" = i18n$t("dimensionless"),
      "dimensionless" = i18n$t("dimensionless")
    )
    data$Unit <- my_mapvalues(data$Unit, names(unit_trans), unit_trans, warn_missing = FALSE)
    return(data)
  }
  
  # General Parameters Table
  output$input_general_table <- DT::renderDataTable({
    req(input_params_data())
    
    data <- input_params_data() %>%
      filter(Type %in% c("gen", "general", i18n$t("general")))
    
    # Translate descriptions
    desc_trans <- c(
      "Extraction vessel volume" = i18n$t("Extraction vessel volume"),
      "Material loading per batch" = i18n$t("Material loading per batch"),
      "Feed flow rate" = i18n$t("Feed flow rate"),
      "Feed density" = i18n$t("Feed density"),
      "Extraction pressure" = i18n$t("Extraction pressure"),
      "Extraction temperature" = i18n$t("Extraction temperature"),
      "Main solvent flow (CO2 or water; mL/min or g/min)" = i18n$t("Main solvent flow (CO2 or water; mL/min or g/min)"),
      "Additional batch processing time (changeover etc.)" = i18n$t("Additional batch processing time (changeover etc.)"),
      "Dilution factor of the crude extract (if any)" = i18n$t("Dilution factor of the crude extract (if any)"),
      "Sale price of the final extract" = i18n$t("Sale price of the final extract"),
      "Co-solvent flow (EtOH or CO2; mL/min or g/min)" = i18n$t("Co-solvent flow (EtOH or CO2; mL/min or g/min)"),
      "Flow measurement pressure" = i18n$t("Flow measurement pressure"),
      "Flow measurement temperature" = i18n$t("Flow measurement temperature")
    )
    data$Description <- my_mapvalues(data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    # Translate units
    data <- translate_units(data, i18n)
    
    data <- data %>% select(-Type)
    
    render_compact_dt(data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # CRM Input Parameters Table
  output$input_crm_table <- DT::renderDataTable({
    req(input_params_data())
    
    data <- input_params_data() %>%
      filter(Type %in% c("crm", "crm_aux", i18n$t("crm"), i18n$t("crm_aux")))
    
    desc_trans <- c(
      "Material bed height" = i18n$t("Material bed height"),
      "Extraction vessel internal diameter" = i18n$t("Extraction vessel internal diameter"),
      "Price of extractable material" = i18n$t("Price of extractable material"),
      "Price of the main solvent (CO2 or H2O)" = i18n$t("Price of the main solvent (CO2 or H2O)"),
      "Price of co-solvent (EtOH or CO2)" = i18n$t("Price of co-solvent (EtOH or CO2)"),
      "CO2 recovery line pressure" = i18n$t("CO2 recovery line pressure"),
      "CO2 recovery line temperature" = i18n$t("CO2 recovery line temperature"),
      "Final CO2 separator temperature" = i18n$t("Final CO2 separator temperature"),
      "Fraction of collected co-solvent lost during post-processing" = i18n$t("Fraction of collected co-solvent lost during post-processing")
    )
    data$Description <- my_mapvalues(data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    # Translate units
    data <- translate_units(data, i18n)
    
    data <- data %>% select(-Type)
    
    render_compact_dt(data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # CUT Input Parameters Table
  output$input_cut_table <- DT::renderDataTable({
    req(input_params_data())
    
    data <- input_params_data() %>%
      filter(Type %in% c("cut", i18n$t("cut")))
    
    desc_trans <- c(
      "Main system power usage" = i18n$t("Main system power usage"),
      "Price of electricity" = i18n$t("Price of electricity"),
      "Power usage for drying (pre-processing)" = i18n$t("Power usage for drying (pre-processing)"),
      "Power usage for comminution (pre-processing)" = i18n$t("Power usage for comminution (pre-processing)"),
      "Power usage for evaporation (post-processing)" = i18n$t("Power usage for evaporation (post-processing)"),
      "Hourly drying capacity (pre-processing)" = i18n$t("Hourly drying capacity (pre-processing)"),
      "Hourly comminution capacity (pre-processing)" = i18n$t("Hourly comminution capacity (pre-processing)"),
      "Hourly evaporation capacity (post-processing)" = i18n$t("Hourly evaporation capacity (post-processing)")
    )
    data$Description <- my_mapvalues(data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    # Translate units
    data <- translate_units(data, i18n)
    
    data <- data %>% select(-Type)
    
    render_compact_dt(data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # COL Input Parameters Table
  output$input_col_table <- DT::renderDataTable({
    req(input_params_data())
    
    data <- input_params_data() %>%
      filter(Type %in% c("col", i18n$t("col")))
    
    desc_trans <- c(
      "Number of operator staff required" = i18n$t("Number of operator staff required"),
      "Number of hours per work shift" = i18n$t("Number of hours per work shift"),
      "Number of work shifts per calendar day" = i18n$t("Number of work shifts per calendar day"),
      "Average monthly wage per operator" = i18n$t("Average monthly wage per operator"),
      "Number of work days per month (28 days)" = i18n$t("Number of work days per month (28 days)")
    )
    data$Description <- my_mapvalues(data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    # Translate units
    data <- translate_units(data, i18n)
    
    data <- data %>% select(-Type)
    
    render_compact_dt(data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)
  
  # FMC Input Parameters Table
  output$input_fci_table <- DT::renderDataTable({
    req(input_params_data())
    
    data <- input_params_data() %>%
      filter(Type %in% c("fci", i18n$t("fci")))
    
    desc_trans <- c(
      "Capital expenditure invested" = i18n$t("Capital expenditure invested"),
      "Monthly maintenance costs" = i18n$t("Monthly maintenance costs"),
      "Other monthly costs" = i18n$t("Other monthly costs"),
      "Depreciation (as CAPEX fraction per year)" = i18n$t("Depreciation (as CAPEX fraction per year)"),
      "Tax rate (for net profit calculation)" = i18n$t("Tax rate (for net profit calculation)")
    )
    data$Description <- my_mapvalues(data$Description, names(desc_trans), desc_trans, warn_missing = FALSE)
    
    # Translate units
    data <- translate_units(data, i18n)
    
    data <- data %>% select(-Type)
    
    render_compact_dt(data, c(i18n$t("Parameter"), i18n$t("Value"), i18n$t("Unit")))
  }, server = FALSE)

  # ============================================================================
  # END: Enhanced Detailed Results Tab
  # ============================================================================

  # Dynamic UI for Payback plot Y-axis range slider
  output$payback_y_range_ui <- renderUI({
    req(results$simple_data)
    req(results$is_multi_value)
    req(results$com_result)
    req(results$com_result$plots$payback)
    
    # Get min/max payback values from data - check for valid values
    payback_vals <- results$simple_data$payback_yr
    
    # Filter out negative values and detect outliers
    valid_payback <- payback_vals[is.finite(payback_vals) & payback_vals > 0]
    
    # Only show slider if we have valid payback values
    if (length(valid_payback) < 2) return(NULL)
    
    # Detect outliers using IQR method - values significantly above typical range
    q1 <- quantile(valid_payback, 0.25, na.rm = TRUE)
    q3 <- quantile(valid_payback, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    upper_fence <- q3 + 3 * iqr  # Using 3*IQR for extreme outliers only
    
    # Filter to non-outlier values for default range
    non_outlier_payback <- valid_payback[valid_payback <= upper_fence]
    if (length(non_outlier_payback) < 1) non_outlier_payback <- valid_payback
    
    min_payback <- min(non_outlier_payback, na.rm = TRUE)
    max_payback <- max(non_outlier_payback, na.rm = TRUE)
    
    # Set range with buffer
    range_min <- max(0, floor(min_payback * 0.9))
    range_max <- ceiling(max_payback * 1.1)
    if (range_max < 1) range_max <- 1
    
    # Get current slider values or default to filtered range
    current_val <- isolate(input$payback_y_range)
    if (is.null(current_val) || length(current_val) != 2) {
      current_val <- c(range_min, range_max)
    }
    
    div(
      style = "margin-top: 10px; padding: 0 10px;",
      fluidRow(
        column(9,
          sliderInput(
            ns("payback_y_range"),
            i18n$t("Y-axis Range"),
            min = range_min,
            max = range_max,
            value = current_val,
            step = if (range_max > 10) 0.5 else 0.1
          )
        ),
        column(3,
          div(style = "padding-top: 25px;",
            actionButton(ns("payback_autoscale"), i18n$t("Autoscale"), 
              class = "btn-sm btn-default",
              style = "width: 100%; margin-bottom: 5px;",
              title = i18n$t("Autoscale to fit visible data")
            ),
            actionButton(ns("payback_reset"), i18n$t("Reset"), 
              class = "btn-sm btn-default",
              style = "width: 100%;",
              title = i18n$t("Reset to show all data")
            )
          )
        )
      )
    )
  })
  
  # Autoscale button for payback plot - scales to currently visible points (excluding outliers)
  observeEvent(input$payback_autoscale, {
    req(results$simple_data)
    
    # Get current Y-axis range from slider
    current_range <- input$payback_y_range
    
    # Get payback values - filter out negative and infinite
    payback_vals <- results$simple_data$payback_yr
    payback_vals <- payback_vals[is.finite(payback_vals) & payback_vals > 0]
    
    if (length(payback_vals) < 1) return()
    
    # If slider is set, filter to only points currently visible
    if (!is.null(current_range) && length(current_range) == 2) {
      visible_vals <- payback_vals[payback_vals >= current_range[1] & payback_vals <= current_range[2]]
      # Only use visible values if there are any, otherwise fall back to all
      if (length(visible_vals) >= 1) {
        payback_vals <- visible_vals
      }
    }
    
    min_payback <- min(payback_vals, na.rm = TRUE)
    max_payback <- max(payback_vals, na.rm = TRUE)
    range_min <- max(0, floor(min_payback * 0.9))
    range_max <- ceiling(max_payback * 1.1)
    if (range_max < 1) range_max <- 1
    
    updateSliderInput(session, "payback_y_range", value = c(range_min, range_max))
  })
  
  # Reset button for payback plot - resets to show filtered data (excluding negative and outliers)
  observeEvent(input$payback_reset, {
    req(results$simple_data)
    
    # Get all finite positive payback values
    payback_vals <- results$simple_data$payback_yr
    payback_vals <- payback_vals[is.finite(payback_vals) & payback_vals > 0]
    
    if (length(payback_vals) < 1) return()
    
    # Detect outliers using IQR method
    q1 <- quantile(payback_vals, 0.25, na.rm = TRUE)
    q3 <- quantile(payback_vals, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    upper_fence <- q3 + 3 * iqr
    
    # Filter to non-outlier values
    non_outlier_payback <- payback_vals[payback_vals <= upper_fence]
    if (length(non_outlier_payback) < 1) non_outlier_payback <- payback_vals
    
    min_payback <- min(non_outlier_payback, na.rm = TRUE)
    max_payback <- max(non_outlier_payback, na.rm = TRUE)
    range_min <- max(0, floor(min_payback * 0.9))
    range_max <- ceiling(max_payback * 1.1)
    if (range_max < 1) range_max <- 1
    
    updateSliderInput(session, "payback_y_range", value = c(range_min, range_max))
  })
  
  # Dynamic UI for Specific Cost plot Y-axis range slider
  output$sc_y_range_ui <- renderUI({
    req(results$simple_data)
    req(results$is_multi_value)
    req(results$com_result)
    req(results$com_result$plots$sc)
    
    # Get min/max SC values from data - check for valid values
    sc_vals <- results$simple_data$SC_per_kg
    sc_vals <- sc_vals[is.finite(sc_vals)]
    
    # Only show slider if we have valid SC values
    if (length(sc_vals) < 2) return(NULL)
    
    min_sc <- min(sc_vals, na.rm = TRUE)
    max_sc <- max(sc_vals, na.rm = TRUE)
    
    # Set range with buffer
    range_min <- max(0, floor(min_sc * 0.9))
    range_max <- ceiling(max_sc * 1.1)
    if (range_max < 1) range_max <- 1
    
    # Get current slider values or default to full range
    current_val <- isolate(input$sc_y_range)
    if (is.null(current_val) || length(current_val) != 2) {
      current_val <- c(range_min, range_max)
    }
    
    div(
      style = "margin-top: 10px; padding: 0 10px;",
      fluidRow(
        column(9,
          sliderInput(
            ns("sc_y_range"),
            i18n$t("Y-axis Range"),
            min = range_min,
            max = range_max,
            value = current_val,
            step = if (range_max > 10) 0.5 else 0.1
          )
        ),
        column(3,
          div(style = "padding-top: 25px;",
            actionButton(ns("sc_autoscale"), i18n$t("Autoscale"), 
              class = "btn-sm btn-default",
              style = "width: 100%; margin-bottom: 5px;",
              title = i18n$t("Autoscale to fit visible data")
            ),
            actionButton(ns("sc_reset"), i18n$t("Reset"), 
              class = "btn-sm btn-default",
              style = "width: 100%;",
              title = i18n$t("Reset to show all data")
            )
          )
        )
      )
    )
  })
  
  # Autoscale button for specific cost plot - scales to currently visible points
  observeEvent(input$sc_autoscale, {
    req(results$simple_data)
    
    # Get current Y-axis range from slider
    current_range <- input$sc_y_range
    
    # Get SC values
    sc_vals <- results$simple_data$SC_per_kg
    sc_vals <- sc_vals[is.finite(sc_vals)]
    
    if (length(sc_vals) < 1) return()
    
    # If slider is set, filter to only points currently visible
    if (!is.null(current_range) && length(current_range) == 2) {
      visible_vals <- sc_vals[sc_vals >= current_range[1] & sc_vals <= current_range[2]]
      # Only use visible values if there are any, otherwise fall back to all
      if (length(visible_vals) >= 1) {
        sc_vals <- visible_vals
      }
    }
    
    min_sc <- min(sc_vals, na.rm = TRUE)
    max_sc <- max(sc_vals, na.rm = TRUE)
    range_min <- max(0, floor(min_sc * 0.9))
    range_max <- ceiling(max_sc * 1.1)
    if (range_max < 1) range_max <- 1
    
    updateSliderInput(session, "sc_y_range", value = c(range_min, range_max))
  })
  
  # Reset button for specific cost plot - resets to show all data
  observeEvent(input$sc_reset, {
    req(results$simple_data)
    
    # Get all finite SC values
    sc_vals <- results$simple_data$SC_per_kg
    sc_vals <- sc_vals[is.finite(sc_vals)]
    
    if (length(sc_vals) < 1) return()
    
    min_sc <- min(sc_vals, na.rm = TRUE)
    max_sc <- max(sc_vals, na.rm = TRUE)
    range_min <- max(0, floor(min_sc * 0.9))
    range_max <- ceiling(max_sc * 1.1)
    if (range_max < 1) range_max <- 1
    
    updateSliderInput(session, "sc_y_range", value = c(range_min, range_max))
  })

  # Render Payback Plot
  output$payback_plot <- renderPlotly({
    if (results$is_multi_value) {

         if (!is.null(results$com_result) && !is.null(results$com_result$plots$payback)) {
    
    # Get the original plot and data
    original_plot <- results$com_result$plots$payback
    plot_data <- results$simple_data
    
    # Identify problematic points
    payback_vals <- plot_data$payback_yr
    negative_idx <- which(payback_vals < 0 | !is.finite(payback_vals))
    
    # Detect outliers using IQR method on valid positive values
    valid_payback <- payback_vals[is.finite(payback_vals) & payback_vals > 0]
    outlier_idx <- integer(0)
    
    if (length(valid_payback) >= 4) {  # Need enough points for IQR
      q1 <- quantile(valid_payback, 0.25, na.rm = TRUE)
      q3 <- quantile(valid_payback, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      upper_fence <- q3 + 3 * iqr  # 3*IQR for extreme outliers
      
      # Find outlier indices in original data
      outlier_idx <- which(payback_vals > upper_fence & is.finite(payback_vals) & payback_vals > 0)
    }
    
    # Combine all points to exclude
    exclude_idx <- unique(c(negative_idx, outlier_idx))
    
    # Build notification message if points were excluded
    if (length(exclude_idx) > 0) {
      excluded_times <- plot_data$time[exclude_idx]
      
      msg_parts <- c()
      if (length(negative_idx) > 0) {
        neg_times <- plot_data$time[negative_idx]
        msg_parts <- c(msg_parts, paste0(i18n$t("Negative/infinite payback at: "), paste(neg_times, collapse = ", "), " min"))
      }
      if (length(outlier_idx) > 0) {
        out_times <- plot_data$time[outlier_idx]
        out_vals <- round(payback_vals[outlier_idx], 1)
        msg_parts <- c(msg_parts, paste0(i18n$t("Break-even outliers removed at: "), paste(out_times, collapse = ", "), " min"))
      }
      
      # Store notification for display (only show once per calculation)
      if (is.null(results$payback_filter_notified) || !results$payback_filter_notified) {
        showNotification(
          HTML(paste0(
            "<strong>", i18n$t("Payback plot filtered"), "</strong><br>",
            paste(msg_parts, collapse = "<br>"),
            "<br><small>", i18n$t("These points indicate near break-even or unprofitable conditions"), "</small>"
          )),
          type = "warning",
          duration = 8
        )
        results$payback_filter_notified <- TRUE
      }
    }
    
    # Create filtered data for the plot
    if (length(exclude_idx) > 0) {
      filtered_data <- plot_data[-exclude_idx, , drop = FALSE]
    } else {
      filtered_data <- plot_data
    }
    
    # Rebuild plot with filtered data if we have points to show
    if (nrow(filtered_data) > 0) {
      # Find minimum point in filtered data
      min_idx <- which.min(filtered_data$payback_yr)
      min_point <- filtered_data[min_idx, ]
      
      plot <- ggplot(data = filtered_data, aes(x = .data[["time"]], y = .data[["payback_yr"]])) +
        geom_hline(yintercept = min(filtered_data$payback_yr, na.rm = TRUE), lty = 2, colour = "darkgreen", linewidth = 0.8) +
        geom_point(pch = 16, colour = "darkred") + 
        geom_line(colour = "darkred") +
        geom_point(data = min_point, colour = "darkgreen", size = 2) +
        geom_label(data = min_point, 
                   aes(label = round(.data[["payback_yr"]], 2)), 
                   colour = "white", fontface = "bold", fill = "darkgreen",
                   nudge_x = (max(filtered_data$time) - min_point$time) * 0.1) +
        scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
        labs(title = "Payback time plot", x = "Time (min)", y = "Payback time (yr)") +
        theme(aspect.ratio = 1,
              panel.background = element_blank(),
              axis.text = element_text(colour = "black", size = 12),
              axis.title = element_text(colour = "black", size = 13),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(),
              legend.position.inside = c(0.93, 0.15),
              legend.text = element_text(size = 11))
    } else {
      # All points filtered - show empty plot with message
      plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = i18n$t("No viable payback data to display"), size = 5) +
        theme_void()
    }
    
    # Apply Y-axis range if specified
    y_range <- input$payback_y_range
    if (!is.null(y_range) && length(y_range) == 2 && nrow(filtered_data) > 0) {
      plot <- plot + scale_y_continuous(
        limits = c(y_range[1], y_range[2]),
        breaks = scales::breaks_pretty(n = 6),
        oob = scales::oob_censor
      )
    }
    
    plot <- translate_plot_labels(
      plot,
      i18n_r(),
      title = "Payback time plot",
      x = "Time (min)",
      y = "Payback time (yr)"
    )
    
    plot
  } else {
    plotly_empty()
  }
 
    } else { # Single value case
      if (!is.null(selected_simple_data())) {
        payback_value <- selected_simple_data()$payback_yr

        plot_data <- data.frame(
          x = c(0, payback_value),
          y = c(0, 0)
        )

        p <- plot_ly(plot_data, x = ~x, y = ~y, type = "scatter", mode = "lines") %>%
          add_segments(
            x = 0, xend = payback_value, y = 0, yend = 0,
            line = list(color = "orange", width = 5),
            name = i18n$t("Payback Period")
          ) %>%
          layout(
            title = paste0(i18n$t("Payback Period: "), format(payback_value, digits = 2), " years"),
            xaxis = list(title = "Years"),
            yaxis = list(title = "", showticklabels = FALSE),
            showlegend = FALSE
          )
        return(p)
      } else {
        plotly_empty()
      }
    }
  })


  # Render Specific Cost Plot
 output$sc_plot <- renderPlotly({
  if (!is.null(results$com_result) && !is.null(results$com_result$plots$sc)) {
    plot <- results$com_result$plots$sc
    
    # Apply Y-axis range if specified (using scale_y_continuous to hide out-of-range points)
    y_range <- input$sc_y_range
    if (!is.null(y_range) && length(y_range) == 2) {
      plot <- plot + scale_y_continuous(
        limits = c(y_range[1], y_range[2]),
        breaks = scales::breaks_pretty(n = 6),
        oob = scales::oob_censor  # Hide points outside range
      )
    }
    
    plot <- translate_plot_labels(
      plot,
      i18n_r(),
      title = "Specific Cost plot",
      x = "Time (min)",
      y = "Specific Cost (USD/kg)"
    )
    
    plot
  } else {
    plotly_empty()
  }
})


  # ---- Plot Data CSV Exporters ----
  # COM Breakdown (pie chart data)
  output$export_com_breakdown_csv <- downloadHandler(
    filename = function() { paste0(generate_filename_with_timestamp("com_breakdown_data"), ".csv") },
    content = function(file) {
      req(cost_components_data())
      costs <- cost_components_data()
      df <- data.frame(
        Component = c("CRM", "CUT", "COL", "FMC", "COM"),
        Value_USD_month = c(costs$CRM, costs$CUT, costs$COL, costs$FCI, costs$COM),
        stringsAsFactors = FALSE
      )
      df$Percentage <- round(df$Value_USD_month / costs$COM * 100, 2)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Monthly Cost Summary (waterfall chart data)
  output$export_cost_summary_csv <- downloadHandler(
    filename = function() { paste0(generate_filename_with_timestamp("cost_summary_data"), ".csv") },
    content = function(file) {
      req(cost_components_data(), selected_simple_data())
      costs <- cost_components_data()
      simple <- selected_simple_data()
      revenue <- simple$sales_volume
      gross_profit <- revenue - costs$COM
      taxrate <- results$params$taxrate
      net_profit <- if(!is.null(taxrate) && !is.na(taxrate) && taxrate > 0)
        gross_profit * (1 - taxrate / 100) else gross_profit

      df <- data.frame(
        Category = c("CRM", "CUT", "COL", "FMC", "Total COM", "Revenue", "Gross Profit", "Net Profit"),
        Value_USD_month = c(-costs$CRM, -costs$CUT, -costs$COL, -costs$FCI, -costs$COM,
                            revenue, gross_profit, net_profit),
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Payback Plot data
  output$export_payback_csv <- downloadHandler(
    filename = function() { paste0(generate_filename_with_timestamp("payback_data"), ".csv") },
    content = function(file) {
      req(results$simple_data)
      sd <- results$simple_data
      df <- data.frame(
        Time_min = sd$time,
        Payback_yr = sd$payback_yr,
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Specific Cost Plot data
  output$export_sc_csv <- downloadHandler(
    filename = function() { paste0(generate_filename_with_timestamp("specific_cost_data"), ".csv") },
    content = function(file) {
      req(results$simple_data)
      sd <- results$simple_data
      df <- data.frame(
        Time_min = sd$time,
        Specific_Cost_USD_kg = sd$SC_per_kg,
        stringsAsFactors = FALSE
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$performance_overview_table <- DT::renderDataTable(
    {
      req(results$simple_data)

      add_prettynames <-
        c(
          i18n_r()$t("Time (min)"),
          i18n_r()$t("Yield (%)"),
          i18n_r()$t("Monthly Profit ($)"),
          i18n_r()$t("Margin (%)"),
          i18n_r()$t("Payback Period (years)"),
          i18n_r()$t("Specific Cost ($/kg)")
        )

      # if (results$is_multi_value) {
      if (TRUE) {
        # Format the performance overview data
        display_data <- results$simple_data %>%
          select(time, yield, monthly_profit, margin_percent, payback_yr, SC_per_kg)
      } else {
        # For single value, show a message instead of empty table
        display_data <- data.frame(Message = i18n$t("This overview is available only when multiple time-yield data points are provided."))
      }

      DT::datatable(
        display_data,
        extensions = "Buttons",
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_performance_overview")
        )
      ) %>%
        formatRound(columns = c("yield", "monthly_profit", "margin_percent", "payback_yr", "SC_per_kg"), digits = 2)
    },
    server = FALSE
  )





  # Download All Results
  output$download_all_results <- downloadHandler(
    filename = function() {
      paste0(generate_filename_with_timestamp("supercrit_com_analysis_export"), ".zip")
    },
    content = function(file) {
      req(results$com_result) # Ensure results are available
      
      # Show progress notification
      progress_id <- showNotification(
        i18n$t("Preparing export... Please wait."),
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )
      
      # Ensure notification is removed when done
      on.exit(removeNotification(progress_id), add = TRUE)

      # Create a temporary directory for export
      temp_dir <- file.path(tempdir(), paste0("com_analysis_export_", format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")))
      dir.create(temp_dir, recursive = TRUE)

      # Export results using com_export with translated results
      tryCatch({
        # Translate results to current language
        translated_results <- translate_com_results(results$com_result, i18n)
        
        # Export 
        com_export(comres = translated_results, expath = temp_dir, silent = TRUE)
        
        # Post-process .csv files: add UTF-8 BOM and translate section headers
        csv_files <- list.files(temp_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
        for (csv_file in csv_files) {
          # Read original content as raw bytes
          original_bytes <- readBin(csv_file, "raw", file.info(csv_file)$size)
          content <- rawToChar(original_bytes)
          
          # Section header translations
          translations <- list(
            "COST OF MANUFACTURING ANALYSIS RESULTS" = i18n$t("COST OF MANUFACTURING ANALYSIS RESULTS"),
            "Generated:" = paste0(i18n$t("Generated"), ":"),
            "PRODUCTION OVERVIEW" = i18n$t("PRODUCTION OVERVIEW"),
            "COST OF RAW MATERIALS (CRM)" = i18n$t("COST OF RAW MATERIALS (CRM)"),
            "COST OF UTILITIES (CUT)" = i18n$t("COST OF UTILITIES (CUT)"),
            "FINANCIAL RESULTS (COMBINED SUMMARY)" = i18n$t("FINANCIAL RESULTS (COMBINED SUMMARY)"),
            "PER-RESPONSE FINANCIAL BREAKDOWN" = i18n$t("PER-RESPONSE FINANCIAL BREAKDOWN"),
            "PERFORMANCE OVERVIEW (TIME-SERIES DATA)" = i18n$t("PERFORMANCE OVERVIEW (TIME-SERIES DATA)"),
            "INPUT PARAMETERS" = i18n$t("INPUT PARAMETERS"),
            "General Parameters" = i18n$t("General Parameters"),
            "Raw Material Parameters" = i18n$t("Raw Material Parameters"),
            "Utility Parameters" = i18n$t("Utility Parameters"),
            "Labour Parameters" = i18n$t("Labour Parameters"),
            "Fixed Cost Parameters" = i18n$t("Fixed Cost Parameters"),
            "ADDITIONAL RESULTS" = i18n$t("ADDITIONAL RESULTS"),
            "INPUT DATA PROVIDED BY USER" = i18n$t("INPUT DATA PROVIDED BY USER"),
            "DETAILED OUTPUT DATA" = i18n$t("DETAILED OUTPUT DATA"),
            "ABBREVIATED OUTPUT DATA" = i18n$t("ABBREVIATED OUTPUT DATA"),
            "Description" = i18n$t("Description"),
            "Value" = i18n$t("Value"),
            "Unit" = i18n$t("Unit")
          )
          
          # Replace section headers
          for (en_text in names(translations)) {
            content <- gsub(en_text, translations[[en_text]], content, fixed = TRUE)
          }
          
          # Remove FUNCTION CALL section from Shiny export (everything from FUNCTION CALL to end of file)
          # Handle both Unix and Windows line endings
          content <- gsub("\r\n", "\n", content)  # Normalize line endings
          fc_pos <- regexpr("\n\nFUNCTION CALL\n", content)
          if (fc_pos > 0) {
            content <- substr(content, 1, fc_pos - 1)
          }
          
          # Write BOM + translated content
          con <- file(csv_file, "wb")
          writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
          writeBin(charToRaw(content), con)
          close(con)
        }
        
        # Get list of files to zip
        files_to_zip <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

        # Create the zip file
        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
      }, error = function(e) {
        showNotification(paste(i18n$t("Error exporting results:"), e$message), type = "error")
      }, finally = {
        # Clean up the temporary directory
        unlink(temp_dir, recursive = TRUE)
      })
    },
    contentType = "application/zip"
  )

  # Download Default CSV Template
  output$download_default_csv <- downloadHandler(
    filename = function() {
      "com_analysis_default.csv"
    },
    content = function(file) {
      default_path <- system.file("extdata", "gui_com_input-test-data-0.csv", package = "supeRcrit")
      if (file.exists(default_path)) {
        file.copy(default_path, file)
      } else {
        showNotification(i18n$t("Default template file not found."), type = "error")
      }
    },
    contentType = "text/csv"
  )
  # Help button for units
  observeEvent(input$show_units, {
    showModal(modalDialog(
      title = i18n$t("Cost of Manufacturing Parameters"),
      DT::dataTableOutput(ns("units_table")),
           easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton(i18n$t("Close"))
      )))

  })



  output$units_table <- DT::renderDataTable(
    {
      com_pars <- show_pars("com")
      # show_pars("com") returns a list with $input and $output dataframes
      # Combine them for display
      if (is.list(com_pars) && length(com_pars) == 2) {
        # Add a source column to distinguish input vs output parameters
        input_df <- com_pars$input
        output_df <- com_pars$output
        if (!is.null(input_df) && nrow(input_df) > 0) {
          input_df$source <- "Input Parameters"
        }
        if (!is.null(output_df) && nrow(output_df) > 0) {
          output_df$source <- "Output Parameters"
        }
        # Combine the dataframes
        combined_df <- bind_rows(input_df, output_df)

        # Translate column headers
        add_prettynames <- c(
          i18n_r()$t("mandatory"),
          i18n_r()$t("Type"),
          i18n_r()$t("parameter"),
          i18n_r()$t("Unit"),
          i18n_r()$t("Description"),
          i18n_r()$t("source")
        )

        # Translation mappings for mandatory column
        mandatory_translations <- c(
          "yes" = i18n_r()$t("yes"),
          "no" = i18n_r()$t("no")
        )

        # Translation mappings for type column (already in json)
        type_translations <- c(
          "general" = i18n_r()$t("general"),
          "crm" = i18n_r()$t("crm"),
          "cut" = i18n_r()$t("cut"),
          "col" = i18n_r()$t("col"),
          "fci" = i18n_r()$t("fci"),
          "crm_aux" = i18n_r()$t("crm_aux")
        )

        # Translation mappings for units column
        units_translations <- c(
          "L" = i18n_r()$t("L"),
          "kg" = i18n_r()$t("kg"),
          "bar" = i18n_r()$t("bar"),
          "degC" = i18n_r()$t("°C"),
          "°C" = i18n_r()$t("°C"),
          "variable" = i18n_r()$t("variable"),
          "mL/min" = i18n_r()$t("mL/min"),
          "g/min" = i18n_r()$t("g/min"),
          "min" = i18n_r()$t("min"),
          "none" = i18n_r()$t("none"),
          "USD/kg" = i18n_r()$t("USD/kg"),
          "cm" = i18n_r()$t("cm"),
          "kW" = i18n_r()$t("kW"),
          "kWh" = i18n_r()$t("kWh"),
          "USD/kWh" = i18n_r()$t("USD/kWh"),
          "kW/cycle" = i18n_r()$t("kW/cycle"),
          "people" = i18n_r()$t("people"),
          "hours" = i18n_r()$t("hours"),
          "shifts/day" = i18n_r()$t("shifts/day"),
          "h/shift" = i18n_r()$t("h/shift"),
          "days/month" = i18n_r()$t("days/month"),
          "%/year" = i18n_r()$t("dimensionless"),
          "dimensionless" = i18n_r()$t("dimensionless"),
          "USD" = i18n_r()$t("USD"),
          "USD/month" = i18n_r()$t("USD/month"),
          "kg/h" = i18n_r()$t("kg/h"),
          "L/h" = i18n_r()$t("L/h"),
          "kg/cycle" = i18n_r()$t("kg/cycle"),
          "kg/month" = i18n_r()$t("kg/month"),
          "kWh/month" = i18n_r()$t("kWh/month"),
          "percent" = i18n_r()$t("percent"),
          "years" = i18n_r()$t("years")
        )

        # Translation mappings for description column
        description_translations <- c(
          "Extraction vessel volume" = i18n_r()$t("Extraction vessel volume"),
          "Material loading per batch" = i18n_r()$t("Material loading per batch"),
          "Feed flow rate" = i18n_r()$t("Feed flow rate"),
        "Feed density" = i18n_r()$t("Feed density"),
          "Extraction pressure" = i18n_r()$t("Extraction pressure"),
          "Extraction temperature" = i18n_r()$t("Extraction temperature"),
          "Additional batch processing time (changeover etc.)" = i18n_r()$t("Additional batch processing time (changeover etc.)"),
          "Main solvent flow (CO2 or water; mL/min or g/min)" = i18n_r()$t("Main solvent flow (CO2 or water; mL/min or g/min)"),
          "Co-solvent flow (EtOH or CO2; mL/min or g/min)" = i18n_r()$t("Co-solvent flow (EtOH or CO2; mL/min or g/min)"),
          "Dilution factor of the crude extract (if any)" = i18n_r()$t("Dilution factor of the crude extract (if any)"),
          "Sale price of the final extract" = i18n_r()$t("Sale price of the final extract"),
          "Material bed height" = i18n_r()$t("Material bed height"),
          "Extraction vessel internal diameter" = i18n_r()$t("Extraction vessel internal diameter"),
          "Price of extractable material" = i18n_r()$t("Price of extractable material"),
          "Price of the main solvent (CO2 or H2O)" = i18n_r()$t("Price of the main solvent (CO2 or H2O)"),
          "Price of co-solvent (EtOH or CO2)" = i18n_r()$t("Price of co-solvent (EtOH or CO2)"),
          "CO2 recovery line pressure" = i18n_r()$t("CO2 recovery line pressure"),
          "CO2 recovery line temperature" = i18n_r()$t("CO2 recovery line temperature"),
          "Final CO2 separator temperature" = i18n_r()$t("Final CO2 separator temperature"),
          "Fraction of collected co-solvent lost during post-processing" = i18n_r()$t("Fraction of collected co-solvent lost during post-processing"),
          "Main system power usage" = i18n_r()$t("Main system power usage"),
          "Price of electricity" = i18n_r()$t("Price of electricity"),
          "Power usage for drying (pre-processing)" = i18n_r()$t("Power usage for drying (pre-processing)"),
          "Power usage for comminution (pre-processing)" = i18n_r()$t("Power usage for comminution (pre-processing)"),
          "Any additional power usage per extraction cycle" = i18n_r()$t("Any additional power usage per extraction cycle"),
          "Power usage for evaporation (post-processing)" = i18n_r()$t("Power usage for evaporation (post-processing)"),
          "Hourly drying capacity (pre-processing)" = i18n_r()$t("Hourly drying capacity (pre-processing)"),
          "Hourly comminution capacity (pre-processing)" = i18n_r()$t("Hourly comminution capacity (pre-processing)"),
          "Hourly evaporation capacity (post-processing)" = i18n_r()$t("Hourly evaporation capacity (post-processing)"),
          "Number of operator staff required" = i18n_r()$t("Number of operator staff required"),
          "Number of hours per work shift" = i18n_r()$t("Number of hours per work shift"),
          "Number of work shifts per calendar day" = i18n_r()$t("Number of work shifts per calendar day"),
          "Average monthly wage per operator" = i18n_r()$t("Average monthly wage per operator"),
          "Number of work days per month (28 days)" = i18n_r()$t("Number of work days per month (28 days)"),
          "Capital expenditure invested" = i18n_r()$t("Capital expenditure invested"),
          "Depreciation (as CAPEX fraction per year)" = i18n_r()$t("Depreciation (as CAPEX fraction per year)"),
          "Monthly maintenance costs" = i18n_r()$t("Monthly maintenance costs"),
          "Other monthly costs" = i18n_r()$t("Other monthly costs"),
          "Extraction cycles per month" = i18n_r()$t("Extraction cycles per month"),
          "Main solvent loss per cycle (CO2 or water)" = i18n_r()$t("Main solvent loss per cycle (CO2 or water)"),
          "Co-solvent loss per batch (EtOH or CO2)" = i18n_r()$t("Co-solvent loss per batch (EtOH or CO2)"),
          "CO2 loss per month" = i18n_r()$t("CO2 loss per month"),
          "Co-solvent loss per month" = i18n_r()$t("Co-solvent loss per month"),
          "S/M mass ratio (CO2)" = i18n_r()$t("S/M mass ratio (CO2)"),
          "S/M mass ratio (co-solvent)" = i18n_r()$t("S/M mass ratio (co-solvent)"),
          "S/F mass ratio (CO2)" = i18n_r()$t("S/F mass ratio (CO2)"),
          "S/F mass ratio (co-solvent)" = i18n_r()$t("S/F mass ratio (co-solvent)"),
          "Raw material requirement (monthly)" = i18n_r()$t("Raw material requirement (monthly)"),
          "Main solvent requirement (monthly)" = i18n_r()$t("Main solvent requirement (monthly)"),
          "Co-solvent requirement (monthly)" = i18n_r()$t("Co-solvent requirement (monthly)"),
          "Requirement of all auxiliary materials (monthly)" = i18n_r()$t("Requirement of all auxiliary materials (monthly)"),
          "Raw material cost (monthly)" = i18n_r()$t("Raw material cost (monthly)"),
          "Main solvent cost (monthly)" = i18n_r()$t("Main solvent cost (monthly)"),
          "Co-solvent cost (monthly)" = i18n_r()$t("Co-solvent cost (monthly)"),
          "Cost of all auxiliary materials (monthly)" = i18n_r()$t("Cost of all auxiliary materials (monthly)"),
          "Main power requirement (monthly)" = i18n_r()$t("Main power requirement (monthly)"),
          "Additional power requirement (monthly)" = i18n_r()$t("Additional power requirement (monthly)"),
          "Drying power requirement (monthly)" = i18n_r()$t("Drying power requirement (monthly)"),
          "Comminution power requirement (monthly)" = i18n_r()$t("Comminution power requirement (monthly)"),
          "Evaporation power requirement (monthly)" = i18n_r()$t("Evaporation power requirement (monthly)"),
          "Main power cost (monthly)" = i18n_r()$t("Main power cost (monthly)"),
          "Additional power cost (monthly)" = i18n_r()$t("Additional power cost (monthly)"),
          "Drying power cost (monthly)" = i18n_r()$t("Drying power cost (monthly)"),
          "Comminution power cost (monthly)" = i18n_r()$t("Comminution power cost (monthly)"),
          "Evaporation power cost (monthly)" = i18n_r()$t("Evaporation power cost (monthly)"),
          "Extract produced (monthly)" = i18n_r()$t("Extract produced (monthly)"),
          "Specific extract cost" = i18n_r()$t("Specific extract cost"),
          "Monthly gross profit (pre-tax)" = i18n_r()$t("Monthly gross profit (pre-tax)"),
          "Monthly profit (after tax)" = i18n_r()$t("Monthly profit (after tax)"),
          "Sales volume (monthly)" = i18n_r()$t("Sales volume (monthly)"),
          "Profit margin (monthly)" = i18n_r()$t("Profit margin (monthly)"),
          "Payback period" = i18n_r()$t("Payback period"),
          "Cost of Raw Materials" = i18n_r()$t("Cost of Raw Materials"),
          "Cost of Utilities" = i18n_r()$t("Cost of Utilities"),
          "Cost of Manufacturing" = i18n_r()$t("Cost of Manufacturing"),
          "Price of auxiliary material: oil" = i18n_r()$t("Price of auxiliary material: oil"),
          "Required fraction of auxiliary material: oil" = i18n_r()$t("Required fraction of auxiliary material: oil")
        )

        # Translation mappings for source column
        source_translations <- c(
          "Input Parameters" = i18n_r()$t("Input Parameters"),
          "Output Parameters" = i18n_r()$t("Output Parameters")
        )

        # Apply translations to data
        dt_data <- combined_df
        if ("mandatory" %in% names(dt_data)) {
          dt_data$mandatory <- my_mapvalues(dt_data$mandatory, names(mandatory_translations), mandatory_translations, warn_missing = FALSE)
        }
        if ("type" %in% names(dt_data)) {
          dt_data$type <- my_mapvalues(dt_data$type, names(type_translations), type_translations, warn_missing = FALSE)
        }
        if ("units" %in% names(dt_data)) {
          dt_data$units <- my_mapvalues(dt_data$units, names(units_translations), units_translations, warn_missing = FALSE)
        }
        if ("description" %in% names(dt_data)) {
          dt_data$description <- my_mapvalues(dt_data$description, names(description_translations), description_translations, warn_missing = FALSE)
        }
        if ("source" %in% names(dt_data)) {
          dt_data$source <- my_mapvalues(dt_data$source, names(source_translations), source_translations, warn_missing = FALSE)
        }

        DT::datatable(
          dt_data,
          extensions = "Buttons",
          colnames = add_prettynames,
          options = list(
            dom = "Bfrtip",
            language = tablang(),
            buttons = list(
              list(
                extend = "copy",
                text = i18n_r()$t("Copy"),
                titleAttr = i18n_r()$t("Copy"),
                action = copy_button_no_popup(
                  copy_label = i18n_r()$t("Copy"),
                  copied_label = i18n_r()$t("Copied!")
                )
              )
            )
          ),
          rownames = FALSE
        )
      } else {
        # Fallback in case structure changes
        DT::datatable(
          com_pars,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            language = tablang(),
            buttons = list(
              list(
                extend = "copy",
                text = i18n_r()$t("Copy"),
                titleAttr = i18n_r()$t("Copy"),
                action = copy_button_no_popup(
                  copy_label = i18n_r()$t("Copy"),
                  copied_label = i18n_r()$t("Copied!")
                )
              )
            )
          ),
          rownames = FALSE
        )
      }
    },
    server = FALSE
  )

  output$csv_file_div <- renderUI({
    div(
      style = "display: flex; align-items: flex-start; gap: 10px;",
      div(style = "flex: 1;",
        fileInput(
          ns("csv_file"),
          i18n_r()$t("Upload CSV File"),
          accept = ".csv",
          buttonLabel = i18n_r()$t("Browse"),
          placeholder = i18n_r()$t("No file selected")
        )
      ),
      div(style = "margin-top: 25px; flex-shrink: 0;",
        actionButton(ns("reload_csv"), i18n_r()$t("Reload"),
          icon = icon("refresh"),
          class = "btn btn-default"
        )
      )
    )
  })

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "add_response_button_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "additional_responses_section_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "additional_responses_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "aux_materials_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "column_validation_message", suspendWhenHidden = FALSE)
  outputOptions(output, "column_validation_message_general", suspendWhenHidden = FALSE)
  outputOptions(output, "com_analysis_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "com_equation_display", suspendWhenHidden = FALSE)
  outputOptions(output, "csv_file_div", suspendWhenHidden = FALSE)
  outputOptions(output, "data_preview_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "load_example_data_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "optimal_time_buttons", suspendWhenHidden = FALSE)
  outputOptions(output, "optimal_time_buttons_detailed", suspendWhenHidden = FALSE)
  outputOptions(output, "payback_y_range_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "per_response_extract_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "per_response_revenue_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "primary_response_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "rename_col_controls_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sc_y_range_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "single_time_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "time_column_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "yield_column_ui", suspendWhenHidden = FALSE)

}
