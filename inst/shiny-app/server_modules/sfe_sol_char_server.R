solute_characterization_server <- function(input, output, session, defaults, i18n, tablang, sfe_rv) {



  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Reactive output to check if results are available
  output$has_results <- reactive({
    !is.null(sfe_rv$gcm_results)
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Reactive output to check if GCM comparison results are available
  output$has_gcm_comparison <- reactive({
    !is.null(sfe_rv$gcm_comparison)
  })
  outputOptions(output, "has_gcm_comparison", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  # Tab 0: Solute Characterization Results - enabled when gcm_results available
  # Tab 1: Predicted Parameters (HSP & Critical) - enabled when gcm_results available
  # Tab 2: Group Contributions - enabled when gcm_results available
  # Tab 3: GCM Comparison - enabled ONLY when gcm_comparison available
  observe({
    has_results <- !is.null(sfe_rv$gcm_results)
    has_gcm_comparison <- !is.null(sfe_rv$gcm_comparison)
    
    if (has_results) {
      # Enable all tabs first
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("sol_char_tabs")))
      # But disable tab 3 (GCM Comparison) if no comparison results
      if (!has_gcm_comparison) {
        session$sendCustomMessage("disableTabsByIndex", list(tabsetId = ns("sol_char_tabs"), indices = c(3)))
      }
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("sol_char_tabs"), keepFirst = TRUE))
    }
  })

  # Render Solute Identification inputs
  output$cas_input_ui <- renderUI({
    textInput(ns("cas_input"),
      tags$span(i18n_r()$t("CAS Number"),
        input_help(i18n_r()$t("Chemical Abstracts Service registry number of the target solute (e.g. 7235-40-7 for beta-carotene). Used to retrieve molecular structure and SMILES automatically."),
                   title = i18n_r()$t("CAS Number"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$cas_input) %||% defaults$cas_input)
  })

  output$name_input_ui <- renderUI({
    textInput(ns("name_input"),
      tags$span(i18n_r()$t("Display Name"),
        input_help(i18n_r()$t("A descriptive name for the solute, shown in plots and results tables. Can be any text."),
                   title = i18n_r()$t("Display Name"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$name_input) %||% defaults$name_input)
  })

  output$specify_smiles_ui <- renderUI({
    tags$span(
      checkboxInput(ns("specify_smiles"), i18n_r()$t("Specify SMILES"),
                    value = isolate(input$specify_smiles) %||% defaults$specify_smiles),
      title = i18n_r()$t("Enable to manually enter the SMILES string instead of retrieving it automatically from the CAS number.")
    )
  })

  output$smiles_input_ui <- renderUI({
    textInput(ns("smiles_input"),
      tags$span(i18n_r()$t("SMILES String"),
        input_help(i18n_r()$t("Simplified Molecular Input Line Entry System notation describing the molecular structure of the solute. Required for Group Contribution Method calculations."),
                   title = "SMILES", buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$smiles_input) %||% defaults$smiles_input)
  })

  output$tb_manual_input_ui <- renderUI({
    numericInput(ns("tb_manual_input"),
      tags$span(i18n_r()$t("Boiling Point (K):"),
        input_help(i18n_r()$t("Normal boiling point of the solute in Kelvin. Provide this when the Boiling Point Method is set to Manual Input instead of a GCM estimation."),
                   title = i18n_r()$t("Boiling Point"), buttonLabel = i18n_r()$t("OK"))),
      value = isolate(input$tb_manual_input) %||% defaults$tb_manual_input, min = 50, max = 1000, step = 0.1)
  })

  # Render selectInput elements with i18n translations
  output$gcm_tb_ui <- renderUI({
    selectInput(ns("gcm_tb"),
      tags$span(i18n_r()$t("Boiling Point Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating the normal boiling point. Choose Manual Input to enter a known value directly. Each method uses different structural group databases and correlation equations."),
                   title = i18n_r()$t("Boiling Point Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("manual", "JR", "JR_corr", "SB", "SB_corr", "NL04", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("Manual Input", "Joback-Reid", "Joback-Reid corrected", "Stein-Brown", "Stein-Brown corrected", "Nannoolal 2004", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))
      ),
      selected = defaults$gcm_tb %||% "manual"
    )
  })
  outputOptions(output, "gcm_tb_ui", suspendWhenHidden = FALSE)

  output$gcm_crit_ui <- renderUI({
    selectInput(ns("gcm_crit"),
      tags$span(i18n_r()$t("Critical Parameters Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating critical temperature, pressure, and volume. These are needed for temperature-dependent HSP corrections. Set to None if not needed."),
                   title = i18n_r()$t("Critical Parameters Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("none", "JR", "NL07", "NL07_robust", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("None", "Joback-Reid", "Nannoolal 2007", "Nannoolal 2007 robust", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))
      ),
      selected = defaults$gcm_crit %||% "none"
    )
  })
  outputOptions(output, "gcm_crit_ui", suspendWhenHidden = FALSE)


  output$gcm_hsp_ui <- renderUI({
    selectInput(ns("gcm_hsp"),
      tags$span(i18n_r()$t("Hansen Solubility (HSP) Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating Hansen Solubility Parameters (dispersion, polarity, and hydrogen bonding components). Required for co-solvent miscibility evaluation. Set to None if not needed."),
                   title = i18n_r()$t("HSP Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("none", "SP08", "SP12", "HKR_STW", "HKR_SIM"),
        i18n_r()$t(c("None", "Stefanis-Panayiotou 2008", "Stefanis-Panayiotou 2012", "Hukkerikar-Stepwise", "Hukkerikar-Simultaneous"))
      ),
      selected = defaults$gcm_hsp %||% "none"
    )
  })
  outputOptions(output, "gcm_hsp_ui", suspendWhenHidden = FALSE)

  output$gcm_vdw_ui <- renderUI({
    selectInput(ns("gcm_vdw"),
      tags$span(i18n_r()$t("Van der Waals Volume Method"),
        input_help(i18n_r()$t("Group Contribution Method for estimating the Van der Waals molecular volume. Used for molar volume calculations. Set to None if not needed."),
                   title = i18n_r()$t("VdW Volume Method"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("none", "ZHAO", "BND", "SLON"),
        i18n_r()$t(c("None", "Zhao 2003", "Bondi 1964", "Slonimskii 1970"))
      ),
      selected = "none"
    )
  })
  outputOptions(output, "gcm_vdw_ui", suspendWhenHidden = FALSE)

  output$gcm_simplicity_ui <- renderUI({
    selectInput(ns("gcm_simplicity"),
      tags$span(i18n_r()$t("Fragmentation Simplicity"),
        input_help(i18n_r()$t("Controls how the molecule is broken into GCM groups. Auto selects the best setting for each method. Simple allows overlapping groups. Normal prevents overlaps. Complex tries all possible fragmentations (slower but more thorough)."),
                   title = i18n_r()$t("Fragmentation"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("auto", "simple", "normal", "complex"),
        i18n_r()$t(c("Auto", "Simple", "Normal", "Complex"))
      ),
      selected = "auto"
    )
  })
  outputOptions(output, "gcm_simplicity_ui", suspendWhenHidden = FALSE)

  output$gcm_comp_simplicity_ui <- renderUI({
    selectInput(ns("gcm_comp_simplicity"),
      tags$span(i18n_r()$t("Fragmentation Simplicity"),
        input_help(i18n_r()$t("Controls how molecules are broken into functional groups. Simple uses fewer, larger groups. Complex uses more detailed fragmentation. Auto selects automatically based on molecule complexity."),
                   title = i18n_r()$t("Fragmentation Simplicity"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(
        c("auto", "simple", "normal", "complex"),
        i18n_r()$t(c("Auto", "Simple", "Normal", "Complex"))
      ),
      selected = "auto"
    )
  })
  outputOptions(output, "gcm_comp_simplicity_ui", suspendWhenHidden = FALSE)

  # Maximum group order available per GCM method
  gcm_max_order <- c(
    "manual" = 0, "none" = 0,
    "JR" = 1, "JR_corr" = 1, "SB" = 1, "SB_corr" = 1,
    "NL04" = 2, "NL07" = 2, "NL07_robust" = 2,
    "SP08" = 2, "SP12" = 2,
    "HKR_STW" = 3, "HKR_SIM" = 3,
    "ZHAO" = 1, "BND" = 1, "SLON" = 1
  )

  # Helper to render a group order input with COM-style range badge
  render_gorder_input <- function(input_id, method_value, show_help = FALSE) {
    max_ord <- gcm_max_order[method_value]
    if (is.na(max_ord)) max_ord <- 0
    is_disabled <- max_ord <= 1
    
    # Range badge text and tooltip
    if (is_disabled) {
      badge_text <- "1"
      badge_title <- i18n_r()$t("1st order only")
      badge_bg <- "#999"
    } else {
      badge_text <- sprintf("0\u2013%d", max_ord)
      badge_title <- sprintf("%s: 0\u2013%d", i18n_r()$t("Valid range"), max_ord)
      badge_bg <- "#6c757d"
    }
    
    num_input <- numericInput(ns(input_id), NULL, value = 0, min = 0, max = max_ord, step = 1)
    if (is_disabled) {
      num_input <- shinyjs::disabled(num_input)
    }
    
    help_icon <- if (show_help) {
      input_help(i18n_r()$t("Maximum order of structural groups to consider in the fragmentation. 0 uses all available orders. Higher orders capture more complex structural features but may not be available for all methods."),
                 title = i18n_r()$t("Group Order"), buttonLabel = i18n_r()$t("OK"))
    } else NULL
    
    tags$div(
      tags$label(
        i18n_r()$t("Group Order:"),
        help_icon,
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          badge_text,
          style = sprintf("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: %s; color: white; margin-left: auto; font-weight: normal;", badge_bg),
          title = badge_title
        )
      ),
      num_input
    )
  }

  # Boiling point group order
  output$gcm_tb_gorder_ui <- renderUI({
    method <- input$gcm_tb
    if (is.null(method) || method %in% c("manual", "none", "")) return(NULL)
    render_gorder_input("gcm_tb_gorder", method, show_help = TRUE)
  })
  outputOptions(output, "gcm_tb_gorder_ui", suspendWhenHidden = FALSE)

  # Critical parameters group order
  output$gcm_crit_gorder_ui <- renderUI({
    method <- input$gcm_crit
    if (is.null(method) || method == "none" || method == "") return(NULL)
    render_gorder_input("gcm_crit_gorder", method)
  })
  outputOptions(output, "gcm_crit_gorder_ui", suspendWhenHidden = FALSE)

  # HSP group order
  output$gcm_hsp_gorder_ui <- renderUI({
    method <- input$gcm_hsp
    if (is.null(method) || method == "none" || method == "") return(NULL)
    render_gorder_input("gcm_hsp_gorder", method)
  })
  outputOptions(output, "gcm_hsp_gorder_ui", suspendWhenHidden = FALSE)

  # Reactive value to store solute information and GCM results


  # Render molecular plot
  #   output$molecular_plot <- renderPlot({
  #   grid::grid.raster(
  #     sfe_rv$gcm_results[["visres"]][[1]],
  #     width = 1,
  #     height = 1,
  #     interpolate = TRUE
  #   )
  # }, res = 150)  #

  # HSP Visualization

  observeEvent(input$show_hsp_vis, {
    req(sfe_rv$gcm_results$visres$HSP)
    showModal(modalDialog(
      title = i18n$t("HSP Visualization"),
      size = "xl",
      div(
        style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("hsp_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$hsp_fullsize_img <- renderImage(
    {
      req(sfe_rv$gcm_results$visres$HSP)
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = 1200, height = 1200, res = 300)
      grid::grid.raster(sfe_rv$gcm_results$visres$HSP,
        width = 1, height = 1,
        interpolate = TRUE
      )
      dev.off()
      list(
        src = outfile,
        contentType = "image/png",
        width = "100%",
        height = "auto",
        alt = "HSP Visualization"
      )
    },
    deleteFile = TRUE
  )

  # Critical Parameters Visualization
  observeEvent(input$show_critical_vis, {
    req(sfe_rv$gcm_results$visres$Critical)
    showModal(modalDialog(
      title = i18n$t("Critical Parameters Visualization"),
      size = "xl",
      div(
        style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("critical_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$critical_fullsize_img <- renderImage(
    {
      req(sfe_rv$gcm_results$visres$Critical)
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = 1200, height = 1200, res = 300)
      grid::grid.raster(sfe_rv$gcm_results$visres$Critical,
        width = 1, height = 1,
        interpolate = TRUE
      )
      dev.off()
      list(
        src = outfile,
        contentType = "image/png",
        width = "100%",
        height = "auto",
        alt = "Critical Parameters Visualization"
      )
    },
    deleteFile = TRUE
  )

  # Tb Visualization
  observeEvent(input$show_tb_vis, {
    req(sfe_rv$gcm_results$visres$Tb)
    showModal(modalDialog(
      title = HTML(paste0("<i>T</i><sub><i>b</i></sub> ", i18n$t("Visualization"))),
      size = "xl",
      div(
        style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("tb_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$tb_fullsize_img <- renderImage(
    {
      req(sfe_rv$gcm_results$visres$Tb)
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = 1200, height = 1200, res = 300)
      grid::grid.raster(sfe_rv$gcm_results$visres$Tb,
        width = 1, height = 1,
        interpolate = TRUE
      )
      dev.off()
      list(
        src = outfile,
        contentType = "image/png",
        width = "100%",
        height = "auto",
        alt = "Tb Visualization"
      )
    },
    deleteFile = TRUE
  )


  # Render predicted parameters table
  output$predicted_params_table <- DT::renderDataTable({
    req(sfe_rv$gcm_results$pares)

    # Combine Critical and HSP parameters
    critical_df <- data.frame(
      Parameter = names(sfe_rv$gcm_results$pares$Critical),
      Value = unlist(sfe_rv$gcm_results$pares$Critical),
      Category = "Critical Parameters"
    )

    hsp_df <- data.frame(
      Parameter = names(sfe_rv$gcm_results$pares$HSP),
      Value = unlist(sfe_rv$gcm_results$pares$HSP),
      Category = "Hansen Solubility Parameters"
    )

    # Combine all parameters
    parameters_df <- rbind(critical_df, hsp_df)

    # Add VDW if available
    if (!is.null(sfe_rv$gcm_results$pares$VDW)) {
      vdw_df <- data.frame(
        Parameter = "VDW",
        Value = sfe_rv$gcm_results$pares$VDW,
        Category = "Van der Waals Volume"
      )
      parameters_df <- rbind(parameters_df, vdw_df)
    }
    
    # Format parameter names with proper scientific notation (HTML) and explanatory text
    param_formatting <- c(
      "Tb" = paste0("<i>T</i><sub><i>b</i></sub> (", i18n_r()$t("Boiling Point"), ", K)"),
      "Tb_corr" = paste0("<i>T</i><sub><i>b,corr</i></sub> (", i18n_r()$t("Corrected Boiling Point"), ", K)"),
      "Tc" = paste0("<i>T</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Temperature"), ", K)"),
      "Pc" = paste0("<i>P</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Pressure"), ", bar)"),
      "Vc" = paste0("<i>V</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Volume"), ", cm³/mol)"),
      "dD" = paste0("δ<sub>D</sub> (", i18n_r()$t("Dispersion"), ", √MPa)"),
      "dP" = paste0("δ<sub>P</sub> (", i18n_r()$t("Polarity"), ", √MPa)"),
      "dHB" = paste0("δ<sub>HB</sub> (", i18n_r()$t("Hydrogen Bonding"), ", √MPa)")
    )
    parameters_df$Parameter <- my_mapvalues(parameters_df$Parameter, names(param_formatting), param_formatting, warn_missing = FALSE)

    # Translate category values
    category_translations <- c(
      "Critical Parameters" = i18n_r()$t("Critical Parameters"),
      "Hansen Solubility Parameters" = i18n_r()$t("Hansen Solubility Parameters"),
      "Van der Waals Volume" = i18n_r()$t("Van der Waals Volume")
    )
    parameters_df$Category <- my_mapvalues(parameters_df$Category, names(category_translations), category_translations, warn_missing = FALSE)

    # Create column headers with tooltips
    cols_short <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Value"),
      i18n_r()$t("Category")
    )

    cols_full <- c(
      i18n_r()$t("Parameter Type"),
      i18n_r()$t("Numeric Value"),
      i18n_r()$t("Parameter Category")
    )

    header_html <- sprintf(
      "<span title='%s'>%s</span>",
      cols_full, cols_short
    )

    DT::datatable(
      parameters_df,
      extensions = "Buttons",
      colnames = header_html,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_predicted_params")
      )
    ) %>%
      DT::formatRound(columns = "Value", digits = 2)
  })

  # Render GCM comparison table
  output$gcm_comparison_table <- DT::renderDataTable({
    req(sfe_rv$gcm_comparison)

    # Prepare data for translation
    dt_data <- sfe_rv$gcm_comparison

    # Translate parameter values
    param_translations <- c(
      "tb" = i18n_r()$t("Boiling Point"),
      "crit" = i18n_r()$t("Critical Parameters"),
      "hsp" = i18n_r()$t("Hansen Solubility Parameters"),
      "vdw" = i18n_r()$t("Van der Waals Volume")
    )
    dt_data$parameter <- my_mapvalues(dt_data$parameter, names(param_translations), param_translations, warn_missing = FALSE)

    # Translate method names
    method_translations <- c(
      "JR" = i18n_r()$t("Joback-Reid"),
      "JR_corr" = i18n_r()$t("Joback-Reid corrected"),
      "SB" = i18n_r()$t("Stein-Brown"),
      "SB_corr" = i18n_r()$t("Stein-Brown corrected"),
      "NL04" = i18n_r()$t("Nannoolal 2004"),
      "NL07" = i18n_r()$t("Nannoolal 2007"),
      "NL07_robust" = i18n_r()$t("Nannoolal 2007 robust"),
      "SP08" = i18n_r()$t("Stefanis-Panayiotou 2008"),
      "SP12" = i18n_r()$t("Stefanis-Panayiotou 2012"),
      "HKR_STW" = i18n_r()$t("Hukkerikar-Stepwise"),
      "HKR_SIM" = i18n_r()$t("Hukkerikar-Simultaneous"),
      "ZHAO" = i18n_r()$t("Zhao 2003"),
      "BND" = i18n_r()$t("Bondi 1964"),
      "SLON" = i18n_r()$t("Slonimskii 1970")
    )
    dt_data$method <- my_mapvalues(dt_data$method, names(method_translations), method_translations, warn_missing = FALSE)
    
    # Remove method abbreviations in parentheses from display (e.g. "Joback-Reid" -> "Joback-Reid")
    dt_data$method <- trimws(gsub("\\s*\\([^)]*\\)\\s*$", "", dt_data$method))

    # Translate simplicity values
    simplicity_translations <- c(
      "auto" = i18n_r()$t("Auto"),
      "simple" = i18n_r()$t("Simple"),
      "normal" = i18n_r()$t("Normal"),
      "complex" = i18n_r()$t("Complex")
    )
    dt_data$simplicity <- my_mapvalues(dt_data$simplicity, names(simplicity_translations), simplicity_translations, warn_missing = FALSE)

    # Create column headers with tooltips - use HTML for proper scientific notation
    add_prettynames_short <- c(
      i18n_r()$t("Parameter"),
      i18n_r()$t("Method"),
      i18n_r()$t("Simplicity"),
      i18n_r()$t("Order"),
      "<i>T</i><sub><i>b</i></sub>",
      "<i>T</i><sub><i>b,corr</i></sub>",
      "<i>T</i><sub><i>c</i></sub>",
      "<i>P</i><sub><i>c</i></sub>",
      "<i>V</i><sub><i>c</i></sub>",
      "δ<sub>D</sub>",
      "δ<sub>P</sub>",
      "δ<sub>HB</sub>",
      i18n_r()$t("VDW")
    )

    add_prettynames_full <- c(
      i18n_r()$t("Parameter Type"),
      i18n_r()$t("Estimation Method"),
      i18n_r()$t("Fragmentation Simplicity Level"),
      i18n_r()$t("Maximum Group Order"),
      i18n_r()$t("Boiling Point (K)"),
      i18n_r()$t("Corrected Boiling Point (K)"),
      i18n_r()$t("Critical Temperature (K)"),
      i18n_r()$t("Critical Pressure (bar)"),
      i18n_r()$t("Critical Volume (cm³/mol)"),
      i18n_r()$t("Dispersion Component (√MPa)"),
      i18n_r()$t("Polarity Component (√MPa)"),
      i18n_r()$t("Hydrogen Bonding Component (√MPa)"),
      i18n_r()$t("Van der Waals Volume (cm³/mol)")
    )

    add_prettynames_html <- sprintf(
      "<span title='%s'>%s</span>",
      add_prettynames_full, add_prettynames_short
    )

    DT::datatable(
      dt_data,
      extensions = "Buttons",
      colnames =add_prettynames_html,
      escape = FALSE,
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        pageLength = 20,
        language = tablang(),
        buttons = create_dt_export_buttons(i18n_r, "supercrit_gcm_comparison")
      ),
      rownames = FALSE
    ) %>% 
      formatRound(columns = c("Tb", "Tb_corr", "Tc", "Pc", "Vc", "dD", "dP", "dHB"), digits = 2) %>%
      formatRound(columns = c("VDW"), digits = 8)
  })

  # Render GCM comparison title with solute name
  output$gcm_comparison_title <- renderUI({
    req(sfe_rv$gcm_comparison)
    solute_name <- input$name_input
    if (is.null(solute_name) || solute_name == "") {
      solute_name <- i18n_r()$t("Unknown Solute")
    }
    div(
      style = "margin-bottom: 15px;",
      h4(sprintf("%s: %s", i18n_r()$t("GCM Comparison for"), solute_name))
    )
  })

  # Render group contributions table
  output$group_contributions_table <- renderUI({
    req(sfe_rv$gcm_results$pares$Contribs)

    contribs_list <- sfe_rv$gcm_results$pares$Contribs

    # Define translation mapping for all possible column names
    col_name_mapping <- list(
      # Common columns
      "Group" = list(short = i18n_r()$t("Group"), full = i18n_r()$t("Functional Group Name")),
      "ID" = list(short = i18n_r()$t("ID"), full = i18n_r()$t("Group Identifier")),
      "Type" = list(short = i18n_r()$t("Type"), full = i18n_r()$t("Group Order")),
      "Occurrences" = list(short = i18n_r()$t("Occurrences"), full = i18n_r()$t("Number of Occurrences")),

      # Tb columns
      "Tb_cont" = list(short = "<i>T</i><sub><i>b</i></sub>", full = i18n_r()$t("Single group contribution to boiling point")),
      "Sum_Tb_cont" = list(short = paste0("Σ<i>T</i><sub><i>b</i></sub>"), full = i18n_r()$t("Cumulative contribution (group × occurrences)")),

      # Critical columns
      "Tc_cont" = list(short = "<i>T</i><sub><i>c</i></sub>", full = i18n_r()$t("Single group contribution to critical temperature")),
      "Sum_Tc_cont" = list(short = "Σ<i>T</i><sub><i>c</i></sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "Pc_cont" = list(short = "<i>P</i><sub><i>c</i></sub>", full = i18n_r()$t("Single group contribution to critical pressure")),
      "Sum_Pc_cont" = list(short = "Σ<i>P</i><sub><i>c</i></sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "Vc_cont" = list(short = "<i>V</i><sub><i>c</i></sub>", full = i18n_r()$t("Single group contribution to critical volume")),
      "Sum_Vc_cont" = list(short = "Σ<i>V</i><sub><i>c</i></sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),

      # HSP columns
      "dD_cont" = list(short = "δ<sub>D</sub>", full = i18n_r()$t("Single group contribution to dispersion parameter")),
      "Sum_dD_cont" = list(short = "Σδ<sub>D</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dP_cont" = list(short = "δ<sub>P</sub>", full = i18n_r()$t("Single group contribution to polarity parameter")),
      "Sum_dP_cont" = list(short = "Σδ<sub>P</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dHB_cont" = list(short = "δ<sub>HB</sub>", full = i18n_r()$t("Single group contribution to hydrogen bonding parameter")),
      "Sum_dHB_cont" = list(short = "Σδ<sub>HB</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dP_low_cont" = list(short = "δ<sub>P,low</sub>", full = i18n_r()$t("Single group contribution to polarity parameter (low)")),
      "Sum_dP_low_cont" = list(short = "Σδ<sub>P,low</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),
      "dHB_low_cont" = list(short = "δ<sub>HB,low</sub>", full = i18n_r()$t("Single group contribution to hydrogen bonding parameter (low)")),
      "Sum_dHB_low_cont" = list(short = "Σδ<sub>HB,low</sub>", full = i18n_r()$t("Cumulative contribution (group × occurrences)")),

      # VDW columns
      "VDW_cont" = list(short = i18n_r()$t("VDW"), full = i18n_r()$t("Single group contribution to Van der Waals volume")),
      "Sum_VDW_cont" = list(short = paste0("Σ", i18n_r()$t("VDW")), full = i18n_r()$t("Cumulative contribution (group × occurrences)"))
    )

    # Display names for section headers (use plain strings, will wrap with HTML when rendering)
    display_names_translations <- c(
      "Boiling Point (Tb) Contributions" = paste0(i18n_r()$t("Boiling Point"), " (<i>T</i><sub><i>b</i></sub>) ", i18n_r()$t("Contributions")),
      "Critical Parameters Contributions" = i18n_r()$t("Critical Parameters Contributions"),
      "Hansen Solubility Parameters (HSP) Contributions" = i18n_r()$t("Hansen Solubility Parameters (HSP) Contributions"),
      "Van der Waals Volume Contributions" = i18n_r()$t("Van der Waals Volume Contributions")
    )

    display_names <- c(
      "Tb" = "Boiling Point (Tb) Contributions",
      "Critical" = "Critical Parameters Contributions",
      "HSP" = "Hansen Solubility Parameters (HSP) Contributions",
      "VDW" = "Van der Waals Volume Contributions"
    )

    # Generate a list of data tables, one for each contribution type
    table_outputs <- lapply(names(contribs_list), function(param_name) {
      df <- contribs_list[[param_name]]

      # Skip if NULL or empty (VDW may not exist for some molecules)
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      # Get actual column names from dataframe
      actual_cols <- colnames(df)

      # Map column names to short and full translations
      cols_short <- sapply(actual_cols, function(col) {
        if (!is.null(col_name_mapping[[col]])) {
          col_name_mapping[[col]]$short
        } else {
          col  # Fallback to original name if no mapping
        }
      })

      cols_full <- sapply(actual_cols, function(col) {
        if (!is.null(col_name_mapping[[col]])) {
          col_name_mapping[[col]]$full
        } else {
          col  # Fallback to original name if no mapping
        }
      })

      # Create tooltip headers
      header_html <- sprintf("<span title='%s'>%s</span>", cols_full, cols_short)

      output_id <- paste0("group_contrib_", param_name)
      output[[output_id]] <- DT::renderDataTable({
        # Identify numeric columns for rounding (exclude integer columns)
        integer_cols <- intersect(names(df), c("ID", "Type", "Occurrences"))
        numeric_cols <- setdiff(names(df)[sapply(df, is.numeric)], integer_cols)
        
        dt <- DT::datatable(
          df,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "Bfrtip",
            buttons = list(
              list(
                extend = "copy",
                text = i18n_r()$t("Copy"),
                titleAttr = i18n_r()$t("Copy"),
                action = copy_button_no_popup(
                  copy_label = i18n_r()$t("Copy"),
                  copied_label = i18n_r()$t("Copied!")
                )
              ),
              list(extend = "csv", filename = generate_filename_with_timestamp(paste0("supercrit_contrib_", param_name))),
              list(extend = "excel", filename = generate_filename_with_timestamp(paste0("supercrit_contrib_", param_name))),
              list(extend = "pdf", filename = generate_filename_with_timestamp(paste0("supercrit_contrib_", param_name)))
            ),
            language = tablang()
          ),
          extensions = "Buttons",
          colnames = header_html,
          escape = FALSE,
          rownames = FALSE
        )
        
        # Format integer columns with 0 decimal places
        if (length(integer_cols) > 0) {
          dt <- DT::formatRound(dt, columns = integer_cols, digits = 0)
        }
        
        # Round remaining numeric columns to 4 decimal places
        if (length(numeric_cols) > 0) {
          dt <- DT::formatRound(dt, columns = numeric_cols, digits = 4)
        }
        
        dt
      })

      # Get translated display name
      display_key <- display_names[param_name]
      translated_name <- display_names_translations[display_key]
      if (is.na(translated_name)) translated_name <- param_name

      tagList(
        h4(HTML(translated_name)),
        DT::dataTableOutput(ns(output_id))
      )
    })

    # Remove NULL entries (from skipped tables)
    table_outputs <- Filter(Negate(is.null), table_outputs)

    # Return all tables wrapped in a tagList
    do.call(tagList, table_outputs)
  })



  # Render solute details
  output$solute_details <- renderUI({
    req(sfe_rv$gcm_results)

    ids <- sfe_rv$gcm_results$solute_data$IDs
    synonyms <- sfe_rv$gcm_results$solute_data$Synonyms
    pares <- sfe_rv$gcm_results$pares
    
    # Build name display with synonyms
    display_name <- ids[["Name"]]
    if (!is.null(synonyms) && length(synonyms) > 0) {
      # Take first 3 synonyms for display
      syns_to_show <- head(synonyms, 3)
      synonyms_text <- paste(syns_to_show, collapse = ", ")
      name_display <- tagList(
        tags$span(display_name),
        tags$br(),
        tags$small(
          class = "text-muted",
          style = "font-style: italic;",
          paste0(i18n_r()$t("Names from CAS: "), synonyms_text)
        )
      )
    } else {
      name_display <- display_name
    }

    tagList(
      # Molecular Descriptors & Properties Table
      fluidRow(
        column(
          width = 12,
          h4(style = "color: #3c8dbc; border-bottom: 2px solid #3c8dbc; padding-bottom: 5px; margin-bottom: 15px;",
            icon("flask"), " ", i18n_r()$t("Molecular Descriptors")
          ),
          tags$table(
            class = "table table-striped table-hover table-bordered table-sm",
            style = "margin-bottom: 20px;",
            tags$thead(
              tags$tr(
                tags$th(style = "width: 25%; background-color: #f4f4f4;", i18n_r()$t("Property")),
                tags$th(style = "width: 75%; background-color: #f4f4f4;", i18n_r()$t("Value"))
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Display Name:"))),
                tags$td(name_display)
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("CAS:"))),
                tags$td(ids[["CAS"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Molecular Formula:"))),
                tags$td(ids[["MF"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Molecular Weight:"))),
                tags$td(round(as.numeric(ids[["MW"]]), 3), " ", tags$em(class = "text-muted", i18n_r()$t("g/mol")))
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Atom Count:"))),
                tags$td(ids[["Atom_Count"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Hydrogen Count:"))),
                tags$td(ids[["H_Count"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("Aromaticity:"))),
                tags$td(ids[["Aromaticity"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("SMILES:"))),
                tags$td(style = "word-break: break-all; font-family: monospace; font-size: 0.9em;", ids[["SMILES"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("InChI:"))),
                tags$td(style = "word-break: break-all; font-family: monospace; font-size: 0.85em;", ids[["InChI"]])
              ),
              tags$tr(
                tags$td(tags$strong(i18n_r()$t("InChIKey:"))),
                tags$td(style = "font-family: monospace; font-size: 0.9em;", ids[["InChIKey"]])
              )
            )
          )
        )
      ),
      # Critical Parameters & Hansen Parameters Tables
      fluidRow(
        column(
          width = 6,
          h4(style = "color: #f39c12; border-bottom: 2px solid #f39c12; padding-bottom: 5px; margin-bottom: 15px;",
            icon("temperature-high"), " ", i18n_r()$t("Critical Parameters")
          ),
          tags$table(
            class = "table table-striped table-hover table-bordered table-sm",
            style = "margin-bottom: 20px;",
            tags$thead(
              tags$tr(
                tags$th(style = "width: 40%; background-color: #fff3e0;", i18n_r()$t("Parameter")),
                tags$th(style = "width: 60%; background-color: #fff3e0;", i18n_r()$t("Value"))
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$strong(HTML(paste0("<i>T</i><sub><i>b</i></sub> (", i18n_r()$t("Boiling Point"), "):")))),
                tags$td(round(pares$Critical["Tb"], 2), " ", tags$em(class = "text-muted", i18n_r()$t("K")))
              ),
              tags$tr(
                tags$td(tags$strong(HTML(paste0("<i>T</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Temperature"), "):")))),
                tags$td(round(pares$Critical["Tc"], 2), " ", tags$em(class = "text-muted", i18n_r()$t("K")))
              ),
              tags$tr(
                tags$td(tags$strong(HTML(paste0("<i>P</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Pressure"), "):")))),
                tags$td(round(pares$Critical["Pc"], 2), " ", tags$em(class = "text-muted", i18n_r()$t("MPa")))
              ),
              tags$tr(
                tags$td(tags$strong(HTML(paste0("<i>V</i><sub><i>c</i></sub> (", i18n_r()$t("Critical Volume"), "):")))),
                tags$td(round(pares$Critical["Vc"], 2), " ", tags$em(class = "text-muted", i18n_r()$t("mL/mol")))
              )
            )
          )
        ),
        column(
          width = 6,
          h4(style = "color: #605ca8; border-bottom: 2px solid #605ca8; padding-bottom: 5px; margin-bottom: 15px;",
            icon("water"), " ", i18n_r()$t("Hansen Solubility Parameters")
          ),
          tags$table(
            class = "table table-striped table-hover table-bordered table-sm",
            style = "margin-bottom: 20px;",
            tags$thead(
              tags$tr(
                tags$th(style = "width: 40%; background-color: #f3f2f7;", i18n_r()$t("Parameter")),
                tags$th(style = "width: 60%; background-color: #f3f2f7;", i18n_r()$t("Value"))
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$strong(HTML(paste0("δ<sub>D</sub> (", i18n_r()$t("Dispersion"), "):")))),
                tags$td(round(pares$HSP["dD"], 2), " ", tags$em(class = "text-muted", i18n_r()$t("√MPa")))
              ),
              tags$tr(
                tags$td(tags$strong(HTML(paste0("δ<sub>P</sub> (", i18n_r()$t("Polarity"), "):")))),
                tags$td(round(pares$HSP["dP"], 2), " ", tags$em(class = "text-muted", i18n_r()$t("√MPa")))
              ),
              tags$tr(
                tags$td(tags$strong(HTML(paste0("δ<sub>HB</sub> (", i18n_r()$t("Hydrogen Bonding"), "):")))),
                tags$td(round(pares$HSP["dHB"], 2), " ", tags$em(class = "text-muted", i18n_r()$t("√MPa")))
              )
            )
          )
        )
      ),
      # Van der Waals Volume (if available)
      if (!is.null(pares$VDW)) {
        fluidRow(
          column(
            width = 12,
            h4(style = "color: #00a65a; border-bottom: 2px solid #00a65a; padding-bottom: 5px; margin-bottom: 15px;",
              icon("cube"), " ", i18n_r()$t("Van der Waals Volume")
            ),
            tags$table(
              class = "table table-striped table-hover table-bordered table-sm",
              style = "margin-bottom: 20px;",
              tags$thead(
                tags$tr(
                  tags$th(style = "width: 25%; background-color: #edfaf4;", i18n_r()$t("Parameter")),
                  tags$th(style = "width: 75%; background-color: #edfaf4;", i18n_r()$t("Value"))
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$strong(i18n_r()$t("VDW Volume:"))),
                  tags$td(round(pares$VDW, 6), " ", tags$em(class = "text-muted", i18n_r()$t("L/mol")))
                )
              )
            )
          )
        )
      }
    )
  })




  # Observe "Characterize Solute (GCM)" button click
  observeEvent(input$characterize_solute, {
    req(input$cas_input, input$name_input) # SMILES is now optional

    if (isTRUE(input$specify_smiles)) {
      req(input$smiles_input)
      solute_input <- c(
        SMILES = input$smiles_input,
        CAS = input$cas_input,
        Name = input$name_input
      )
    } else {
      solute_input <- c(
        CAS = input$cas_input,
        Name = input$name_input
      )
    }

    withProgress(message = i18n$t("Characterizing Solute (GCM)..."), {
      sfe_rv$gcm_results <- tryCatch(
        {
          # First get solute data
          sfe_rv$solute_data <- mol_find(solute_input)

          # Then characterize with GCM
          # Build named gorder vector from per-method inputs
          gorder_vec <- c()
          if (!is.null(input$gcm_tb) && !input$gcm_tb %in% c("manual", "none", "")) {
            val <- if (!is.null(input$gcm_tb_gorder)) input$gcm_tb_gorder else 0
            gorder_vec[input$gcm_tb] <- val
          }
          if (!is.null(input$gcm_crit) && !input$gcm_crit %in% c("none", "")) {
            val <- if (!is.null(input$gcm_crit_gorder)) input$gcm_crit_gorder else 0
            gorder_vec[input$gcm_crit] <- val
          }
          if (!is.null(input$gcm_hsp) && !input$gcm_hsp %in% c("none", "")) {
            val <- if (!is.null(input$gcm_hsp_gorder)) input$gcm_hsp_gorder else 0
            gorder_vec[input$gcm_hsp] <- val
          }
          # Use 0 (all orders) as fallback if no methods selected
          if (length(gorder_vec) == 0) gorder_vec <- 0

          params <- list(
            solute = solute_input,
            simplicity = input$gcm_simplicity,
            gorder = gorder_vec,
            hlight = TRUE, # Always highlight for visualization
            silent = TRUE
          )

          # Only add parameters if they have valid values (not "none" or empty)
          if (!is.na(input$gcm_tb) && input$gcm_tb != "none" && input$gcm_tb != "") {
            if (input$gcm_tb == "manual") {
              # Use the manual input value (numeric)
              req(input$tb_manual_input)
              params$tb <- as.numeric(input$tb_manual_input)
            } else {
              params$tb <- input$gcm_tb
            }
          }
          if (!is.na(input$gcm_crit) && input$gcm_crit != "none" && input$gcm_crit != "") {
            params$crit <- input$gcm_crit
          }
          if (!is.na(input$gcm_hsp) && input$gcm_hsp != "none" && input$gcm_hsp != "") {
            params$hsp <- input$gcm_hsp
          }
          if (!is.na(input$gcm_vdw) && input$gcm_vdw != "none" && input$gcm_vdw != "") {
            params$vdw <- input$gcm_vdw
          }
          print("Debug Tb")
          print(params)

          gcm_output <- do.call(est_gcm, params)

          gcm_output
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during GCM characterization:"), e$message), type = "error")
          return(NULL)
        }
      )

      # Save the characterization results after successful processing
      if (!is.null(sfe_rv$gcm_results)) {
        # Save the calculation to session storage for misc_comp and misc_opt modules
        # FIFO limit: Maximum 10 calculations (including seed data)
        if (length(sfe_rv$saved_calculations) >= 10) {
          # Find oldest calculation based on timestamp
          timestamps <- sapply(sfe_rv$saved_calculations, function(x) as.POSIXct(x$timestamp))
          oldest_idx <- which.min(timestamps)

          # Remove oldest and compact list
          sfe_rv$saved_calculations[[oldest_idx]] <- NULL
          sfe_rv$saved_calculations <- Filter(Negate(is.null), sfe_rv$saved_calculations)
        }

        # Generate display name
        methods <- paste(
          c(
            if (!is.na(input$gcm_tb) && input$gcm_tb != "none") input$gcm_tb else NULL,
            if (!is.na(input$gcm_crit) && input$gcm_crit != "none") input$gcm_crit else NULL,
            if (!is.na(input$gcm_hsp) && input$gcm_hsp != "none") input$gcm_hsp else NULL
          ),
          collapse = "-"
        )
        time_str <- format(Sys.time(), "%H:%M:%S")
        display_name <- sprintf("%s | %s | %s", input$name_input, methods, time_str)

        # Save to session-based storage (only characterize parameters for misc_comp/misc_opt usage)
        new_id <- length(sfe_rv$saved_calculations) + 1
        sfe_rv$saved_calculations[[new_id]] <- list(
          id = new_id,
          display_name = display_name,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          source_module = "Solute Characterization",
          parameters = list(
            cas_input = input$cas_input,
            name_input = input$name_input,
            smiles_input = if (isTRUE(input$specify_smiles)) input$smiles_input else "",
            specify_smiles = isTRUE(input$specify_smiles),
            gcm_tb = if (!is.na(input$gcm_tb) && input$gcm_tb != "none") input$gcm_tb else "none",
            tb_manual_input = if (input$gcm_tb == "manual") input$tb_manual_input else NA,
            gcm_crit = if (!is.na(input$gcm_crit) && input$gcm_crit != "none") input$gcm_crit else "none",
            gcm_hsp = if (!is.na(input$gcm_hsp) && input$gcm_hsp != "none") input$gcm_hsp else "none",
            gcm_simplicity = input$gcm_simplicity,
            gcm_tb_gorder = if (!is.null(input$gcm_tb_gorder)) input$gcm_tb_gorder else 0,
            gcm_crit_gorder = if (!is.null(input$gcm_crit_gorder)) input$gcm_crit_gorder else 0,
            gcm_hsp_gorder = if (!is.null(input$gcm_hsp_gorder)) input$gcm_hsp_gorder else 0
          )
        )

        # Sort calculations by timestamp (newest first) for dropdown display
        timestamps <- sapply(sfe_rv$saved_calculations, function(x) as.POSIXct(x$timestamp))
        sorted_indices <- order(timestamps, decreasing = TRUE)
        sfe_rv$saved_calculations <- sfe_rv$saved_calculations[sorted_indices]

        showNotification(i18n$t("GCM characterization completed and saved to session."), type = "message")
        updateTabsetPanel(session, "sol_char_tabs", selected = "solute_results")
      }
    })
  })

  observeEvent(input$specify_smiles,
    {
      if (isTRUE(input$specify_smiles)) {
        shinyjs::show("smiles_div")
      } else {
        shinyjs::hide("smiles_div")
      }
    },
    ignoreInit = FALSE
  )

  # Toggle manual Tb input visibility (replaces Group Order field)
  observeEvent(input$gcm_tb,
    {
      if (input$gcm_tb == "manual") {
        shinyjs::show("tb_manual_div")
        shinyjs::hide("gcm_tb_gorder_wrapper")
      } else {
        shinyjs::hide("tb_manual_div")
        shinyjs::show("gcm_tb_gorder_wrapper")
      }
    },
    ignoreInit = FALSE
  )

  # GCM Comparison: Maximum Group Order with range badge
  output$gcm_comp_gorder_ui <- renderUI({
    cur_val <- input$gcm_comp_gorder
    val <- if (!is.null(cur_val) && !is.na(cur_val)) max(0, min(3, cur_val)) else 0
    tags$div(
      tags$label(
        i18n_r()$t("Maximum Group Order"),
        input_help(i18n_r()$t("Maximum order of molecular fragment groups used in the group contribution method. 0 = first-order only (fastest, least accurate). Higher values use higher-order corrections for better accuracy but slower computation."),
                   title = i18n_r()$t("Maximum Group Order"), buttonLabel = i18n_r()$t("OK")),
        class = "control-label",
        style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
        tags$span(
          "0\u20133",
          style = "font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: #6c757d; color: white; margin-left: auto; font-weight: normal;",
          title = paste0(i18n$t("Valid range:"), " 0 \u2013 3")
        )
      ),
      numericInput(ns("gcm_comp_gorder"), label = NULL, value = val, min = 0, max = 3, step = 1)
    )
  })

  # Observe "Compare GCM" button click
  observeEvent(input$compare_gcm, {
    req(input$cas_input, input$name_input) # SMILES is now optional

    if (isTRUE(input$specify_smiles)) {
      req(input$smiles_input)
      solute_input <- c(
        SMILES = input$smiles_input,
        CAS = input$cas_input,
        Name = input$name_input
      )
    } else {
      solute_input <- c(
        CAS = input$cas_input,
        Name = input$name_input
      )
    }

    withProgress(message = i18n$t("Comparing GCM..."), {
      sfe_rv$gcm_comparison <- tryCatch(
        {
          compare_gcm(
            solute = solute_input,
            gorder = input$gcm_comp_gorder,
            simplicity = input$gcm_comp_simplicity
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during GCM comparison:"), e$message), type = "error")
          return(NULL)
        }
      )

      if (!is.null(sfe_rv$gcm_comparison)) {
        updateTabsetPanel(session, "sol_char_tabs", selected = "gcm_comparison")
        showNotification(i18n$t("GCM comparison completed"), type = "message")
      } else {
        showNotification(i18n$t("No comparison results"), type = "warning")
      }
    })
  })

  # ===== SAVE TO DISK FUNCTIONALITY =====
  
  # Set up shinyFiles directory chooser with system volumes
  volumes <- c(
    Home = Sys.getenv("HOME"),
    shinyFiles::getVolumes()()
  )
  shinyFiles::shinyDirChoose(
    input, 
    "save_modal-browse_directory", 
    roots = volumes,
    session = session
  )
  
  # Update text input when directory is selected via browser
  observeEvent(input[["save_modal-browse_directory"]], {
    req(input[["save_modal-browse_directory"]])
    dir_path <- shinyFiles::parseDirPath(volumes, input[["save_modal-browse_directory"]])
    if (length(dir_path) > 0 && nchar(dir_path) > 0) {
      updateTextInput(session, "save_modal-save_directory", value = dir_path)
    }
  })
  
  # Observe "Save to Disk" button click - show modal
  observeEvent(input$save_to_disk, {
    # Check if there's a recent calculation to save
    if (length(sfe_rv$saved_calculations) == 0) {
      showNotification(i18n$t("No calculations to save. Run a calculation first."), type = "warning")
      return()
    }
    
    # Get the most recent calculation
    recent_calc <- sfe_rv$saved_calculations[[1]]
    
    # Get default directory
    default_dir <- file.path(
      system.file(package = "supeRcrit"),
      "shiny-app", "config", "user-settings", "sfe_gcm_calculations"
    )
    
    # Show save modal
    showModal(modalDialog(
      title = i18n$t("Save Calculation to Disk"),
      size = "m",
      
      # Show which calculation will be saved
      div(
        class = "alert alert-info",
        icon("info-circle"),
        strong(i18n$t("Saving:")), " ", recent_calc$display_name
      ),
      
      # Filename input
      textInput(
        ns("save_modal-filename"),
        i18n$t("Filename:"),
        value = gsub("[^a-zA-Z0-9_.-]", "_", recent_calc$parameters$name_input),
        width = "100%"
      ),
      
      # Directory selection
      div(
        style = "display: flex; gap: 10px; align-items: flex-end;",
        div(
          style = "flex-grow: 1;",
          textInput(
            ns("save_modal-save_directory"),
            i18n$t("Save Directory"),
            value = default_dir,
            width = "100%"
          )
        ),
        div(
          style = "margin-bottom: 15px;",
          shinyFiles::shinyDirButton(
            ns("save_modal-browse_directory"),
            label = i18n$t("Browse..."),
            title = i18n$t("Select Save Directory"),
            class = "btn-default"
          )
        )
      ),
      tags$small(
        class = "text-muted",
        i18n$t("Leave empty to use the default package directory.")
      ),
      
      # Filename preview
      uiOutput(ns("save_modal-filename_preview")),
      
      footer = tagList(
        modalButton(i18n$t("Cancel")),
        actionButton(ns("save_modal-confirm_save"), i18n$t("Save"), class = "btn btn-success", style = "color: white;")
      )
    ))
  })
  
  # Render filename preview
  output[["save_modal-filename_preview"]] <- renderUI({
    req(input[["save_modal-filename"]])
    filename <- input[["save_modal-filename"]]
    
    # Sanitize filename
    sanitized <- gsub("[^a-zA-Z0-9_.-]", "_", filename)
    if (nchar(sanitized) == 0) sanitized <- "untitled"
    
    # Add timestamp
    timestamp_str <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
    final_filename <- paste0(sanitized, "_", timestamp_str, ".json")
    
    div(
      style = "margin-top: 10px;",
      class = "alert alert-secondary",
      strong(i18n$t("Final filename:")), " ", final_filename
    )
  })
  
  # Confirm save to disk
  observeEvent(input[["save_modal-confirm_save"]], {
    req(input[["save_modal-filename"]])
    
    removeModal()
    
    tryCatch({
      # Get the most recent calculation
      recent_calc <- sfe_rv$saved_calculations[[1]]
      
      # Sanitize filename
      filename <- gsub("[^a-zA-Z0-9_.-]", "_", input[["save_modal-filename"]])
      if (nchar(filename) == 0) filename <- "untitled"
      
      # Add timestamp to filename
      timestamp_str <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
      final_filename <- paste0(filename, "_", timestamp_str, ".json")
      
      # Determine save directory
      save_dir <- input[["save_modal-save_directory"]]
      if (is.null(save_dir) || nchar(trimws(save_dir)) == 0) {
        save_dir <- file.path(
          system.file(package = "supeRcrit"),
          "shiny-app", "config", "user-settings", "sfe_gcm_calculations"
        )
      }
      
      # Ensure directory exists
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Full file path
      filepath <- file.path(save_dir, final_filename)
      
      # Prepare data to save (same structure as session storage)
      save_data <- list(
        display_name = recent_calc$display_name,
        timestamp = recent_calc$timestamp,
        source_module = recent_calc$source_module,
        parameters = recent_calc$parameters,
        metadata = list(
          created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          app_version = "0.9.0"
        )
      )
      
      # Save as JSON
      jsonlite::write_json(save_data, filepath, auto_unbox = TRUE, pretty = TRUE)
      
      showNotification(
        paste(i18n$t("Calculation saved to:"), basename(filepath)),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste(i18n$t("Error saving file:"), e$message),
        type = "error"
      )
    })
  })

  # Observe "Reset" button click
  observeEvent(input$reset, {
    # Reset solute information inputs
    updateTextInput(session, "cas_input", value = defaults$cas_input)
    updateTextInput(session, "name_input", value = defaults$name_input)
    updateCheckboxInput(session, "specify_smiles", value = FALSE)
    updateTextInput(session, "smiles_input", value = defaults$smiles_input)
    shinyjs::hide("smiles_div")

    # Reset GCM method selection inputs
    updateSelectInput(session, "gcm_tb", selected = defaults$gcm_tb %||% "manual")
    updateNumericInput(session, "tb_manual_input", value = 298.15)
    shinyjs::hide("tb_manual_div")
    updateSelectInput(session, "gcm_crit", selected = defaults$gcm_crit %||% "none")
    updateSelectInput(session, "gcm_hsp", selected = defaults$gcm_hsp %||% "none")
    updateSelectInput(session, "gcm_vdw", selected = "none")
    updateSelectInput(session, "gcm_simplicity", selected = "auto")
    updateNumericInput(session, "gcm_tb_gorder", value = 0)
    updateNumericInput(session, "gcm_crit_gorder", value = 0)
    updateNumericInput(session, "gcm_hsp_gorder", value = 0)

    # Reset GCM comparison inputs
    updateNumericInput(session, "gcm_comp_gorder", value = 0)
    updateSelectInput(session, "gcm_comp_simplicity", selected = "auto")

    # Clear reactive values
    sfe_rv$gcm_results <- NULL
    sfe_rv$gcm_comparison <- NULL
    sfe_rv$solute_data <- NULL

    # Show success notification
    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Render HELP output
  output$sfe_sol_char_HELP <- renderUI({
    create_help_modal(i18n_r, "sfe_sol_char_help_en")
  })

  # Render accordion expand/collapse button with translated tooltips
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "accordion", start_expanded = FALSE)
  })
  # Ensure UI outputs in collapsed accordions are rendered immediately
  outputOptions(output, "gcm_tb_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_crit_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_hsp_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_vdw_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_simplicity_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_comp_simplicity_ui", suspendWhenHidden = FALSE)

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "cas_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_comp_gorder_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_comparison_title", suspendWhenHidden = FALSE)
  outputOptions(output, "group_contributions_table", suspendWhenHidden = FALSE)
  outputOptions(output, "name_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sfe_sol_char_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "smiles_input_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "solute_details", suspendWhenHidden = FALSE)
  outputOptions(output, "specify_smiles_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "tb_manual_input_ui", suspendWhenHidden = FALSE)

}
