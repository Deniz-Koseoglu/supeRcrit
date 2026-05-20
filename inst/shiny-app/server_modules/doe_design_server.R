# DOE Design Server Module
doe_design_server <- function(input, output, session, defaults, i18n, tablang, doe_rv) {



  # Load required libraries
  library(dplyr)
  library(DT)
  library(shinyWidgets)

  # Helper for creating namespaced ids inside this module
  ns <- session$ns
  i18n_r <- reactive({
    i18n
  })

  # Local wrapper for range_badge_input that uses module's ns and i18n_r
  # Calls the global version from utils/general_helpers.R with include_minmax = FALSE
  local_range_badge_input <- function(input_id, label_text, value, min_val, max_val,
                                      step = NA, range_text = NULL, tooltip = NULL,
                                      help_content = NULL, help_title = NULL) {
    range_badge_input(ns, i18n_r, input_id, label_text, value, min_val, max_val,
                      step = step, range_text = range_text, tooltip = tooltip,
                      help_content = help_content, help_title = help_title,
                      include_minmax = FALSE)
  }

  # Helper: translate design description from source code output
  translate_design_description <- function(desc, i18n_fn) {
    if (is.null(desc) || desc == "") return("")
    
    # Parse the description to extract components
    # Examples:
    # "A 3^3 BBD (BOX-BEHNKEN) OPTIMIZATION DESIGN WITH 15 RUNS"
    # "A 3^4 CCC (CENTRAL COMPOSITE) OPTIMIZATION DESIGN WITH 8 STAR POINTS AND 6 CENTER POINTS FOR A TOTAL OF 30 RUNS"
    # "A 2^5 FFD (FULL FACTORIAL) SCREENING DESIGN WITH 32 RUNS"
    # "A 2^(5-2) FRFD (FRACTIONAL FACTORIAL) SCREENING DESIGN WITH 8 RUNS"
    # "A 3^4 TM (TAGUCHI METHOD) SCREENING DESIGN WITH 9 RUNS"
    
    # Extract components using regex
    levels_match <- regmatches(desc, regexpr("[0-9]+\\^", desc))
    factors_match <- regmatches(desc, regexpr("\\^[0-9]+|\\^\\([0-9]+-[0-9]+\\)", desc))
    runs_match <- regmatches(desc, regexpr("[0-9]+ RUNS", desc))
    
    if (length(levels_match) == 0 || length(runs_match) == 0) return(desc)
    
    levels <- gsub("\\^", "", levels_match)
    factors_str <- gsub("\\^", "", factors_match)
    runs <- gsub(" RUNS", "", runs_match)
    
    # Build levels^factors string (keep parentheses for fractional)
    level_factor_str <- paste0(levels, "^", factors_str)
    
    # Determine design type and key
    design_type <- ""
    design_key <- ""
    if (grepl("BBD|BOX-BEHNKEN", desc)) {
      design_type <- "BBD"
      design_key <- "Box-Behnken"
    } else if (grepl("CCC|CIRCUMSCRIBED", desc)) {
      design_type <- "CCC"
      design_key <- "Circumscribed Central Composite"
    } else if (grepl("CCF|FACE-CENTERED", desc)) {
      design_type <- "CCF"
      design_key <- "Face-Centered Central Composite"
    } else if (grepl("CENTRAL COMPOSITE", desc)) {
      design_type <- "CCD"
      design_key <- "Central Composite"
    } else if (grepl("FRFD|FRACTIONAL FACTORIAL", desc)) {
      design_type <- "FrFD"
      design_key <- "Fractional Factorial"
    } else if (grepl("FFD|FULL FACTORIAL", desc)) {
      design_type <- "FFD"
      design_key <- "Full Factorial"
    } else if (grepl("TM|TAGUCHI", desc)) {
      design_type <- "TM"
      design_key <- "Taguchi Method"
    }
    
    # Determine if optimization or screening
    is_optimization <- grepl("OPTIMIZATION", desc)
    
    # Check for star points (CCD)
    has_star_points <- grepl("STAR POINTS", desc)
    
    # Use full sentence templates with placeholders for proper grammar in each language
    # Template: "{purpose} {design_name} design {level_factor} ({type}) with {runs} runs"
    # Russian: "Оптимизационный дизайн Бокса-Бенкена 3^3 (BBD) с 15 опытами"
    
    if (has_star_points) {
      star_match <- regmatches(desc, regexpr("[0-9]+ STAR POINTS", desc))
      star_num <- gsub(" STAR POINTS", "", star_match)
      center_match <- regmatches(desc, regexpr("[0-9]+ CENTER POINTS", desc))
      center_num <- gsub(" CENTER POINTS", "", center_match)
      
      # Template for CCD with star/center points
      if (is_optimization) {
        template <- i18n_fn$t("Optimization {design} design {lf} ({type}) with {star} star points and {center} center points; {runs} runs total")
      } else {
        template <- i18n_fn$t("Screening {design} design {lf} ({type}) with {star} star points and {center} center points; {runs} runs total")
      }
      
      # Replace placeholders
      result <- template
      result <- gsub("\\{design\\}", i18n_fn$t(design_key), result)
      result <- gsub("\\{lf\\}", level_factor_str, result)
      result <- gsub("\\{type\\}", design_type, result)
      result <- gsub("\\{star\\}", star_num, result)
      result <- gsub("\\{center\\}", center_num, result)
      result <- gsub("\\{runs\\}", runs, result)
      
    } else {
      # Simple template without star/center points
      if (is_optimization) {
        template <- i18n_fn$t("Optimization {design} design {lf} ({type}) with {runs} runs")
      } else {
        template <- i18n_fn$t("Screening {design} design {lf} ({type}) with {runs} runs")
      }
      
      # Replace placeholders
      result <- template
      result <- gsub("\\{design\\}", i18n_fn$t(design_key), result)
      result <- gsub("\\{lf\\}", level_factor_str, result)
      result <- gsub("\\{type\\}", design_type, result)
      result <- gsub("\\{runs\\}", runs, result)
    }
    
    return(result)
  }

  # Helper: translate design matrix name (e.g., "BBD_3sup3" -> "BBD 3³")
  translate_matrix_name <- function(name, i18n_fn) {
    if (is.null(name) || name == "") return("")
    
    # Parse patterns like "BBD_3sup3", "CCC_2sup4", "FFD_2sup5", "FRFD_2sup5-2", "TM_3sup4"
    if (grepl("sup", name)) {
      parts <- strsplit(name, "_")[[1]]
      design_type <- parts[1]
      
      # Extract levels and factors from "3sup3" or "2sup5-2"
      level_factor <- gsub("^[A-Z]+_", "", name)
      if (grepl("-", level_factor)) {
        # Fractional: "2sup5-2" -> "2^(5-2)"
        match <- regmatches(level_factor, regexpr("([0-9]+)sup([0-9]+)-([0-9]+)", level_factor, perl = TRUE))
        if (length(match) > 0) {
          nums <- regmatches(match, gregexpr("[0-9]+", match))[[1]]
          return(paste0(design_type, " ", nums[1], "^(", nums[2], "-", nums[3], ")"))
        }
      } else {
        # Regular: "3sup3" -> "3³"
        match <- regmatches(level_factor, regexpr("([0-9]+)sup([0-9]+)", level_factor, perl = TRUE))
        if (length(match) > 0) {
          nums <- regmatches(match, gregexpr("[0-9]+", match))[[1]]
          # Use Unicode superscript
          superscripts <- c("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")
          sup_str <- paste0(sapply(strsplit(nums[2], "")[[1]], function(d) superscripts[as.numeric(d) + 1]), collapse = "")
          return(paste0(design_type, " ", nums[1], sup_str))
        }
      }
    }
    
    return(name)
  }

  output$doe_design_HELP <- renderUI({
    create_help_modal(i18n_r, "doe_design_help_en")
  })

  # Accordion expand/collapse all button
  output$accordion_toggle_btn <- renderUI({
    create_accordion_toggle_btn(ns, i18n_r, "param_accordion")
  })

  # Note: %||% operator and generate_filename_with_timestamp are now defined
  # in utils/general_helpers.R and available globally

  # Function to get design requirements and validation
  get_design_requirements <- function(design_type, n_factors, design_specific = NULL) {
    switch(design_type,
      "bbd" = {
        if (n_factors < 3 || n_factors > 4) {
          list(
            valid = FALSE,
            error = i18n$t("Box-Behnken Design requires 3-4 factors only!")
          )
        } else {
          list(
            valid = TRUE,
            info = i18n$t("BBD designs include 3 center points by default. Additional center points can be added.")
          )
        }
      },
      "ccd" = {
        if (n_factors < 2 || n_factors > 4) {
          list(
            valid = FALSE,
            error = i18n$t("Central Composite Design requires 2-4 factors only!")
          )
        } else {
          recommended_cpts <- if (n_factors == 2) 4 else 6
          star_points <- 2 * n_factors
          list(
            valid = TRUE,
            info = sprintf(i18n$t("CCD includes star points automatically. Recommended center points: %d"), recommended_cpts)
          )
        }
      },
      "ffd" = {
        # Get the levels from design_specific if available
        levels <- if (!is.null(design_specific$levels)) design_specific$levels else 2

        # Validate levels
        if (levels < 2 || levels > 3) {
          list(
            valid = FALSE,
            error = i18n$t("Only 2- and 3-level FFDs are available at present!")
          )
        } else if (levels == 3) {
          # 3-level FFD: only 2-3 factors allowed
          if (n_factors < 2 || n_factors > 3) {
            list(
              valid = FALSE,
              error = i18n$t("Only FFDs for 2 to 3 factors are available for 3-level FFD!")
            )
          } else {
            list(
              valid = TRUE,
              info = i18n$t("Note that 3-level FFD designs must contain 3 center points by default! Only ADDITIONAL center points are provided in 'cpts'.")
            )
          }
        } else {
          # 2-level FFD: 2-5 factors allowed
          if (n_factors < 2 || n_factors > 5) {
            list(
              valid = FALSE,
              error = i18n$t("Only FFDs for 2 to 5 factors are available for 2-level FFD!")
            )
          } else {
            list(
              valid = TRUE,
              info = i18n$t("2-level FFD designs do not include default center points.")
            )
          }
        }
      },
      "frfd" = {
        if (n_factors < 3 || n_factors > 5) {
          list(
            valid = FALSE,
            error = i18n$t("Fractional Factorial Design requires 3-5 factors!")
          )
        } else {
          p_required <- if (n_factors %in% 2:4) 1 else "1-2"

          # Check if custom aliasing is selected
          info_msg <- sprintf(i18n$t("Fraction (p) must be %s for %d factors."), p_required, n_factors)

          if (isTRUE(design_specific$aliasing_mode == "custom")) {
            info_msg <- paste0(info_msg, i18n$t(" Custom aliasing patterns will be used if provided."))
          }

          list(
            valid = TRUE,
            info = info_msg
          )
        }
      },
      "tm" = {
        if (n_factors < 3 || n_factors > 5) {
          list(
            valid = FALSE,
            error = i18n$t("Taguchi Method requires 3-5 factors!")
          )
        } else if (design_specific$levels < 2 | design_specific$levels > 4) {
          list(
            valid = FALSE,
            error = i18n$t("For Taguchi designs, 2-4 factor 'levels' are currently supported!")
          )
        } else {
          list(
            valid = TRUE,
            info = i18n$t("Taguchi designs do not support additional center points.")
          )
        }
      },
      list(valid = TRUE, info = NULL, warning = NULL, error = NULL)
    )
  }

  # Reactive values for storing results
  design_results <- reactiveValues(
    full_design_result = NULL, # Stores the complete output from doe_* functions
    design_data = NULL, # Stores the data.frame for display
    design_description = NULL, # YENİ
    design_matrix_name = NULL, # YENİ
    confounding_pattern = NULL, # YENİ (sadece frfd için)
    design_type = NULL,
    factors = NULL,
    factor_names = NULL,
    factor_limits = NULL
  )

  # Observer to enable/disable Save Design button based on results availability
  observe({
    if (!is.null(design_results$full_design_result)) {
      shinyjs::enable("save_design")
    } else {
      shinyjs::disable("save_design")
    }
  })

  # -- Settings modal integration ---------------------------------------------
  observeEvent(input$open_settings, {
    showModal(doe_design_settings_modal_ui(ns("settings_modal"), i18n))
  })

  output$design_type_ui <- renderUI({
    radioButtons(ns("design_type"),
      tags$span(i18n_r()$t("Select Design Type"),
        input_help(i18n_r()$t("Choose the type of experimental design to generate. Box-Behnken and Central Composite are response surface designs. Full and Fractional Factorial are screening designs. Taguchi uses orthogonal arrays for robust design."),
                   title = i18n_r()$t("Design Type"), buttonLabel = i18n_r()$t("OK"))),
      choiceNames = list(
        i18n_r()$t("Box-Behnken Design (BBD)"),
        i18n_r()$t("Central Composite Design (CCD)"),
        i18n_r()$t("Full Factorial Design (FFD)"),
        i18n_r()$t("Fractional Factorial Design (FrFD)"),
        i18n_r()$t("Taguchi Method (TM)")
      ),
      choiceValues = c("bbd", "ccd", "ffd", "frfd", "tm"),
      selected = isolate(input$design_type) %||% defaults$design_type
    )
  })

  output$randomize_ui <- renderUI({
    tags$span(
      checkboxInput(ns("randomize"), i18n_r()$t("Randomize Run Order"),
                    value = isolate(input$randomize) %||% defaults$randomize),
      title = i18n_r()$t("Randomize the order of experimental runs. Highly recommended to reduce the effect of systematic errors and time-dependent variability.")
    )
  })

  # Number of factors input with dynamic range based on design type
  output$factors_input <- renderUI({
    design_type <- input$design_type %||% defaults$design_type %||% "bbd"
    
    # Determine min/max factors based on design type
    # BBD: 3-4, CCD: 2-4, FFD: 2-5 (or 2-3 for 3-level), FrFD: 2-5, TM: 3-5
    factor_range <- switch(design_type,
      "bbd" = c(3, 4),
      "ccd" = c(2, 4),
      "ffd" = c(2, 5),  # Will be 2-3 for 3-level, but we show max range
      "frfd" = c(3, 5),
      "tm" = c(3, 5),
      c(2, 5)  # default
    )
    
    cur_val <- if (is.null(input$factors)) defaults$factors else input$factors
    
    local_range_badge_input("factors", i18n_r()$t("Number of Factors"),
      value = cur_val, min_val = factor_range[1], max_val = factor_range[2],
      help_content = i18n_r()$t("Number of independent variables (factors) in the experimental design. The valid range depends on the selected design type."),
      help_title = i18n_r()$t("Number of Factors")
    )
  })

  # Center points input (conditional for Taguchi Method)
  output$center_points_input <- renderUI({
    req(input$design_type)
    # Touch tm_levels to re-render badge when value changes
    input$tm_levels

    # For Taguchi: show Levels input on same row as factors (no center points)
    if (input$design_type == "tm") {
      return(local_range_badge_input("tm_levels", i18n_r()$t("Levels"), value = input$tm_levels %||% 3, min_val = 2, max_val = 4,
        help_content = i18n_r()$t("Number of levels per factor in the Taguchi orthogonal array. More levels capture more complex factor effects but require more runs."),
        help_title = i18n_r()$t("Levels")))
    }

    numericInput(ns("cpts"),
      tags$span(i18n_r()$t("Center Points"),
        input_help(i18n_r()$t("Number of extra center point runs to add to the design. Center points help detect curvature and estimate pure error. Three-level designs already include 3 center points by default."),
                   title = i18n_r()$t("Center Points"), buttonLabel = i18n_r()$t("OK"))),
      value = 6, min = 0)
  })

  # Design requirements and validation
  output$design_requirements <- renderUI({
    req(input$design_type, input$factors)

    design_type <- input$design_type
    n_factors <- input$factors
    design_specific <- get_design_specific_params(input)

    requirements <- get_design_requirements(design_type, n_factors, design_specific)

    if (!requirements$valid && !is.null(requirements$error)) {
      div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        strong(i18n$t("Invalid Configuration:")),
        br(),
        requirements$error
      )
    } else if (!is.null(requirements$warning)) {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        strong(i18n$t("Requirements:")),
        br(),
        requirements$warning
      )
    } else if (!is.null(requirements$info)) {
      div(
        class = "alert alert-info",
        icon("info-circle"),
        strong(i18n$t("Information:")),
        br(),
        requirements$info
      )
    }
  })

  # Dynamic UI for factor inputs
  output$factor_inputs <- renderUI({
    req(input$factors, input$design_type)

    n_factors <- input$factors
    design_type <- input$design_type
    design_specific <- get_design_specific_params(input)

    # Validate factor count for design type
    requirements <- get_design_requirements(design_type, n_factors, design_specific)
    if (!requirements$valid) {
      return(NULL)
    }

    factor_ui <- lapply(1:n_factors, function(i) {
      # Use template for factor name label: "Factor {n} Name"
      factor_label <- gsub("\\{n\\}", i, i18n_r()$t("Factor {n} Name"))
      
      # Add help icons only to the first factor row
      name_label <- if (i == 1) {
        tags$span(factor_label,
          input_help(i18n_r()$t("A descriptive name for this factor (e.g. Pressure, Temperature, Time). Used in design matrix column headers and analysis outputs."),
                     title = i18n_r()$t("Factor Name"), buttonLabel = i18n_r()$t("OK")))
      } else factor_label
      
      min_label <- if (i == 1) {
        tags$span(i18n_r()$t("Min"),
          input_help(i18n_r()$t("The minimum (low) level of this factor in the experimental design. For CCD designs, star points may extend beyond this range."),
                     title = i18n_r()$t("Min"), buttonLabel = i18n_r()$t("OK")))
      } else i18n_r()$t("Min")
      
      max_label <- if (i == 1) {
        tags$span(i18n_r()$t("Max"),
          input_help(i18n_r()$t("The maximum (high) level of this factor in the experimental design. For CCD designs, star points may extend beyond this range."),
                     title = i18n_r()$t("Max"), buttonLabel = i18n_r()$t("OK")))
      } else i18n_r()$t("Max")
      
      row <- fluidRow(
        column(if (design_type == "ccd") 4 else 4, textInput(ns(paste0("factor_name_", i)),
          name_label,
          value = paste0("Factor", i)
        )),
        column(if (design_type == "ccd") 3 else 4, numericInput(ns(paste0("factor_min_", i)),
          min_label,
          value = if (i == 1) 100 else if (i == 2) 35 else if (i == 3) 2 else 0
        )),
        column(if (design_type == "ccd") 3 else 4, numericInput(ns(paste0("factor_max_", i)),
          max_label,
          value = if (i == 1) 300 else if (i == 2) 65 else if (i == 3) 4 else 10
        )),
        if (design_type == "ccd") {
          column(2, 
            tags$label("\u00A0", style = "display: block; margin-bottom: 5px;"),
            tags$div(
              title = i18n_r()$t("Constrain star points within factor range"),
              style = "margin-top: 2px;",
              checkboxInput(ns(paste0("hard_limit_", i)),
                i18n_r()$t("Hard"),
                value = FALSE
              )
            )
          )
        }
      )
      row
    })

    do.call(tagList, factor_ui)
  })

  # Render CCD design type selectInput (server-side for i18n)
  output$ccd_design_ui <- renderUI({
    selectInput(ns("ccd_design"),
      tags$span(i18n_r()$t("CCD Type"),
        input_help(i18n_r()$t("Circumscribed (CCC) places star points outside the factor range at a distance of \u00B11.414, creating 5 levels. Face-Centered (CCF) places star points at the factor faces (\u00B11.0), creating 3 levels."),
                   title = i18n_r()$t("CCD Type"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("CCC", "CCF"), c(i18n_r()$t("Circumscribed (CCC)"), i18n_r()$t("Face-Centered (CCF)"))),
      selected = input$ccd_design %||% "CCC"
    )
  })

  # Render FRFD aliasing selectInput (server-side for i18n)
  output$frfd_aliasing_ui <- renderUI({
    selectInput(ns("frfd_aliasing"),
      tags$span(i18n_r()$t("Aliasing"),
        input_help(i18n_r()$t("Aliasing pattern for the fractional factorial design. Default uses the standard generator. Custom allows you to specify your own aliasing structure."),
                   title = i18n_r()$t("Aliasing"), buttonLabel = i18n_r()$t("OK"))),
      choices = setNames(c("default", "custom"), c(i18n_r()$t("Default"), i18n_r()$t("Custom"))),
      selected = input$frfd_aliasing %||% "default"
    )
  })

  # Dynamic UI for design-specific parameters
  output$design_specific_params <- renderUI({
    req(input$design_type, input$factors)
    # Touch level/fraction inputs to re-render badges when values change
    input$ffd_levels
    input$frfd_p

    n_factors <- input$factors

    switch(input$design_type,
      "ccd" = {
        # CCD automatically sets levels based on design type
        ccd_type <- input$ccd_design
        if (!is.null(ccd_type)) {
          auto_levels <- if (ccd_type == "CCC") 5 else 3
          ccd_description <- if (ccd_type == "CCC") {
            i18n$t("Star points extend beyond factor range (±1.414)")
          } else {
            i18n$t("Star points are at factor faces (±1.0)")
          }

          fluidRow(
            column(6, uiOutput(ns("ccd_design_ui"))),
            column(6, div(
              style = "margin-top: 25px;",
              strong(i18n$t("Levels")), paste0(auto_levels, " (", i18n$t("auto-set"), ")"),
              br(),
              em(style = "font-size: 0.9em; color: #666;", ccd_description)
            ))
          )
        } else {
          fluidRow(
            column(6, uiOutput(ns("ccd_design_ui"))),
            column(6, div(
              style = "margin-top: 25px;",
              strong(i18n$t("Levels")), paste0("5 (", i18n$t("auto-set"), ")"),
              br(),
              em(style = "font-size: 0.9em; color: #666;", i18n$t("Star points extend beyond factor range (±1.414)"))
            ))
          )
        }
      },
      "ffd" = fluidRow(
        column(6, local_range_badge_input("ffd_levels", i18n_r()$t("Levels"), value = input$ffd_levels %||% 3, min_val = 2, max_val = 3,
          help_content = i18n_r()$t("Number of levels per factor. 2 levels test linear effects only. 3 levels can detect curvature (quadratic effects)."),
          help_title = i18n_r()$t("Levels")))
      ),
      "frfd" = {
        # Validate p value based on factors
        max_p <- if (n_factors == 5) 2 else 1
        current_p <- input$frfd_p
        if (!is.null(current_p) && current_p > max_p) {
          current_p <- max_p
        }

        tagList(
          fluidRow(
            column(6, local_range_badge_input("frfd_p", i18n_r()$t("Fraction (p)"),
              value = current_p %||% 1, min_val = 1, max_val = max_p,
              help_content = i18n_r()$t("Fractionation level. A 2^(k-p) design uses 1/2^p of the full factorial runs. Higher p = fewer runs but more aliased (confounded) effects."),
              help_title = i18n_r()$t("Fraction (p)"))),
            column(6, uiOutput(ns("frfd_aliasing_ui")))
          ),
          # Custom aliasing input (conditional)
          conditionalPanel(
            condition = "input.frfd_aliasing == 'custom'",
            ns = ns,
            fluidRow(
              column(
                12,
                textAreaInput(ns("frfd_custom_aliasing"),
                  i18n_r()$t("Custom Aliasing Patterns:"),
                  placeholder = i18n_r()$t("Enter aliasing patterns separated by commas, e.g., AB=C, AC=B"),
                  rows = 3
                ),
                div(
                  class = "alert alert-info",
                  style = "margin-top: 10px;",
                  icon("info-circle"),
                  " ",
                  i18n_r()$t("Enter aliasing patterns as a comma-separated character vector (e.g., 'AB=C', 'AC=B')")
                )
              )
            )
          )
        )
      },
      "tm" = NULL,  # Levels now in center_points_input slot
      # BBD has no additional parameters
      fluidRow()
    )
  })

  # Calculate design
  observeEvent(input$calculate, {
    tryCatch(
      {
        # Show progress
        withProgress(message = i18n$t("Generating design..."), value = 0, {
          # Prepare parameters
          incProgress(0.2, detail = i18n$t("Preparing parameters..."))
          params <- prepare_design_parameters(input)

          # Generate design based on type
          incProgress(0.6, detail = i18n$t("Generating design matrix..."))
          design_result_full <- generate_design(input$design_type, params)

          # Store results
          design_results$full_design_result <- design_result_full$full_result # Store the complete result
          design_results$design_data <- design_result_full$data # Store the data.frame for display
          design_results$design_description <- design_result_full$full_result$description # Store description
          design_results$design_matrix_name <- names(design_result_full$full_result$doe)[1] # Store matrix name
          design_results$confounding_pattern <- if (input$design_type == "frfd" && !is.null(design_result_full$full_result$doe[["CONFOUNDING PATTERN"]])) {
            design_result_full$full_result$doe[["CONFOUNDING PATTERN"]]
          } else {
            NULL
          }
          design_results$design_type <- input$design_type
          design_results$factors <- input$factors
          design_results$factor_names <- params$fnames
          design_results$factor_limits <- params$flims

          incProgress(1, detail = i18n$t("Completed!"))
        })

        showNotification(i18n$t("Design generated successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error:"), e$message), type = "error")
      }
    )
  })

  # Prepare design parameters
  prepare_design_parameters <- function(input) {
    n_factors <- input$factors

    # Get factor names
    fnames <- sapply(1:n_factors, function(i) {
      name <- input[[paste0("factor_name_", i)]]
      if (is.null(name) || name == "") paste0("Factor", i) else name
    })

    # Get factor limits (with hard/soft naming for CCD)
    flims <- lapply(1:n_factors, function(i) {
      min_val <- input[[paste0("factor_min_", i)]]
      max_val <- input[[paste0("factor_max_", i)]]
      c(min_val, max_val)
    })
    
    # For CCD designs, name flims as "hard" or "soft" based on checkboxes
    if (input$design_type == "ccd") {
      names(flims) <- sapply(1:n_factors, function(i) {
        is_hard <- input[[paste0("hard_limit_", i)]]
        if (!is.null(is_hard) && isTRUE(is_hard)) "hard" else "soft"
      })
    }

    # Handle center points (NULL for Taguchi Method)
    cpts <- if (input$design_type == "tm") 0 else (input$cpts %||% 0)

    # Get randomize option
    randomize <- input$randomize %||% TRUE

    list(
      factors = n_factors,
      cpts = cpts,
      fnames = fnames,
      flims = flims,
      randomize = randomize,
      design_specific = get_design_specific_params(input)
    )
  }

  # Get design-specific parameters
  get_design_specific_params <- function(input) {
    switch(input$design_type,
      "ccd" = list(
        design = input$ccd_design %||% "CCC",
        levels = if (input$ccd_design %||% "CCC" == "CCC") 5 else 3
      ),
      "ffd" = list(
        levels = input$ffd_levels %||% 3
      ),
      "frfd" = {
        aliasing_mode <- input$frfd_aliasing %||% "default"

        if (aliasing_mode == "custom") {
          # Parse custom aliasing patterns
          custom_text <- input$frfd_custom_aliasing
          if (!is.null(custom_text) && nchar(trimws(custom_text)) > 0) {
            # Split by comma and trim whitespace
            aliasing_patterns <- trimws(strsplit(custom_text, ",")[[1]])
            # Remove empty strings
            aliasing_patterns <- aliasing_patterns[nchar(aliasing_patterns) > 0]

            list(
              p = input$frfd_p %||% 1,
              aliasing = if (length(aliasing_patterns) > 0) aliasing_patterns else "default",
              aliasing_mode = "custom"
            )
          } else {
            # If custom is selected but no input provided, use default
            list(
              p = input$frfd_p %||% 1,
              aliasing = "default",
              aliasing_mode = "custom"
            )
          }
        } else {
          list(
            p = input$frfd_p %||% 1,
            aliasing = "default",
            aliasing_mode = "default"
          )
        }
      },
      "tm" = list(
        levels = input$tm_levels %||% 3
      ),
      "bbd" = list(), # No additional parameters
      list()
    )
  }

  # Generate design based on type
  generate_design <- function(design_type, params) {
    switch(design_type,
      "bbd" = {
        result <- doe_bbd(
          factors = params$factors,
          cpts = params$cpts,
          fnames = params$fnames,
          flims = params$flims,
          randomize = params$randomize
        )


        data_df <- as.data.frame(result$doe[[1]])


        list(
          full_result = result, # Store the complete result
          data = data_df
        )
      },
      "ccd" = {
        result <- doe_ccd(
          design = params$design_specific$design,
          levels = params$design_specific$levels,
          factors = params$factors,
          cpts = params$cpts,
          fnames = params$fnames,
          flims = params$flims,
          randomize = params$randomize
        )
        # Extract and clean the data frame
        data_df <- as.data.frame(result$doe[[1]])

        list(
          full_result = result, # Store the complete result
          data = data_df
        )
      },
      "ffd" = {
        default_cpts <- if (params$design_specific$levels == 3) 3 else 0
        total_cpts <- default_cpts + params$cpts

        result <- doe_ffd(
          levels = params$design_specific$levels,
          factors = params$factors,
          cpts = params$cpts,
          fnames = params$fnames,
          flims = params$flims,
          randomize = params$randomize
        )

        # Extract and clean the data frame
        data_df <- as.data.frame(result$doe[[1]])

        # Ensure proper column names and structure
        if (!is.null(params$fnames) && length(params$fnames) > 0) {
          # Check if we need to rename columns
          expected_cols <- c("Standard_Order", "A", "B", "C", params$fnames)
          if (ncol(data_df) >= length(params$fnames)) {
            # Keep existing structure but ensure it's a proper data frame
            data_df <- data_df[, 1:min(ncol(data_df), length(expected_cols)), drop = FALSE]
          }
        }


        list(
          full_result = result, # Store the complete result
          data = data_df
        )
      },
      "frfd" = {
        result <- doe_frfd(
          factors = params$factors,
          p = params$design_specific$p,
          cpts = params$cpts,
          aliasing = params$design_specific$aliasing,
          fnames = params$fnames,
          flims = params$flims,
          randomize = params$randomize
        )
        # Extract and clean the data frame
        data_df <- as.data.frame(result$doe[[1]])


        list(
          full_result = result, # Store the complete result
          data = data_df
        )
      },
      "tm" = {
        result <- doe_tm(
          levels = params$design_specific$levels,
          factors = params$factors,
          fnames = params$fnames,
          flims = params$flims,
          randomize = params$randomize
        )
        # Extract and clean the data frame
        data_df <- as.data.frame(result$doe[[1]])


        list(
          full_result = result, # Store the complete result
          data = data_df
        )
      }
    )
  }

  # Render design table
  output$design_table <- DT::renderDataTable(
    {
      req(design_results$design_data)
      data <- design_results$design_data
      
      # Fix column names for display
      col_names <- names(data)
      # Replace "Standard_Order" with "Standard Order"
      col_names <- gsub("Standard_Order", i18n_r()$t("Standard Order"), col_names)
      # Remove "A_", "B_", "C_", etc. prefixes from uncoded factor names
      col_names <- gsub("^[A-Z]_", "", col_names)
      names(data) <- col_names
      
      total_data_cols <- ncol(data)


      format_cols_index <- 2:total_data_cols # (1+((total_data_cols-1)/2))



      dt_output <- DT::datatable(
        data,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          # columnDefs = list(defaultContent = "-",targets = "_all"),
          pageLength = 30,
          dom = "Bfrtip",
          language = tablang(),
          buttons = create_dt_export_buttons(i18n_r, "supercrit_doe_design_matrix")
        ),
        extensions = "Buttons"
      )

      if (length(format_cols_index) > 0) {
        dt_output <- dt_output %>% DT::formatRound(columns = format_cols_index, digits = 2)
      }

      dt_output
    },
    server = FALSE
  )

  # Design description output (translated)
  output$design_description <- renderText({
    req(design_results$design_description)
    translated_desc <- translate_design_description(design_results$design_description, i18n_r())
    HTML(paste0(translated_desc, "<br>"))
  })

  # Design matrix name output (translated to human-readable format)
  output$design_matrix_name <- renderUI({
    req(design_results$design_matrix_name)
    translated_name <- translate_matrix_name(design_results$design_matrix_name, i18n_r())
    tags$h5(style = "color:#ffffff;", paste0("(", translated_name, ")"))
  })

  # Confounding pattern table output
  output$confounding_table <- DT::renderDataTable(
    {
      req(design_results$confounding_pattern)
      add_prettynames= c(
        i18n_r()$t("Effect"),
        i18n_r()$t("Alias")
        
     
      )
      DT::datatable(
        design_results$confounding_pattern,
        rownames = FALSE,
        colnames = add_prettynames,
        options = list(
          scrollX = TRUE,
          pageLength = 30,
          language = tablang(),
          dom = "Bfrtip",
          buttons = create_dt_export_buttons(i18n_r, "supercrit_doe_confounding_pattern")
        ),
        extensions = "Buttons"
      )
    },
    server = FALSE
  )

  # Reactive to check if confounding pattern exists for conditional panel
  output$has_confounding_pattern <- reactive({
    !is.null(design_results$confounding_pattern)
  })
  outputOptions(output, "has_confounding_pattern", suspendWhenHidden = FALSE)

  # Reactive to check if export button should be visible
  output$show_export_button <- reactive({
    !is.null(design_results$design_data)
  })
  outputOptions(output, "show_export_button", suspendWhenHidden = FALSE)

  # Ensure accordion-hidden UI outputs render properly
  outputOptions(output, "factors_input", suspendWhenHidden = FALSE)
  outputOptions(output, "center_points_input", suspendWhenHidden = FALSE)
  outputOptions(output, "design_requirements", suspendWhenHidden = FALSE)
  outputOptions(output, "factor_inputs", suspendWhenHidden = FALSE)
  outputOptions(output, "ccd_design_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "frfd_aliasing_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "design_specific_params", suspendWhenHidden = FALSE)
  outputOptions(output, "design_matrix_name", suspendWhenHidden = FALSE)

  # Observer to enable/disable result tabs based on results availability
  observe({
    has_results <- !is.null(design_results$design_data)
    if (has_results) {
      session$sendCustomMessage("enableTabs", list(tabsetId = ns("results_tabs")))
    } else {
      session$sendCustomMessage("disableTabs", list(tabsetId = ns("results_tabs"), keepFirst = TRUE))
    }
  })

  # Save design - show modal
  observeEvent(input$save_design, {
    req(design_results$design_data)
    showModal(doe_design_save_modal_ui(ns("save_modal"), i18n))
  })

  # Render filename preview in modal
  output[["save_modal-design_name_preview"]] <- renderUI({
    req(input[["save_modal-design_name"]])
    user_input_name <- input[["save_modal-design_name"]]

    # Basic sanitization for filename
    sanitized_name <- gsub("[^a-zA-Z0-9_.-]", "_", user_input_name)
    if (nchar(sanitized_name) == 0) {
      sanitized_name <- "untitled"
    }

    # timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    final_filename <- generate_filename_with_timestamp(paste0(sanitized_name, "_", input$design_type))

    div(
      class = "alert alert-info",
      icon("info-circle"),
      strong(i18n$t("Final filename:")),
      br(),
      final_filename
    )
  })

  # Confirm save design from modal
  observeEvent(input[["save_modal-confirm_save_design"]], {
    req(design_results$design_data, input[["save_modal-design_name"]])

    removeModal() # Close the modal

    tryCatch(
      {
        # Create design data structure
        design_data <- list(
          design_type = design_results$design_type,
          parameters = list(
            factors = design_results$factors,
            factor_names = design_results$factor_names,
            factor_limits = design_results$factor_limits
          ),
          results = list(
            design_matrix = design_results$design_data
          ),
          metadata = list(
            created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            app_version = "0.9.0"
          )
        )

        # Generate display name using user input
        user_input_name <- input[["save_modal-design_name"]]
        sanitized_name <- gsub("[^a-zA-Z0-9_.-]", "_", user_input_name)
        if (nchar(sanitized_name) == 0) {
          sanitized_name <- "untitled"
        }

        # Save to session-based storage
        new_id <- length(doe_rv$saved_designs) + 1
        doe_rv$saved_designs[[new_id]] <- list(
          id = new_id,
          name = sanitized_name,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          design_type = input$design_type,
          data = design_data
        )

        showNotification(i18n$t("Design saved successfully!"), type = "message")
      },
      error = function(e) {
        showNotification(paste(i18n$t("Error saving design:"), e$message), type = "error")
      }
    )
  })

  # Export design
  output$export_design <- downloadHandler(
    filename = function() {
      # Use the helper function to generate the filename (changed to .csv)
      paste0(generate_filename_with_timestamp(paste0("supercrit_doe_design_export_", design_results$design_type)), ".csv")
    },
    content = function(file) {
      req(design_results$full_design_result, design_results$design_description)

      # Show preparing notification
      prep_notif <- showNotification(i18n$t("Preparing export..."), type = "message", duration = NULL)
      on.exit(removeNotification(prep_notif), add = TRUE)

      tryCatch(
        {
          # Get the design data frame
          design_df <- as.data.frame(design_results$full_design_result$doe[[1]])
          
          # Translate column names (same as in UI table)
          col_names <- names(design_df)
          # Replace "Standard_Order" with translated version
          col_names <- gsub("Standard_Order", i18n_r()$t("Standard Order"), col_names)
          # Remove "A_", "B_", etc. prefixes from uncoded factor names
          col_names <- gsub("^[A-Z]_", "", col_names)
          names(design_df) <- col_names
          
          # Get the translated design description
          translated_desc <- translate_design_description(design_results$design_description, i18n_r())
          
          # Build CSV content with header comment
          header_line <- translated_desc
          
          # Create CSV content
          csv_content <- capture.output({
            cat(header_line, "\n", sep = "")
            write.csv(design_df, row.names = FALSE)
          })
          
          # Append confounding pattern for FrFD designs
          if (!is.null(design_results$confounding_pattern)) {
            conf_lines <- capture.output({
              cat("\n")
              cat(i18n_r()$t("Confounding Pattern"), "\n")
              write.csv(design_results$confounding_pattern, row.names = FALSE)
            })
            csv_content <- c(csv_content, conf_lines)
          }
          
          content_str <- paste(csv_content, collapse = "\n")
          
          # Write with UTF-8 BOM for proper Cyrillic display
          con <- file(file, "wb")
          writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)  # UTF-8 BOM
          writeBin(charToRaw(content_str), con)
          close(con)
          
          removeNotification(prep_notif)
          showNotification(i18n$t("Done!"), type = "message", duration = 3)
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error exporting design:"), e$message), type = "error")
        }
      )
    }
  )

  # Reset function
  observeEvent(input$reset, {
    # Reset all inputs
    updateRadioButtons(session, "design_type", selected = defaults$design_type)
    updateNumericInput(session, "factors", value = defaults$factors)
    updateNumericInput(session, "cpts", value = defaults$center_points)
    updateCheckboxInput(session, "randomize", value = defaults$randomize)

    # Clear results
    design_results$full_design_result <- NULL
    design_results$design_data <- NULL
    design_results$design_description <- NULL
    design_results$design_matrix_name <- NULL
    design_results$confounding_pattern <- NULL
    design_results$design_type <- NULL

    showNotification(i18n$t("Parameters reset"), type = "message")
  })

  # Apply design settings function
  apply_design_settings <- function(session, settings) {
    updateRadioButtons(session, "design_type", selected = settings$design_type)
    updateNumericInput(session, "factors", value = settings$factors)
    updateNumericInput(session, "cpts", value = settings$cpts)

    # Update factor inputs after a short delay to allow UI to render

    if (!is.null(settings$factor_names)) {
      for (i in 1:length(settings$factor_names)) {
        updateTextInput(session, paste0("factor_name_", i), value = settings$factor_names[i])
      }
    }
    if (!is.null(settings$factor_limits)) {
      for (i in 1:length(settings$factor_limits)) {
        updateNumericInput(session, paste0("factor_min_", i), value = settings$factor_limits[[i]][1])
        updateNumericInput(session, paste0("factor_max_", i), value = settings$factor_limits[[i]][2])
      }
    }
  }

  # suspendWhenHidden for renderUI outputs (auto-added)
  outputOptions(output, "accordion_toggle_btn", suspendWhenHidden = FALSE)
  outputOptions(output, "design_type_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "doe_design_HELP", suspendWhenHidden = FALSE)
  outputOptions(output, "randomize_ui", suspendWhenHidden = FALSE)

}
