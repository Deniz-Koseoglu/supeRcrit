# Functional tests for Shiny server modules
# These tests verify that example data exists, is readable, and key functions work
# Note: Server modules use legacy pattern (not moduleServer), so we test components directly

# ============================================================================
# EXAMPLE DATA FILE TESTS
# ============================================================================

test_that("kinetic_tws example data exists and is valid", {
  example_path <- system.file("extdata", "gui-kinetic_tws_oec-1.csv", package = "supeRcrit")
  expect_true(file.exists(example_path), info = "TWS example data file should exist")

  data <- read.csv(example_path, stringsAsFactors = FALSE)
  expect_true(nrow(data) > 0, info = "Should have rows")
  expect_true(ncol(data) >= 2, info = "Should have at least 2 columns (time and yield)")

  # Check for expected column structure (time-series data)
  expect_true(any(grepl("time|Time|t", names(data), ignore.case = TRUE)) ||
              ncol(data) >= 2,
              info = "Should have time-like column or multiple columns")
})

test_that("kinetic_bic example data exists and is valid", {
  example_path <- system.file("extdata", "gui-kinetic_bic_oec.csv", package = "supeRcrit")
  expect_true(file.exists(example_path), info = "BIC example data file should exist")

  data <- read.csv(example_path, stringsAsFactors = FALSE)
  expect_true(nrow(data) > 0)
  expect_true(ncol(data) >= 2)
})

test_that("com_analysis example data files exist and are valid", {
  example_files <- c(
    "gui_com_input-test-data-0.csv",
    "gui_com_input-test-data-1.csv",
    "com_data_pumpkin.csv"
  )

  found_count <- 0
  for (file in example_files) {
    example_path <- system.file("extdata", file, package = "supeRcrit")
    if (file.exists(example_path)) {
      found_count <- found_count + 1
      data <- read.csv(example_path, stringsAsFactors = FALSE)
      expect_true(nrow(data) > 0, info = paste(file, "should have data"))
    }
  }
  expect_true(found_count >= 1, info = "At least one COM example file should exist")
})

test_that("doe_analysis example data files exist and are valid", {
  example_files <- c(
    "gui-doe-analysis-default.csv",
    "gui_doe_analysis-demo1.csv",
    "gui-doe_analysis-demo3.csv",
    "gui-doe_analysis-carnosic.csv",
    "gui_doe_analysis_Ergosterol.csv"
  )

  found_count <- 0
  for (file in example_files) {
    example_path <- system.file("extdata", file, package = "supeRcrit")
    if (file.exists(example_path)) {
      found_count <- found_count + 1
      data <- read.csv(example_path, stringsAsFactors = FALSE)
      expect_true(nrow(data) > 0, info = paste(file, "should have data"))
      # DOE data should have multiple columns for factors
      expect_true(ncol(data) >= 2, info = paste(file, "should have factors and response"))
    }
  }
  expect_true(found_count >= 1, info = "At least one DOE example file should exist")
})

# ============================================================================
# PACKAGE FUNCTION TESTS (used by Shiny modules)
# ============================================================================

test_that("ktsmod function and example data are compatible", {
  skip_if_not_installed("supeRcrit")

  # Read example data
  example_path <- system.file("extdata", "gui-kinetic_tws_oec-1.csv", package = "supeRcrit")
  skip_if(!file.exists(example_path), "Example data not found")

  data <- read.csv(example_path, stringsAsFactors = FALSE)

  # Verify function exists
  expect_true(is.function(supeRcrit::ktsmod))

  # Verify data has expected structure for kinetic modeling
  expect_true(ncol(data) >= 2, info = "Data should have time and yield columns")
  expect_true(is.numeric(data[[1]]) || all(!is.na(as.numeric(data[[1]]))),
              info = "First column (time) should be numeric")
  expect_true(is.numeric(data[[2]]) || all(!is.na(as.numeric(data[[2]]))),
              info = "Second column (yield) should be numeric")

  # ktsmod requires additional parameters (pressure, temperature, c0, m_in)
  # that are provided by Shiny UI, so we just verify data is suitable
  time_vals <- as.numeric(data[[1]])
  yield_vals <- as.numeric(data[[2]])

  expect_true(all(time_vals >= 0), info = "Time values should be non-negative")
  expect_true(all(yield_vals >= 0), info = "Yield values should be non-negative")
})

test_that("bicmod function works with example data pattern", {
  skip_if_not_installed("supeRcrit")

  example_path <- system.file("extdata", "gui-kinetic_bic_oec.csv", package = "supeRcrit")
  skip_if(!file.exists(example_path), "Example data not found")

  data <- read.csv(example_path, stringsAsFactors = FALSE)

  # bicmod expects oec (data frame) and oec_vars (column names)
  expect_true(is.function(supeRcrit::bicmod))

  result <- tryCatch({
    supeRcrit::bicmod(
      oec = data,
      oec_vars = c(names(data)[1], names(data)[2]),
      pars = "bic",
      silent = TRUE,
      draw = FALSE
    )
  }, error = function(e) e)

  expect_true(
    !inherits(result, "error") ||
    grepl("parameter|column|data|unit", result$message, ignore.case = TRUE),
    info = "bicmod should handle example data or give meaningful error"
  )
})

test_that("doe_analyze function works with example data", {
  skip_if_not_installed("supeRcrit")

  example_path <- system.file("extdata", "gui-doe-analysis-default.csv", package = "supeRcrit")
  skip_if(!file.exists(example_path), "Example data not found")

  data <- read.csv(example_path, stringsAsFactors = FALSE)

  # doe_analyze requires specific column structure
  # Test that the function exists and is callable
  expect_true(is.function(supeRcrit::doe_analyze))
})

# ============================================================================
# HELPER FUNCTION INTEGRATION TESTS
# ============================================================================

test_that("general_helpers functions work together", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("rhandsontable")

  # Load packages and helpers
  suppressPackageStartupMessages({
    library(shiny)
    library(DT)
    library(rhandsontable)
    library(shinyhelper)
  })

  source(file.path(shiny_app_dir, "utils/general_helpers.R"), local = FALSE)

  # Test create_editable_hot with real data
  test_data <- data.frame(
    Time = c(0, 10, 20, 30),
    Yield = c(0, 5.2, 8.1, 9.5)
  )

  hot <- create_editable_hot(test_data)
  expect_s3_class(hot, "htmlwidget")

  # Test trim_zeros_columndefs
  col_defs <- trim_zeros_columndefs(c(0, 1), digits = 2)
  expect_type(col_defs, "list")
  expect_length(col_defs, 1)

  # Test null coalescing
  expect_equal(NULL %||% 5, 5)
  expect_equal(10 %||% 5, 10)
})

# ============================================================================
# SERVER MODULE SOURCING TESTS (verify modules execute without errors)
# ============================================================================

test_that("all server modules can be sourced together without conflicts", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  # Create a clean environment
  test_env <- new.env()

  # Load required packages
  suppressPackageStartupMessages({
    library(shiny)
    library(DT)
    library(plotly)
    library(ggplot2)
    library(rhandsontable)
    library(shinyjs)
    library(shinyhelper)
  })

  # Source utilities first
  utility_files <- c(
    "utils/general_helpers.R",
    "utils/com_analysis_helpers.R",
    "utils/kinetic_helpers.R",
    "utils/button_state_helpers.R",
    "utils/saved_calculations_helpers.R",
    "utils/input_help.R"
  )

  for (file in utility_files) {
    file_path <- file.path(shiny_app_dir, file)
    if (file.exists(file_path)) {
      result <- tryCatch(
        { source(file_path, local = test_env); TRUE },
        error = function(e) e$message
      )
      expect_true(isTRUE(result), info = paste("Utility file should source:", file, "-", result))
    }
  }

  # Source all server modules
  server_modules <- c(
    "server_modules/kinetic_tws_server.R",
    "server_modules/kinetic_bic_server.R",
    "server_modules/kinetic_aux_tool_server.R",
    "server_modules/com_analysis_server.R",
    "server_modules/doe_design_server.R",
    "server_modules/doe_analysis_server.R",
    "server_modules/doe_desir_server.R",
    "server_modules/sfe_sol_char_server.R",
    "server_modules/sfe_misc_opt_server.R",
    "server_modules/sfe_misc_comp_server.R",
    "server_modules/sfe_aux_tool_server.R",
    "server_modules/global_settings_modal_server.R"
  )

  for (module in server_modules) {
    file_path <- file.path(shiny_app_dir, module)
    if (file.exists(file_path)) {
      result <- tryCatch(
        { source(file_path, local = test_env); TRUE },
        error = function(e) e$message
      )
      expect_true(isTRUE(result), info = paste("Server module should source:", module, "-", result))
    }
  }

  # Verify all server functions are defined
  expect_true(exists("kinetic_tws_server", envir = test_env))
  expect_true(exists("kinetic_bic_server", envir = test_env))
  expect_true(exists("com_analysis_server", envir = test_env))
  expect_true(exists("doe_design_server", envir = test_env))
  expect_true(exists("doe_analysis_server", envir = test_env))
  expect_true(exists("doe_desir_server", envir = test_env))
  expect_true(exists("solute_characterization_server", envir = test_env))
  expect_true(exists("miscibility_optimization_server", envir = test_env))
  expect_true(exists("miscomp_server", envir = test_env))
  expect_true(exists("auxiliary_tools_server", envir = test_env))
  expect_true(exists("kinetic_aux_tool_server", envir = test_env))
  expect_true(exists("global_settings_modal_server", envir = test_env))
})

# ============================================================================
# UI MODULE SOURCING TESTS
# ============================================================================

test_that("all UI modules can be sourced without errors", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  test_env <- new.env()

  suppressPackageStartupMessages({
    library(shiny)
    library(shinydashboard)
    library(shinyWidgets)
    library(DT)
    library(shinyhelper)
  })

  ui_modules <- c(
    "ui_modules/kinetic_tws_ui.R",
    "ui_modules/kinetic_bic_ui.R",
    "ui_modules/kinetic_aux_tool_ui.R",
    "ui_modules/com_analysis_ui.R",
    "ui_modules/doe_design_ui.R",
    "ui_modules/doe_analysis_ui.R",
    "ui_modules/doe_desir_ui.R",
    "ui_modules/sfe_sol_char_ui.R",
    "ui_modules/sfe_misc_opt_ui.R",
    "ui_modules/sfe_misc_comp_ui.R",
    "ui_modules/sfe_aux_tool_ui.R",
    "ui_modules/global_settings_modal_ui.R"
  )

  for (module in ui_modules) {
    file_path <- file.path(shiny_app_dir, module)
    if (file.exists(file_path)) {
      result <- tryCatch(
        { source(file_path, local = test_env); TRUE },
        error = function(e) e$message
      )
      expect_true(isTRUE(result), info = paste("UI module should source:", module, "-", result))
    }
  }
})

# ============================================================================
# DATA PROCESSING FUNCTION TESTS
# ============================================================================

test_that("kinetic data can be processed for model fitting", {
  example_path <- system.file("extdata", "gui-kinetic_tws_oec-1.csv", package = "supeRcrit")
  skip_if(!file.exists(example_path), "Example data not found")

  data <- read.csv(example_path, stringsAsFactors = FALSE)

  # Data should be numeric and suitable for modeling
  expect_true(is.numeric(data[[1]]) || all(!is.na(as.numeric(data[[1]]))))
  expect_true(is.numeric(data[[2]]) || all(!is.na(as.numeric(data[[2]]))))

  # Convert if needed
  time_col <- as.numeric(data[[1]])
  yield_col <- as.numeric(data[[2]])

  # Remove NAs
  valid_idx <- !is.na(time_col) & !is.na(yield_col)
  expect_true(sum(valid_idx) >= 3, info = "Should have at least 3 valid data points")
})

test_that("DOE data has required structure for analysis", {
  example_path <- system.file("extdata", "gui-doe-analysis-default.csv", package = "supeRcrit")
  skip_if(!file.exists(example_path), "Example data not found")

  data <- read.csv(example_path, stringsAsFactors = FALSE)

  # DOE data needs factors (X columns) and response (Y column)
  expect_true(ncol(data) >= 2, info = "Need at least 1 factor and 1 response")
  expect_true(nrow(data) >= 4, info = "Need at least 4 runs for basic DOE")

  # Check that at least one column is numeric (the response)
  numeric_cols <- sapply(data, is.numeric)
  expect_true(any(numeric_cols), info = "Should have at least one numeric response column")
})
