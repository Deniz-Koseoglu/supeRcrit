# Test that server modules can be loaded and define expected functions
# These tests verify the modules are structurally sound

# Helper to load dependencies needed by server modules
load_module_dependencies <- function(env) {
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

  # Source utility files first (they define helper functions used by modules)
  utility_files_to_source <- c(
    "utils/general_helpers.R",
    "utils/com_analysis_helpers.R",
    "utils/kinetic_helpers.R",
    "utils/button_state_helpers.R",
    "utils/saved_calculations_helpers.R",
    "utils/input_help.R"
  )

  for (file in utility_files_to_source) {
    file_path <- file.path(shiny_app_dir, file)
    if (file.exists(file_path)) {
      tryCatch(
        source(file_path, local = env),
        error = function(e) NULL
      )
    }
  }
}

test_that("com_analysis_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/com_analysis_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("com_analysis_server", envir = local_env))
  expect_true(is.function(local_env$com_analysis_server))
})

test_that("kinetic_tws_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/kinetic_tws_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("kinetic_tws_server", envir = local_env))
  expect_true(is.function(local_env$kinetic_tws_server))
})

test_that("kinetic_bic_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/kinetic_bic_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("kinetic_bic_server", envir = local_env))
  expect_true(is.function(local_env$kinetic_bic_server))
})

test_that("kinetic_aux_tool_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/kinetic_aux_tool_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("kinetic_aux_tool_server", envir = local_env))
  expect_true(is.function(local_env$kinetic_aux_tool_server))
})

test_that("doe_design_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/doe_design_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("doe_design_server", envir = local_env))
  expect_true(is.function(local_env$doe_design_server))
})

test_that("doe_analysis_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  # DOE analysis has additional helper file
  helpers_path <- file.path(shiny_app_dir, "server_modules/doe_analysis_helpers.R")
  if (file.exists(helpers_path)) {
    source(helpers_path, local = local_env)
  }

  file_path <- file.path(shiny_app_dir, "server_modules/doe_analysis_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("doe_analysis_server", envir = local_env))
  expect_true(is.function(local_env$doe_analysis_server))
})

test_that("doe_desir_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/doe_desir_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("doe_desir_server", envir = local_env))
  expect_true(is.function(local_env$doe_desir_server))
})

test_that("sfe_sol_char_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/sfe_sol_char_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("solute_characterization_server", envir = local_env))
  expect_true(is.function(local_env$solute_characterization_server))
})

test_that("sfe_misc_opt_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/sfe_misc_opt_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("miscibility_optimization_server", envir = local_env))
  expect_true(is.function(local_env$miscibility_optimization_server))
})

test_that("sfe_misc_comp_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/sfe_misc_comp_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("miscomp_server", envir = local_env))
  expect_true(is.function(local_env$miscomp_server))
})

test_that("sfe_aux_tool_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  file_path <- file.path(shiny_app_dir, "server_modules/sfe_aux_tool_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("auxiliary_tools_server", envir = local_env))
  expect_true(is.function(local_env$auxiliary_tools_server))
})

test_that("global_settings_modal_server module loads and defines server function", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- new.env()
  load_module_dependencies(local_env)

  # Global settings needs settings_utils
  settings_path <- file.path(shiny_app_dir, "utils/settings_utils.R")
  if (file.exists(settings_path)) {
    source(settings_path, local = local_env)
  }
  manager_path <- file.path(shiny_app_dir, "utils/global_settings_manager.R")
  if (file.exists(manager_path)) {
    source(manager_path, local = local_env)
  }

  file_path <- file.path(shiny_app_dir, "server_modules/global_settings_modal_server.R")
  expect_no_error(source(file_path, local = local_env))
  expect_true(exists("global_settings_modal_server", envir = local_env))
  expect_true(is.function(local_env$global_settings_modal_server))
})
