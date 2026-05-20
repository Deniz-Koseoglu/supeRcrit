# Setup file for Shiny app tests
# This file is automatically sourced before tests run

# Store the shiny app directory path
shiny_app_dir <- system.file("shiny-app", package = "supeRcrit")

# Create a mock i18n translator that returns the key as-is
# This allows testing without actual translation files
create_mock_i18n <- function() {

  mock_i18n <- list(
    t = function(key) key,
    get_translation_language = function() "en",
    set_translation_language = function(lang) invisible(NULL)
  )
  class(mock_i18n) <- "Translator"
  mock_i18n
}

# Create a reactive wrapper for mock i18n (used by helper functions)
create_mock_i18n_r <- function() {
  mock_i18n <- create_mock_i18n()
  function() mock_i18n
}

# Create mock default settings
create_mock_defaults <- function() {
  list(
    language = "en",
    pressure_unit = "bar",
    temperature_unit = "C",
    decimal_places = 3,
    theme = "light"
  )
}

# Create mock reactive values for SFE modules
create_mock_sfe_rv <- function() {
  shiny::reactiveValues(
    saved_calculations = list(),
    current_calculation = NULL
  )
}

# Create mock reactive values for DOE modules
create_mock_doe_rv <- function() {
  shiny::reactiveValues(
    designs = list(),
    current_design = NULL,
    analyses = list()
  )
}

# Helper to source a file from the shiny app directory
source_shiny_file <- function(relative_path) {
  full_path <- file.path(shiny_app_dir, relative_path)
  if (file.exists(full_path)) {
    source(full_path, local = FALSE)
    return(TRUE)
  }

  return(FALSE)
}

# Helper to check if a file parses without errors
check_file_syntax <- function(relative_path) {
  full_path <- file.path(shiny_app_dir, relative_path)
  if (!file.exists(full_path)) {
    return(list(valid = FALSE, error = "File not found"))
  }

  tryCatch({
    parse(file = full_path)
    list(valid = TRUE, error = NULL)
  }, error = function(e) {
    list(valid = FALSE, error = e$message)
  })
}

# List of all server module files
server_module_files <- c(
  "server_modules/global_settings_modal_server.R",
  "server_modules/com_analysis_server.R",
  "server_modules/sfe_sol_char_server.R",
  "server_modules/sfe_misc_comp_server.R",
  "server_modules/sfe_aux_tool_server.R",
  "server_modules/sfe_misc_opt_server.R",
  "server_modules/doe_design_server.R",
  "server_modules/doe_analysis_server.R",
  "server_modules/doe_desir_server.R",
  "server_modules/kinetic_tws_server.R",
  "server_modules/kinetic_bic_server.R",
  "server_modules/kinetic_aux_tool_server.R"
)

# List of all utility files
utility_files <- c(
  "utils/general_helpers.R",
  "utils/com_analysis_input_data.R",
  "utils/com_analysis_helpers.R",
  "utils/com_analysis_download.R",
  "utils/settings_utils.R",
  "utils/kinetic_helpers.R",
  "utils/button_state_helpers.R",
  "utils/saved_calculations_helpers.R",
  "utils/doe_seed_data_helpers.R",
  "utils/input_help.R",
  "utils/sfe_seed_data_helpers.R",
  "utils/global_settings_manager.R"
)

# List of all UI module files
ui_module_files <- c(
  "ui_modules/global_settings_modal_ui.R",
  "ui_modules/com_analysis_ui.R",
  "ui_modules/sfe_sol_char_ui.R",
  "ui_modules/sfe_misc_comp_ui.R",
  "ui_modules/sfe_aux_tool_ui.R",
  "ui_modules/sfe_misc_opt_ui.R",
  "ui_modules/doe_design_ui.R",
  "ui_modules/doe_analysis_ui.R",
  "ui_modules/doe_desir_ui.R",
  "ui_modules/kinetic_tws_ui.R",
  "ui_modules/kinetic_bic_ui.R",
  "ui_modules/kinetic_aux_tool_ui.R"
)
