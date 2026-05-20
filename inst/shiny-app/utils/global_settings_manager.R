# Global Settings Manager for supeRcrit
#
# Collects settings from all inputs and applies them back.
# Used by JSON import/export and Load Defaults.

# =============================================================================
# COLLECT SETTINGS
# =============================================================================

#' Collect all current settings from Shiny inputs
#'
#' @param input Shiny input object (from main session)
#' @param default_settings Default settings structure (from config JSON)
#' @return Named list of all settings, organized by module
collect_global_settings <- function(input, default_settings) {
  settings <- list()

  # Settings that are complex data, not simple UI inputs
  excluded_settings <- c(
    "input_data", "saved_design", "multi_solutes",
    "specific_solvents", "mixtures"
  )

  for (module_name in names(default_settings)) {
    if (module_name == "behavior") next

    module_defaults <- default_settings[[module_name]]
    module_settings <- list()

    for (setting_name in names(module_defaults)) {
      if (setting_name %in% excluded_settings) next

      input_id <- paste0(module_name, "-", setting_name)
      value <- input[[input_id]]

      module_settings[[setting_name]] <- if (!is.null(value)) value else module_defaults[[setting_name]]
    }

    settings[[module_name]] <- module_settings
  }

  # Add metadata
  settings[["_metadata"]] <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    version = tryCatch(as.character(packageVersion("supeRcrit")), error = function(e) "unknown"),
    app = "supeRcrit"
  )

  settings
}


# =============================================================================
# APPLY SETTINGS
# =============================================================================

#' Apply settings to Shiny inputs (for selected modules)
#'
#' @param session Shiny session object (main session)
#' @param settings Named list of settings to apply (only included modules)
#' @param default_settings Default settings structure (for reference)
#' @return Invisible TRUE
apply_global_settings <- function(session, settings, default_settings) {

  excluded_settings <- c(
    "input_data", "saved_design", "multi_solutes",
    "specific_solvents", "mixtures"
  )

  # Remove metadata if present
  settings[["_metadata"]] <- NULL

  for (module_name in names(settings)) {
    if (module_name == "behavior") next

    module_settings <- settings[[module_name]]
    if (!is.list(module_settings)) next

    for (setting_name in names(module_settings)) {
      if (setting_name %in% excluded_settings) next

      value <- module_settings[[setting_name]]
      input_id <- paste0(module_name, "-", setting_name)

      tryCatch(
        update_input_by_type(session, input_id, value),
        error = function(e) {},
        warning = function(w) {}
      )
    }
  }

  invisible(TRUE)
}


# =============================================================================
# HELPER: Update Input by Type
# =============================================================================

#' Update a Shiny input based on its value type
#'
#' @param session Shiny session
#' @param input_id Input ID to update
#' @param value Value to set
update_input_by_type <- function(session, input_id, value) {
  if (is.null(value)) return(invisible(NULL))

  tryCatch({
    if (is.logical(value) && length(value) == 1) {
      updateCheckboxInput(session, input_id, value = value)

    } else if (is.numeric(value) && length(value) == 1) {
      updateNumericInput(session, input_id, value = value)

    } else if (is.character(value) && length(value) == 1) {
      updateTextInput(session, input_id, value = value)
      suppressWarnings(suppressMessages({
        try(updateSelectInput(session, input_id, selected = value), silent = TRUE)
        try(updateRadioButtons(session, input_id, selected = value), silent = TRUE)
      }))

    } else if (is.character(value) && length(value) > 1) {
      suppressWarnings(suppressMessages({
        try(updateCheckboxGroupInput(session, input_id, selected = value), silent = TRUE)
        try(updateSelectInput(session, input_id, selected = value), silent = TRUE)
      }))

    } else if (is.list(value)) {
      return(invisible(NULL))

    } else {
      updateTextInput(session, input_id, value = as.character(value))
    }
  }, error = function(e) {}, warning = function(w) {})

  invisible(NULL)
}


# =============================================================================
# VALIDATION
# =============================================================================

#' Validate settings structure
#'
#' @param settings Settings object to validate
#' @return TRUE if valid
validate_settings <- function(settings) {
  is.list(settings) && length(settings) > 0
}

#' Merge settings with defaults (fill missing values)
#'
#' @param settings User settings
#' @param defaults Default settings
#' @return Merged settings
merge_with_defaults <- function(settings, defaults) {
  merged <- defaults
  for (module_name in names(settings)) {
    if (module_name %in% names(merged)) {
      for (setting_name in names(settings[[module_name]])) {
        merged[[module_name]][[setting_name]] <- settings[[module_name]][[setting_name]]
      }
    } else {
      merged[[module_name]] <- settings[[module_name]]
    }
  }
  merged
}
