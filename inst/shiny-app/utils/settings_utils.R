# Utility helpers for per-application settings management
#
# Each application stores its user-defined settings as JSON files in:
#   inst/shiny-app/config/user-settings/<app_name>/<settings_name>.json
#
# All functions below are agnostic of the specific Shiny module.  They just
# read / write lists (which Shiny modules will create from current input
# values).  They return invisible(NULL) on success or the requested list
# object on reads.
#
# Required packages: jsonlite, fs  (fs is base-R in recent R builds; if the
# user’s environment lacks it they can replace fs::dir_create with
# dir.create(..., recursive = TRUE).)

settings_root <- file.path(
  system.file(package = "supeRcrit"),
  "shiny-app", "config", "user-settings"
)

#----- internal helpers --------------------------------------------------------

.ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.settings_dir <- function(app_name) {
  dir_path <- file.path(settings_root, app_name)
  .ensure_dir(dir_path)
}

.settings_path <- function(app_name, settings_name) {
  file.path(.settings_dir(app_name), paste0(settings_name, ".json"))
}

#----- public API --------------------------------------------------------------

save_settings <- function(app_name, settings, settings_name) {
  stopifnot(is.list(settings))
  path <- .settings_path(app_name, settings_name)
  tryCatch({
    jsonlite::write_json(settings, path, auto_unbox = TRUE, pretty = TRUE)
    invisible(path)
  }, error = function(e) {
    warning(paste(i18n$t("Error saving settings:"), e$message))
    invisible(NULL) # Return NULL on error
  })
}

#' Return vector of saved setting names for an app
list_saved_settings <- function(app_name) {
  dir <- .settings_dir(app_name)
  files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
  sub("\\.json$", "", files)
}

#' Load a settings file and return it as a list.
load_settings <- function(app_name, settings_name) {
  path <- .settings_path(app_name, settings_name)
  if (!file.exists(path)) stop("Settings file not found: ", settings_name)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

#' Import from arbitrary JSON file path; copies into the app folder and returns the list.
import_settings_file <- function(app_name, file_path) {
  obj <- jsonlite::read_json(file_path, simplifyVector = TRUE)
  fname <- tools::file_path_sans_ext(basename(file_path))
  save_settings(app_name, obj, fname)
  invisible(obj)
}

#' Export current settings list to chosen file_path (absolute)
export_settings_file <- function(settings, file_path) {
  jsonlite::write_json(settings, file_path, auto_unbox = TRUE, pretty = TRUE)
  invisible(file_path)
}
