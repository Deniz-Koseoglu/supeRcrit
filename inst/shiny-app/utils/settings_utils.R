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

# Determine settings root based on whether package is installed or in dev mode
settings_root <- if (nchar(system.file(package = "supeRcrit")) > 0) {
  # Installed package mode
  file.path(system.file(package = "supeRcrit"), "shiny-app", "config", "user-settings")
} else {
  # Development mode: assume we're running from inst/shiny-app/
  file.path("config", "user-settings")
}

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

#----- DOE Analysis specific functions (RData format) --------------------------

#' Save DOE analysis result to .RData file
#' @param app_name Application name (e.g., "doe_analysis")
#' @param analysis_obj Analysis result object from doe_analyze() (plots removed)
#' @param analysis_name User-provided name for the analysis
#' @return File path of saved .RData file
save_doe_analysis <- function(app_name, analysis_obj, analysis_name) {
  stopifnot(is.list(analysis_obj))

  # Ensure plots are removed to reduce file size (but keep as list for validation)
  analysis_obj$plots <- list()

  # Create directory if needed
  dir_path <- .settings_dir(app_name)
  print(paste("DEBUG save_doe_analysis: Saving to directory:", dir_path))

  # Generate timestamped filename
  timestamp <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
  filename <- paste0(analysis_name, "_", timestamp, ".RData")
  filepath <- file.path(dir_path, filename)
  print(paste("DEBUG save_doe_analysis: Full filepath:", filepath))

  # Save using save() to create .RData file
  tryCatch({
    save(analysis_obj, file = filepath)
    print(paste("DEBUG save_doe_analysis: File saved successfully"))
    print(paste("DEBUG save_doe_analysis: File exists?", file.exists(filepath)))
    invisible(filepath)
  }, error = function(e) {
    warning(paste("Error saving DOE analysis:", e$message))
    print(paste("DEBUG save_doe_analysis: ERROR -", e$message))
    invisible(NULL)
  })
}

#' List all saved DOE analyses (.RData files)
#' @param app_name Application name (e.g., "doe_analysis")
#' @return Named vector: names are display names, values are full file paths
list_doe_analyses <- function(app_name) {
  dir <- .settings_dir(app_name)
  print(paste("DEBUG list_doe_analyses: Looking in directory:", dir))
  print(paste("DEBUG list_doe_analyses: Directory exists?", dir.exists(dir)))

  files <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)
  print(paste("DEBUG list_doe_analyses: Found", length(files), "RData files"))

  if (length(files) == 0) {
    return(setNames(character(0), character(0)))
  }

  # Extract display names from filenames (remove timestamp and extension)
  display_names <- sapply(files, function(f) {
    base <- basename(f)
    # Remove .RData extension
    base <- sub("\\.RData$", "", base)
    # Try to remove timestamp pattern _YYYYMMDD_HHMMSS
    base <- sub("_\\d{8}_\\d{6}$", "", base)
    base
  }, USE.NAMES = FALSE)

  setNames(files, display_names)
}

#' Load a DOE analysis from .RData file
#' @param filepath Full path to .RData file
#' @return Analysis object
load_doe_analysis <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("Analysis file not found: ", filepath)
  }

  # Load .RData file into a new environment
  env <- new.env()
  load(filepath, envir = env)

  # Return the analysis_obj (should be the only object in the file)
  if ("analysis_obj" %in% ls(env)) {
    return(env$analysis_obj)
  } else {
    stop("Invalid .RData file: 'analysis_obj' not found")
  }
}
