# Helper functions for loading SFE/GCM seed data
# These functions load pre-existing GCM calculation setups from config files
# to provide users with example data when starting a new session

# Get SFE GCM calculations seed data directory
get_sfe_gcm_seed_dir <- function() {
  file.path(
    system.file(package = "supeRcrit"),
    "shiny-app", "config", "seed-data", "sfe_gcm_calculations"
  )
}

# Load all SFE GCM seed data files
# Returns a list of calculations with standardized structure
load_sfe_gcm_seed_data <- function() {
  dir_path <- get_sfe_gcm_seed_dir()

  if (!dir.exists(dir_path)) {
    return(list())
  }

  files <- list.files(dir_path, pattern = "\\.json$", full.names = TRUE)

  if (length(files) == 0) {
    return(list())
  }

  # Sort files by modification time (newest first)
  file_info <- file.info(files)
  files <- files[order(file_info$mtime, decreasing = TRUE)]

  # Read each file and convert to standardized format
  calculations <- lapply(seq_along(files), function(i) {
    tryCatch({
      json_data <- jsonlite::fromJSON(files[i], simplifyVector = FALSE)

      # Display name = compound name + created date only (methods are intentionally NOT listed —
      # they conflated "manual" across properties). Falls back to the stored display_name if the
      # compound name is missing.
      created <- json_data$metadata$created_date %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      disp <- if (!is.null(json_data$parameters$name_input) && nzchar(json_data$parameters$name_input))
        sprintf("%s | %s", json_data$parameters$name_input, created) else json_data$display_name

      # Return standardized structure
      list(
        id = i,
        display_name = disp,
        timestamp = created,
        parameters = json_data$parameters,
        is_seed = TRUE  # Flag to identify seed data
      )
    }, error = function(e) {
      NULL
    })
  })

  # Remove NULL entries (failed reads)
  calculations <- Filter(Negate(is.null), calculations)

  return(calculations)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
