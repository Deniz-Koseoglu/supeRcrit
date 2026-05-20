# Helper functions for loading DOE seed data
# These functions load pre-existing analysis setups from config files
# to provide users with example data when starting a new session

# Get DOE Analysis seed data directory (separate from user-saved data)
get_doe_analysis_seed_dir <- function() {
  file.path(
    system.file(package = "supeRcrit"),
    "shiny-app", "config", "seed-data", "doe_analysis"
  )
}

# Load all DOE Analysis seed data files (.RData format)
# Returns a list of analyses with standardized structure
load_doe_analysis_seed_data <- function() {
  dir_path <- get_doe_analysis_seed_dir()

  if (!dir.exists(dir_path)) {
    return(list())
  }

  files <- list.files(dir_path, pattern = "\\.RData$", full.names = TRUE)

  if (length(files) == 0) {
    return(list())
  }

  # Sort files by modification time (newest first)
  file_info <- file.info(files)
  files <- files[order(file_info$mtime, decreasing = TRUE)]

  # Read each file and convert to standardized format
  analyses <- lapply(seq_along(files), function(i) {
    tryCatch({
      # Load .RData file into a new environment
      env <- new.env()
      load(files[i], envir = env)
      
      # Get the analysis object
      if (!"analysis_obj" %in% ls(env)) {
        return(NULL)
      }
      analysis_obj <- env$analysis_obj

      # Extract display name from filename
      # Format: name_YYYYMMDD_HHMMSS.RData
      filename <- basename(files[i])
      display_name <- sub("\\.RData$", "", filename)
      # Remove timestamp pattern _YYYYMMDD_HHMMSS
      display_name <- sub("_\\d{8}_\\d{6}$", "", display_name)

      # Get timestamp from metadata if available
      timestamp <- if (!is.null(analysis_obj$metadata$timestamp)) {
        analysis_obj$metadata$timestamp
      } else {
        format(file_info$mtime[i], "%Y-%m-%d %H:%M:%S")
      }

      # Return standardized structure matching what doe_desir expects
      list(
        id = i,
        name = display_name,
        timestamp = timestamp,
        analysis_obj = analysis_obj,  # Use analysis_obj field name
        is_seed = TRUE  # Flag to identify seed data
      )
    }, error = function(e) {
      NULL
    })
  })

  # Remove NULL entries (failed reads)
  analyses <- Filter(Negate(is.null), analyses)

  return(analyses)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
