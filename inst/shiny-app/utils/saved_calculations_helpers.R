# Helper functions for saving and loading GCM calculations

# Storage path
get_gcm_storage_dir <- function() {
  file.path(
    system.file(package = "supeRcrit"),
    "shiny-app", "config", "user-settings", "sfe_gcm_calculations"
  )
}

# Dosya adı oluştur (timestamp ile)
generate_calc_filename <- function(inputs) {
  # Methods kısa format
  methods <- paste(
    c(inputs$gcm_tb, inputs$gcm_crit, inputs$gcm_hsp),
    collapse = "-"
  )
  methods <- gsub("none-?|^-|-$", "", methods)
  
  # Timestamp: HHMMSS (dosya adında : kullanılamaz)
  time_str <- format(Sys.time(), "%H%M%S")
  
  # İsim temizle (özel karakterleri kaldır, boşlukları alt çizgiye çevir)
  clean_name <- gsub("[^A-Za-z0-9_]", "", inputs$name_input)
  clean_name <- gsub(" ", "_", clean_name) # Replace spaces with underscores
  
  # Ensure methods is not empty
  if (methods == "") {
    methods <- "no_method"
  }

  # Ensure clean_name is not empty
  if (clean_name == "") {
    clean_name <- "unnamed_solute"
  }

  sprintf("%s_%s_simp%s_gord%s-%s.json",
          clean_name,
          methods,
          inputs$gcm_simplicity,
          inputs$gcm_gorder,
          time_str)
}

# Display name (UI için)
generate_calc_display_name <- function(inputs) {
  methods <- paste(
    c(inputs$gcm_tb, inputs$gcm_crit, inputs$gcm_hsp),
    collapse = "-"
  )
  methods <- gsub("none-?|^-|-$", "", methods)
  
  time_str <- format(Sys.time(), "%H:%M")
  
  sprintf("%s | %s | %s", inputs$name_input, methods, time_str)
}

# Kaydet
save_gcm_calculation <- function(inputs) {
  dir_path <- get_gcm_storage_dir()
  print(paste("DEBUG: save_gcm_calculation called. dir_path:", dir_path))
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  
  # FIFO: Eski dosyaları temizle (10'dan fazla varsa)
  existing_files <- list.files(dir_path, pattern = "\\.json$", full.names = TRUE)
  print(paste("DEBUG: Existing files count:", length(existing_files)))
  if (length(existing_files) >= 10) {
    # En eski dosyayı sil
    file_info <- file.info(existing_files)
    oldest_file <- rownames(file_info)[which.min(file_info$mtime)]
    print(paste("DEBUG: Deleting oldest file:", oldest_file))
    file.remove(oldest_file)
  }
  
  # Yeni dosya oluştur
  calc_data <- list(
    display_name = generate_calc_display_name(inputs),
    parameters = list(
      cas_input = inputs$cas_input,
      name_input = inputs$name_input,
      smiles_input = inputs$smiles_input,
      specify_smiles = inputs$specify_smiles,
      gcm_tb = inputs$gcm_tb,
      gcm_crit = inputs$gcm_crit,
      gcm_hsp = inputs$gcm_hsp,
      gcm_simplicity = inputs$gcm_simplicity,
      gcm_gorder = inputs$gcm_gorder
    ),
    metadata = list(
      created_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      app_version = "0.9.0"
    )
  )
  
  filename <- generate_calc_filename(inputs)
  filepath <- file.path(dir_path, filename)
  print(paste("DEBUG: Saving to filepath:", filepath))
  
  tryCatch({
    jsonlite::write_json(calc_data, filepath, pretty = TRUE, auto_unbox = TRUE)
    print("DEBUG: File written successfully.")
    return(TRUE)
  }, error = function(e) {
    print(paste("ERROR: Failed to write JSON file:", e$message))
    return(FALSE)
  })
}

# Dosyaları listele (dropdown için)
get_saved_calculations <- function() {
  dir_path <- get_gcm_storage_dir()
  
  if (!dir.exists(dir_path)) {
    return(list())
  }
  
  files <- list.files(dir_path, pattern = "\\.json$", full.names = TRUE)
  
  if (length(files) == 0) {
    return(list())
  }
  
  # Dosyaları tarihe göre sırala (en yeni önce)
  file_info <- file.info(files)
  files <- files[order(file_info$mtime, decreasing = TRUE)]
  
  # Her dosyayı oku
  calcs <- lapply(files, function(f) {
    tryCatch({
      jsonlite::fromJSON(f, simplifyVector = FALSE)
    }, error = function(e) {
      NULL
    })
  })
  
  # NULL olanları filtrele
  calcs <- Filter(Negate(is.null), calcs)
  
  return(calcs)
}

# Dropdown choices
get_calculation_choices <- function() {
  calcs <- get_saved_calculations()
  
  if (length(calcs) == 0) {
    return(c("No saved calculations" = ""))
  }
  
  choices <- sapply(calcs, function(calc) {
    calc$display_name
  })
  
  # Return a named vector where names are display_name and values are indices
  # This allows the UI to display the display_name and use the index for retrieval
  return(setNames(seq_along(calcs), choices))
}

# ID ile getir
get_calculation_by_id <- function(calc_id) {
  calcs <- get_saved_calculations()
  
  if (calc_id == "" || as.numeric(calc_id) > length(calcs)) {
    return(NULL)
  }
  
  return(calcs[[as.numeric(calc_id)]])
}
