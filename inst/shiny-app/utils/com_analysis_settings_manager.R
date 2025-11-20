# com_analysis_settings_manager.R

#------------- helpers to recognise input types --------------------------------
is_scalar_numeric <- function(x) is.numeric(x) && length(x) == 1
is_scalar_logical <- function(x) is.logical(x) && length(x) == 1



#------------- PUBLIC  API -----------------------------------------------------

get_current_settings <- function(input, results) {
  # Numeric inputs
  num_ids <- c(
    "volex", "load", "pres", "temp", "flow", "extime",
    "dilfac", "pr_sale",
    "bh", "id", "pr_mat", "pr_msol", "pr_csol",
    "recp", "rect", "sept",
    "pw_main", "pr_kwh", "pw_dry", "pw_com", "pw_evap",
    "cap_dry", "cap_com", "cap_evap",
    "oper", "whr", "shifts", "wage", "wdays",
    "capex", "maint", "other", "depr",
    "cosol_loss", "csol_flow"
  )

  txt_ids <- c("aux_price", "aux_fraction", "taxrate", "flowpar")

  # Collect values
  settings <- lapply(num_ids, function(id) input[[id]])
  names(settings) <- num_ids
  for (id in txt_ids) settings[[id]] <- input[[id]]

  # Special widgets
  settings$comode     <- input[["comode"]]
  settings$use_coefs  <- isTRUE(input[["use_coefs"]])

  # Add input_data and input_type
  settings$input_type <- input$input_type
  if (!is.null(results$input_data) && nrow(results$input_data) > 0) {
    settings$input_data <- lapply(1:nrow(results$input_data), function(i) {
      current_time <- results$input_data$time[i]
      current_yield <- results$input_data$yield[i]
      list(
        time = if (!is.null(current_time) && !is.na(as.numeric(current_time))) as.numeric(current_time) else 0,
        yield = if (!is.null(current_yield) && !is.na(as.numeric(current_yield))) as.numeric(current_yield) else 0
      )
    })
  } else {
    settings$input_data <- NULL
  }

  settings
}

apply_settings <- function(session, settings, results) {
  # Iterate names and dispatch to appropriate updater
  for (id in names(settings)) {
    val <- settings[[id]]
    if (id %in% c("comode")) {
      updateRadioButtons(session, id, selected = val)
    } else if (id %in% c("use_coefs")) {
      updateCheckboxInput(session, id, value = isTRUE(val))
    } else if (is_scalar_numeric(val)) {
      updateNumericInput(session, id, value = val)
    } else if (is_scalar_logical(val)) {
      updateCheckboxInput(session, id, value = val)
    } else { # treat as text
      updateTextInput(session, id, value = val)
    }
  }

  # Handle input_data and input_type
  if (!is.null(settings$input_type)) {
    updateRadioButtons(session, "input_type", selected = settings$input_type)

    restored_list <- list()
    if (!is.null(settings$input_data) && length(settings$input_data) > 0) {
      if (is.data.frame(settings$input_data)) {
        # If it's already a data.frame, convert to list of lists
        restored_list <- lapply(1:nrow(settings$input_data), function(i) {
          list(time = settings$input_data$time[i], yield = settings$input_data$yield[i])
        })
      } else if (is.list(settings$input_data)) {
        # Check if it's a list of lists (like [{time: X, yield: Y}])
        if (length(settings$input_data) > 0 && is.list(settings$input_data[[1]]) &&
            "time" %in% names(settings$input_data[[1]]) && "yield" %in% names(settings$input_data[[1]])) {
          restored_list <- settings$input_data
        } else if (length(settings$input_data) > 0 && is.vector(settings$input_data[[1]]) && length(settings$input_data[[1]]) == 2) {
          # Handle the problematic {V1: [X, Y], V2: [A, B]} case
          restored_list <- lapply(settings$input_data, function(x) {
            list(time = x[1], yield = x[2])
          })
        }
      }
    }

    # Now convert the normalized list to a data.frame
    if (length(restored_list) > 0) {
      restored_df <- as.data.frame(do.call(rbind, lapply(restored_list, function(x) {
        data.frame(time = as.numeric(x$time), yield = as.numeric(x$yield))
      })))
    } else {
      restored_df <- data.frame(time = numeric(0), yield = numeric(0))
    }

    # Ensure time and yield columns are numeric and handle NAs
    if ("time" %in% names(restored_df)) {
      restored_df$time <- as.numeric(restored_df$time)
      restored_df$time[is.na(restored_df$time)] <- 0 # Default NA to 0 or handle as appropriate
    } else {
      restored_df$time <- numeric(nrow(restored_df))
    }
    if ("yield" %in% names(restored_df)) {
      restored_df$yield <- as.numeric(restored_df$yield)
      restored_df$yield[is.na(restored_df$yield)] <- 0 # Default NA to 0 or handle as appropriate
    } else {
      restored_df$yield <- numeric(nrow(restored_df))
    }

    results$input_data <- restored_df
    results$is_multi_value <- nrow(restored_df) > 1

    if (settings$input_type == "single") {
      time_val <- 0
      yield_val <- 0
      if (nrow(restored_df) > 0) {
        time_val <- restored_df$time[1]
        yield_val <- restored_df$yield[1]
      }
      updateNumericInput(session, "single_time", value = time_val)
      updateNumericInput(session, "single_yield", value = yield_val)
    } else if (settings$input_type == "text" || settings$input_type == "csv") {
      csv_lines <- c("time,yield")
      if (nrow(restored_df) > 0) {
        for (i in 1:nrow(restored_df)) {
          csv_lines <- c(csv_lines, paste(restored_df$time[i], restored_df$yield[i], sep = ","))
        }
      }
      csv_string <- paste(csv_lines, collapse = "\n")
      updateTextAreaInput(session, "text_input", value = csv_string)
      if (settings$input_type == "csv") {
        updateRadioButtons(session, "input_type", selected = "text")
      }
    }
  } else { # If settings$input_type is NULL (e.g., old settings file)
    results$input_data <- data.frame(time = numeric(0), yield = numeric(0))
    results$is_multi_value <- FALSE
    updateNumericInput(session, "single_time", value = 0)
    updateNumericInput(session, "single_yield", value = 0)
    updateTextAreaInput(session, "text_input", value = "")
    updateRadioButtons(session, "input_type", selected = "single") # Default to single
  }

  invisible(TRUE)
}
