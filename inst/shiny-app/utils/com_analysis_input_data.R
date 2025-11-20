parsed_input_data_util <- function(input, results, i18n) {
  req(input$input_type)
  
  if (input$input_type == "single") {
    req(input$single_time, input$single_yield)
    data <- data.frame(time = input$single_time, yield = input$single_yield)
    results$is_multi_value <- FALSE
    return(data)
  } else if (input$input_type == "csv") {
    req(input$csv_file, input$time_column, input$yield_column)
    
    tryCatch({
      # Read the CSV file
      data <- read.csv(input$csv_file$datapath, header = TRUE, stringsAsFactors = FALSE, 
                       na.strings = c("", "NA", "N/A"), strip.white = TRUE)
      
      # Remove completely empty columns
      data <- data[, !sapply(data, function(x) all(is.na(x) | x == ""))]
      
      # Check if selected columns exist
      if (!input$time_column %in% names(data)) {
        stop(i18n$t("Selected time column does not exist in the CSV file."))
      }
      if (!input$yield_column %in% names(data)) {
        stop(i18n$t("Selected yield column does not exist in the CSV file."))
      }
      
      # Extract selected columns
      time_data <- data[[input$time_column]]
      yield_data <- data[[input$yield_column]]
      
      # Remove rows with NA values
      valid_rows <- !is.na(time_data) & !is.na(yield_data)
      time_data <- time_data[valid_rows]
      yield_data <- yield_data[valid_rows]
      
      # Check if we have any valid data left
      if (length(time_data) == 0) {
        stop(i18n$t("No valid data found in selected columns after removing NA values."))
      }
      
      # Convert to numeric and validate
      time_numeric <- suppressWarnings(as.numeric(time_data))
      yield_numeric <- suppressWarnings(as.numeric(yield_data))
      
      if (any(is.na(time_numeric))) {
        stop(i18n$t("Time column contains non-numeric values that cannot be converted."))
      }
      if (any(is.na(yield_numeric))) {
        stop(i18n$t("Yield column contains non-numeric values that cannot be converted."))
      }
      
      # Check for negative values
      if (any(time_numeric < 0)) {
        stop(i18n$t("Time values cannot be negative."))
      }
      if (any(yield_numeric < 0)) {
        stop(i18n$t("Yield values cannot be negative."))
      }
      
      # Create final data frame
      final_data <- data.frame(time = time_numeric, yield = yield_numeric)
      
      # Set multi-value flag
      results$is_multi_value <- nrow(final_data) > 1

      message("Final data structure:")
      message(paste(capture.output(print(final_data)), collapse = "\n"))
      message(print(class(final_data)))
      message(print(names(final_data)))
      
      return(final_data)
      
    }, error = function(e) {
      showNotification(paste(i18n$t("Error parsing CSV file:"), e$message), type = "error")
      results$is_multi_value <- FALSE # Reset multi-value flag on error
      return(NULL)
    })
  } else if (input$input_type == "text") {
    req(input$text_input)
    if (nchar(trimws(input$text_input)) == 0) {
      results$is_multi_value <- FALSE # Reset multi-value flag if text input is empty
      return(NULL) # No input yet
    }
    tryCatch({
      data <- read.csv(text = input$text_input, header = TRUE, stringsAsFactors = FALSE)
      if (ncol(data) != 2) {
        stop(i18n$t("Text input must have exactly two columns (time, yield)."))
      }
      if (!all(names(data) == c("time", "yield"))) {
        stop(i18n$t("Text input column names must be 'time' and 'yield'."))
      }
      if (!is.numeric(data$time) || !is.numeric(data$yield)) {
        stop(i18n$t("Time and yield columns must be numeric."))
      }
      results$is_multi_value <- nrow(data) > 1
      return(data)
    }, error = function(e) {
      showNotification(paste(i18n$t("Error parsing text input:"), e$message), type = "error")
      results$is_multi_value <- FALSE # Reset multi-value flag on error
      return(NULL)
    })
  }
  return(NULL)
}
