# com_analysis_download.R


download_roi_monthly_processed_data <- function(data) {
  current_time <- Sys.time()

  formatted_time <- format(current_time, "%Y-%m-%d-%H%M%S")
  downloadHandler(
    filename = function() {
      paste("com-analysis-monthly-", formatted_time, ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}


download_detailed_data <- function(detailed_data) {
  current_time <- Sys.time()

  formatted_time <- format(current_time, "%Y-%m-%d-%H%M%S")

  downloadHandler(
    filename = function() {
      paste("com-analysis-detailed-", formatted_time, ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(detailed_data)) {
        write.csv(detailed_data, file, row.names = FALSE)
      }
    }
  )
}
