# com_analysis_helpers.R

render_roi_plot <- function(selected_simple_data, capex, results,i18n) {
  if (!is.null(selected_simple_data)) {
    num_months <- 5 * 12 # Total months for the plot

    # Calculate monthly profit
    monthly_profit_val <- selected_simple_data$monthly_profit

    # Create plot data for months
    plot_data <- data.frame(
      month = 0:num_months, # Start from month 0 (initial investment)
      cumulative_profit = cumsum(c(0, rep(monthly_profit_val, num_months))), # Profit line starts at 0
      cumulative_net_profit = cumsum(c(-capex, rep(monthly_profit_val, num_months))) # Net profit starts with initial investment
    )

    p <- plot_ly(plot_data) %>%
      add_trace( # Gross Profit Line
        x = ~month,
        y = ~cumulative_profit,
        type = "scatter",
        mode = "lines",
        name = i18n$t("Cumulative Gross Profit"),
        line = list(color = "rgb(22, 96, 167)", width = 3)
      ) %>%
      add_trace( # Net Profit Line
        x = ~month,
        y = ~cumulative_net_profit,
        type = "scatter",
        mode = "lines",
        name = i18n$t("Cumulative Net Profit"),
        line = list(color = "rgb(205, 12, 24)", width = 3) # A different color for net profit
      ) %>%
      layout(
        title = paste0(i18n$t('Cumulative Profit Over 5 Years ('), num_months, i18n$t(' Months)')),
        xaxis = list(
          title = "Month",
          # Custom tick text for years
          tickvals = seq(0, num_months, by = 12),
          ticktext = c(i18n$t("Start"), paste0(1:(num_months / 12), i18n$t(" Year"))) # Label 0 as "Start", then years
        ),
        yaxis = list(title = i18n$t("Cumulative Profit (USD)")),
        showlegend = TRUE # Show legend to distinguish lines
      )

    return(p)
  } else {
    plotly_empty()
  }
}


prepare_roi_monthly_data <- function(detailed_data, is_multi_value, time_slider, capex, results, tax_rate_param,i18n) {
  req(detailed_data)

  # Determine the column name based on input type and slider value
  if (is_multi_value) {
    req(time_slider)
    time_cols <- names(detailed_data)[grepl("_min$", names(detailed_data))]
    numeric_times <- as.numeric(gsub("_min$", "", time_cols))

    if (length(numeric_times) == 0 || all(is.na(numeric_times))) {
      selected_col_name <- "60_min" # Fallback to default
    } else {
      closest_time_idx <- which.min(abs(numeric_times - time_slider))
      selected_col_name <- time_cols[closest_time_idx]
    }
  } else { # Single value case
    # Find the single _min column in detailed_data
    single_value_cols <- names(detailed_data)[grepl("_min$", names(detailed_data))]
    if (length(single_value_cols) == 1) {
      selected_col_name <- single_value_cols[1]
    } else {
      # Fallback if no single _min column found (should not happen with valid calcom output)
      selected_col_name <- "60_min" # Keep as a last resort fallback
    }
  }

  # Ensure the selected column exists, fallback to a safe default if not
  if (!selected_col_name %in% names(detailed_data)) {
    # Try to find any _min column if the selected one is missing
    available_min_cols <- names(detailed_data)[grepl("_min$", names(detailed_data))]
    if (length(available_min_cols) > 0) {
      selected_col_name <- available_min_cols[1] # Use the first available _min column
    } else {
      selected_col_name <- "60_min" # Absolute last resort fallback
    }
  }

  # Helper function to get value by parameter name from the selected column
  get_value <- function(data, param_name, col_name) {
    row_index <- which(data$parameter == param_name)
    if (length(row_index) > 0) {
      return(data[row_index, col_name])
    } else {
      return(0) # Return 0 or appropriate default if not found
    }
  }

  cumulative_profit <- 0
  cumulative_net_profit <- 0 - capex

  current_tax_rate <- 0 # Initialize with 0 or NA

  if (!is.null(tax_rate_param)) {
    current_tax_rate <- tax_rate_param
  } 
 
  
 
  # 60 ay için verileri oluştur
  display_data_list <- lapply(1:60, function(i) {
    mgp_usd <- get_value(detailed_data, "mgp", selected_col_name) # USD/month
    svol_usd <- get_value(detailed_data, "svol", selected_col_name) # USD/month
    crm_usd <- get_value(detailed_data, "crm", selected_col_name) # USD/month
    cut_usd <- get_value(detailed_data, "cut", selected_col_name) # USD/month
    com_usd <- get_value(detailed_data, "com", selected_col_name) # USD/month
    fci_usd <- if (!is.null(results) && !is.null(results$extra)) results$extra[["fci"]] else 0 # USD/month
    col_usd <- if (!is.null(results) && !is.null(results$extra)) results$extra[["col"]] else 0 # USD/month

 
    cumulative_profit <<- cumulative_profit + mgp_usd
  
    mnp_usd <- get_value(detailed_data, "mnp", selected_col_name) # USD/month
    if (mnp_usd != 0) {
      cumulative_net_profit <<- cumulative_net_profit + mnp_usd
    } else {
      cumulative_net_profit <<- cumulative_net_profit + mgp_usd
    }
  
    base_df <- data.frame(
      "Month" = i,
      "Cumulative Gross Profit (USD)" = round(cumulative_profit),
      "Cumulative Net Profit (USD)" = round(cumulative_net_profit),
      "Sales Volume (USD/month)" = round(svol_usd),
      "Cost of Manufacturing (USD/month)" = round(com_usd),
      "Cost of Raw Materials (USD/month)" = round(crm_usd),
      "Cost of Utilities (USD/month)" = round(cut_usd),
      "Fixed Cost (USD/month)" = round(fci_usd),
      "Cost of Labour (USD/month)" = round(col_usd),
      "Tax Rate (%)" = round(current_tax_rate, 2),
      check.names = FALSE
    )
    names(base_df) <- c(
      i18n$t("Month"),
      i18n$t("Cumulative Gross Profit (USD)"),
      i18n$t("Cumulative Net Profit (USD)"),
      i18n$t("Sales Volume (USD/month)"),
      i18n$t("Cost of Manufacturing (USD/month)"),
      i18n$t("Cost of Raw Materials (USD/month)"),
      i18n$t("Cost of Utilities (USD/month)"),
      i18n$t("Fixed Cost (USD/month)"),
      i18n$t("Cost of Labour (USD/month)"),
      i18n$t("Tax Rate (%)")
    )

    return(base_df)
  })

  do.call(rbind, display_data_list)
}


render_roi_monthly_table <- function(detailed_data, is_multi_value, time_slider, capex, results, tax_rate_param, i18n) {
  display_data <- prepare_roi_monthly_data(detailed_data, is_multi_value, time_slider, capex, results, tax_rate_param, i18n)
  
  DT::datatable(
    display_data,
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      scrollY = "400px", # Add vertical scroll for all data
      pageLength = -1, # Display all entries
      rownames = FALSE,
      dom = 'Bfrtip',  # visible buttons
      buttons = list(
        list(extend = 'csv', filename = generate_filename_with_timestamp('supercrit_cost_analysis_roi_monthly_data')),
        list(extend = 'excel', filename = generate_filename_with_timestamp('supercrit_cost_analysis_roi_monthly_data')),
        list(extend = 'pdf', filename = generate_filename_with_timestamp('supercrit_cost_analysis_roi_monthly_data'))
      )
    )
  )
}