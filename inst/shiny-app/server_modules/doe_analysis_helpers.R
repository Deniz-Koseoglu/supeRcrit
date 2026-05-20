# ==============================================================================
# OPTIMIZATION RESULTS HELPERS
# Helper functions to generate optimization outputs for both initial and final models
# ==============================================================================

# Helper: Create Predicted Response output
create_predicted_response_output <- function(analysis_results, model_type = "final", i18n = NULL) {
  result <- analysis_results$analysis_result

  if (!is.null(result$results[[model_type]]$Model_Metrics$Canonical_Analysis)) {
    ca <- result$results[[model_type]]$Model_Metrics$Canonical_Analysis
    predicted <- ca$predicted

    html_content <- paste0(
      '<div style="text-align: center; padding: 20px;">',
      '<h2 style="color: #3c8dbc;">', sprintf("%.4f", predicted), "</h2>",
      '<p style="margin-top: 15px; font-size: 14px;">',
      if (!is.null(i18n)) i18n$t("Predicted") else "Predicted", " ",
      analysis_results$response_var, " ",
      if (!is.null(i18n)) i18n$t("at stationary point") else "at stationary point",
      "</p>",
      "</div>"
    )
  } else {
    html_content <- paste0(
      '<div style="text-align: center; padding: 20px;">',
      '<p class="text-muted">',
      if (!is.null(i18n)) i18n$t("Not available") else "Not available",
      "</p>",
      "</div>"
    )
  }

  return(HTML(html_content))
}

# Helper: Create Optimal Conditions Detailed table
create_optimal_conditions_detailed <- function(analysis_results, model_type = "final") {
  result <- analysis_results$analysis_result
  ca <- result$results[[model_type]]$Model_Metrics$Canonical_Analysis

  if (!is.null(ca)) {
    coded_names <- names(ca$xs)
    realnames <- result$models[[model_type]]$realnames

    # Build the optimal conditions data frame
    optimal_df <- data.frame(
      Factor = coded_names,
      stringsAsFactors = FALSE
    )

    # Add Factor Name column if uncoded names are available
    has_realnames <- !is.null(realnames) && length(realnames) == length(coded_names)
    if (has_realnames) {
      optimal_df$Name <- realnames
    }

    optimal_df$Coded <- round(as.vector(ca$xs), 4)

    # Add decoded values if available
    if (!is.null(ca$xs_decoded)) {
      optimal_df$Decoded <- round(as.vector(ca$xs_decoded), 4)
      if (has_realnames) {
        add_prettynames <- c(
          i18n_r()$t("Factor"),
          i18n_r()$t("Factor Name"),
          i18n_r()$t("Coded Value"),
          i18n_r()$t("Decoded Value")
        )
      } else {
        add_prettynames <- c(
          i18n_r()$t("Factor"),
          i18n_r()$t("Coded Value"),
          i18n_r()$t("Decoded Value")
        )
      }
    } else {
      if (has_realnames) {
        add_prettynames <- c(
          i18n_r()$t("Factor"),
          i18n_r()$t("Factor Name"),
          i18n_r()$t("Coded Value")
        )
      } else {
        add_prettynames <- c(
          i18n_r()$t("Factor"),
          i18n_r()$t("Coded Value")
        )
      }
    }

    DT::datatable(
      optimal_df,
      colnames = add_prettynames,
      options = list(
        dom = "t",
        pageLength = 20,
        language = tablang()
      ),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; color: #666; font-size: 14px; padding: 10px;",
        i18n$t("Stationary point from Canonical Analysis (for quadratic models)")
      )
    )
  } else {
    DT::datatable(
      data.frame(Message = i18n_r()$t("Canonical analysis not available (requires quadratic model)")),
      options = list(dom = "t", language = tablang()),
      rownames = FALSE
    )
  }
}

# Helper: Create Min/Max Comparison table
create_minmax_comparison <- function(analysis_results, model_type = "final") {
  result <- analysis_results$analysis_result

  if (!is.null(result$results[[model_type]]$Model_Metrics$Trad_Opt)) {
    trad_opt <- result$results[[model_type]]$Model_Metrics$Trad_Opt

    # Determine if we should show coded, decoded, or both
    has_coded <- !is.null(trad_opt$coded)
    has_decoded <- !is.null(trad_opt$decoded)

    if (has_coded && has_decoded) {
      # Show both coded and decoded factor values
      coded_df <- trad_opt$coded
      decoded_df <- trad_opt$decoded

      if (nrow(coded_df) >= 2 && nrow(decoded_df) >= 2) {
        coded_factor_cols <- setdiff(names(coded_df), analysis_results$response_var)
        decoded_factor_cols <- setdiff(names(decoded_df), analysis_results$response_var)

        comparison_list <- list(
          Type = c(i18n_r()$t("Minimum"), i18n_r()$t("Maximum"))
        )

        # Coded factors (A, B, C)
        for (col in coded_factor_cols) {
          comparison_list[[col]] <- round(coded_df[[col]], 4)
        }

        # Decoded factors (actual names like Pressure, Temperature)
        for (col in decoded_factor_cols) {
          comparison_list[[col]] <- round(decoded_df[[col]], 4)
        }

        # Response
        comparison_list[[i18n_r()$t("Response")]] <- round(coded_df[[analysis_results$response_var]], 4)

        comparison_df <- as.data.frame(comparison_list, stringsAsFactors = FALSE, check.names = FALSE)
        colnames(comparison_df)[1] <- i18n_r()$t("Type")

        DT::datatable(
          comparison_df,
          options = list(
            dom = "t",
            pageLength = 10,
            scrollX = TRUE,
            language = tablang()
          ),
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = "caption-side: top; text-align: center; color: #666; font-size: 14px; padding: 10px;",
            i18n_r()$t("Min/Max predictions from Traditional Optimization")
          )
        )
      } else {
        DT::datatable(
          data.frame(Message = i18n_r()$t("Insufficient optimization data")),
          options = list(dom = "t", language = tablang()),
          rownames = FALSE
        )
      }
    } else if (has_coded) {
      # Show only coded values
      coded_df <- trad_opt$coded

      if (nrow(coded_df) >= 2) {
        factor_cols <- setdiff(names(coded_df), analysis_results$response_var)

        # Get decoded factor names if available
        realnames <- result$models[[model_type]]$realnames
        # Build named mapping: A->Pressure, B->Temperature, etc.
        codenames_vec <- if (!is.null(result$models[[model_type]]$codenames)) {
          result$models[[model_type]]$codenames[nchar(result$models[[model_type]]$codenames[, "data"]) == 1, "data"]
        } else factor_cols
        rn_map <- if (!is.null(realnames) && length(realnames) == length(codenames_vec)) {
          setNames(realnames, codenames_vec)
        } else NULL
        
        comparison_list <- list(
          Type = c(i18n_r()$t("Minimum"), i18n_r()$t("Maximum"))
        )

        for (col in factor_cols) {
          col_label <- if (!is.null(rn_map) && col %in% names(rn_map)) rn_map[[col]] else col
          comparison_list[[col_label]] <- round(coded_df[[col]], 4)
        }
        
        comparison_list[[i18n_r()$t("Response")]] <- round(coded_df[[analysis_results$response_var]], 4)

        comparison_df <- as.data.frame(comparison_list, stringsAsFactors = FALSE, check.names = FALSE)
        colnames(comparison_df)[1] <- i18n_r()$t("Type")

        DT::datatable(
          comparison_df,
          options = list(
            dom = "t",
            pageLength = 10,
            language = tablang()
          ),
          rownames = FALSE,
          caption = htmltools::tags$caption(
            style = "caption-side: top; text-align: center; color: #666; font-size: 14px; padding: 10px;",
            i18n_r()$t("Min/Max predictions (coded values only)")
          )
        )
      } else {
        DT::datatable(
          data.frame(Message = i18n_r()$t("Insufficient optimization data")),
          options = list(dom = "t", language = tablang()),
          rownames = FALSE
        )
      }
    } else {
      DT::datatable(
        data.frame(Message = i18n_r()$t("Traditional optimization data format issue")),
        options = list(dom = "t", language = tablang()),
        rownames = FALSE
      )
    }
  } else {
    DT::datatable(
      data.frame(Message = i18n_r()$t("Traditional optimization not available")),
      options = list(dom = "t", language = tablang()),
      rownames = FALSE
    )
  }
}

# Helper: Create Eigenvalues Table
create_eigenvalues_table <- function(analysis_results, model_type = "final") {
  result <- analysis_results$analysis_result

  if (!is.null(result$results[[model_type]]$Model_Metrics$Canonical_Analysis)) {
    ca <- result$results[[model_type]]$Model_Metrics$Canonical_Analysis

    # Create translated sign labels
    sign_positive <- i18n_r()$t("Positive (+)")
    sign_negative <- i18n_r()$t("Negative (-)")
    sign_zero <- i18n_r()$t("Zero (0)")

    eigen_df <- data.frame(
      Factor = names(ca$xs),
      Eigenvalue = round(ca$eigen$values, 6),
      Sign = ifelse(ca$eigen$values > 0, sign_positive,
        ifelse(ca$eigen$values < 0, sign_negative, sign_zero)
      ),
      stringsAsFactors = FALSE
    )
    add_prettynames <- c(
      i18n_r()$t("Factor"),
      i18n_r()$t("Eigenvalue"),
      i18n_r()$t("Sign")
    )

    DT::datatable(
      eigen_df,
      colnames=add_prettynames,
      options = list(
        dom = "t",
        pageLength = 20,
        language = tablang()
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Sign",
        backgroundColor = DT::styleEqual(
          c(sign_positive, sign_negative, sign_zero),
          c("#d4edda", "#f8d7da", "#fff3cd")
        )
      )
  } else {
    DT::datatable(
      data.frame(Message = "Eigenvalue analysis not available"),
      options = list(dom = "t", language = tablang()),
      rownames = FALSE
    )
  }
}

# Helper: Create Optimization Notes output
create_ca_warnings <- function(analysis_results, model_type = "final") {
  result <- analysis_results$analysis_result
  statements <- result$statements
  
  if (is.null(statements)) {
    return(HTML(paste0('<p style="color: #666; font-style: italic;">', i18n_r()$t("No optimization notes available."), '</p>')))
  }
  
  # Helper to translate dynamic source-code statements
  translate_stmt <- function(name, text) {
    if (grepl("Optim_1_", name)) {
      text <- sub("^The (initial|final) model yielded the following stationary point",
                  i18n_r()$t("The model yielded the following stationary point"), text)
      text <- sub("\\(uncoded values in brackets if available\\)",
                  paste0("(", i18n_r()$t("uncoded values in brackets if available"), ")"), text)
      text <- sub(", with eigenvalues:", paste0(", ", i18n_r()$t("with eigenvalues"), ":"), text)
    } else if (grepl("Optim_4_", name)) {
      text <- sub("The stationary point found by canonical analysis is a ",
                  paste0(i18n_r()$t("The stationary point found by canonical analysis is a"), " "), text)
      text <- sub("maximum\\.", paste0(i18n_r()$t("maximum"), "."), text)
      text <- sub("minimum\\.", paste0(i18n_r()$t("minimum"), "."), text)
      text <- sub("saddle point\\.", paste0(i18n_r()$t("saddle point"), "."), text)
    } else if (grepl("Optim_5_", name)) {
      text <- sub("Model prediction at the stationary point yields the response value:",
                  paste0(i18n_r()$t("Model prediction at the stationary point yields the response value"), ":"), text)
    } else if (grepl("Optim_2_", name)) {
      text <- sub("^NOTE:", paste0(i18n_r()$t("NOTE"), ":"), text)
      text <- sub("optimum decoded CA values are out of range!",
                  i18n_r()$t("optimum decoded CA values are out of range!"), text)
    } else if (grepl("Optim_3_", name)) {
      text <- sub("^NOTE:", paste0(i18n_r()$t("NOTE"), ":"), text)
      text <- sub("CA eigen values are positive!",
                  i18n_r()$t("CA eigen values are positive!"), text)
    }
    text
  }

  # Get model-specific suffix
  model_suffix <- if (model_type == "final") "_final" else "_initial"
  
  # Collect relevant statements for THIS model only
  notes <- c()
  
  # Optim_1: Stationary point description
  optim1_key <- paste0("Optim_1", model_suffix)
  if (optim1_key %in% names(statements)) {
    notes <- c(notes, translate_stmt(optim1_key, statements[[optim1_key]]))
  }
  
  # Optim_4: Stationary point type (maximum/minimum/saddle)
  optim4_key <- paste0("Optim_4", model_suffix)
  if (optim4_key %in% names(statements)) {
    notes <- c(notes, translate_stmt(optim4_key, statements[[optim4_key]]))
  }
  
  # Optim_5: Predicted response at stationary point
  optim5_key <- paste0("Optim_5", model_suffix)
  if (optim5_key %in% names(statements)) {
    notes <- c(notes, translate_stmt(optim5_key, statements[[optim5_key]]))
  }
  
  # Optim_2: Out of range warning (if present)
  optim2_key <- paste0("Optim_2", model_suffix)
  if (optim2_key %in% names(statements)) {
    notes <- c(notes, translate_stmt(optim2_key, statements[[optim2_key]]))
  }
  
  # Optim_3: Eigenvalue sign note (if present)
  optim3_key <- paste0("Optim_3", model_suffix)
  if (optim3_key %in% names(statements)) {
    notes <- c(notes, translate_stmt(optim3_key, statements[[optim3_key]]))
  }
  
  if (length(notes) > 0) {
    # Format as simple paragraphs
    html_content <- paste0(
      '<div style="padding: 10px;">',
      paste0('<p style="margin-bottom: 10px; line-height: 1.5;">', notes, '</p>', collapse = ""),
      '</div>'
    )
  } else {
    html_content <- paste0('<p style="color: #666; font-style: italic; padding: 10px;">', i18n_r()$t("No canonical analysis available for this model. Canonical analysis requires a quadratic model (Model Order = 2)."), '</p>')
  }
  
  return(HTML(html_content))
}

# Helper: Create Steepest Ascent Table
create_steepest_ascent_table <- function(analysis_results, model_type = "final") {
  result <- analysis_results$analysis_result

  steep_data <- result$results[[model_type]]$Model_Metrics$Steepest_Ascent
  
  # Check if steepest ascent data exists and is a data frame
  if (!is.null(steep_data) && is.data.frame(steep_data)) {
    steep_df <- steep_data
    
    # Only try to round the response variable if it exists in the data frame
    resp_var <- analysis_results$response_var
    if (!is.null(resp_var) && resp_var %in% names(steep_df)) {
      steep_df[[resp_var]] <- round(steep_df[[resp_var]], 3)
    }

    DT::datatable(
      steep_df,
      options = list(
        dom = "t",
        pageLength = 10,
        language = tablang()
      ),
      rownames = FALSE
    )
  } else {
    DT::datatable(
      data.frame(Message = i18n_r()$t("Steepest Ascent data not available for this model.")),
      colnames = i18n_r()$t("Message"),
      options = list(dom = "t", language = tablang()),
      rownames = FALSE
    )
  }
}

# Helper: Create Coefficients Table
create_coefficients_table <- function(analysis_results, model_type = "final") {
  result <- analysis_results$analysis_result

  if (is.null(result)) {
    return(DT::datatable(
      data.frame(Message = "No analysis results available"),
      options = list(dom = "t", language = tablang()),
      rownames = FALSE
    ))
  }

  coeffs <- result$results[[model_type]]$Model_Results %>%
    select(Term, Estimate, Stnd_Error, t_value, p_value, Signif_Level) %>%
    mutate(
      Estimate = round(Estimate, 4),
      Stnd_Error = round(Stnd_Error, 4),
      t_value = round(t_value, 4),
      p_value = round(p_value, 4)
    )

  # Column names translation
  add_prettynames <- c(
    i18n_r()$t("Term"),
    i18n_r()$t("Estimate"),
    i18n_r()$t("Standard Error"),
    paste0("<em>t</em>", i18n_r()$t("-value")),
    paste0("<em>p</em>", i18n_r()$t("-value")),
    i18n_r()$t("Significance Level")
  )

  # Term values translation (Intercept and common DOE terms)
  term_translations <- c(
    "Intercept" = i18n_r()$t("Intercept")
  )

  # Apply translation to Term column
  dt_data <- coeffs
  dt_data$Term <- my_mapvalues(dt_data$Term, names(term_translations), term_translations, warn_missing = FALSE)

  DT::datatable(
    dt_data,
    extensions = "Buttons",
    colnames = add_prettynames,
    rownames = FALSE,
    escape = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 20,
      language = tablang(),
      dom = "Bfrtip",
      buttons = list(
        list(
          extend = "copy",
          text = i18n$t("Copy"),
          titleAttr = i18n$t("Copy"),
          action = copy_button_no_popup(
            copy_label = i18n$t("Copy"),
            copied_label = i18n$t("Copied!")
          )
        ),
        list(extend = "csv", filename = generate_filename_with_timestamp(paste0("supercrit_doe_", model_type, "_model_coefficient"))),
        list(extend = "excel", filename = generate_filename_with_timestamp(paste0("supercrit_doe_", model_type, "_model_coefficient"))),
        list(extend = "pdf", filename = generate_filename_with_timestamp(paste0("supercrit_doe_", model_type, "_model_coefficient")))
      )
    )
  )
}

# Helper: Create Model Data Table
create_model_data_table <- function(analysis_results, model_type = "final") {
  result <- analysis_results$analysis_result

  if (is.null(result)) {
    return(DT::datatable(
      data.frame(Message = "No analysis results available"),
      options = list(dom = "t", language = tablang()),
      rownames = FALSE
    ))
  }

  model_data <- result$results[[model_type]]$Model_Data

  # Translate column names
  col_translations <- c(
    "Residual" = i18n$t("Residual")
  )
  curr_names <- colnames(model_data)
  for (i in seq_along(curr_names)) {
    if (curr_names[i] %in% names(col_translations)) {
      curr_names[i] <- col_translations[[curr_names[i]]]
    }
  }

  DT::datatable(
    model_data,
    colnames = curr_names,
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 25,
      language = tablang(),
      dom = "Bfrtip",
      buttons = list(
        list(
          extend = "copy",
          text = i18n$t("Copy"),
          titleAttr = i18n$t("Copy"),
          action = copy_button_no_popup(
            copy_label = i18n$t("Copy"),
            copied_label = i18n$t("Copied!")
          )
        ),
        list(extend = "csv", filename = generate_filename_with_timestamp(paste0("supercrit_doe_", model_type, "_model_data"))),
        list(extend = "excel", filename = generate_filename_with_timestamp(paste0("supercrit_doe_", model_type, "_model_data"))),
        list(extend = "pdf", filename = generate_filename_with_timestamp(paste0("supercrit_doe_", model_type, "_model_data")))
      )
    ),
    extensions = "Buttons"
  ) %>%
    formatRound("Residual", 3)
}
