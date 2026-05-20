# Helper function to add legend and caption to a plotly object
add_legend_and_caption <- function(plotly_p, ggplot_obj) {
  if (!is.null(ggplot_obj) && inherits(ggplot_obj, "ggplot")) {
    # Legend labels ve caption bilgilerini al
    legend_labels <- ggplot_obj$scales$scales[[1]]$labels
    caption_text <- ggplot_obj$labels$caption

    # Legend ve caption'ı ekle
    plotly_p <- plotly_p %>%
      layout(
        showlegend = TRUE,
        legend = list(
          title = list(text = ""),
          tracegroupgap = 0
        ),
        # Caption'ı annotation olarak ekle
        annotations = list(
          list(
            x = 1.2,
            y = -0.125,
            text = caption_text,
            xref = "paper",
            yref = "paper",
            xanchor = "right",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 13)
          )
        )
      )

    # Legend isimlerini düzelt
    for (i in seq_along(plotly_p$x$data)) {
      if (!is.null(plotly_p$x$data[[i]]$name)) {
        # Eğer legend_labels varsa, bunları kullan
        if (i <= length(legend_labels)) {
          plotly_p$x$data[[i]]$name <- legend_labels[i]
        }
      }
    }
  }
  return(plotly_p)
}

# Helper function to add formula annotations to a plotly object
add_formula_annotations <- function(plotly_p, ggplot_obj) {
  built <- ggplot_build(ggplot_obj)
  stats_layer <- built$data[[length(built$data)]]

  if ("label" %in% names(stats_layer) && nrow(stats_layer) > 0) {
    # npc koordinatları
    if ("npcx" %in% names(stats_layer) && "npcy" %in% names(stats_layer)) {
      x_pos <- stats_layer$npcx[1]
      y_pos <- stats_layer$npcy[1]
      xref <- "paper"
      yref <- "paper"
    } else {
      x_pos <- 0.05
      y_pos <- 0.95
      xref <- "paper"
      yref <- "paper"
    }

    # R expression'ı al
    label_text <- as.character(stats_layer$label[1])

    # R expression'ı HTML'e parse et
    label_text <- gsub("italic\\(([^\\)]+)\\)", "<i>\\1</i>", label_text)
    label_text <- gsub("\\^([0-9]+)", "<sup>\\1</sup>", label_text)
    label_text <- gsub("plain\\(\"([^\"]+)\"\\)", "\\1", label_text)
    label_text <- gsub("`([^`]+)`", "\\1", label_text)
    label_text <- gsub("~", " ", label_text)
    label_text <- gsub("\\*", "", label_text)
    label_text <- gsub('"', "", label_text)
    label_text <- gsub(";\\s*", "<br>", label_text)
    label_text <- gsub("\\s+", " ", label_text)
    label_text <- trimws(label_text)

    plotly_p <- plotly_p %>%
      layout(
        annotations = list(
          list(
            x = x_pos,
            y = y_pos,
            text = label_text,
            xref = xref,
            yref = yref,
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 13, color = "black"),
            bgcolor = "rgba(255, 255, 255, 0.85)",
            bordercolor = "black",
            borderwidth = 1,
            borderpad = 4
          )
        )
      )
  }
  return(plotly_p)
}

# Helper function to add layer labels as annotations to a plotly object
add_layer_labels <- function(plotly_p, ggplot_obj, layers_to_check = c(4, 5)) {
  built <- ggplot_build(ggplot_obj)
  annotations <- list()

  for (layer_idx in layers_to_check) {
    if (layer_idx <= length(built$data)) {
      layer_data <- built$data[[layer_idx]]

      if ("label" %in% names(layer_data) &&
          "x" %in% names(layer_data) &&
          "y" %in% names(layer_data)) {
        for (i in 1:nrow(layer_data)) {
          if (!is.na(layer_data$label[i])) {
            annotations[[length(annotations) + 1]] <- list(
              x = layer_data$x[i],
              y = layer_data$y[i],
              text = as.character(layer_data$label[i]),
              xref = "x",
              yref = "y",
              showarrow = FALSE,
              font = list(size = 14),
              xanchor = "center",
              yanchor = "bottom",
              bgcolor = "rgba(255, 255, 255, 0.8)",
              bordercolor = "red",
              borderwidth = 1,
              borderpad = 4
            )
          }
        }
      }
    }
  }
  if (length(annotations) > 0) {
    plotly_p <- plotly_p %>%
      layout(annotations = annotations)
  }
  return(plotly_p)
}


merge_translation_jsons <- function(
  dir_path = "./www/i18n/",
  output_filename = "translation.json",
  exclude_files = output_filename,
  special_keys = c("cultural_date_format", "languages"),
  verbose = FALSE
) {
  # Ensure dir_path ends with a slash
  if (!endsWith(dir_path, "/")) {
    dir_path <- paste0(dir_path, "/")
  }
  output_filepath <- paste0(dir_path, output_filename)
  
  if (verbose) {
    print(paste0(i18n$t("merge_translation_jsons is running. Output file: "), output_filepath))
  }
  
  # 1. Read existing translation.json ONLY for special keys
  special_key_values <- list()
  if (file.exists(output_filepath)) {
    tryCatch({
      current_main_json <- jsonlite::read_json(output_filepath, simplifyVector = FALSE)
      # Only preserve special keys, ignore existing translations
      for (key in special_keys) {
        if (!is.null(current_main_json[[key]])) {
          special_key_values[[key]] <- current_main_json[[key]]
        }
      }
      if (verbose) {
        print(paste0("Preserved ", length(special_key_values), " special keys from existing ", output_filename, " file."))
      }
    }, error = function(e) {
      message("Warning: Could not read existing ", output_filename, " file. Error: ", e$message)
    })
  }
  
  # 2. Start fresh - no existing translations preserved
  merged_translation_list <- list()
  existing_lookup <- list() # For preventing duplicates within current merge
  
  # 3. Discover and read all other JSON files
  all_files <- list.files(dir_path, pattern = "\\.json$", full.names = TRUE)
  files_to_merge <- setdiff(all_files, output_filepath)
  
  if (verbose) {
    print(paste0("Number of files to merge: ", length(files_to_merge)))
  }
  
  for (file_path in files_to_merge) {
    if (basename(file_path) %in% exclude_files) {
      next
    }
    
    if (verbose) {
      print(paste0("Processing file: ", basename(file_path)))
    }
    
    tryCatch({
      json_data <- jsonlite::read_json(file_path, simplifyVector = FALSE)
      if (!is.null(json_data$translation)) {
        for (item in json_data$translation) {
          if (!is.null(item$en) && is.null(existing_lookup[[item$en]])) {
            # Add all new translations (fresh start)
            merged_translation_list <- c(merged_translation_list, list(item))
            existing_lookup[[item$en]] <- item # Prevent duplicates within current merge
            if (verbose) {
              print(paste0("Translation added (EN): ", item$en))
            }
          } else if (!is.null(item$en)) {
            if (verbose) {
              print(paste0("Duplicate translation skipped (EN): ", item$en))
            }
          }
        }
      }
    }, error = function(e) {
      message("Warning: Could not read or parse ", basename(file_path), ". Error: ", e$message)
    })
  }
  
  # 4. Reconstruct the final JSON with preserved special keys and fresh translations
  final_json_content <- list(translation = merged_translation_list)
  for (key in names(special_key_values)) {
    final_json_content[[key]] <- special_key_values[[key]]
  }
  
  # Reorder elements to ensure special keys are at the top
  ordered_final_json_content <- list()
  for (key in special_keys) {
    if (!is.null(final_json_content[[key]])) {
      ordered_final_json_content[[key]] <- final_json_content[[key]]
    }
  }
  ordered_final_json_content[["translation"]] <- final_json_content[["translation"]]
  
  for (key in names(final_json_content)) {
    if (!(key %in% names(ordered_final_json_content))) {
      ordered_final_json_content[[key]] <- final_json_content[[key]]
    }
  }
  
  # 5. Write the result back to translation.json
  tryCatch({
    jsonlite::write_json(ordered_final_json_content, output_filepath, pretty = TRUE, auto_unbox = TRUE)
    message("Translation files successfully merged into ", output_filename, " (fresh rebuild).")
    if (verbose) {
      print(paste0("Total number of translation entries: ", length(ordered_final_json_content$translation)))
    }
  }, error = function(e) {
    message("Error: Could not write to ", output_filename, " file. Error: ", e$message)
  })
}

generate_filename_with_timestamp <- function(base_name) {
  timestamp <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
  paste0(base_name, "_", timestamp)
}

render_grob_as_image <- function(grob_object, width = 800, height = 600) {
  # Ensure grid and grDevices are loaded
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop("Package 'grDevices' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Create a temporary file for the PNG image
  temp_png <- tempfile(fileext = ".png")

  # Open a PNG graphics device
  grDevices::png(filename = temp_png, width = width, height = height, units = "px", res = 96)

  # Draw the grob object
  grid::grid.draw(grob_object)

  # Close the graphics device
  grDevices::dev.off()

  # Read the PNG file as raw bytes
  png_data <- readBin(temp_png, "raw", file.info(temp_png)$size)

  # Encode to base64
  base64_data <- base64enc::base64encode(png_data)

  # Construct data URI
  data_uri <- paste0("data:image/png;base64,", base64_data)

  # Clean up the temporary file
  unlink(temp_png)

  return(data_uri)
}

# Helper function to validate column selections and add visual warnings
# Checks if the same column is selected multiple times and adds CSS styling
#
# @param session Shiny session object
# @param select_ids Character vector of selectInput IDs (without namespace)
# @param input Shiny input object
# @param wrapper_ids Character vector of wrapper div IDs (optional, for styling)
#
# @return List with: has_duplicates (logical), duplicate_column (character), message (character)
#
# @examples
# # In a Shiny server:
# validation <- validate_column_selections(
#   session = session,
#   select_ids = c("time_column", "yield_column"),
#   input = input,
#   wrapper_ids = c("time_column_wrapper", "yield_column_wrapper")
# )
# if (validation$has_duplicates) {
#   showNotification(validation$message, type = "warning")
# }
validate_column_selections <- function(session, select_ids, input, wrapper_ids = NULL) {
  # Collect all current selections
  all_selections <- sapply(select_ids, function(id) {
    val <- input[[id]]
    if (is.null(val) || val == "" || val == "None") NULL else val
  })
  all_selections <- unlist(all_selections[!sapply(all_selections, is.null)])

  # Check for duplicates
  has_duplicates <- length(all_selections) != length(unique(all_selections))
  duplicate_column <- NULL

  if (has_duplicates) {
    # Find which column is duplicated
    duplicate_column <- all_selections[duplicated(all_selections)][1]

    # Add red border to the wrapper divs that have duplicates
    if (!is.null(wrapper_ids)) {
      for (i in seq_along(select_ids)) {
        wrapper_id <- wrapper_ids[i]
        current_val <- input[[select_ids[i]]]

        if (!is.null(current_val) && current_val != "" && current_val != "None") {
          if (current_val == duplicate_column) {
            # Add red border
            shinyjs::runjs(sprintf(
              "$('#%s').css({'border': '2px solid #dc3545', 'border-radius': '4px', 'padding': '5px'});",
              wrapper_id
            ))
          } else {
            # Remove red border
            shinyjs::runjs(sprintf(
              "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
              wrapper_id
            ))
          }
        }
      }
    }

    message <- sprintf("Warning: Column '%s' is selected multiple times!", duplicate_column)
  } else {
    # Remove all red borders if no duplicates
    if (!is.null(wrapper_ids)) {
      for (wrapper_id in wrapper_ids) {
        shinyjs::runjs(sprintf(
          "$('#%s').css({'border': '', 'border-radius': '', 'padding': ''});",
          wrapper_id
        ))
      }
    }
    message <- NULL
  }

  return(list(
    has_duplicates = has_duplicates,
    duplicate_column = duplicate_column,
    message = message
  ))
}

# Translate ggplot labels
translate_plot_labels <- function(plot, i18n, title = NULL, x = NULL, y = NULL, ...) {
  if (is.null(plot)) return(NULL)

  labels_list <- list()

  if (!is.null(title)) {
    labels_list$title <- i18n$t(title)
  }
  if (!is.null(x)) {
    labels_list$x <- i18n$t(x)
  }
  if (!is.null(y)) {
    labels_list$y <- i18n$t(y)
  }

  # Extra labels (subtitle, caption, fill, colour, etc.)
  extra_labels <- list(...)
  if (length(extra_labels) > 0) {
    for (label_name in names(extra_labels)) {
      labels_list[[label_name]] <- i18n$t(extra_labels[[label_name]])
    }
  }

  # Apply labels if any exist
  if (length(labels_list) > 0) {
    plot <- plot + ggplot2::labs(!!!labels_list)
  }

  return(plot)
}

# Function to customize the copy button of the buttons extension (DT package)
# Removes the popup and shows a green checkmark with translated "Copied!" message
copy_button_no_popup <- function(copy_label = "Copy", copied_label = "Copied!") {
  JS(sprintf("
      function (e, dt, node, config) {
        // Temporarily disable the info popup
        var oldInfo = dt.buttons.info;
        dt.buttons.info = function() {};

        // Perform copy without popup
        $.fn.dataTable.ext.buttons.copyHtml5.action.call(this, e, dt, node, config);

        // Restore the original info function
        dt.buttons.info = oldInfo;

        // Change button text to a green check mark
        $(node).html('<span style=\"color:green;\">✔ %s</span>');

        // Revert after 2 seconds
        setTimeout(function(){
          $(node).html('%s');
        }, 2000);
      }
    ",
             copied_label, copy_label
  ))
}

# Translate grob text elements (for doe_analyze EDA plots)
translate_grob_text <- function(grob_object, i18n, translations = NULL, wrap_width = 28) {
  if (is.null(grob_object) || !inherits(grob_object, "grob")) {
    return(grob_object)
  }

  # Helper function to wrap long text
  wrap_text <- function(text, width = wrap_width) {
    if (is.null(text) || is.na(text) || nchar(text) <= width) {
      return(text)
    }

    # Split by words
    words <- strsplit(text, " ")[[1]]
    if (length(words) <= 1) return(text)

    lines <- character()
    current_line <- words[1]

    for (i in 2:length(words)) {
      test_line <- paste(current_line, words[i])
      if (nchar(test_line) <= width) {
        current_line <- test_line
      } else {
        lines <- c(lines, current_line)
        current_line <- words[i]
      }
    }
    lines <- c(lines, current_line)

    return(paste(lines, collapse = "\n"))
  }

  # Default translation map for DOE EDA plots
  # Order matters: longer patterns first to avoid partial replacements
  if (is.null(translations)) {
    translations <- list(
      # Plot titles (longest first)
      "Normal Q-Q for " = i18n$t("Normal Q-Q for "),
      "Run Order Plot - " = i18n$t("Run Order Plot - "),
      "Histogram - " = i18n$t("Histogram - "),
      "Box Plot of " = i18n$t("Box Plot of "),
      # Axis labels
      "Theoretical Quantiles" = i18n$t("Theoretical Quantiles"),
      "Sample Quantiles" = i18n$t("Sample Quantiles"),
      "Actual Run Order" = i18n$t("Actual Run Order"),
      # Model labels
      "Initial Model," = i18n$t("Initial Model,"),
      "Final Model," = i18n$t("Final Model,"),
      # Other labels
      "Frequency" = i18n$t("Frequency"),
      "Arbitrary" = i18n$t("Arbitrary"),
      "Residual" = i18n$t("Residual")
    )
  }

  # Track which texts are titles (longer, need wrapping)
  title_patterns <- c("Normal Q-Q for ", "Run Order Plot - ", "Histogram - ", "Box Plot of ")

  # Recursive function to traverse grob tree
  translate_grob_recursive <- function(g) {
    if (inherits(g, "text")) {
      # Translate text elements
      if (!is.null(g$label)) {
        # label can be a character vector
        for (i in seq_along(g$label)) {
          original_text <- g$label[i]
          is_title <- FALSE

          # Try each translation pattern
          for (old_text in names(translations)) {
            if (grepl(old_text, original_text, fixed = TRUE)) {
              g$label[i] <- gsub(old_text, translations[[old_text]], original_text, fixed = TRUE)
              original_text <- g$label[i]  # Update for next pattern

              # Check if this is a title (contains title pattern)
              if (old_text %in% title_patterns) {
                is_title <- TRUE
              }
            }
          }

          # Apply word wrapping to titles if they're too long
          if (is_title) {
            if (nchar(g$label[i]) > wrap_width) {
              g$label[i] <- wrap_text(g$label[i], wrap_width)
            }

            # Reduce font size for wrapped titles to prevent overlap
            if (!is.null(g$gp) && !is.null(g$gp$fontsize)) {
              # Reduce to 90% of original size for titles
              g$gp$fontsize <- g$gp$fontsize * 0.9
            }
          }
        }
      }
    }

    # Recursively process children (used by regular grobs)
    if (!is.null(g$children)) {
      for (i in seq_along(g$children)) {
        g$children[[i]] <- translate_grob_recursive(g$children[[i]])
      }
    }

    # Process grobs (used by arrangeGrob from gridExtra)
    if (!is.null(g$grobs)) {
      for (i in seq_along(g$grobs)) {
        g$grobs[[i]] <- translate_grob_recursive(g$grobs[[i]])
      }
    }

    # Process childrenOrder if it exists
    if (!is.null(g$childrenOrder)) {
      for (child_name in g$childrenOrder) {
        if (!is.null(g$children[[child_name]])) {
          g$children[[child_name]] <- translate_grob_recursive(g$children[[child_name]])
        }
      }
    }

    return(g)
  }

  # Apply translation
  translated_grob <- translate_grob_recursive(grob_object)
  return(translated_grob)
}

# Null coalescing operator - use first non-NULL value
# Define globally so all modules can use it
`%||%` <- function(x, y) if (is.null(x)) y else x

# Helper: create numericInput with inline range badge (badge on right side)
# Consolidated version used across all server modules
#
# @param ns Shiny namespace function from session$ns
# @param i18n_r Reactive i18n function for translations
# @param input_id Input ID (without namespace)
# @param label_text Label text for the input
# @param value Current value
# @param min_val Minimum valid value (for badge display and optional validation)
# @param max_val Maximum valid value (for badge display and optional validation)
# @param step Step size (optional)
# @param range_text Custom text for the badge (optional, defaults to "min-max")
# @param tooltip Custom tooltip text (optional)
# @param help_content Help text content (optional)
# @param help_title Help popup title (optional, defaults to label_text)
# @param include_minmax Whether to include min/max attributes on the numericInput (default TRUE)
#
# @return A tags$div containing the labeled numericInput with range badge
range_badge_input <- function(ns, i18n_r, input_id, label_text, value, min_val, max_val,
                              step = NA, range_text = NULL, tooltip = NULL,
                              help_content = NULL, help_title = NULL, include_minmax = TRUE) {
  badge_text <- if (!is.null(range_text)) range_text else paste0(min_val, "\u2013", max_val)
  badge_title <- if (!is.null(tooltip)) tooltip else paste0(min_val, " \u2013 ", max_val)
  badge_color <- "#6c757d"  # Default gray
  if (!is.null(value) && !is.na(value) && (value < min_val || value > max_val)) {
    badge_color <- "#dc3545"  # Red for out-of-range
  }
  help_icon <- if (!is.null(help_content)) {
    input_help(help_content, title = help_title %||% label_text, buttonLabel = i18n_r()$t("OK"))
  } else NULL

  args <- list(inputId = ns(input_id), label = NULL, value = value)
  if (include_minmax) {
    args$min <- min_val
    args$max <- max_val
  }
  if (!is.na(step)) args$step <- step

  tags$div(
    tags$label(
      label_text,
      help_icon,
      class = "control-label",
      style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
      tags$span(
        badge_text,
        style = paste0("font-size: 10px; padding: 1px 5px; border-radius: 3px; background-color: ",
          badge_color, "; color: white; margin-left: auto; font-weight: normal;"),
        title = badge_title
      )
    ),
    do.call(numericInput, args)
  )
}

# Helper: create standard DT export buttons with copy, CSV, Excel, PDF
# Consolidated version to reduce code duplication across all server modules
#
# @param i18n_r Reactive i18n function for translations
# @param base_filename Base name for exported files (timestamp will be appended)
#
# @return A list of button configurations for DT::datatable
create_dt_export_buttons <- function(i18n_r, base_filename) {
  list(
    list(
      extend = "copy",
      text = i18n_r()$t("Copy"),
      titleAttr = i18n_r()$t("Copy"),
      action = copy_button_no_popup(
        copy_label = i18n_r()$t("Copy"),
        copied_label = i18n_r()$t("Copied!")
      )
    ),
    list(extend = "csv", filename = generate_filename_with_timestamp(base_filename)),
    list(extend = "excel", filename = generate_filename_with_timestamp(base_filename)),
    list(extend = "pdf", filename = generate_filename_with_timestamp(base_filename))
  )
}

# Helper: set outputOptions for multiple outputs at once
# Reduces repetitive outputOptions calls at the end of server modules
#
# @param output Shiny output object
# @param output_ids Character vector of output IDs
# @param suspendWhenHidden Whether to suspend output when hidden (default FALSE)
#
# @return NULL (called for side effects)
set_output_options <- function(output, output_ids, suspendWhenHidden = FALSE) {
  for (id in output_ids) {
    tryCatch({
      outputOptions(output, id, suspendWhenHidden = suspendWhenHidden)
    }, error = function(e) {
      # Silently ignore if output doesn't exist yet
    })
  }
  invisible(NULL)
}

# Helper: create accordion toggle button with translated tooltips
# Consolidated version for consistent accordion expand/collapse buttons
#
# @param ns Shiny namespace function
# @param i18n_r Reactive i18n function for translations
# @param accordion_id The ID of the accordion to toggle (without namespace)
# @param start_expanded If TRUE, panels start expanded (show collapse icon);
#                       if FALSE, panels start collapsed (show expand icon)
#
# @return A tags$button element
create_accordion_toggle_btn <- function(ns, i18n_r, accordion_id, start_expanded = TRUE) {
  if (start_expanded) {
    title <- i18n_r()$t("Collapse all")
    icon_name <- "compress-alt"
  } else {
    title <- i18n_r()$t("Expand all")
    icon_name <- "expand-alt"
  }
  tags$button(
    type = "button",
    class = "btn btn-box-tool",
    style = "padding: 2px 6px;",
    title = title,
    `data-title-expand` = i18n_r()$t("Expand all"),
    `data-title-collapse` = i18n_r()$t("Collapse all"),
    onclick = sprintf("toggleAllPanels('%s', this)", ns(accordion_id)),
    icon(icon_name)
  )
}

# Helper: create standardized help modal content
# Consolidates the repeated helper() call pattern for HELP outputs
#
# @param i18n_r Reactive i18n function for translations
# @param content_key The translation key for the help content (e.g., "module_help_en")
# @param size Modal size: "s", "m", "l" (default "l")
#
# @return A helper() element for use in renderUI
create_help_modal <- function(i18n_r, content_key, size = "l") {
  helper(
    shiny_tag = "",
    buttonLabel = i18n_r()$t("Okay"),
    content = i18n_r()$t(content_key),
    type = "markdown",
    size = size,
    style = "color:white; font-size:20px; vertical-align:middle ; margin-top: 0; margin-right: 3%;"
  )
}

# Helper: create standardized "Load Example" button
# Consolidates the repeated load example data button pattern
#
# @param ns Shiny namespace function
# @param i18n_r Reactive i18n function for translations
#
# @return An actionButton element
create_load_example_btn <- function(ns, i18n_r) {
  actionButton(
    ns("load_example_data"), i18n_r()$t("Load Example"),
    icon = icon("flask"),
    class = "btn btn-info btn-sm",
    style = "margin-bottom: 15px;",
    title = i18n_r()$t("Load example data")
  )
}

# Helper: create standardized "Estimate from data" action link
# Consolidates the repeated estimate link pattern
#
# @param ns Shiny namespace function
# @param i18n_r Reactive i18n function for translations
# @param link_id The input ID for the action link (without namespace)
#
# @return An actionLink element
create_estimate_link <- function(ns, i18n_r, link_id) {
  actionLink(
    ns(link_id), NULL,
    icon = icon("magic-wand-sparkles"),
    style = "color: #9b59b6; font-size: 13px; margin-left: auto;",
    title = i18n_r()$t("Estimate from data")
  )
}

# Helper: DT columnDefs for trimming trailing zeros
# Creates columnDefs that format numeric columns with specified decimal places,
# then strips trailing zeroes and orphan decimal point.
# Integers display as integers, 2.500 displays as 2.5, 2.000 displays as 2.
#
# @param col_idx_zero_based Numeric vector of 0-based column indices
# @param digits Number of decimal places (default 3)
#
# @return A list suitable for DT options columnDefs
trim_zeros_columndefs <- function(col_idx_zero_based, digits = 3) {
  col_idx <- col_idx_zero_based[!is.na(col_idx_zero_based)]
  if (length(col_idx) == 0) return(list())
  list(list(
    targets = as.integer(col_idx),
    render = DT::JS(
      "function(data, type, row) {",
      "  if (type !== 'display') return data;",
      "  if (data === null || data === undefined || data === '') return data;",
      "  var n = parseFloat(data);",
      "  if (isNaN(n)) return data;",
      sprintf("  var s = n.toFixed(%d).replace(/\\.?0+$/, '');", as.integer(digits)),
      "  return s;",
      "}"
    )
  ))
}

# Helper: create editable rhandsontable with dynamic height
# Creates a standardized editable rhandsontable with row headers, highlighting,
# column resizing, and context menu for row/column editing.
#
# @param data Data frame to display
# @param max_height Maximum table height in pixels (default 180)
# @param min_height Minimum table height in pixels (default 80)
# @param row_height Approximate height per row in pixels (default 25)
# @param header_height Height for header in pixels (default 30)
#
# @return An rhandsontable object or NULL if data is empty
create_editable_hot <- function(data, max_height = 180, min_height = 80,
                                 row_height = 25, header_height = 30) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  num_rows <- nrow(data)
  table_height <- min(max(header_height + num_rows * row_height, min_height), max_height)

  rhandsontable::rhandsontable(data, height = table_height, rowHeaders = TRUE, useTypes = FALSE) %>%
    rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
    rhandsontable::hot_cols(manualColumnResize = TRUE) %>%
    rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
}

