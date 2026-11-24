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

# callr::r_bg() wrapper that muffles one harmless, callr-internal warning. On the SECOND+ background run in
# a session, callr:::prepare_client_files() stages its processx client DLLs into <tempdir>/callr/<arch>/<hash>
# with dir.create(dirname(fn), recursive = TRUE) and NO showWarnings = FALSE, so it warns
# "'...\\callr\\x64\\...' already exists". The directory legitimately already exists; the run is unaffected.
# Muffle just that one "already exists" warning (raised synchronously while r_bg launches the process); every
# other warning passes through untouched. Use this instead of callr::r_bg() for the app's background workers.
r_bg_quiet <- function(...) {
  withCallingHandlers(
    callr::r_bg(...),
    warning = function(w) if (grepl("already exists", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning"))
}

# Convert a ggplot to an interactive plotly widget, silencing the two purely-cosmetic "feature not yet
# implemented in plotly" warnings that ggplotly() emits for things it cannot render — geom_label
# ("geom_GeomLabel() has yet to be implemented in plotly") and a theme aspect.ratio ("Aspect ratios aren't
# yet implemented..."). Both are plotly limitations with no user action possible: the label layer is dropped
# and the aspect ratio ignored (plotly sizes to its container). aspect.ratio is additionally stripped up front
# so plotly never sees it. A non-ggplot input (an already-built plotly widget / plotly_empty()) passes straight
# through. ALL other warnings propagate normally. Use in renderPlotly() outputs that return a ggplot.
quiet_ggplotly <- function(p, ...) {
  if (!inherits(p, "ggplot")) return(p)
  p$theme$aspect.ratio <- NULL
  withCallingHandlers(
    plotly::ggplotly(p, ...),
    warning = function(w) if (grepl("geom_GeomLabel|Aspect ratios aren't yet implemented",
      conditionMessage(w))) invokeRestart("muffleWarning"))
}

# Writable per-user data directory for app settings / saved items. The installed package
# directory is READ-ONLY in hosted deployments (shinyapps.io) and system-library installs,
# so user-generated files must live here instead. Uses the standard R user-data dir
# (tools::R_user_dir, persistent on desktop); falls back to a tempdir on locked-down hosts.
# `subdir` may be a nested path like "user-settings/eos_components"; the dir is created.
supercrit_user_dir <- function(subdir = NULL) {
  base <- tryCatch(tools::R_user_dir("supeRcrit", "data"), error = function(e) NA_character_)
  if (is.na(base) || !nzchar(base)) base <- file.path(tempdir(), "supeRcrit")
  path <- if (is.null(subdir) || !nzchar(subdir)) base else file.path(base, subdir)
  if (!dir.exists(path)) try(dir.create(path, recursive = TRUE, showWarnings = FALSE), silent = TRUE)
  path
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

# Multi-value (tag / selectize) numeric input WITH a range badge. Unlike the plain tag inputs, the
# badge itself turns RED when ANY entered value is out of [min_val, max_val] (not just the individual
# tags). Because the caller renders this inside a renderUI that reads the input's current value, the
# badge re-evaluates on every edit. `current_vals` is the character vector currently entered.
# Uniform "Selected Solutes" formatting across the app: a " (CAS)" suffix when a CAS number is present,
# NULL otherwise (so the bold name stands alone). Used with strong(name) in the selected-solutes lists.
solute_cas_suffix <- function(cas) {
  cas <- if (is.null(cas) || length(cas) < 1 || is.na(cas[1])) "" else trimws(as.character(cas[1]))
  if (!nzchar(cas)) NULL else paste0(" (", cas, ")")
}

range_badge_multi_input <- function(ns, i18n_r, input_id, label_text, current_vals, min_val, max_val,
                                    range_text = NULL, tooltip = NULL, placeholder = NULL,
                                    help_content = NULL, help_title = NULL, max_items = NULL,
                                    trailing = NULL, disabled = FALSE) {
  badge_text  <- if (!is.null(range_text)) range_text else paste0(min_val, "–", max_val)
  badge_title <- if (!is.null(tooltip)) tooltip else paste0(min_val, " – ", max_val)
  nums <- suppressWarnings(as.numeric(current_vals))
  any_oor <- any(is.finite(nums) & (nums < min_val | nums > max_val))
  badge_color <- if (isTRUE(any_oor)) "#dc3545" else "#6c757d"
  help_icon <- if (!is.null(help_content))
    input_help(help_content, title = if (!is.null(help_title)) help_title else label_text, buttonLabel = i18n_r()$t("OK")) else NULL
  render_js <- sprintf(paste0(
    "{ item: function(item, escape) { var val = parseFloat(item.value);",
    " var oor = isNaN(val) || val < %s || val > %s;",
    " var style = oor ? 'background-color:#dc3545;border-color:#dc3545;color:#fff;' : '';",
    " return '<div class=\"item\" style=\"' + style + '\">' + escape(item.value) + '</div>'; } }"),
    min_val, max_val)
  sel <- selectizeInput(ns(input_id), label = NULL, choices = current_vals, selected = current_vals,
      multiple = TRUE,
      options = c(list(create = TRUE, persist = FALSE,
        placeholder = if (!is.null(placeholder)) placeholder else "", render = I(render_js)),
        if (!is.null(max_items)) list(maxItems = max_items) else NULL))
  # When a trailing control shares the input's row, zero the input's form-group bottom margin so the
  # vertical centring lands on the visible box (not its margin) — robust, no hard-coded pixel offsets.
  if (!is.null(trailing)) sel <- htmltools::tagAppendAttributes(sel, style = "margin-bottom:0;")
  # `disabled` greys the input box (and blocks interaction) without touching any trailing control.
  sel_box <- if (isTRUE(disabled)) tags$div(style = "opacity:0.5; pointer-events:none;", sel) else sel
  label_tag <- tags$label(label_text, help_icon, class = "control-label",
    style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
    tags$span(badge_text, title = badge_title,
      style = paste0("font-size:10px; padding:1px 5px; border-radius:3px; background-color:",
        badge_color, "; color:white; margin-left:auto; font-weight:normal;")))
  if (is.null(trailing)) {
    tags$div(label_tag, sel_box)
  } else {
    # Label + input form one column so the BADGE aligns to the input field's right edge (not over the
    # trailing control). The trailing (e.g. an Optimize checkbox) sits in a second column that stretches to
    # the same height; a hidden spacer label matches the label row, and the checkbox is centred on the
    # input BOX below it. A bottom margin restores the normal inter-field spacing (the input's own was zeroed).
    tags$div(style = "display:flex; gap:10px; align-items:stretch; margin-bottom:15px;",
      tags$div(style = "flex:1; min-width:0;", label_tag, sel_box),
      tags$div(style = "flex:0 0 auto; white-space:nowrap; display:flex; flex-direction:column;",
        tags$label(HTML("&nbsp;"), class = "control-label", style = "visibility:hidden; display:block;"),
        tags$div(style = "flex:1; display:flex; align-items:center;", trailing)))
  }
}

# Helper: create standard DT export buttons with copy, CSV, Excel, PDF
# Consolidated version to reduce code duplication across all server modules
#
# @param i18n_r Reactive i18n function for translations
# @param base_filename Base name for exported files (timestamp will be appended)
#
# @return A list of button configurations for DT::datatable
create_dt_export_buttons <- function(i18n_r, base_filename, export_options = NULL) {
  copy_btn  <- list(
    extend = "copy",
    text = i18n_r()$t("Copy"),
    titleAttr = i18n_r()$t("Copy"),
    action = copy_button_no_popup(
      copy_label = i18n_r()$t("Copy"),
      copied_label = i18n_r()$t("Copied!")
    )
  )
  # bom = TRUE prepends the UTF-8 BOM so Excel reads non-ASCII (°C, γ, ∞, ≤, —, ●, ⚠, Cyrillic, …) correctly
  # instead of as CP1252 mojibake. Applies to the client-side CSV export used across every module.
  csv_btn   <- list(extend = "csv",   filename = generate_filename_with_timestamp(base_filename), bom = TRUE)
  excel_btn <- list(extend = "excel", filename = generate_filename_with_timestamp(base_filename))
  # PDF: pdfmake's default font (Roboto) lacks symbols (γ ∞ ≤ — ● ⚠, Greek/Cyrillic) and renders them as
  # boxes. Register the embedded Unicode font (window.__DEJAVU_VFS_B64__ from js/pdf_font.js) with pdfmake at
  # export time and use it; silently falls back to the default font if the global is unavailable.
  pdf_btn   <- list(extend = "pdf",   filename = generate_filename_with_timestamp(base_filename),
    customize = DT::JS(paste0(
      "function(doc){ try { if (window.pdfMake && window.__DEJAVU_VFS_B64__) {",
      " pdfMake.vfs = pdfMake.vfs || {};",
      " if (!pdfMake.vfs['DejaVuSans.ttf']) pdfMake.vfs['DejaVuSans.ttf'] = window.__DEJAVU_VFS_B64__;",
      " pdfMake.fonts = pdfMake.fonts || {};",
      " pdfMake.fonts.DejaVuSans = {normal:'DejaVuSans.ttf',bold:'DejaVuSans.ttf',italics:'DejaVuSans.ttf',bolditalics:'DejaVuSans.ttf'};",
      " doc.defaultStyle = doc.defaultStyle || {}; doc.defaultStyle.font = 'DejaVuSans'; } } catch(e){} }")))
  # Optional DataTables exportOptions (e.g. list(columns = ":visible") to drop hidden helper/flag columns).
  if (!is.null(export_options)) {
    copy_btn$exportOptions  <- export_options
    csv_btn$exportOptions   <- export_options
    excel_btn$exportOptions <- export_options
    pdf_btn$exportOptions   <- export_options
  }
  list(copy_btn, csv_btn, excel_btn, pdf_btn)
}

# Hang-proof replacement for shinyFiles::getVolumes()() — builds the shinyFiles directory-picker roots
# WITHOUT letting a slow drive stall app startup. Every stock enumeration method BLOCKS on this machine:
# shinyFiles shells out to `wmic logicaldisk get VolumeName`, and probing letters via dir.exists() or asking
# .NET's GetLogicalDrives() all stall for ~2-3 MINUTES on G:, which is a Google Drive (a cloud virtual drive
# whose first touch wakes the sync engine). Several module servers build their roots EAGERLY at init
# (volumes <- c(Home = ..., getVolumes()())), so that stall freezes server() before waiter_hide() runs and
# the app is stuck forever on the loading splash.
#
# Strategy: (1) env-derived roots (pure string ops, zero filesystem access) are ALWAYS included — the system
# drive, the home drive, and the drive the app runs from. (2) To also surface other LOCAL drives (e.g. D:)
# without risking the hang, a short-lived CHILD process probes A..Z IN ORDER and appends each existing letter
# as it finds it; the parent gives it a brief window, then kills it and harvests whatever it confirmed. Fast
# local drives are recorded before the probe reaches a slow cloud/virtual drive and hangs, so they're
# included and the slow one is simply skipped. The APP process never stats any drive. Cached per session.
.supercrit_vol_cache <- new.env(parent = emptyenv())
supercrit_get_volumes <- function() {
  if (.Platform$OS.type != "windows") return(c(Computer = "/"))
  if (!is.null(.supercrit_vol_cache$v)) return(.supercrit_vol_cache$v)

  as_root <- function(p) {
    p <- toupper(trimws(p))
    if (grepl("^[A-Z]:", p)) paste0(substr(p, 1, 2), "/") else NA_character_
  }
  drives <- stats::na.omit(c(
    as_root(Sys.getenv("SystemDrive")),   # usually C:
    as_root(Sys.getenv("HOMEDRIVE")),     # user's home drive
    as_root(getwd())                      # the drive the app is running from (e.g. the data/app drive)
  ))

  # Discover other local drive letters via a kill-after-timeout child probe (see note above). Any drive that
  # hangs (e.g. a Google Drive mount) is never reached in time and is simply left out — no stall > the budget.
  if (requireNamespace("callr", quietly = TRUE)) {
    extra <- tryCatch({
      tf <- tempfile(fileext = ".txt")
      p <- callr::r_bg(function(f) {
        for (L in LETTERS) if (dir.exists(paste0(L, ":/"))) cat(L, ":/\n", sep = "", file = f, append = TRUE)
      }, args = list(f = tf))
      p$wait(timeout = 2500)
      if (p$is_alive()) p$kill()
      if (file.exists(tf)) toupper(trimws(readLines(tf, warn = FALSE))) else character(0)
    }, error = function(e) character(0))
    drives <- c(drives, extra[nzchar(extra)])
  }

  drives <- sort(unique(drives))
  if (length(drives) == 0) drives <- "C:/"
  res <- stats::setNames(drives, sub("/$", "", drives))
  .supercrit_vol_cache$v <- res
  res
}

# Package-wide "Save Selected to Disk" for the solute / component lists. Wires a Save button
# (opts$trigger) to a directory-picker modal (text field + shinyFiles browse) and writes ONE JSON file
# per selected item, so the saved files round-trip through each module's existing multi-file "Load from
# Disk". `opts$collect()` returns a list of list(name = <base filename>, data = <list written as JSON>)
# for the CURRENT selection (NULL / empty list when nothing is selected). Call once per module server,
# alongside the "Remove Selected from Session" button. IDs in `opts` are un-namespaced (the module's
# `session$ns` namespaces them), so the same ids can be reused across modules without collision.
setup_save_selected_to_disk <- function(input, session, i18n_r, opts) {
  ns <- session$ns
  volumes <- c(Home = Sys.getenv("HOME"), supercrit_get_volumes())
  shinyFiles::shinyDirChoose(input, opts$browse, roots = volumes, session = session)
  observeEvent(input[[opts$browse]], {
    req(input[[opts$browse]])
    dp <- shinyFiles::parseDirPath(volumes, input[[opts$browse]])
    if (length(dp) > 0 && nzchar(dp)) updateTextInput(session, opts$dir_input, value = dp)
  })
  observeEvent(input[[opts$trigger]], {
    items <- opts$collect()
    if (is.null(items) || length(items) == 0) {
      showNotification(i18n_r()$t("Select one or more items in the list to save."), type = "warning"); return()
    }
    showModal(modalDialog(
      title = i18n_r()$t("Save Selected to Disk"), size = "m",
      div(class = "alert alert-info", icon("info-circle"), " ",
        sprintf(i18n_r()$t("Saving %d selected item(s) — one JSON file each."), length(items))),
      div(style = "display:flex; gap:10px; align-items:flex-end;",
        div(style = "flex-grow:1;", textInput(ns(opts$dir_input), i18n_r()$t("Save Directory"),
          value = opts$default_dir(), width = "100%")),
        div(style = "margin-bottom:15px;", shinyFiles::shinyDirButton(ns(opts$browse),
          label = i18n_r()$t("Browse..."), title = i18n_r()$t("Select Save Directory"), class = "btn-default"))),
      tags$small(class = "text-muted", i18n_r()$t("Leave empty to use the default package directory.")),
      footer = tagList(modalButton(i18n_r()$t("Cancel")),
        actionButton(ns(opts$confirm), i18n_r()$t("Save"), class = "btn btn-success", style = "color:white;"))))
  })
  observeEvent(input[[opts$confirm]], {
    removeModal()
    items <- opts$collect(); req(length(items) > 0)
    dir <- input[[opts$dir_input]]; if (is.null(dir) || !nzchar(trimws(dir %||% ""))) dir <- opts$default_dir()
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%d-%m-%Y_%Hhrs-%Mmin-%Ssec")
    okv <- vapply(seq_along(items), function(k) {
      it <- items[[k]]
      base <- gsub("[^a-zA-Z0-9_.-]", "_", it$name %||% "untitled"); if (!nzchar(base)) base <- "untitled"
      fn <- if (length(items) > 1) sprintf("%s_%s_%02d.json", base, stamp, k) else sprintf("%s_%s.json", base, stamp)
      isTRUE(tryCatch({ jsonlite::write_json(it$data, file.path(dir, fn), auto_unbox = TRUE, pretty = TRUE); TRUE },
        error = function(e) FALSE))
    }, logical(1))
    ok <- sum(okv); err <- sum(!okv)
    if (ok > 0) showNotification(sprintf(i18n_r()$t("Saved %d file(s) to: %s"), ok, dir), type = "message", duration = 5)
    if (err > 0) showNotification(sprintf(i18n_r()$t("%d file(s) could not be saved."), err), type = "error")
  })
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
create_estimate_link <- function(ns, i18n_r, link_id, title = NULL) {
  actionLink(
    ns(link_id), NULL,
    icon = icon("magic-wand-sparkles"),
    style = "color: #9b59b6; font-size: 13px; margin-left: auto;",
    title = if (is.null(title)) i18n_r()$t("Estimate from data") else title
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

