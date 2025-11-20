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
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
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
