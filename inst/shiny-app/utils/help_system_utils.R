# Custom Help System Utilities
# Mimics shinyhelper functionality without external dependencies

#' Read help markdown file and convert to HTML
#' @param help_id Character string identifying the help topic (e.g., "com_analysis-volex")
#' @param help_dir Directory containing help markdown files (default: "help_mds")
#' @return HTML string with rendered markdown content
read_help_content <- function(help_id, help_dir = "help_mds") {
  tryCatch({
    # Try system.file() first (for installed package)
    help_file <- system.file("shiny-app", help_dir, paste0(help_id, ".md"),
                            package = "supeRcrit")

    # If not found in package, try local path (for development)
    if (help_file == "" || !file.exists(help_file)) {
      help_file <- file.path(help_dir, paste0(help_id, ".md"))
    }

    # Check if file exists
    if (!file.exists(help_file)) {
      return(paste0("<div style='color: red; padding: 20px;'><strong>Help file not found:</strong> ",
                   help_id, ".md</div>"))
    }

    # Read markdown file
    md_text <- paste(readLines(help_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

    # Convert markdown to HTML, preserving math delimiters
    # First protect math expressions
    md_text <- gsub("\\$\\$", "DOUBLEDOLLAR", md_text, fixed = TRUE)
    md_text <- gsub("\\$", "SINGLEDOLLAR", md_text, fixed = TRUE)

    # Convert markdown to HTML
    html_content <- markdown::markdownToHTML(
      text = md_text,
      fragment.only = TRUE,
      options = c("use_xhtml", "smartypants")
    )

    # Restore math delimiters
    html_content <- gsub("DOUBLEDOLLAR", "$$", html_content, fixed = TRUE)
    html_content <- gsub("SINGLEDOLLAR", "$", html_content, fixed = TRUE)

    # Return HTML wrapped in a styled container
    return(paste0("<div class='help-content' style='padding: 15px;'>", html_content, "</div>"))

  }, error = function(e) {
    return(paste0("<div style='color: red; padding: 20px;'><strong>Error reading help file:</strong> ",
                 e$message, "</div>"))
  })
}

#' Create help icon wrapper for Shiny inputs
#' Custom implementation - use 'custom_help_topic_trigger' to avoid conflicts
#' @param shiny_tag A Shiny input element
#' @param type Type of help content (default: "markdown")
#' @param content Content identifier (markdown filename without extension)
#' @param icon Font Awesome icon name (default: "question-circle")
#' @param colour Icon color (optional)
#' @param size Modal size: "s", "m", "l" (default: "m")
#' @return tagList with input and help icon
custom_helper <- function(shiny_tag, type = "markdown", content, icon = "question-circle",
                          colour = NULL, size = "m") {

  # Generate unique ID for the help icon
  help_id <- paste0("custom_help_icon_", gsub("[^a-zA-Z0-9]", "_", content))

  # Icon color styling
  icon_style <- if (!is.null(colour)) {
    paste0("color: ", colour, "; cursor: pointer; margin-left: 5px;")
  } else {
    "cursor: pointer; margin-left: 5px; color: #5cb85c;"  # Green to differentiate
  }

  # Create help icon button
  help_icon <- tags$span(
    id = help_id,
    icon(icon),
    style = icon_style,
    onclick = sprintf(
      "Shiny.setInputValue('custom_help_topic_trigger', '%s', {priority: 'event'});",
      content
    ),
    title = "Click for detailed help"
  )

  # Return input with help icon
  tagList(
    div(
      style = "display: inline-flex; align-items: center; width: 100%;",
      div(style = "flex-grow: 1;", shiny_tag),
      help_icon
    )
  )
}

# Keep helper() as alias for backward compatibility
helper <- custom_helper
