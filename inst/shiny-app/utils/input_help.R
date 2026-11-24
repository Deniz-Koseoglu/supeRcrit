#' Create an inline help icon (filled blue circle with white ?)
#' that triggers a shinyhelper modal when clicked.
#'
#' Uses an inline SVG for crisp rendering at any size. The icon carries
#' shinyhelper data-modal-* attributes so observe_helpers() picks it up
#' automatically.
#'
#' All parameters are coerced to plain character strings to avoid
#' i18n span tags leaking into HTML data attributes.
#'
#' @param content Character or i18n tag. Help text (HTML allowed for inline).
#' @param title Character or i18n tag. Modal dialog title.
#' @param type "inline" or "markdown". Default "inline".
#' @param size "s", "m", or "l". Modal size. Default "s".
#' @param buttonLabel Character or i18n tag. OK button text. Default "OK".
#' @return A shiny tag (inline <span> wrapping an SVG)
input_help <- function(content, title = "", type = "inline", size = "s",
                       buttonLabel = "OK") {
  # Helper: extract plain text from i18n tags or shiny.tag objects.
  #
  # shiny.i18n (in JS mode) wraps a translatable string as
  #   <span class="i18n" data-key="<English source>">translation</span>
  # and translates it live on the client by swapping the span's TEXT. The help
  # text here is baked into data-modal-* attributes, which that DOM-text
  # translator cannot reach. So we store the English data-key (not the rendered
  # text) and let help_i18n.js re-translate the attributes to the active
  # language. Falling back to as.character() keeps plain (non-i18n) strings.
  to_text <- function(x) {
    if (inherits(x, "shiny.tag")) {
      cls <- x$attribs[["class"]]
      key <- x$attribs[["data-key"]]
      if (!is.null(key) && !is.null(cls) && grepl("i18n", cls, fixed = TRUE)) {
        return(as.character(key))
      }
      # Extract text children recursively
      paste(unlist(lapply(x$children, function(ch) {
        if (inherits(ch, "shiny.tag")) to_text(ch) else as.character(ch)
      })), collapse = "")
    } else {
      as.character(x)
    }
  }

  content_str <- to_text(content)
  title_str   <- to_text(title)
  button_str  <- to_text(buttonLabel)

  if (type == "inline") {
    content_str <- paste(content_str, collapse = "<br>")
  } else {
    content_str <- paste0(content_str, ".md")
  }

  tags$span(
    class = "shinyhelper-icon input-help-icon",
    `data-modal-size` = size,
    `data-modal-type` = type,
    `data-modal-title` = title_str,
    `data-modal-content` = content_str,
    `data-modal-label` = button_str,
    `data-modal-easyclose` = "TRUE",
    `data-modal-fade` = "FALSE",
    HTML('<svg width="16" height="16" viewBox="0 0 16 16" style="vertical-align: middle;"><circle cx="8" cy="8" r="8" fill="#3c8dbc"/><text x="8" y="12" text-anchor="middle" fill="white" font-size="11" font-weight="700" font-family="Georgia, serif">?</text></svg>')
  )
}

#' Attach a translation-safe NATIVE hover tooltip to a tag (no help icon).
#'
#' A `title=""` attribute cannot hold a shiny.i18n tag — `i18n$t()` returns a <span class="i18n"> which would
#' leak markup into the attribute, and the built-in text-node translator never touches `title`. Instead we
#' store the PLAIN English source string in both `title` (correct for the English key language, and a graceful
#' no-JS fallback) and `data-title-key`, and tag the element with class "i18n-tooltip". help_i18n.js then
#' rewrites `title` to the active language on load and on every language switch, so the browser tooltip
#' translates dynamically. The source string must also exist in the translation tables (www/i18n/*.json).
#'
#' @param tag A shiny tag (e.g. an actionButton) to receive the tooltip.
#' @param key Character. The PLAIN English source string (NOT i18n$t(...)).
#' @return The tag with the tooltip attributes + class appended.
with_tooltip <- function(tag, key) {
  key <- as.character(key)[1]
  tagAppendAttributes(tag, class = "i18n-tooltip", title = key, `data-title-key` = key)
}
