
setup_button_validator <- function(input, session, button_id, conditions, initial_state = "disabled", ns = NULL) {
  # Ensure shinyjs is available
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop("shinyjs package is required for button validation. Install with: install.packages('shinyjs')")
  }

  # Use namespace if provided
  # full_button_id <- if (!is.null(ns)) ns(button_id) else button_id
  if (!is.null(ns)) {
    full_button_id <- ns(button_id)
  } else if (!is.null(session$ns)) {
    full_button_id <- session$ns(button_id)
  } else {
    full_button_id <- button_id
  }

  # Reactive to track if all conditions are met
    is_valid <- reactive({
    all_conditions_met <- TRUE
    
    for (cond_name in names(conditions)) {
      cond <- conditions[[cond_name]]
      
      # Get input ID - use session$ns if available and ns not provided
      if (!is.null(ns)) {
        input_id <- ns(cond$input)
      } else if (!is.null(session$ns)) {
        input_id <- session$ns(cond$input)
      } else {
        input_id <- cond$input
      }
      
      # Get input value
      input_value <- input[[input_id]]
      
      # Check conditional application
      if (!is.null(cond$when)) {
        if (!cond$when()) next  # Skip if condition not active
      }
      
      # Apply validation rule
      valid <- switch(cond$rule,
        "not_empty" = !is.null(input_value) && input_value != "" && length(input_value) > 0,
        "selected" = !is.null(input_value) && input_value != "" && input_value != "Choose...",
        "numeric" = !is.na(suppressWarnings(as.numeric(input_value))),
        "positive" = {
          num_val <- suppressWarnings(as.numeric(input_value))
          !is.na(num_val) && num_val > 0
        },
        TRUE  # Default to true if unknown rule
      )
      
      all_conditions_met <- all_conditions_met && valid
      if (!all_conditions_met) break  # Early exit for efficiency
    }
    
    all_conditions_met
  })

  # Observe changes and update button state
  observeEvent(is_valid(),
    {
      if (is_valid()) {
        shinyjs::enable(full_button_id)
        shinyjs::removeClass(full_button_id, "btn-disabled")
      } else {
        shinyjs::disable(full_button_id)
        shinyjs::addClass(full_button_id, "btn-disabled")
      }
    },
    ignoreNULL = FALSE
  )

  # Set initial state
  if (initial_state == "disabled") {
    shinyjs::disable(full_button_id)
    shinyjs::addClass(full_button_id, "btn-disabled")
  } else {
    shinyjs::enable(full_button_id)
    shinyjs::removeClass(full_button_id, "btn-disabled")
  }

  # Return the validation reactive for external use if needed
  list(
    is_valid = is_valid,
    button_id = full_button_id
  )
}

#' Add Validation Rule
#'
#' Dynamically add a new validation rule to an existing button validator.
#'
#' @param validator_result Result from setup_button_validator
#' @param new_condition New condition to add (same format as in setup_button_validator)
#'
#' @examples
#' validator <- setup_button_validator(...)
#' add_validation_rule(validator, list(extra_field = list(input = "extra", rule = "not_empty")))
#'
#' @export
add_validation_rule <- function(validator_result, new_condition) {
  # This would require more complex reactive management; for now, suggest recreating validator
  warning("Dynamic rule addition not fully implemented. Recreate validator with updated conditions.")
}

#' Get Button State
#'
#' Returns current state of a button validator.
#'
#' @param validator_result Result from setup_button_validator
#'
#' @return Logical: TRUE if valid/enabled
#'
#' @export
get_button_state <- function(validator_result) {
  validator_result$is_valid()
}
