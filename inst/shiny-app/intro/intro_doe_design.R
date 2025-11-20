# DOE Design Application Intro Steps (Adaptive)
intro_steps_doe_design <- function(ns, i18n, design_type = "bbd", n_factors = 3) {
  # Helper function to check if element exists in DOM
  element_exists <- function(selector) {
    # This will be checked client-side by intro.js
    # For now, we'll assume elements exist based on our logic
    TRUE
  }

  # Base steps that are always shown
  steps <- list(
    list(
      element = paste0("#", ns("design_type")),
      intro = i18n$t("Select the type of experimental design you want to create. Each design type has different characteristics and requirements."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("factors")),
      intro = sprintf(i18n$t("Specify the number of factors (variables) in your experiment. Current selection: %d factors."), n_factors),
      position = "auto"
    )
  )

  # Add center points input (except for Taguchi Method)
  if (design_type != "tm") {
    center_points_step <- list(
      element = paste0("#", ns("center_points_input")),
      intro = i18n$t("Set the number of additional center points. Center points help estimate experimental error and check for curvature."),
      position = "auto"
    )
    steps <- c(steps, list(center_points_step))
  }

  # Add randomize option
  steps <- c(steps, list(
    list(
      element = paste0("#", ns("randomize")),
      intro = i18n$t("Enable randomization of run order to minimize systematic bias and confounding effects."),
      position = "auto"
    )
  ))

  # Add design-specific parameters based on design type
  design_specific_step <- switch(design_type,
    "ccd" = list(
      element = paste0("#", ns("design_specific_params")),
      intro = i18n$t("Choose between Circumscribed (CCC) or Face-Centered (CCF) CCD. CCC extends star points beyond the factor range, CCF places them at the faces."),
      position = "auto"
    ),
    "ffd" = list(
      element = paste0("#", ns("design_specific_params")),
      intro = i18n$t("Set the number of levels for each factor and choose whether to use default center points."),
      position = "auto"
    ),
    "frfd" = list(
      element = paste0("#", ns("design_specific_params")),
      intro = i18n$t("Specify the fraction size (p) and aliasing structure for the fractional factorial design."),
      position = "auto"
    ),
    "tm" = list(
      element = paste0("#", ns("design_specific_params")),
      intro = i18n$t("Set the number of levels for the Taguchi orthogonal array design."),
      position = "auto"
    ),
    NULL # BBD has no additional parameters
  )

  if (!is.null(design_specific_step)) {
    steps <- c(steps, list(design_specific_step))
  }

  # Add factor inputs step
  factor_inputs_step <- list(
    element = paste0("#", ns("factor_inputs")),
    intro = sprintf(i18n$t("Define names and value ranges for each of your %d factors. These will be used to create the experimental design matrix."), n_factors),
    position = "auto"
  )
  
  steps <- c(steps, list(factor_inputs_step))

  # Add design requirements step (if visible)
  design_req_step <- list(
    element = paste0("#", ns("design_requirements")),
    intro = i18n$t("Review any specific requirements or information for your selected design type and factor configuration."),
    position = "auto"
  )
  steps <- c(steps, list(design_req_step))

  # Add action buttons
  action_steps <- list(
    list(
      element = paste0("#", ns("reset")),
      intro = i18n$t("Reset all parameters to their default values."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("calculate")),
      intro = i18n$t("Generate the experimental design matrix based on your specifications."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("save_design")),
      intro = i18n$t("Save your design configuration and results for later use."),
      position = "auto"
    )
  )
  steps <- c(steps, action_steps)

  list(
    steps = steps,
    "nextLabel" = i18n$t("Next"),
    "prevLabel" = i18n$t("Back"),
    "doneLabel" = i18n$t("Done"),
    "skipLabel" = i18n$t("Skip"),
    "showProgress" = TRUE,
    "showBullets" = FALSE
  )

}
