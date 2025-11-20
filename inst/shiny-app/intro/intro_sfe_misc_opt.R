

# SFE Miscibility Optimization Application Intro Steps
intro_steps_sfe_misc_opt_latest <- function(ns, i18n) {
  steps <- list(

       list(
      element = paste0("#", ns("load_selected_calculation_wrapper")),
      intro = i18n$t("Load a previously saved solute characterization for optimization."),
      position = "auto"
    )
  )

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

intro_steps_sfe_misc_opt_gcm <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("gcm_tb_wrapper")),
      intro = i18n$t("Select the method for estimating boiling point (Tb) in GCM calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gcm_crit_wrapper")),
      intro = i18n$t("Choose the method for calculating critical parameters."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gcm_hsp_wrapper")),
      intro = i18n$t("Select the method for Hansen Solubility Parameters (HSP) estimation."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gcm_simplicity_wrapper")),
      intro = i18n$t("Define the fragmentation complexity for molecular group contribution."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("gcm_gorder_wrapper")),
      intro = i18n$t("Set the maximum order for group contributions (0 for automatic)."),
      position = "auto"
    )
  )

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

intro_steps_sfe_misc_opt_solute <- function(ns, i18n) {
  steps <- list(
    
    list(
      element = paste0("#", ns("cas_input_wrapper")),
      intro = i18n$t("Enter the CAS number of the solute you want to characterize."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("name_input_wrapper")),
      intro = i18n$t("Provide the name of the solute."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("specify_smiles_wrapper")),
      intro = i18n$t("Enable to specify the molecular structure using SMILES notation."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("modifier_selection_wrapper")),
      intro = i18n$t("Select multiple co-solvents to optimize miscibility across different conditions."),
      position = "auto"
    )
  )

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

intro_steps_sfe_misc_opt_solvent_blend <- function(ns, i18n) {
  steps <- list(
    
    list(
      element = paste0("#", ns("blend_solvent_selector_wrapper")),
      intro = i18n$t("Choose a solvent to add to your custom blend for optimization."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("add_to_blend_wrapper")),
      intro = i18n$t("Add the selected solvent to your blend composition."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("clear_blend_wrapper")),
      intro = i18n$t("Clear the current blend composition."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("save_blend_wrapper")),
      intro = i18n$t("Save the current blend for future optimization runs."),
      position = "auto"
    )
  )

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
intro_steps_sfe_misc_opt_pressure <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("vfrac_input_wrapper")),
      intro = i18n$t("Set the co-solvent volume fraction percentage for optimization."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("pressure_min_wrapper")),
      intro = i18n$t("Define the minimum pressure range for optimization."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("pressure_max_wrapper")),
      intro = i18n$t("Define the maximum pressure range for optimization."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("temperature_min_wrapper")),
      intro = i18n$t("Set the minimum temperature range for optimization."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("temperature_max_wrapper")),
      intro = i18n$t("Set the maximum temperature range for optimization."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("optimize_miscibility_wrapper")),
      intro = i18n$t("Start the miscibility optimization across the selected parameter ranges."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("add_to_comparison_wrapper")),
      intro = i18n$t("Add the optimization results to comparison with other solutes."),
      position = "auto"
    )
  )

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
