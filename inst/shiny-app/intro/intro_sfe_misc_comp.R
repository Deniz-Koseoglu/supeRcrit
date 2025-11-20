# SFE Miscibility Comparison Application Intro Steps

# Intro for loading saved calculations
intro_steps_sfe_misc_comp_load <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("load_selected_calculations_wrapper")),
      intro = i18n$t("Load previously saved solute characterizations for comparison."),
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

# Intro for manual solute entry
intro_steps_sfe_misc_comp_manual <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("manual_cas_wrapper")),
      intro = i18n$t("Enter the CAS number of the solute you want to characterize."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("manual_name_wrapper")),
      intro = i18n$t("Provide the name of the solute."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("manual_smiles_wrapper")),
      intro = i18n$t("Enter the SMILES string to specify the molecular structure."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("add_manual_solute_wrapper")),
      intro = i18n$t("Add the manually entered solute to the comparison list."),
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

# Intro for GCM methods
intro_steps_sfe_misc_comp_gcm <- function(ns, i18n) {
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

# Intro for co-solvent selection and blends
intro_steps_sfe_misc_comp_cosolvent_blend <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("modifier_selection_wrapper")),
      intro = i18n$t("Select the co-solvent to use for miscibility comparison."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("blend_solvent_selector_wrapper")),
      intro = i18n$t("Choose a solvent to add to your custom blend."),
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
      intro = i18n$t("Save the current blend for future use."),
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

# Intro for process conditions
intro_steps_sfe_misc_comp_process <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("pres_input_wrapper")),
      intro = i18n$t("Enter pressure values (bar) separated by commas for the comparison."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("pres_comp_wrapper")),
      intro = i18n$t("Select the reference pressure for comparison calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("temp_wrapper")),
      intro = i18n$t("Set the temperature for the miscibility calculations."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("vfrac_input_wrapper")),
      intro = i18n$t("Enter volume fractions separated by commas for the co-solvent."),
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

# Intro for advanced plot options
intro_steps_sfe_misc_comp_plot <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("plt_title_wrapper")),
      intro = i18n$t("Toggle to show or hide plot titles in the results."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("use_custom_colors_wrapper")),
      intro = i18n$t("Enable custom color selection for plots."),
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

# Intro for run button
intro_steps_sfe_misc_comp_run <- function(ns, i18n) {
  steps <- list(
    list(
      element = paste0("#", ns("run_miscomp_wrapper")),
      intro = i18n$t("Start the miscibility comparison calculation with your selected parameters."),
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
