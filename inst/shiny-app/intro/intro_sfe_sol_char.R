# SFE Solute Characterization Application Intro Steps
intro_steps_sfe_sol_char <- function(ns, i18n) {
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
            element = paste0("#", ns("gcm_vdw_wrapper")),
            intro = i18n$t("Choose the method for Van der Waals volume calculation."),
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
        ),
        list(
            element = paste0("#", ns("characterize_solute_wrapper")),
            intro = i18n$t("Start the solute characterization using the selected GCM methods."),
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

# SFE Solute Characterization Application Intro Steps
intro_steps_sfe_sol_char_comp <- function(ns, i18n) {
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
            element = paste0("#", ns("gcm_comp_gorder_wrapper")),
            intro = i18n$t("Set the maximum group order for GCM method comparison."),
            position = "auto"
        ),
        list(
            element = paste0("#", ns("gcm_comp_simplicity_wrapper")),
            intro = i18n$t("Select fragmentation simplicity for the comparison."),
            position = "auto"
        ),
        list(
            element = paste0("#", ns("compare_gcm_wrapper")),
            intro = i18n$t("Compare different GCM methods for the solute characterization."),
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
