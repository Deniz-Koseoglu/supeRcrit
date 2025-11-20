solute_characterization_server <- function(input, output, session, defaults, i18n, sfe_rv) {
  ns <- session$ns

  # Reactive value to store solute information and GCM results


  # Render molecular plot
  #   output$molecular_plot <- renderPlot({
  #   grid::grid.raster(
  #     sfe_rv$gcm_results[["visres"]][[1]],
  #     width = 1,
  #     height = 1,
  #     interpolate = TRUE
  #   )
  # }, res = 150)  #

  # HSP Visualization
  observeEvent(input$show_hsp_vis, {
    req(sfe_rv$gcm_results$visres$HSP)
    showModal(modalDialog(
      title = i18n$t("HSP Visualization"),
      size = "xl",
      div(
        style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("hsp_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$hsp_fullsize_img <- renderImage({
    req(sfe_rv$gcm_results$visres$HSP)
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 1200, height = 1200, res = 300)
    grid::grid.raster(sfe_rv$gcm_results$visres$HSP,
      width = 1, height = 1,
      interpolate = TRUE
    )
    dev.off()
    list(
      src = outfile,
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "HSP Visualization"
    )
  }, deleteFile = TRUE)

  # Critical Parameters Visualization
  observeEvent(input$show_critical_vis, {
    req(sfe_rv$gcm_results$visres$Critical)
    showModal(modalDialog(
      title = i18n$t("Critical Parameters Visualization"),
      size = "xl",
      div(
        style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("critical_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$critical_fullsize_img <- renderImage({
    req(sfe_rv$gcm_results$visres$Critical)
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 1200, height = 1200, res = 300)
    grid::grid.raster(sfe_rv$gcm_results$visres$Critical,
      width = 1, height = 1,
      interpolate = TRUE
    )
    dev.off()
    list(
      src = outfile,
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Critical Parameters Visualization"
    )
  }, deleteFile = TRUE)

  # Tb Visualization
  observeEvent(input$show_tb_vis, {
    req(sfe_rv$gcm_results$visres$Tb)
    showModal(modalDialog(
      title = i18n$t("Tb Visualization"),
      size = "xl",
      div(
        style = "max-height: 85vh; overflow: auto; text-align: center;",
        imageOutput(ns("tb_fullsize_img"), width = "100%", height = "auto")
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  output$tb_fullsize_img <- renderImage({
    req(sfe_rv$gcm_results$visres$Tb)
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 1200, height = 1200, res = 300)
    grid::grid.raster(sfe_rv$gcm_results$visres$Tb,
      width = 1, height = 1,
      interpolate = TRUE
    )
    dev.off()
    list(
      src = outfile,
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Tb Visualization"
    )
  }, deleteFile = TRUE)


  # Render predicted parameters table
  output$predicted_params_table <- DT::renderDataTable({
    req(sfe_rv$gcm_results$pares)

    # Combine Critical and HSP parameters
    critical_df <- data.frame(
      Parameter = names(sfe_rv$gcm_results$pares$Critical),
      Value = unlist(sfe_rv$gcm_results$pares$Critical),
      Category = "Critical Parameters"
    )

    hsp_df <- data.frame(
      Parameter = names(sfe_rv$gcm_results$pares$HSP),
      Value = unlist(sfe_rv$gcm_results$pares$HSP),
      Category = "Hansen Solubility Parameters"
    )

    # Combine all parameters
    parameters_df <- rbind(critical_df, hsp_df)

    # Add VDW if available
    if (!is.null(sfe_rv$gcm_results$pares$VDW)) {
      vdw_df <- data.frame(
        Parameter = "VDW",
        Value = sfe_rv$gcm_results$pares$VDW,
        Category = "Van der Waals Volume"
      )
      parameters_df <- rbind(parameters_df, vdw_df)
    }

    DT::datatable(
      parameters_df,
      options = list(dom = "t", scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Render GCM comparison table
  output$gcm_comparison_table <- DT::renderDataTable({
    req(sfe_rv$gcm_comparison)

    DT::datatable(
      sfe_rv$gcm_comparison,
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Render group contributions table
  output$group_contributions_table <- renderUI({
    req(sfe_rv$gcm_results$pares$Contribs)

    contribs_list <- sfe_rv$gcm_results$pares$Contribs

    # Define display names for each contribution type
    display_names <- list(
      "Tb" = "Boiling Point (Tb) Contributions",
      "Critical" = "Critical Parameters Contributions",
      "HSP" = "Hansen Solubility Parameters (HSP) Contributions",
      "VDW" = "Van der Waals Volume Contributions"
    )

    # Generate a list of data tables, one for each contribution type
    table_outputs <- lapply(names(contribs_list), function(param_name) {
      df <- contribs_list[[param_name]]
      if (nrow(df) > 0) {
        output_id <- paste0("group_contrib_", param_name)
        output[[output_id]] <- DT::renderDataTable({
          DT::datatable(
            df,
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "pdf")
            ),
            extensions = "Buttons",
            rownames = FALSE
          )
        })
        tagList(
          h4(display_names[[param_name]] %||% param_name), # Use display name or fallback to param_name
          DT::dataTableOutput(ns(output_id))
        )
      }
    })

    # Return all tables wrapped in a tagList
    do.call(tagList, table_outputs)
  })



  # Render solute details
  output$solute_details <- renderUI({
    req(sfe_rv$gcm_results)

    ids <- sfe_rv$gcm_results$solute_data$IDs
    pares <- sfe_rv$gcm_results$pares

    tagList(
      fluidRow(
        column(
          width = 6,
            style = "border-right: 1px solid #ddd; padding-right: 15px;",
          h4("Molecular Information"),
          h5(strong("Name:"), ids[["Name"]]),
          h5(strong("CAS:"), ids[["CAS"]]),
          h5(strong("SMILES:"), ids[["SMILES"]]),
          h5(strong("InChI:"), ids[["InChI"]]),
          h5(strong("InChIKey:"), ids[["InChIKey"]])
        ),
        column(
          width = 6,
          h4("Molecular Properties"),
          h5(strong("Molecular Formula:"), ids[["MF"]]),
          h5(strong("Molecular Weight:"), paste(ids[["MW"]], "g/mol")),
          h5(strong("Atom Count:"), ids[["Atom_Count"]]),
          h5(strong("Hydrogen Count:"), ids[["H_Count"]]),
          h5(strong("Aromaticity:"), ids[["Aromaticity"]])
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 6,
            style = "border-right: 1px solid #ddd; padding-right: 15px;",
          h4("Critical Parameters"),
          tags$ul(
            tags$li(paste("Tb:", round(pares$Critical["Tb"], 2), "K")),
            tags$li(paste("Tc:", round(pares$Critical["Tc"], 2), "K")),
            tags$li(paste("Pc:", round(pares$Critical["Pc"], 2), "MPa")),
            tags$li(paste("Vc:", round(pares$Critical["Vc"], 2), "mL/mol"))
          )
        ),
        column(
          width = 6,
          h4("Hansen Solubility Parameters"),
          tags$ul(
            tags$li(paste("δD:", round(pares$HSP["dD"], 2), "MPa^0.5")),
            tags$li(paste("δP:", round(pares$HSP["dP"], 2), "MPa^0.5")),
            tags$li(paste("δHB:", round(pares$HSP["dHB"], 2), "MPa^0.5"))
          )
        )
      ),
      if (!is.null(pares$VDW)) {
        fluidRow(
          column(
            width = 12,
            h4("Van der Waals Volume"),
            h5(paste(round(pares$VDW, 6), "L/mol"))
          )
        )
      },
      br(),
      hr(),
    )
  })




  # Observe "Characterize Solute (GCM)" button click
  observeEvent(input$characterize_solute, {
    req(input$cas_input, input$name_input) # SMILES is now optional

    if (isTRUE(input$specify_smiles)) {
      req(input$smiles_input)
      solute_input <- c(
        SMILES = input$smiles_input,
        CAS = input$cas_input,
        Name = input$name_input
      )
    } else {
      solute_input <- c(
        CAS = input$cas_input,
        Name = input$name_input
      )
    }

    withProgress(message = i18n$t("Characterizing Solute (GCM)..."), {
      sfe_rv$gcm_results <- tryCatch(
        {
          # First get solute data
          sfe_rv$solute_data <- mol_find(solute_input)

          # Then characterize with GCM
          params <- list(
            solute = solute_input,
            simplicity = input$gcm_simplicity,
            gorder = input$gcm_gorder,
            hlight = TRUE, # Always highlight for visualization
            silent = TRUE
          )

          # Only add parameters if they have valid values (not "none" or empty)
          if (!is.na(input$gcm_tb) && input$gcm_tb != "none" && input$gcm_tb != "") {
            params$tb <- input$gcm_tb
          }
          if (!is.na(input$gcm_crit) && input$gcm_crit != "none" && input$gcm_crit != "") {
            params$crit <- input$gcm_crit
          }
          if (!is.na(input$gcm_hsp) && input$gcm_hsp != "none" && input$gcm_hsp != "") {
            params$hsp <- input$gcm_hsp
          }
          if (!is.na(input$gcm_vdw) && input$gcm_vdw != "none" && input$gcm_vdw != "") {
            params$vdw <- input$gcm_vdw
          }

          gcm_output <- do.call(est_gcm, params)
          
          # Save the calculation
          if (!is.null(gcm_output)) {
            inputs_to_save <- list(
              cas_input = input$cas_input,
              name_input = input$name_input,
              smiles_input = if(isTRUE(input$specify_smiles)) input$smiles_input else "",
              specify_smiles = isTRUE(input$specify_smiles),
              gcm_tb = if (!is.na(input$gcm_tb) && input$gcm_tb != "none") input$gcm_tb else "none",
              gcm_crit = if (!is.na(input$gcm_crit) && input$gcm_crit != "none") input$gcm_crit else "none",
              gcm_hsp = if (!is.na(input$gcm_hsp) && input$gcm_hsp != "none") input$gcm_hsp else "none",
              gcm_simplicity = input$gcm_simplicity,
              gcm_gorder = input$gcm_gorder
            )
            print("DEBUG: inputs_to_save object:")
            print(inputs_to_save)
            save_gcm_calculation(inputs_to_save)
            showNotification(i18n$t("GCM calculation saved."), type = "message")
          }
          
          gcm_output
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during GCM characterization:"), e$message), type = "error")
          return(NULL)
        }
      )
    })
  })

  observeEvent(input$specify_smiles, {
  if (isTRUE(input$specify_smiles)) {
    shinyjs::show("smiles_div")
  } else {
    shinyjs::hide("smiles_div")
  }
}, ignoreInit = FALSE)


  # Observe "Compare GCM" button click
  observeEvent(input$compare_gcm, {
    req(input$cas_input, input$name_input) # SMILES is now optional

    if (isTRUE(input$specify_smiles)) {
      req(input$smiles_input)
      solute_input <- c(
        SMILES = input$smiles_input,
        CAS = input$cas_input,
        Name = input$name_input
      )
    } else {
      solute_input <- c(
        CAS = input$cas_input,
        Name = input$name_input
      )
    }

    withProgress(message = i18n$t("Comparing GCM..."), {
      sfe_rv$gcm_comparison <- tryCatch(
        {
          compare_gcm(
            solute = solute_input,
            gorder = input$gcm_comp_gorder,
            simplicity = input$gcm_comp_simplicity
          )
        },
        error = function(e) {
          showNotification(paste(i18n$t("Error during GCM comparison:"), e$message), type = "error")
          return(NULL)
        }
      )
    })
  })

  # Intro Help Button Observer for SFE Solute Char
  observeEvent(input$sfe_solute_char_help, {
    introjs(session, options = intro_steps_sfe_sol_char(ns, i18n))
  })

   observeEvent(input$sfe_solute_char_comp_help, {
    introjs(session, options = intro_steps_sfe_sol_char_comp(ns, i18n))
  })
}
