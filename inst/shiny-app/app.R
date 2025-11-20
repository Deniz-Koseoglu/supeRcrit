# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(shinydashboardPlus)
library(jsonlite)
library(shiny.i18n)
library(rhandsontable)
library(rintrojs)
library(shinyjs)
library(grid)
library(grDevices)
library(base64enc)
library(colourpicker)


Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

# Load default settings
app_config_path <- system.file("shiny-app", "config", "default-settings.json", package = "supeRcrit")

default_settings <- tryCatch(
  {
    jsonlite::read_json(app_config_path)
  },
  error = function(e) {
    message("An error occurred reading config: ", e$message)
    NULL
  }
)

# Convert NULL to NA recursively
convert_null_to_na <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  if (is.list(x)) {
    lapply(x, convert_null_to_na)
  } else {
    x
  }
}
default_settings <- lapply(default_settings, convert_null_to_na)

# Helper modules
source("utils/com_analysis_input_data.R")
source("utils/com_analysis_helpers.R")
source("utils/com_analysis_settings_manager.R")
source("utils/com_analysis_download.R")
source("utils/general_helpers.R")
source("utils/settings_utils.R")
source("utils/kinetic_helpers.R")
source("utils/button_state_helpers.R")
source("utils/saved_calculations_helpers.R")


# Cost analysis modules
source("ui_modules/com_analysis_ui.R")
source("server_modules/com_analysis_server.R")
source("ui_modules/com_analysis_settings_modal_ui.R")
source("server_modules/com_analysis_settings_modal_server.R")

# SFE Co-Solvent Selection modules
source("ui_modules/sfe_sol_char_ui.R")
source("server_modules/sfe_sol_char_server.R")

source("ui_modules/sfe_misc_comp_ui.R")
source("server_modules/sfe_misc_comp_server.R")

source("ui_modules/sfe_aux_tool_ui.R")
source("server_modules/sfe_aux_tool_server.R")

source("ui_modules/sfe_misc_opt_ui.R")
source("server_modules/sfe_misc_opt_server.R")

# DOE modules
source("ui_modules/doe_design_ui.R")
source("server_modules/doe_design_server.R")
source("ui_modules/doe_analysis_ui.R")
source("server_modules/doe_analysis_server.R")
source("ui_modules/doe_desir_ui.R")
source("server_modules/doe_desir_server.R")


# Kinetic modules
source("ui_modules/kinetic_tws_ui.R")
source("server_modules/kinetic_tws_server.R")
source("ui_modules/kinetic_bic_ui.R")
source("server_modules/kinetic_bic_server.R")



# Intro modules
source("intro/intro_general.R")
source("intro/intro_com_analysis.R")
source("intro/intro_doe_design.R")
source("intro/intro_doe_analysis.R")
source("intro/intro_kinetic_tws.R")
source("intro/intro_kinetic_bic.R")

source("intro/intro_sfe_sol_char.R")
source("intro/intro_sfe_misc_opt.R")
source("intro/intro_sfe_misc_comp.R")
source("intro/intro_sfe_aux_tool.R")



# Initialize translation
merge_translation_jsons(dir_path = "./www/i18n/", output_filename = "translation.json")
i18n <- Translator$new(translation_json_path = "./www/i18n/translation.json")
i18n$set_translation_language("en")

# Main UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "supeRcrit",
    tags$li(
      class = "dropdown",
      div(
        style = "display: flex; align-items: center;margin-top:15px;margin-right:10px;",
        actionButton("intro_general", i18n$t("Help"), icon = icon("question-circle"), class = "btn-info")
      )
    ),
    tags$li(
      class = "dropdown",
      div(
        style = "display: flex; align-items: center;margin-top:15px;margin-right:100px;",
        pickerInput(
          inputId = "selected_language",
          label = NULL,
          choices = setNames(i18n$get_languages(), c("English", "Русский", "Türkçe")), # Dil isimlerini görünür yapın
          selected = i18n$get_key_translation(),
          width = "100%",
          options = list(
            style = "btn-secondary",
            size = 5,
            `icon-base` = "fa"
          )
        )
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "main_menu",
      # DOE ana menüsü
      menuItem(
        text = textOutput("menu_doe", inline = TRUE),
        icon = icon("project-diagram"), tabName = "doe_main",
        menuSubItem(textOutput("menu_doe_design", inline = TRUE), tabName = "doe_design", icon = icon("project-diagram")),
        menuSubItem(textOutput("menu_doe_analysis", inline = TRUE), tabName = "doe_analysis", icon = icon("chart-line")),
        menuSubItem(textOutput("menu_doe_desir", inline = TRUE), tabName = "doe_desir", icon = icon("star"))
      ),
      # Kinetic Modeling ana menüsü
      menuItem(
        text = textOutput("menu_kinetic_modeling", inline = TRUE),
        icon = icon("flask"),
        tabName = "kinetic_main",
        menuSubItem(textOutput("menu_kinetic_tws", inline = TRUE), tabName = "kinetic_tws", icon = icon("wave-square")),
        menuSubItem(textOutput("menu_kinetic_bic", inline = TRUE), tabName = "kinetic_bic", icon = icon("atom"))
      ),
    
      # SFE Co-Solvent Selection ana menüsü
      menuItem(
        text = textOutput("menu_sfe_co_sel", inline = TRUE),
        icon = icon("flask"),
        tabName = "sfe_co_sel_main", # New tabName for the main SFE Co-Solvent Selection menu
        menuSubItem(textOutput("menu_solute_characterization", inline = TRUE), tabName = "solute_characterization", icon = icon("atom")),
        menuSubItem(textOutput("menu_miscibility_optimization", inline = TRUE), tabName = "miscibility_optimization", icon = icon("flask")),
        menuSubItem(textOutput("menu_miscibility_comparison", inline = TRUE), tabName = "miscibility_comparison", icon = icon("balance-scale")),
        menuSubItem(textOutput("menu_auxiliary_tools", inline = TRUE), tabName = "auxiliary_tools", icon = icon("toolbox"))
      ),
        # Basit menü öğeleri
      menuItem(textOutput("menu_cost_analysis", inline = TRUE), tabName = "com_analysis", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    shiny.i18n::usei18n(i18n),
    introjsUI(),
    useShinyjs(),

    # KaTeX CSS and JS
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css",
        integrity = "sha384-n8MVd4RsNIU0tAv4ct0nTaAbDJwPJzDEaqSD1odI+WdtXRGWt2kTvGFasHpSy3SV",
        crossorigin = "anonymous"
      ),
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js",
        integrity = "sha384-XjKyOOlGwcjNTAIQHIpgOno0Hl1YQqzUOEleOLALmuqehneUG+vnGctmUb0ZY0l8",
        crossorigin = "anonymous"
      )
    ),
    # Global JavaScript handler for custom messages
    tags$script("
      Shiny.addCustomMessageHandler('refreshSavedDesigns', function(message) {
        // Trigger refresh in DOE Analysis module
        Shiny.setInputValue('doe_analysis-refresh_designs', Math.random());
      });
    "),


    # KaTeX rendering for DataTables
    tags$script(HTML("
  // Render KaTeX after DataTable is drawn
  $(document).on('draw.dt', function(e, settings) {
    // Find all elements with $$ markers (KaTeX format)
    $('table').find('td, th').each(function() {
      var text = $(this).html();
      if(text && typeof text === 'string') {
        // Handle $$ delimited math
        if(text.indexOf('$$') !== -1) {
          var matches = text.match(/(\\$\\$[^\\$]+\\$\\$)/g);
          if(matches) {
            var newtext = text;
            for(var i = 0; i < matches.length; i++) {
              var code = matches[i].slice(2, -2); // Remove $$ delimiters
              try {
                var rendered = katex.renderToString(code, {throwOnError: false});
                newtext = newtext.replace(matches[i], rendered);
              } catch(e) {
                console.log('KaTeX error for ' + code + ':', e);
              }
            }
            $(this).html(newtext);
          }
        }
        // Keep existing %% support for backward compatibility
        if(text.indexOf('%%') !== -1) {
          var matches = text.match(/(%%[^%]+%%)/g);
          if(matches) {
            var newtext = text;
            for(var i = 0; i < matches.length; i++) {
              var code = matches[i].slice(2, -2);
              try {
                var rendered = katex.renderToString(code, {throwOnError: false});
                newtext = newtext.replace(matches[i], rendered);
              } catch(e) {
                console.log('KaTeX error for ' + code + ':', e);
              }
            }
            $(this).html(newtext);
          }
        }
      }
    });
  });

  // Also render on initial load
  $(document).ready(function() {
    setTimeout(function() {
      $('table').find('td, th').each(function() {
        var text = $(this).html();
        if(text && typeof text === 'string') {
          // Handle both $$ and %% formats
          if(text.indexOf('$$') !== -1) {
            var matches = text.match(/(\\$\\$[^\\$]+\\$\\$)/g);
            if(matches) {
              var newtext = text;
              for(var i = 0; i < matches.length; i++) {
                var code = matches[i].slice(2, -2);
                try {
                  var rendered = katex.renderToString(code, {throwOnError: false});
                  newtext = newtext.replace(matches[i], rendered);
                } catch(e) {
                  console.log('KaTeX error for ' + code + ':', e);
                }
              }
              $(this).html(newtext);
            }
          }
          if(text.indexOf('%%') !== -1) {
            var matches = text.match(/(%%[^%]+%%)/g);
            if(matches) {
              var newtext = text;
              for(var i = 0; i < matches.length; i++) {
                var code = matches[i].slice(2, -2);
                try {
                  var rendered = katex.renderToString(code, {throwOnError: false});
                  newtext = newtext.replace(matches[i], rendered);
                } catch(e) {
                  console.log('KaTeX error for ' + code + ':', e);
                }
              }
              $(this).html(newtext);
            }
          }
        }
      });
    }, 500);
  });
")),
    tabItems(
      tabItem(
        tabName = "com_analysis",
        com_analysis_ui("com_analysis", default_settings$com_analysis, i18n)
      ),
      tabItem(
        tabName = "doe_design",
        doe_design_ui("doe_design", default_settings$doe_design, i18n)
      ),
      tabItem(
        tabName = "doe_analysis",
        doe_analysis_ui("doe_analysis", default_settings$doe_analysis, i18n)
      ),
      tabItem(
        tabName = "doe_desir",
        doe_desir_ui("doe_desir", default_settings$doe_desir, i18n)
      ),
      tabItem(
        tabName = "kinetic_tws",
        kinetic_tws_ui("kinetic_tws", default_settings$kinetic_tws, i18n)
      ),
      tabItem(
        tabName = "kinetic_bic",
        kinetic_bic_ui("kinetic_bic", default_settings$kinetic_bic, i18n)
      ),
      tabItem(
        tabName = "solute_characterization",
        solute_characterization_ui("solute_characterization", default_settings$solute_characterization, i18n)
      ),
      tabItem(
        tabName = "miscibility_optimization",
        miscibility_optimization_ui("miscibility_optimization", default_settings$miscibility_optimization, i18n)
      ),
      tabItem(
        tabName = "miscibility_comparison",
        miscomp_ui("miscibility_comparison", default_settings$miscibility_comparison, i18n)
      ),
 
      tabItem(
        tabName = "auxiliary_tools",
        auxiliary_tools_ui("auxiliary_tools", default_settings$auxiliary_tools, i18n)
      )
    )
  )
)

# Main Server
server <- function(input, output, session) {
  withMathJax()
  shinyjs::useShinyjs()

  #
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
    i18n$set_translation_language(input$selected_language)
  })


  output$menu_cost_analysis <- renderText({
    i18n$t("Cost Analysis")
  })

  output$menu_doe_design <- renderText({
    i18n$t("DOE Design")
  })

  output$menu_doe_analysis <- renderText({
    i18n$t("DOE Analysis")
  })

  output$menu_doe <- renderText({
    i18n$t("Design of Experiments")
  })

  output$menu_doe_desir <- renderText({
    i18n$t("Desirability Function")
  })

  output$menu_kinetic_modeling <- renderText({
    i18n$t("Kinetic Modeling")
  })

  output$menu_kinetic_tws <- renderText({
    i18n$t("Two-Site Adsorption")
  })

  output$menu_kinetic_bic <- renderText({
    i18n$t("Broken-And-Intact Cell")
  })

  output$menu_sfe_co_sel <- renderText({
    i18n$t("SFE Co-Solvent Selection")
  })

  output$menu_solute_characterization <- renderText({
    i18n$t("Solute Characterization")
  })

  output$menu_miscibility_optimization <- renderText({
    i18n$t("Miscibility Optimization")
  })

  output$menu_miscibility_comparison <- renderText({
    i18n$t("Miscibility Comparison")
  })

  output$menu_auxiliary_tools <- renderText({
    i18n$t("Auxiliary Tools")
  })

  # General Intro
  observeEvent(input$intro_general, {
    introjs(session, options = intro_steps_general(NULL, i18n))
  })

  # Reactive Values for inter-module communication
  sfe_rv <- reactiveValues(
    solute_data = NULL,
    gcm_results = NULL,
    molecular_plot_data = NULL,
    comparison_list = list()
  )

  callModule(com_analysis_server, "com_analysis", default_settings$com_analysis, i18n)
  callModule(doe_design_server, "doe_design", default_settings$doe_design, i18n)
  callModule(doe_analysis_server, "doe_analysis", default_settings$doe_analysis, i18n)
  callModule(doe_desir_server, "doe_desir", default_settings$doe_desir, i18n)
  callModule(kinetic_tws_server, "kinetic_tws", default_settings$kinetic_tws, i18n)
  callModule(kinetic_bic_server, "kinetic_bic", default_settings$kinetic_bic, i18n)
   callModule(solute_characterization_server, "solute_characterization",
             default_settings$solute_characterization, i18n, sfe_rv) # Add sfe_rv argument
  callModule(miscibility_optimization_server, "miscibility_optimization",
             default_settings$miscibility_optimization, i18n, sfe_rv) # Use sfe_rv
  callModule(miscomp_server, "miscibility_comparison",
             default_settings$miscibility_comparison, i18n, sfe_rv) # Use sfe_rv
  callModule(auxiliary_tools_server, "auxiliary_tools",
             default_settings$auxiliary_tools, i18n) # Auxiliary tools might not need solute data, if so, omit sfe_rv.
}

# Launch the app
shinyApp(ui, server)
