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
library(shinyhelper)
library(waiter)
library(shinyalert)


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
source("utils/com_analysis_download.R")
source("utils/general_helpers.R")
source("utils/settings_utils.R")
source("utils/kinetic_helpers.R")
source("utils/button_state_helpers.R")
source("utils/saved_calculations_helpers.R")
source("utils/doe_seed_data_helpers.R")
source("utils/input_help.R")
source("utils/sfe_seed_data_helpers.R")

# Global settings manager
source("utils/global_settings_manager.R")


# Global settings modal
source("ui_modules/global_settings_modal_ui.R")
source("server_modules/global_settings_modal_server.R")

# Cost analysis modules
source("ui_modules/com_analysis_ui.R")
source("server_modules/com_analysis_server.R")

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
source("ui_modules/kinetic_aux_tool_ui.R")
source("server_modules/kinetic_aux_tool_server.R")



# Intro modules
source("intro/intro_general.R")




# Initialize translation
merge_translation_jsons(dir_path = "./www/i18n/", output_filename = "translation.json")
i18n <- Translator$new(translation_json_path = "./www/i18n/translation.json")
i18n$set_translation_language("en")

# Main UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = tags$a(
      href = "#", onclick = "Shiny.setInputValue('show_about', Math.random())",
      style = "color: inherit; text-decoration: none; cursor: pointer;",
      "supeRcrit"
    ),
     tags$li(
      class = "dropdown",
      div(
        style = "display: inline-block; /* div'i satır içi blok yapar */
                 background-color: rgba(255, 255, 255, 0.8); /* Yarı saydam beyaz veya header rengine yakın bir renk */
                 padding: 5px 10px; /* Kenarlara biraz boşluk ekler */
                 border-radius: 8px; /* Köşeleri yumuşatır */
                 margin-top: 7px; margin-right: 5px; /* Konumlandırma */
                 vertical-align: middle;
                 /*box-shadow: 0 0 5px rgba(0, 0, 0, 0.2); Hafif bir gölge ekler */",
        tags$a(
          href = "https://www.altraflora.com",
          target = "_blank",
          tags$img(
            src = "altraflora-15-min.png",
            style = "max-height:200px; max-width:200px; display: inline-block; vertical-align: middle;"
            # Resme artık box-shadow veya border-radius uygulamaya gerek yok
          ),
          class = "dropdown-toggle"
          # Eğer linkin kendi text rengi varsa, onu şeffaf veya aynı renk yapabilirsiniz
          # style = "color: transparent;" # Linkin altında bir metin olmadığından gerek olmayabilir
        )
      )
    ),
    tags$li(
      class = "dropdown",
      div(
        style = "display: inline-block; float: left;margin-top:15px;margin-right:10px;",
        actionButton("intro_general", i18n$t("Intro"), icon = icon("info-circle"), class = "btn-secondary")
      )
    ),
    tags$li(
      class = "dropdown",
      div(
        style = "display: inline-block; float: left;margin-top:15px;margin-right:10px;",
        uiOutput("settings_button_ui")
      )
    ),
    tags$li(
      class = "dropdown",
      div(
        id = "language_picker_wrapper",
        style = "display: inline-block; float: left;margin-top:15px;margin-right:5px;",
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

      # Cost Analysis
      menuItem(textOutput("menu_cost_analysis", inline = TRUE), tabName = "com_analysis", icon = icon("calculator")),
      # SFE Co-Solvent Selection
      menuItem(
        text = textOutput("menu_sfe_co_sel", inline = TRUE),
        icon = icon("flask"),
        tabName = "sfe_co_sel_main", # New tabName for the main SFE Co-Solvent Selection menu
        menuSubItem(textOutput("menu_solute_characterization", inline = TRUE), tabName = "solute_characterization", icon = icon("atom")),
        menuSubItem(textOutput("menu_miscibility_optimization", inline = TRUE), tabName = "miscibility_optimization", icon = icon("flask")),
        menuSubItem(textOutput("menu_miscibility_comparison", inline = TRUE), tabName = "miscibility_comparison", icon = icon("balance-scale")),
        menuSubItem(textOutput("menu_auxiliary_tools", inline = TRUE), tabName = "auxiliary_tools", icon = icon("toolbox"))
      ),
      # DOE
      menuItem(
        text = textOutput("menu_doe", inline = TRUE),
        icon = icon("project-diagram"), tabName = "doe_main",
        menuSubItem(textOutput("menu_doe_design", inline = TRUE), tabName = "doe_design", icon = icon("project-diagram")),
        menuSubItem(textOutput("menu_doe_analysis", inline = TRUE), tabName = "doe_analysis", icon = icon("chart-line")),
        menuSubItem(textOutput("menu_doe_desir", inline = TRUE), tabName = "doe_desir", icon = icon("star"))
      ),
      # Kinetic Modeling
      menuItem(
        text = textOutput("menu_kinetic_modeling", inline = TRUE),
        icon = icon("flask"),
        tabName = "kinetic_main",
        menuSubItem(textOutput("menu_kinetic_tws", inline = TRUE), tabName = "kinetic_tws", icon = icon("wave-square")),
        menuSubItem(uiOutput("menu_kinetic_bic", inline = TRUE), tabName = "kinetic_bic", icon = icon("atom")),
        menuSubItem(textOutput("menu_kinetic_aux_tool", inline = TRUE), tabName = "kinetic_aux_tool", icon = icon("calculator"))
      )
    )
  ),
  dashboardBody(
    shiny.i18n::usei18n(i18n),
    introjsUI(),
    useShinyjs(),
    use_waiter(),
    waiter_show_on_load(
      html = tagList(
        div(style = "text-align:center;color:white",
            tags$h2(i18n$t("supeRcrit is loading...")),
            waiter::spin_ellipsis()
        )
      ),
      color = "#00a65a"
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fileInput.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/disabled_tabs.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/box_header_help.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/tooltip_cursor.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/input_helper.css"),
      tags$script(src = "js/global_settings_manager.js"),
      tags$script(src = "js/tab_controls.js"),
      tags$script(src = "js/accordion_expand_collapse.js")
    ),
    
    


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
      ),
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar_menu.css")
    ),
    # Detect browser language and send to Shiny
    tags$script("
      $(document).on('shiny:connected', function(event) {
        var browserLang = navigator.language || navigator.userLanguage;
        var langCode = browserLang.toLowerCase().split('-')[0];

        // Map browser language to supported languages
        var supportedLangs = {
          'en': 'en',
          'ru': 'ru',
          'tr': 'tr'
        };

        // Default to English if language not supported
        var selectedLang = supportedLangs[langCode] || 'en';

        Shiny.setInputValue('browser_language', selectedLang);
      });
    "),
    # Global JavaScript handler for custom messages
    tags$script("
      Shiny.addCustomMessageHandler('refreshSavedDesigns', function(message) {
        // Trigger refresh in DOE Analysis module
        Shiny.setInputValue('doe_analysis-refresh_designs', Math.random());
      });
      Shiny.addCustomMessageHandler('toggleField', function(message) {
        var el = document.getElementById(message.id);
        if(el) {
          if(message.disabled) {
            el.dataset.savedValue = el.value;
            el.value = '';
            el.disabled = true;
            el.style.backgroundColor = '#e9ecef';
            el.placeholder = message.placeholder || '';
            el.type = 'text';
          } else {
            if(el.type === 'text') el.type = 'number';
            el.disabled = false;
            el.style.backgroundColor = '';
            el.placeholder = '';
            if(el.dataset.savedValue && !el.value) {
              el.value = el.dataset.savedValue;
              $(el).trigger('change');
            }
          }
        }
      });
      Shiny.addCustomMessageHandler('updateFlowLabel', function(msg) {
        var wrapper = document.getElementById(msg.wrapper_id);
        if(!wrapper) return;
        var label = wrapper.querySelector('label.control-label span');
        if(label) {
          var helpIcon = label.querySelector('.shinyhelper-container, [data-modal-size]');
          var helpHtml = helpIcon ? helpIcon.outerHTML : '';
          label.innerHTML = msg.label + ' ' + helpHtml;
        }
        var btn = document.getElementById(msg.btn_id);
        if(btn) {
          btn.textContent = msg.badge_text;
          btn.style.backgroundColor = msg.badge_color;
        }
      });
      Shiny.addCustomMessageHandler('switchLoadToFeed', function(msg) {
        var wrapper = document.getElementById(msg.wrapper_id);
        if(!wrapper) return;
        var label = wrapper.querySelector('label.control-label');
        if(!label) return;
        var helpIcon = label.querySelector('.shinyhelper-container, [data-modal-size]');
        var helpHtml = helpIcon ? helpIcon.outerHTML : '';
        var badge = '<button id=\"' + msg.btn_id + '\" type=\"button\" class=\"btn btn-default action-button btn-xs\" ' +
          'style=\"font-size:10px;padding:1px 5px;border-radius:3px;background-color:' + msg.badge_color +
          ';color:white;margin-left:auto;font-weight:normal;border:none;\">' + msg.badge_text + '</button>';
        label.innerHTML = '<span>' + msg.label + ' ' + helpHtml + '</span>' + badge;
        label.style.display = 'flex';
        label.style.alignItems = 'center';
        label.style.justifyContent = 'space-between';
        label.style.width = '100%';
      });
      Shiny.addCustomMessageHandler('switchLoadToLoad', function(msg) {
        var wrapper = document.getElementById(msg.wrapper_id);
        if(!wrapper) return;
        var label = wrapper.querySelector('label.control-label');
        if(!label) return;
        var helpIcon = label.querySelector('.shinyhelper-container, [data-modal-size]');
        var helpHtml = helpIcon ? helpIcon.outerHTML : '';
        label.innerHTML = '<span>' + msg.label + ' ' + helpHtml + '</span>';
        label.style.display = '';
        label.style.justifyContent = '';
      });
    "),
    tags$head(
  tags$style(HTML("
    /* shinyWidgets checkboxGroupButtons vertical fix */
    .sw-dropdown-content .btn-group-vertical,
    .btn-group-container-sw .btn-group {
      display: block !important;
          text-align: left !important;
      padding-left: 15px !important;
    }
    .sw-dropdown-content .btn-group-vertical > .btn,
    .btn-group-container-sw .btn-group > .btn,
    .btn-group-container-sw .btn-group > label.btn {
      display: block !important;
      width: 100% !important;
      float: none !important;
          text-align: left !important;
      padding-left: 15px !important;
    }

    /* Fix for long button text overflow (multi-language support) */
    .btn-block {
      white-space: normal !important;
      word-wrap: break-word !important;
      overflow-wrap: break-word !important;
      height: auto !important;
      min-height: 34px !important;
      padding-top: 6px !important;
      padding-bottom: 6px !important;
      line-height: 1.3 !important;
    }

  "))
),
    



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
        tabName = "kinetic_aux_tool",
        kinetic_aux_tool_ui("kinetic_aux_tool", default_settings$kinetic_aux_tool, i18n)
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

  # About dialog
  observeEvent(input$show_about, {
    pkg_version <- tryCatch(
      as.character(utils::packageVersion("supeRcrit")),
      error = function(e) "0.9.0"
    )
    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$strong("supeRcrit"),
        tags$span(paste0("v", pkg_version),
                  style = "font-size: 13px; color: #888; font-weight: normal;")
      ),
      tags$div(
        style = "text-align: center; padding: 10px 0;",
        tags$p(style = "font-size: 14px; margin-bottom: 15px;",
          i18n$t("Process design software for supercritical fluid and subcritical water extraction.")),
        tags$hr(),
        tags$p(style = "margin-bottom: 5px;",
          tags$strong(i18n$t("Developer:")), " Deniz Can K\u00f6seo\u011flu"),
        tags$p(style = "margin-bottom: 5px;",
          tags$strong(i18n$t("Company:")), " AltraFlora Natural Extracts Inc."),
        tags$p(style = "margin-bottom: 5px; font-size: 12px; color: #888;",
          "AltraFlora Do\u011fal Bitki \u00dcr\u00fcnleri San. ve Tic. A.\u015e."),
        tags$p(style = "margin-bottom: 5px;",
          tags$strong(i18n$t("Website:")), " ",
          tags$a(href = "https://www.altraflora.com", target = "_blank", "www.altraflora.com")),
        tags$p(style = "margin-bottom: 5px;",
          tags$strong(i18n$t("Contact:")), " ",
          tags$a(href = "mailto:denizcan@altraflora.com", "denizcan@altraflora.com")),
        tags$hr(),
        tags$p(style = "font-size: 12px; color: #888; margin-top: 10px;",
          paste0("\u00a9 ", format(Sys.Date(), "%Y"), " AltraFlora Natural Extracts Inc. ", i18n$t("All rights reserved.")))
      ),
      footer = modalButton(i18n$t("Close")),
      easyClose = TRUE,
      size = "s"
    ))
  })

  # Initialize shinyhelper with MathJax support
  observe_helpers(help_dir = "help_mds", withMathJax = TRUE)

  # Set language based on browser detection
  observeEvent(input$browser_language, {
    req(input$browser_language)
    shiny.i18n::update_lang(input$browser_language)
    i18n$set_translation_language(input$browser_language)
    updatePickerInput(session, "selected_language", selected = input$browser_language)
    updateActionButton(session, "intro_general", label = i18n$t("Intro"))
  }, once = TRUE, priority = 1000)

     tablang <- reactive({
      list(
        sDecimal = i18n$t("."),
        sInfoThousands = i18n$t(","),
        sProcessing = i18n$t("Processing..."),
        sSearch = i18n$t("Search:"),
        sLengthMenu = i18n$t("Show _MENU_ entries"),
        sInfo = i18n$t("Showing _START_ to _END_ of _TOTAL_ entries"),
        sInfoEmpty = i18n$t("Showing 0 to 0 of 0 entries"),
        sInfoFiltered = i18n$t("(filtered from _MAX_ total entries)"),
        sInfoPostFix = "",
        sLoadingRecords = i18n$t("Loading..."),
        sZeroRecords = i18n$t("No matching records found"),
        sEmptyTable = i18n$t("No data available in table"),
        oPaginate = list(
          sFirst = i18n$t("First"), sPrevious = i18n$t("Previous"),
          sNext = i18n$t("Next"), sLast = i18n$t("Last")
        ),
        oAria = list(
          sSortAscending = i18n$t(": activate to sort column ascending"),
          sSortDescending = i18n$t(": activate to sort column descending")
        ),
        buttons = list(
          copy = i18n$t("Copy"),
          copyTitle = i18n$t("Copy to clipboard"),
          copySuccess = list(
            `_` = i18n$t("%d rows copied"),
            `1` = i18n$t("1 row copied")
          )
        ))
    })

  #
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
    i18n$set_translation_language(input$selected_language)
    updateActionButton(session, "intro_general", label = i18n$t("Intro"))
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
    i18n$t("Two-Site Desorption")
  })

  output$menu_kinetic_bic <- renderText({
    i18n$t("Broken-And-Intact Cells")
  })

  output$menu_kinetic_aux_tool <- renderText({
    i18n$t("Auxiliary Tools")
  })

  output$menu_sfe_co_sel <- renderText({
    i18n$t("SFE Co-Solvent Selection")
  })

  output$menu_solute_characterization <- renderText({
    i18n$t("Solute Characterization")
  })

  output$settings_button_ui <- renderUI({
    actionButton("global_settings_button", i18n$t("Settings"), icon = icon("cog"), class = "btn-secondary")
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

  # Global Settings Modal
  observeEvent(input$global_settings_button, {
    showModal(global_settings_modal_ui("global_settings", i18n))
  })

  # Global Settings Server
  global_settings_modal_server(
    "global_settings",
    main_input = input,
    main_session = session,
    default_settings = default_settings,
    i18n = i18n
  )

  # Reactive Values for inter-module communication
  # Load seed data from config files to provide initial examples (loaded on demand via "Load Example" buttons)
  seed_gcm_calculations <- load_sfe_gcm_seed_data()

  sfe_rv <- reactiveValues(
    solute_data = NULL,
    gcm_results = NULL,
    molecular_plot_data = NULL,
    comparison_list = list(),
    saved_calculations = list(),
    seed_calculations = seed_gcm_calculations  # Seed data stored separately, loaded on demand
  )

  # DOE modülleri için shared reactive values
  # Load seed data from config files to provide initial examples (loaded on demand via "Load Example" buttons)
  seed_analyses <- load_doe_analysis_seed_data()

  doe_rv <- reactiveValues(
    saved_designs = list(),
    saved_analyses = list(),
    seed_analyses = seed_analyses  # Seed data stored separately, loaded on demand
  )

  callModule(com_analysis_server, "com_analysis", default_settings$com_analysis, i18n,tablang)
  callModule(doe_design_server, "doe_design", default_settings$doe_design, i18n, tablang, doe_rv)
  callModule(doe_analysis_server, "doe_analysis", default_settings$doe_analysis, i18n, tablang, doe_rv, default_settings$behavior)
  callModule(doe_desir_server, "doe_desir", default_settings$doe_desir, i18n, tablang, doe_rv)
  callModule(kinetic_tws_server, "kinetic_tws", default_settings$kinetic_tws, i18n, tablang)
  callModule(kinetic_bic_server, "kinetic_bic", default_settings$kinetic_bic, i18n, tablang)
  callModule(kinetic_aux_tool_server, "kinetic_aux_tool", default_settings$kinetic_aux_tool, i18n, tablang)
  callModule(
    solute_characterization_server, "solute_characterization",
    default_settings$solute_characterization, i18n, tablang, sfe_rv
  ) # Add sfe_rv argument
  callModule(
    miscibility_optimization_server, "miscibility_optimization",
    default_settings$miscibility_optimization, i18n, tablang, sfe_rv
  ) # Use sfe_rv
  callModule(
    miscomp_server, "miscibility_comparison",
    default_settings$miscibility_comparison, i18n, tablang, sfe_rv
  ) # Use sfe_rv
  callModule(
    auxiliary_tools_server, "auxiliary_tools",
    default_settings$auxiliary_tools, i18n, tablang
  ) # Auxiliary tools might not need solute data, if so, omit sfe_rv.

  # Hide waiter after app is fully loaded
  waiter_hide()
}

# Launch the app
shinyApp(ui, server)
