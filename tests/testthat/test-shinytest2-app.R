# End-to-end tests using shinytest2
# These tests launch the actual Shiny app in a browser and test functionality
#
# WINDOWS USERS: If chromote fails to start Chrome, run from Git Bash first:
#   ./tests/start_chrome_for_tests.sh
# Then set the environment variable before running tests:
#   Sys.setenv(CHROMOTE_CHROME_DEBUG_PORT = "9515")
#   devtools::test(filter = "shinytest2")

library(shinytest2)

# Custom browser class to connect to existing Chrome instance (Windows workaround)
# This is needed because chromote can't launch Chrome on Windows when Chrome is already running
ExistingChromeBrowser <- R6::R6Class("ExistingChromeBrowser",
  public = list(
    initialize = function(host = "127.0.0.1", port = 9515L) {
      private$host <- host
      private$port <- as.integer(port)
      private$process <- NULL

      # Verify connection
      url <- paste0("http://", private$host, ":", private$port, "/json/version")
      resp <- tryCatch(
        jsonlite::fromJSON(url, simplifyVector = FALSE),
        error = function(e) stop("Cannot connect to Chrome on ", host, ":", port)
      )
      if (is.null(resp$webSocketDebuggerUrl)) {
        stop("Invalid Chrome debug response on port ", port)
      }
    },

    is_local = function() TRUE,
    get_process = function() private$process,
    is_alive = function() {
      tryCatch({
        url <- paste0("http://", private$host, ":", private$port, "/json/version")
        resp <- jsonlite::fromJSON(url, simplifyVector = FALSE)
        !is.null(resp$webSocketDebuggerUrl)
      }, error = function(e) FALSE)
    },
    get_host = function() private$host,
    get_port = function() private$port,
    close = function(wait = FALSE) invisible(NULL)  # Don't close external Chrome
  ),

  private = list(
    host = NULL,
    port = NULL,
    process = NULL
  )
)

# Check if an existing Chrome debug instance is available
check_existing_chrome <- function(port = 9515L) {
  tryCatch({
    resp <- jsonlite::fromJSON(
      paste0("http://127.0.0.1:", port, "/json/version"),
      simplifyVector = FALSE
    )
    !is.null(resp$webSocketDebuggerUrl)
  }, error = function(e) FALSE)
}

# Set up chromote to use existing Chrome instance
setup_existing_chrome <- function(port = 9515L) {
  browser <- ExistingChromeBrowser$new(port = port)
  chromote_obj <- chromote::Chromote$new(browser = browser)
  chromote::set_default_chromote_object(chromote_obj)
  invisible(TRUE)
}

# Skip all shinytest2 tests if Chrome is not available
skip_if_no_chrome <- function() {
  skip_if_not_installed("chromote")

  # First check for existing Chrome debug instance (Windows workaround)
  debug_port <- as.integer(Sys.getenv("CHROMOTE_CHROME_DEBUG_PORT", "0"))
  if (debug_port > 0 && check_existing_chrome(debug_port)) {
    # Set up chromote to use the existing Chrome
    tryCatch({
      setup_existing_chrome(debug_port)
      return(invisible(TRUE))
    }, error = function(e) {
      skip(paste("Failed to connect to existing Chrome:", e$message))
    })
  }

  # Try default chromote startup
  tryCatch({
    chromote::find_chrome()
    # Try to actually start chromote to verify it works
    b <- chromote::default_chromote_object()
    return(invisible(TRUE))
  }, error = function(e) {
    # On Windows, chromote often fails due to single-instance Chrome behavior
    if (.Platform$OS.type == "windows") {
      skip(paste(
        "Chrome/chromote not available.",
        "On Windows, run './tests/start_chrome_for_tests.sh' from Git Bash first,",
        "then set Sys.setenv(CHROMOTE_CHROME_DEBUG_PORT = '9515') before testing."
      ))
    } else {
      skip("Chrome not available for shinytest2 tests")
    }
  })
}

# Get the app directory
get_app_dir <- function() {
  app_dir <- system.file("shiny-app", package = "supeRcrit")
  if (app_dir == "" || !dir.exists(app_dir)) {
    skip("Shiny app directory not found")
  }
  app_dir
}

# ============================================================================
# KINETIC TWS MODULE TESTS
# ============================================================================

test_that("Kinetic TWS module loads and calculates with example data", {
  skip_if_no_chrome()
  skip_on_cran()
  skip_on_ci()

  app_dir <- get_app_dir()

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "kinetic-tws-test",
    height = 900,
    width = 1400,
    load_timeout = 30000,
    timeout = 20000
  )

  on.exit(app$stop(), add = TRUE)

  # Wait for app to load
  Sys.sleep(3)

  # Navigate to Kinetic TWS tab
  app$set_inputs(`main_menu` = "kinetic_tws")
  Sys.sleep(2)

  # Set input type to CSV
  app$set_inputs(`kinetic_tws-input_type` = "csv")
  Sys.sleep(1)

  # Click Load Example Data
  app$click("kinetic_tws-load_example_data")
  Sys.sleep(2)

  # Verify data was loaded by checking if the preview table exists
  # The app should show a notification on successful load
  html <- app$get_html("body")
  expect_true(
    grepl("oec_data_preview|rhandsontable|Example data loaded", html, ignore.case = TRUE),
    info = "Example data should be loaded and displayed"
  )
})

# ============================================================================
# KINETIC BIC MODULE TESTS
# ============================================================================

test_that("Kinetic BIC module loads example data", {
  skip_if_no_chrome()
  skip_on_cran()
  skip_on_ci()

  app_dir <- get_app_dir()

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "kinetic-bic-test",
    height = 900,
    width = 1400,
    load_timeout = 30000,
    timeout = 20000
  )

  on.exit(app$stop(), add = TRUE)

  Sys.sleep(3)

  # Navigate to Kinetic BIC tab
  app$set_inputs(`main_menu` = "kinetic_bic")
  Sys.sleep(2)

  # Set input type to CSV
  app$set_inputs(`kinetic_bic-input_type` = "csv")
  Sys.sleep(1)

  # Click Load Example Data
  app$click("kinetic_bic-load_example_data")
  Sys.sleep(2)

  # Verify data was loaded
  html <- app$get_html("body")
  expect_true(
    grepl("oec_data_preview|rhandsontable|Example data loaded", html, ignore.case = TRUE),
    info = "Example data should be loaded"
  )
})

# ============================================================================
# DOE ANALYSIS MODULE TESTS
# ============================================================================

test_that("DOE Analysis module loads example data", {
  skip_if_no_chrome()
  skip_on_cran()
  skip_on_ci()

  app_dir <- get_app_dir()

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "doe-analysis-test",
    height = 900,
    width = 1400,
    load_timeout = 30000,
    timeout = 20000
  )

  on.exit(app$stop(), add = TRUE)

  Sys.sleep(3)

  # Navigate to DOE Analysis tab
  app$set_inputs(`main_menu` = "doe_analysis")
  Sys.sleep(2)

  # Click Load Example Data
  app$click("doe_analysis-load_example_data")
  Sys.sleep(2)

  # Verify data was loaded
  html <- app$get_html("body")
  expect_true(
    grepl("Example data loaded|data_preview|handsontable", html, ignore.case = TRUE),
    info = "DOE example data should be loaded"
  )
})

# ============================================================================
# DOE DESIRABILITY MODULE TESTS
# ============================================================================

test_that("DOE Desirability module loads example data", {
  skip_if_no_chrome()
  skip_on_cran()
  skip_on_ci()

  app_dir <- get_app_dir()

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "doe-desir-test",
    height = 900,
    width = 1400,
    load_timeout = 30000,
    timeout = 20000
  )

  on.exit(app$stop(), add = TRUE)

  Sys.sleep(3)

  # Navigate to DOE Desirability tab
  app$set_inputs(`main_menu` = "doe_desir")
  Sys.sleep(2)

  # Click Load Example Data
  app$click("doe_desir-load_example_data")
  Sys.sleep(2)

  # Verify data was loaded
  html <- app$get_html("body")
  expect_true(
    grepl("Example data loaded|data_preview|loaded", html, ignore.case = TRUE),
    info = "DOE Desirability example data should be loaded"
  )
})

# ============================================================================
# COM ANALYSIS MODULE TESTS
# ============================================================================

test_that("COM Analysis module loads example data", {
  skip_if_no_chrome()
  skip_on_cran()
  skip_on_ci()

  app_dir <- get_app_dir()

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "com-analysis-test",
    height = 900,
    width = 1400,
    load_timeout = 30000,
    timeout = 20000
  )

  on.exit(app$stop(), add = TRUE)

  Sys.sleep(3)

  # Navigate to COM Analysis tab
  app$set_inputs(`main_menu` = "com_analysis")
  Sys.sleep(2)

  # Click Load Example Data
  app$click("com_analysis-load_example_data")
  Sys.sleep(2)

  # Verify data was loaded
  html <- app$get_html("body")
  expect_true(
    grepl("Example data loaded|data_preview|loaded", html, ignore.case = TRUE),
    info = "COM Analysis example data should be loaded"
  )
})

# ============================================================================
# APP STARTUP TEST
# ============================================================================

test_that("App starts without errors", {
  skip_if_no_chrome()
  skip_on_cran()
  skip_on_ci()

  app_dir <- get_app_dir()

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "app-startup-test",
    height = 900,
    width = 1400,
    load_timeout = 30000,
    timeout = 20000
  )

  on.exit(app$stop(), add = TRUE)

  # Wait for app to fully load
  Sys.sleep(5)

  # Get the page HTML to verify app loaded
  html <- app$get_html("body")

  # Check that main UI elements exist
  expect_true(grepl("supeRcrit", html), info = "App title should be present")
  expect_true(grepl("main_menu|sidebarMenu", html, ignore.case = TRUE), info = "Sidebar menu should exist")

  # Check for no error modals/notifications (exclude legitimate uses like "Standard Error")
  # Look for patterns that indicate actual errors, not scientific terms
  has_error_modal <- grepl("shiny-notification-error|alert-danger|modal-error", html, ignore.case = TRUE)
  has_fatal_error <- grepl("fatal error|app crashed|failed to load", html, ignore.case = TRUE)
  expect_false(
    has_error_modal || has_fatal_error,
    info = "App should start without error modals or fatal errors"
  )
})

# ============================================================================
# NAVIGATION TEST
# ============================================================================

test_that("Can navigate between all main tabs", {
  skip_if_no_chrome()
  skip_on_cran()
  skip_on_ci()

  app_dir <- get_app_dir()

  app <- AppDriver$new(
    app_dir = app_dir,
    name = "navigation-test",
    height = 900,
    width = 1400,
    load_timeout = 30000,
    timeout = 20000
  )

  on.exit(app$stop(), add = TRUE)

  Sys.sleep(3)

  # Test navigation to each main tab
  tabs_to_test <- c(
    "com_analysis",
    "solute_characterization",
    "miscibility_optimization",
    "doe_design",
    "doe_analysis",
    "kinetic_tws",
    "kinetic_bic"
  )

  for (tab in tabs_to_test) {
    app$set_inputs(`main_menu` = tab)
    Sys.sleep(1)

    # Verify we navigated (no crash)
    current_html <- app$get_html("body")
    expect_true(nchar(current_html) > 1000, info = paste("Tab", tab, "should render content"))
  }
})
