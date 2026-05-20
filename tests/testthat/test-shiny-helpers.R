# Test helper functions from general_helpers.R

# Helper to create environment with required packages attached
create_helpers_env <- function() {
  env <- new.env(parent = globalenv())

  # Attach required packages to the environment
  suppressPackageStartupMessages({
    library(shiny)
    library(DT)
    library(shinyhelper)
    library(rhandsontable)
  })

  # Source the helpers
  source(file.path(shiny_app_dir, "utils/general_helpers.R"), local = env)
  env
}

# Source the helpers for testing
test_that("general_helpers.R can be sourced", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")

  expect_no_error({
    source(file.path(shiny_app_dir, "utils/general_helpers.R"), local = TRUE)
  })
})

test_that("create_dt_export_buttons returns correct structure", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("DT")

  # Source helpers with packages loaded
  local_env <- create_helpers_env()

  # Create mock i18n reactive
  mock_i18n_r <- create_mock_i18n_r()

  # Test the function
  result <- local_env$create_dt_export_buttons(mock_i18n_r, "test_export")

  expect_type(result, "list")
  expect_length(result, 4)

  # Check button types
  expect_equal(result[[1]]$extend, "copy")
  expect_equal(result[[2]]$extend, "csv")
  expect_equal(result[[3]]$extend, "excel")
  expect_equal(result[[4]]$extend, "pdf")

  # Check that filenames contain the base name
  expect_true(grepl("test_export", result[[2]]$filename))
  expect_true(grepl("test_export", result[[3]]$filename))
  expect_true(grepl("test_export", result[[4]]$filename))
})

test_that("create_accordion_toggle_btn returns valid HTML", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- create_helpers_env()

  mock_i18n_r <- create_mock_i18n_r()
  ns <- shiny::NS("test")

  # Test with start_expanded = TRUE
  result_expanded <- local_env$create_accordion_toggle_btn(ns, mock_i18n_r, "accordion1", start_expanded = TRUE)
  expect_s3_class(result_expanded, "shiny.tag")
  expect_equal(result_expanded$name, "button")

  # Test with start_expanded = FALSE
  result_collapsed <- local_env$create_accordion_toggle_btn(ns, mock_i18n_r, "accordion1", start_expanded = FALSE)
  expect_s3_class(result_collapsed, "shiny.tag")
})

test_that("create_help_modal returns valid helper element", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shinyhelper")

  local_env <- create_helpers_env()

  mock_i18n_r <- create_mock_i18n_r()

  result <- local_env$create_help_modal(mock_i18n_r, "test_help_content", size = "l")

  # Helper returns a shiny tag
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that("create_load_example_btn returns valid actionButton", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- create_helpers_env()

  mock_i18n_r <- create_mock_i18n_r()
  ns <- shiny::NS("test")

  result <- local_env$create_load_example_btn(ns, mock_i18n_r)

  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "button")
  expect_true(grepl("load_example_data", result$attribs$id))
})

test_that("create_estimate_link returns valid actionLink", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("shiny")

  local_env <- create_helpers_env()

  mock_i18n_r <- create_mock_i18n_r()
  ns <- shiny::NS("test")

  result <- local_env$create_estimate_link(ns, mock_i18n_r, "estimate_btn")

  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "a")
})

test_that("trim_zeros_columndefs returns valid DT columnDefs", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("DT")

  local_env <- create_helpers_env()

  # Test with valid column indices
  result <- local_env$trim_zeros_columndefs(c(0, 1, 2), digits = 3)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(result[[1]]$targets, c(0L, 1L, 2L))

  # Test with empty indices
  result_empty <- local_env$trim_zeros_columndefs(c(), digits = 3)
  expect_type(result_empty, "list")
  expect_length(result_empty, 0)

  # Test with NA values (should filter them out)
  result_na <- local_env$trim_zeros_columndefs(c(0, NA, 2), digits = 3)
  expect_equal(result_na[[1]]$targets, c(0L, 2L))
})

test_that("create_editable_hot returns valid rhandsontable or NULL", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")
  skip_if_not_installed("rhandsontable")

  local_env <- create_helpers_env()

  # Test with valid data
  test_data <- data.frame(a = 1:3, b = c("x", "y", "z"))
  result <- local_env$create_editable_hot(test_data)
  expect_s3_class(result, "htmlwidget")

  # Test with NULL data
  result_null <- local_env$create_editable_hot(NULL)
  expect_null(result_null)

  # Test with empty data frame
  result_empty <- local_env$create_editable_hot(data.frame())
  expect_null(result_empty)

  # Test with custom max_height
  result_custom <- local_env$create_editable_hot(test_data, max_height = 200)
  expect_s3_class(result_custom, "htmlwidget")
})

test_that("null coalescing operator works correctly", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")

  local_env <- create_helpers_env()

  `%||%` <- local_env$`%||%`

  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal("" %||% "default", "")
  expect_equal(FALSE %||% "default", FALSE)
})
