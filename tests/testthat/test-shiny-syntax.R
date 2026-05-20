# Test that all Shiny app files have valid R syntax
# This catches parsing errors before runtime

test_that("all server module files have valid syntax", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")

  for (file in server_module_files) {
    result <- check_file_syntax(file)
    expect_true(
      result$valid,
      info = sprintf("Syntax error in %s: %s", file, result$error %||% "unknown")
    )
  }
})

test_that("all utility files have valid syntax", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")

  for (file in utility_files) {
    result <- check_file_syntax(file)
    expect_true(
      result$valid,
      info = sprintf("Syntax error in %s: %s", file, result$error %||% "unknown")
    )
  }
})

test_that("all UI module files have valid syntax", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")

  for (file in ui_module_files) {
    result <- check_file_syntax(file)
    expect_true(
      result$valid,
      info = sprintf("Syntax error in %s: %s", file, result$error %||% "unknown")
    )
  }
})

test_that("main app.R has valid syntax", {
  skip_if(shiny_app_dir == "", "Shiny app directory not found")

  result <- check_file_syntax("app.R")
  expect_true(
    result$valid,
    info = sprintf("Syntax error in app.R: %s", result$error %||% "unknown")
  )
})
