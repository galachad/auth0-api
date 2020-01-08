context("lintr")

test_that("lintr", {
  # The path needs to point to the project directory.
  message(testthat::test_path())
  lintr::expect_lint_free(path = "../../../auth0api", relative_path = TRUE, lintr::line_length_linter(120))
})
