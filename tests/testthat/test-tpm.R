tpm_data <- get_tpm()

test_that("No empty columns", {
  any_empty_col <- tpm_data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- tpm_data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(tpm_data$fecha) <= lubridate::today())
})
