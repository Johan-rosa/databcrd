data <- get_ipc_articulos()

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("Weigth add up to 100 at all levels", {
  total_weight <- data |>
    dplyr::summarise(
      sum_ponderacion = round(sum(ponderacion)),
      .by = c(date, agregacion)
    )

  testthat::expect_true(all(total_weight$sum_ponderacion == 100))
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$date) <= lubridate::today())
})

test_that("Series start at 2010", {
  testthat::expect_true(min(data$year) == 2010)
})

