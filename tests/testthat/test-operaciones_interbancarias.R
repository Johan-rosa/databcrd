data <- get_tasa_interbancaria() |>
  dplyr::mutate(fecha = date)

test_that("There aren't dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

test_that("There aren't haps between dates", {
  test_nmonths <- data |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = fecha == lag_fecha + months(1))

  testthat::expect_true(all(test_nmonths$one_month_diff))
})

test_that("No missing columns", {
  any_missing_columns <- data |>
    sapply(function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(any_missing_columns)
})

test_that("No missing rows", {
  empty_rows <- data |>
    apply(MARGIN = 1, FUN = function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(empty_rows)
})

test_that("Dates are unique", {
  testthat::expect_equal(nrow(data), dplyr::n_distinct(data$fecha))
})

test_that("Dates are ordered after arrange", {
  ordered_data <- data |> dplyr::arrange(fecha)
  testthat::expect_true(all(diff(ordered_data$fecha) > 0))
})

test_that("Year and month are consistent with date", {
  testthat::expect_true(
    all(lubridate::year(data$fecha) == data$year)
  )
  testthat::expect_true(
    all(lubridate::month(data$fecha) == data$mes)
  )
})

test_that("Montos are non-negative", {
  monto_cols <- names(data)[stringr::str_detect(names(data), "^monto")]

  non_negative <- data |>
    dplyr::select(dplyr::all_of(monto_cols)) |>
    sapply(function(x) all(is.na(x) | x >= 0)) |>
    all()

  testthat::expect_true(non_negative)
})

test_that("Tasas are within reasonable bounds", {
  tasa_cols <- names(data)[stringr::str_detect(names(data), "^tasa")]

  reasonable_range <- data |>
    dplyr::select(dplyr::all_of(tasa_cols)) |>
    sapply(function(x) all(is.na(x) | (x >= 0 & x <= 100))) |>
    all()

  testthat::expect_true(reasonable_range)
})

test_that("No duplicated year-month combinations", {
  testthat::expect_equal(
    nrow(data),
    dplyr::n_distinct(data$year, data$mes)
  )
})

test_that("Dataset has at least 12 observations", {
  testthat::expect_gte(nrow(data), 12)
})
