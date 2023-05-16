exports_mensual <- get_exportaciones("mensual")

test_that("All indicators have the same number of rows", {
  count <- exports_mensual |>
    dplyr::count(short_names, nivel)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(exports_mensual$fecha) <= lubridate::today())
})

test_that("There aren't missing dates", {
  months_diff <- exports_mensual |>
    dplyr::group_by(short_names, nivel) |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = lag_fecha + months(1) == fecha)

  testthat::expect_true(all(months_diff$one_month_diff))
})
