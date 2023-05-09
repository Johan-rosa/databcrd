tasas_activas <- get_tasa_activas()

test_that("Pasivas: there aren't dates in the future", {
  testthat::expect_true(max(tasas_activas$fecha) <= lubridate::today())
})

test_that("There aren't haps between dates", {
  test_nmonths <- tasas_activas |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = fecha == lag_fecha + months(1))

  testthat::expect_true(all(test_nmonths$one_month_diff))
})

test_that("No missing columns",{
  any_missing_columns <- tasas_activas |>
    sapply(function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(any_missing_columns)
})

test_that("No missing rows", {
  empty_rows <- tasas_activas |>
    apply(MARGIN = 1, FUN = function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(empty_rows)
})

tasas_pasivas <- get_tasas_pasivas()

test_that("Pasivas: there aren't dates in the future", {
  testthat::expect_true(max(tasas_pasivas$fecha) <= lubridate::today())
})

test_that("There aren't haps between dates", {
  test_nmonths <- tasas_pasivas |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = fecha == lag_fecha + months(1))

  testthat::expect_true(all(test_nmonths$one_month_diff))
})

test_that("No missing columns",{
  any_missing_columns <- tasas_pasivas |>
    sapply(function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(any_missing_columns)
})

test_that("No missing rows", {
  empty_rows <- tasas_pasivas |>
    apply(MARGIN = 1, FUN = function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(empty_rows)
})
