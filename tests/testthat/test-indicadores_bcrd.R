indicadores_monetarios_bcrd <- get_indicadores_monetarios_bcrd()

test_that("All indicators have the same number of rows", {
  count <- indicadores_monetarios_bcrd |>
    dplyr::count(descripcion, nivel)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(indicadores_monetarios_bcrd$fecha) <= lubridate::today())
})

test_that("There aren't missing dates", {
  months_diff <- indicadores_monetarios_bcrd |>
    dplyr::group_by(descripcion, nivel) |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = lag_fecha + months(1) == fecha)

  testthat::expect_true(all(months_diff$one_month_diff))
})
