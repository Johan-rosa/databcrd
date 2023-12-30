eem_data <- get_expectativas(modalidad = "eem")

test_that("All indicators have the same number of rows", {
  count <- eem_data |>
    dplyr::count(short_names, medida)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(eem_data$fecha) <= lubridate::today())
})

test_that("There aren't missing dates", {
  months_diff <- eem_data |>
    dplyr::group_by(short_names, medida) |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = lag_fecha + months(1) == fecha)

  testthat::expect_true(all(months_diff$one_month_diff))
})

eoe_data <- get_expectativas(modalidad = "eoe")

test_that("All indicators have the same number of rows", {
  count <- eoe_data |>
    dplyr::count(descripcion)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(eoe_data$fecha) <= lubridate::today())
})

test_that("There aren't missing dates", {
  months_diff <- eoe_data |>
    dplyr::group_by(descripcion) |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = lag_fecha + months(1) == fecha)

  testthat::expect_true(all(months_diff$one_month_diff))
})

ecc_data <- get_expectativas(modalidad = "ecc")

test_that("All indicators have the same number of rows", {
  count <- ecc_data |>
    dplyr::count(direct_parent, categoria, short_names)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(ecc_data$fecha) <= lubridate::today())
})

test_that("There aren't missing dates", {
  months_diff <- ecc_data |>
    dplyr::group_by(direct_parent, categoria, short_names) |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = lag_fecha + months(3) == fecha)

  testthat::expect_true(all(months_diff$one_month_diff))
})
