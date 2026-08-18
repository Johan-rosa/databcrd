# Mensual
data <- fiscal_operations()

test_that("All indicators have the same number of rows", {
  count <- data |>
    dplyr::count(codigo, level)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(data$fecha) <= lubridate::today())
})

test_that("There aren't missing dates", {
  months_diff <- data |>
    dplyr::group_by(codigo, level) |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = lag_fecha + months(1) == fecha)

  testthat::expect_true(all(months_diff$one_month_diff))
})

# Anual
data <- fiscal_operations(frecuencia = "anual")

test_that("All indicators have the same number of rows", {
  count <- data |>
    dplyr::count(codigo, level)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(data$year) <= lubridate::year(lubridate::today())
  )
})

# Anual
data <- fiscal_operations_gdp()

test_that("All indicators have the same number of rows", {
  count <- data |>
    dplyr::count(codigo, level, sort = TRUE)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(data$year) <= lubridate::year(lubridate::today())
  )
})
