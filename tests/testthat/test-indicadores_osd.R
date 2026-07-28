ind_osd <- indicadores_osd()

test_that("All indicators have the same number of rows", {
  count <- ind_osd |>
    dplyr::count(id_indicador, nivel)

  testthat::expect_equal(min(count$n), max(count$n))
})

test_that("There aren't dates in the future", {
  testthat::expect_true(
    max(ind_osd$date) <= lubridate::today())
})

test_that("There aren't missing dates", {
  months_diff <- ind_osd |>
    dplyr::group_by(id_indicador, nivel) |>
    dplyr::arrange(date) |>
    dplyr::mutate(lag_fecha = dplyr::lag(date)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = lag_fecha + months(1) == date)

  testthat::expect_true(all(months_diff$one_month_diff))
})

test_that("filtro_variable works", {
  values <- c(
    "Activos externos",
    "Pasivos externos",
    "Inversiones",
    "Préstamos",
    "Depósitos",
    "Composición depósitos",
    "Tasa de cambio"
  )

  purrr::walk(values, \(value) {
    data <- indicadores_osd(filtro_variable = value)
    testthat::expect_true(unique(data$variable) == value)
  })
})

test_that("filtro_moneda works", {
  values <- c("USD", "DOP")
  purrr::walk(values, \(value) {
    data <- indicadores_osd(filtro_moneda = value)
    testthat::expect_true(unique(data$moneda) == value)
  })
})

test_that("filtro_entidad works", {
  values <- ind_osd$entidad |> unique()
  purrr::walk(values, \(value) {
    data <- indicadores_osd(filtro_entidad = value)
    testthat::expect_true(unique(data$entidad) == value)
  })
})
