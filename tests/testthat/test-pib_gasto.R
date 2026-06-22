
# PIB nominal series homogenea 1991 ----------------------------------------
pib_gasto_nominal_homogenea <- get_pib_gasto(
  modalidad = "nominal", homogenea_91 = TRUE)

test_that("Series starts at january 1991", {
  expect_equal(min(pib_gasto_nominal_homogenea$fecha), as.Date("1991-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_nominal_homogenea$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_nominal_homogenea |>
    dplyr::count(partida)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_nominal_homogenea),
    c("partida", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})


# PIB nominal series 2007 --------------------------------------------------
pib_gasto_nominal_2018 <- get_pib_gasto(modalidad = "nominal")

test_that("Series starts at january 2018", {
  expect_equal(min(pib_gasto_nominal_2018$fecha), as.Date("2018-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_nominal_2018$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_nominal_2018 |>
    dplyr::count(partida)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_nominal_2018),
    c("partida", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})

test_that("PIB for december 2018 is Ok", {
  pib_2018 <- pib_gasto_nominal_2018 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2018-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2018, 1122176.37792465)
})

test_that("PIB for december 2020 is Ok", {
  pib_2020 <- pib_gasto_nominal_2018 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2020-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2020, 1239354.12086464)
})


# PIB nominal series 2007 acumulada -------------------------------------------
pib_gasto_nominal_2018_acumulado <- get_pib_gasto( # nolint
  modalidad = "nominal", acumulado = TRUE)

test_that("Series starts at january 2018", {
  expect_equal(
    min(pib_gasto_nominal_2018_acumulado$fecha), as.Date("2018-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_nominal_2018_acumulado$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_nominal_2018_acumulado |>
    dplyr::count(partida)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_nominal_2018_acumulado),
    c("partida", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})

test_that("PIB for december 2018 is Ok", {
  pib_2018 <- pib_gasto_nominal_2018_acumulado |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2018-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2018, 4208088.58876332)
})

test_that("PIB for december 2020 is Ok", {
  pib_2020 <- pib_gasto_nominal_2018_acumulado |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2020-10-01")

  expect_equal(pib_2020$pib_nominal, 4439813.97652647)
  expect_equal(pib_2020$ponderacion, 100)
})

# PIB real series 2007 --------------------------------------------------
pib_gasto_real_2018 <- get_pib_gasto(modalidad = "real")

test_that("Series starts at january 2018", {
  expect_equal(min(pib_gasto_real_2018$fecha), as.Date("2018-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_real_2018$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_real_2018 |>
    dplyr::count(partida)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_real_2018),
    c("partida", "fecha", "year", "trimestre", "indice",
      "crecimiento_interanual", "incidencia")
  )
})

test_that("PIB for december 2018 is Ok", {
  pib_2018 <- pib_gasto_real_2018 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2018-10-01") |>
    dplyr::pull(indice)

  expect_equal(pib_2018, 103.97372424462)
})

test_that("PIB for december 2016 is Ok", {
  pib_2020 <- pib_gasto_real_2018 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2020-10-01")

  expect_equal(pib_2020$indice, 102.819041568166)
  expect_equal(pib_2020$crecimiento_interanual, -5.42031754140824)
  expect_equal(pib_2020$incidencia, -5.42031754140824)
})
