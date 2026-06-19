
# PIB nominal series homogenea 1991 ----------------------------------------
pib_sectores_homogenea <- get_pib_sectores(
  modalidad = "nominal", homogenea_91 = TRUE)

test_that("Series starts at january 1991", {
  expect_equal(min(pib_sectores_homogenea$fecha), as.Date("1991-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_sectores_homogenea$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_sectores_homogenea |>
    dplyr::count(sector)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_sectores_homogenea),
    c("sector", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})


# PIB nominal series 2018--------------------------------------------------
pib_sectores_nominal_2018 <- get_pib_sectores(modalidad = "nominal")

test_that("Series starts at january 1991", {
  expect_equal(min(pib_sectores_nominal_2018$fecha), as.Date("2018-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_sectores_nominal_2018$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_sectores_nominal_2018 |>
    dplyr::count(sector)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_sectores_nominal_2018),
    c("sector", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})

test_that("PIB for december 2007 is Ok", {
  pib_2018 <- pib_sectores_nominal_2018 |>
    dplyr::filter(sector == "Producto Interno Bruto", fecha == "2018-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2018, 1122176.37792465)
})

test_that("PIB for december 2016 is Ok", {
  pib_2020 <- pib_gasto_nominal_2018 |>
    dplyr::filter(sector == "Producto Interno Bruto", fecha == "2020-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2020, 1239354.12086464)
})


# PIB nominal series 2007 acumulada -------------------------------------------
pib_sectores_nominal_2018_acumulado <- get_pib_sectores( # nolint
  modalidad = "nominal", acumulado = TRUE)

test_that("Series starts at january 2018", {
  expect_equal(
    min(pib_sectores_nominal_2018_acumulado$fecha), as.Date("2018-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_sectores_nominal_2018_acumulado$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_sectores_nominal_2018_acumulado |>
    dplyr::count(sector)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_sectores_nominal_2018_acumulado),
    c("sector", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})

test_that("PIB for december 2018 is Ok", {
  pib_2018 <- pib_sectores_nominal_2018_acumulado |>
    dplyr::filter(sector == "Producto Interno Bruto", fecha == "2018-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2018, 4208088.58876332)
})

test_that("PIB for december 2016 is Ok", {
  pib_2016 <- pib_gasto_nominal_2007_acumulado |>
    dplyr::filter(sector == "Producto Interno Bruto", fecha == "2016-10-01")

  expect_equal(pib_2016$pib_nominal, 3487292.51270275)
  expect_equal(pib_2016$ponderacion, 1)
})

# PIB real series 2007 --------------------------------------------------
pib_gasto_real_2007 <- get_pib_sectores(modalidad = "real")

test_that("Series starts at january 1991", {
  expect_equal(min(pib_gasto_real_2007$fecha), as.Date("2007-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_real_2007$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_real_2007 |>
    dplyr::count(sector)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_real_2007),
    c("sector", "fecha", "year", "trimestre", "indice",
      "crecimiento_interanual", "incidencia")
  )
})

test_that("PIB for december 2007 is Ok", {
  pib_2007 <- pib_gasto_real_2007 |>
    dplyr::filter(sector == "Producto Interno Bruto", fecha == "2007-10-01") |>
    dplyr::pull(indice)

  expect_equal(pib_2007, 103.525353112715)
})

test_that("PIB for december 2016 is Ok", {
  pib_2016 <- pib_gasto_real_2007 |>
    dplyr::filter(sector == "Producto Interno Bruto", fecha == "2016-10-01")

  expect_equal(pib_2016$indice, 157.009259740035)
  expect_equal(pib_2016$crecimiento_interanual, 5.39533315364895)
  expect_equal(pib_2016$incidencia, 5.39533315364895)
})
