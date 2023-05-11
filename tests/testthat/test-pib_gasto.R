
# PIB nominal serie homogenea 1991 ----------------------------------------

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

# PIB nominal serie 2007 --------------------------------------------------
pib_gasto_nominal_2007 <- get_pib_gasto(modalidad = "nominal")

test_that("Series starts at january 1991", {
  expect_equal(min(pib_gasto_nominal_2007$fecha), as.Date("2007-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_nominal_2007$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_nominal_2007 |>
    dplyr::count(partida)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_nominal_2007),
    c("partida", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})

test_that("PIB for december 2007 is Ok", {
  pib_2007 <- pib_gasto_nominal_2007 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2007-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2007, 391153.303668817)
})

test_that("PIB for december 2016 is Ok", {
  pib_2007 <- pib_gasto_nominal_2007 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2016-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2007, 926379.831619056)
})

# PIB nominal serie 2007 acumulada -------------------------------------------
pib_gasto_nominal_2007_acumulado <- get_pib_gasto( # nolint
  modalidad = "nominal", acumulado = TRUE)

test_that("Series starts at january 1991", {
  expect_equal(
    min(pib_gasto_nominal_2007_acumulado$fecha), as.Date("2007-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_nominal_2007_acumulado$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_nominal_2007_acumulado |>
    dplyr::count(partida)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_nominal_2007_acumulado),
    c("partida", "fecha", "year", "trimestre", "pib_nominal", "ponderacion")
  )
})

test_that("PIB for december 2007 is Ok", {
  pib_2007 <- pib_gasto_nominal_2007_acumulado |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2007-10-01") |>
    dplyr::pull(pib_nominal)

  expect_equal(pib_2007, 1458416.51805624)
})

test_that("PIB for december 2016 is Ok", {
  pib_2016 <- pib_gasto_nominal_2007_acumulado |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2016-10-01")

  expect_equal(pib_2016$pib_nominal, 3487292.51270275)
  expect_equal(pib_2016$ponderacion, 100)
})
# PIB real serie 2007 --------------------------------------------------
pib_gasto_real_2007 <- get_pib_gasto(modalidad = "real")

test_that("Series starts at january 1991", {
  expect_equal(min(pib_gasto_real_2007$fecha), as.Date("2007-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(pib_gasto_real_2007$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- pib_gasto_real_2007 |>
    dplyr::count(partida)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

test_that("Correct column names", {
  expect_equal(
    names(pib_gasto_real_2007),
    c("partida", "fecha", "year", "trimestre", "indice",
      "crecimiento_interanual", "incidencia")
  )
})

test_that("PIB for december 2007 is Ok", {
  pib_2007 <- pib_gasto_real_2007 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2007-10-01") |>
    dplyr::pull(indice)

  expect_equal(pib_2007, 103.525353112715)
})

test_that("PIB for december 2016 is Ok", {
  pib_2016 <- pib_gasto_real_2007 |>
    dplyr::filter(partida == "Producto Interno Bruto", fecha == "2016-10-01")

  expect_equal(pib_2016$indice, 157.009259740035)
  expect_equal(pib_2016$crecimiento_interanual, 5.39533315364895)
  expect_equal(pib_2016$incidencia, 5.39533315364895)
})
