
if (modalidad == "mensual") return(get_remesas_mensuales())
if (modalidad == "por_pais_emisor") return(get_remesas_pais())
if (modalidad == "por_provincia_receptora") return(get_remesas_provincias())
if (modalidad == "cantidad_de_transacciones") return(get_remesas_cnt())
if (modalidad == "promedio_transacciones") return(get_remesas_avg())
if (modalidad == "segun_moneda") return(get_remesas_currency())
if (modalidad == "entidad_pagadora") return(get_remesas_epa())
if (modalidad == "genero_receptor") return(get_remesas_genero())


# Remesas mensual ----
remesas <- get_remesas(modalidad = "mensual")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$fecha) <= lubridate::today())
})

# Remesas por pais emisor ----
remesas <- get_remesas(modalidad = "por_pais_emisor")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$year) <= lubridate::today())
})

# Remesas por provincia receptora ----
remesas <- get_remesas(modalidad = "por_provincia_receptora")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$year) <= lubridate::today())
})

# Remesas cantidad ----
remesas <- get_remesas(modalidad = "cantidad_de_transacciones")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$year) <= lubridate::today())
})

# Remesas promedio ----
remesas <- get_remesas(modalidad = "promedio_transacciones")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$year) <= lubridate::today())
})

# Remesas por moneda ----
remesas <- get_remesas(modalidad = "segun_moneda")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$year) <= lubridate::today())
})

# Remesas por entidad pagadora ----
remesas <- get_remesas(modalidad = "entidad_pagadora")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$year) <= lubridate::today())
})

# Remesas por genero ----
remesas <- get_remesas(modalidad = "genero_receptor")

test_that("No empty columns", {
  any_empty_col <- remesas |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- remesas |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(remesas$year) <= lubridate::today())
})
