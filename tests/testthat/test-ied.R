# IED Pais de origen / trimestral ----
ied <- get_ied(modalidad = "pais_origen", frecuencia = "trimestral")

test_that("No empty columns", {
  any_empty_col <- ied |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- ied |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(ied$fecha) <= lubridate::today())
})

# IED Pais de origen / anual ----
ied <- get_ied(modalidad = "pais_origen", frecuencia = "anual")

test_that("No empty columns", {
  any_empty_col <- ied |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- ied |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(ied$year) <= lubridate::today())
})

# IED Sector destino / trimestral ----
ied <- get_ied(modalidad = "sector_destino", frecuencia = "trimestral")

test_that("No empty columns", {
  any_empty_col <- ied |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- ied |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(ied$fecha) <= lubridate::today())
})

# IED Pais de origen / trimestral ----
ied <- get_ied(modalidad = "sector_destino", frecuencia = "anual")

test_that("No empty columns", {
  any_empty_col <- ied |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- ied |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(ied$year) <= lubridate::today())
})
