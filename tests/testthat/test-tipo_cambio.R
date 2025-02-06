# Diaria, spot, average ----
data <- get_tc(
  frecuencia = "diaria",
  entidad = "spot",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Mensual, spot, average ----
data <- get_tc(
  frecuencia = "mensual",
  entidad = "spot",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Trimestral, spot, average ----
data <- get_tc(
  frecuencia = "trimestral",
  entidad = "spot",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Anual, spot, average ----
data <- get_tc(
  frecuencia = "anual",
  entidad = "spot",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Diaria, spot, fp ----
data <- get_tc(
  frecuencia = "diaria",
  entidad = "spot",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Mensual, spot, fp ----
data <- get_tc(
  frecuencia = "mensual",
  entidad = "spot",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Trimestral, spot, fp ----
data <- get_tc(
  frecuencia = "trimestral",
  entidad = "spot",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Anual, spot, fp ----
data <- get_tc(
  frecuencia = "anual",
  entidad = "spot",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Diaria, eif, average ----
data <- get_tc(
  frecuencia = "diaria",
  entidad = "eif",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Mensual, eif, average ----
data <- get_tc(
  frecuencia = "mensual",
  entidad = "eif",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Trimestral, eif, average ----
data <- get_tc(
  frecuencia = "trimestral",
  entidad = "eif",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Anual, eif, average ----
data <- get_tc(
  frecuencia = "anual",
  entidad = "eif",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Diaria, eif, fp ----
data <- get_tc(
  frecuencia = "diaria",
  entidad = "eif",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Mensual, eif, fp ----
data <- get_tc(
  frecuencia = "mensual",
  entidad = "eif",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Trimestral, eif, fp ----
data <- get_tc(
  frecuencia = "trimestral",
  entidad = "eif",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Anual, eif, fp ----
data <- get_tc(
  frecuencia = "anual",
  entidad = "eif",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Diaria, ac, average ----
data <- get_tc(
  frecuencia = "diaria",
  entidad = "ac",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Mensual, ac, average ----
data <- get_tc(
  frecuencia = "mensual",
  entidad = "ac",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Trimestral, ac, average ----
data <- get_tc(
  frecuencia = "trimestral",
  entidad = "ac",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Anual, ac, average ----
data <- get_tc(
  frecuencia = "anual",
  entidad = "ac",
  average_or_fp = "average")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Diaria, ac, fp ----
data <- get_tc(
  frecuencia = "diaria",
  entidad = "ac",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Mensual, ac, fp ----
data <- get_tc(
  frecuencia = "mensual",
  entidad = "ac",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Trimestral, ac, fp ----
data <- get_tc(
  frecuencia = "trimestral",
  entidad = "ac",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})

# Anual, ac, fp ----
data <- get_tc(
  frecuencia = "anual",
  entidad = "ac",
  average_or_fp = "fp")

test_that("No empty columns", {
  any_empty_col <- data |>
    sapply(\(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_col)
})

test_that("No empty rows", {
  any_empty_row <- data |>
    apply(MARGIN = 1, FUN = \(x) all(is.na(x))) |>
    any()

  testthat::expect_false(any_empty_row)
})

test_that("No dates in the future", {
  testthat::expect_true(max(data$fecha) <= lubridate::today())
})
