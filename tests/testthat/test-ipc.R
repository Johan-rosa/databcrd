# General ----
data <- get_ipc_data(desagregacion = "general")

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

# Grupos ----
data <- get_ipc_data(desagregacion = "grupos")

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

# Regiones ----
data <- get_ipc_data(desagregacion = "regiones")

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

# Subyacente ----
data <- get_ipc_data(desagregacion = "subyacente")

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

# TNT ----
data <- get_ipc_data(desagregacion = "tnt")

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
