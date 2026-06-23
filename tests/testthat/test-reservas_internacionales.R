# tests/testthat/test-parse_file.R
reservas <- get_reservas_internacionales()

test_that("Returns a dataframe", {
  expect_s3_class(reservas, "data.frame")
})

test_that("parse_file() has expected columns", {
  expect_named(
    reservas,
    c("fecha", "year", "mes", "variable",  "modalidad", "value"),
    ignore.order = TRUE
  )
})

test_that("no dates in the future", {
  expect_true(all(reservas$fecha <= Sys.Date()))
})

test_that("variable values are exactly 'Reservas' and 'Activos'", {
  expect_setequal(unique(reservas$variable), c("Reservas", "Activos"))
})

test_that("modalidad values are exactly 'Bruto' and 'Neto'", {
  expect_setequal(unique(reservas$modalidad), c("Bruto", "Neto"))
})

test_that("no missing dates or years", {
  expect_false(anyNA(reservas$fecha))
  expect_false(anyNA(reservas$year))
})

test_that("date day is always 1", {
  expect_true(all(lubridate::day(reservas$fecha) == 1L))
})

test_that("values are numeric", {
  expect_type(reservas$value, "double")
})
