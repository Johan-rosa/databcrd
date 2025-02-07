# Prestamos consolidados ----------------------------------------
data <- get_prestamos_osd(osd = "consolidado")

test_that("Series starts at january 1996", {
  expect_equal(min(data$fecha), as.Date("1996-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(data$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- data |>
    dplyr::count(sectores)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

# Prestamos BM ----------------------------------------
data <- get_prestamos_osd(osd = "bancos_multiples")

test_that("Series starts at january 1996", {
  expect_equal(min(data$fecha), as.Date("1996-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(data$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- data |>
    dplyr::count(sectores)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})

# Prestamos resto ----------------------------------------
data <- get_prestamos_osd(osd = "resto_osd")

test_that("Series starts at january 1996", {
  expect_equal(min(data$fecha), as.Date("1996-01-01"))
})

test_that("Series doesn't ends in the future", {
  expect_true(max(data$fecha) <= lubridate::today())
})

test_that("All series have the same length", {
  n_by_partida <- data |>
    dplyr::count(sectores)

  expect_true(max(n_by_partida$n) == min(max(n_by_partida$n)))
})
