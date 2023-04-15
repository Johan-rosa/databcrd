test_that("Data label works", {
  expect_equal(
    date_label(as.Date("2020-01-01")),
    "Jan 2020"
  )
})

test_that("crear_mes works well", {
  expect_equal(
    crear_mes("enero"),
    1
  )
})

test_that("crear_mes works well", {
  expect_equal(
    crear_mes(1:12, "number_to_text"),
    c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
      "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  )
})
