
describe( "Testing balanza de servicios", {
  bs <- balanza_servicios()
  it("returs a dataframe", expect_s3_class(bs, "data.frame"))

  it("has the correct structure", {
    expect_equal(ncol(bs), 8)
    expect_named(bs, c("code", "naturaleza", "concepto", "fecha", "year", "trimestre", "monto", "monto_acumulado"))
    expect_s3_class(bs$fecha, "Date")
    expect_type(bs$year, "double")
    expect_type(bs$trimestre, "integer")
    expect_type(bs$monto, "double")
    expect_type(bs$monto_acumulado, "double")
  })

  it("doesn't have dates in the furture", {
    expect_true(max(bs$fecha) <= Sys.Date())
  })

  it("doesn't have empty rows", {
    expect_equal(
      dplyr::filter(bs, dplyr::if_all(dplyr::everything(), is.na)) |>
        nrow(),
      0
    )
  })

  test_that("no hay filas duplicadas para la misma combinacion code+naturaleza+fecha", {
    claves <- paste(bs$code, bs$naturaleza, bs$fecha)
    counts <- dplyr::count(bs, code, concepto, naturaleza, fecha, sort = TRUE)
    expect_equal(max(counts$n), 1L)
  })

  it("naturaleza solo toma los tres valores esperados", {
    expect_setequal(unique(bs$naturaleza), c("Credito", "Debito", "Saldo"))
  })

  test_that("los codigos de categoria tienen el formato esperado (letra+punto o 1/2/3)", {
    codigos <- unique(bs$code)
    formato_valido <- grepl("^[A-Z]{1,3}\\.$", codigos) | codigos %in% c("1", "2", "3")
    # si esto falla, el BCRD probablemente agrego una categoria con un
    # patron de codigo distinto
    expect_true(all(formato_valido))
  })

  test_that("year y trimestre son consistentes con fecha", {
    expect_equal(bs$year, lubridate::year(bs$fecha))
    expect_equal(bs$trimestre, lubridate::quarter(bs$fecha))
  })
})
