describe("Testing deuda publica por fuente", {
  dp <- deuda_publica_by_fuente()

  it("returns a dataframe", expect_s3_class(dp, "data.frame"))

  it("has the correct structure", {
    expect_equal(ncol(dp), 10)
    expect_named(
      dp,
      c(
        "codigo", "tipo_deuda", "nivel", "fuente", "fecha",
        "year", "trimestre", "monto", "gdp", "as_gdp_percent"
      )
    )
    expect_s3_class(dp$fecha, "Date")
    expect_type(dp$codigo, "character")
    expect_type(dp$tipo_deuda, "character")
    expect_type(dp$nivel, "character")
    expect_type(dp$fuente, "character")
    expect_type(dp$year, "double")
    expect_type(dp$trimestre, "integer")
    expect_type(dp$monto, "double")
    expect_type(dp$gdp, "double")
    expect_type(dp$as_gdp_percent, "double")
  })

  it("doesn't have dates in the future", {
    expect_true(max(dp$fecha) <= Sys.Date())
  })

  it("doesn't have empty rows", {
    expect_equal(
      dplyr::filter(dp, dplyr::if_all(dplyr::everything(), is.na)) |>
        nrow(),
      0
    )
  })

  it("doesn't have NA en monto ni en gdp", {
    expect_false(anyNA(dp$monto))
    expect_false(anyNA(dp$gdp))
  })

  test_that("no hay filas duplicadas para la misma combinacion codigo+fuente+fecha", {
    counts <- dplyr::count(dp, codigo, tipo_deuda, fuente, fecha, sort = TRUE)
    expect_equal(max(counts$n), 1L)
  })

  it("nivel solo toma los tres valores esperados", {
    expect_setequal(unique(dp$nivel), c("total", "grupo", "detalle"))
  })

  it("tipo_deuda solo toma los tres valores esperados", {
    expect_setequal(
      unique(dp$tipo_deuda),
      c("Consolidada", "Externa", "Interna neta")
    )
  })

  test_that("los codigos tienen el formato esperado (0, A, B, o 1-4)", {
    codigos <- unique(dp$codigo)
    formato_valido <- codigos %in% c("0", "A", "B", "1", "2", "3", "4")
    # si esto falla, el BCRD probablemente agrego o quito una fuente
    expect_true(all(formato_valido))
  })

  test_that("nivel es consistente con codigo", {
    # regresion especifica: la fila " A. DEUDA EXTERNA..." tiene un espacio
    # inicial en el archivo fuente que "B. DEUDA INTERNA NETA..." no tiene;
    # si str_squish() no se aplica antes de extraer codigo, esta fila queda
    # con codigo = NA y nivel = "total" en lugar de "grupo"
    nivel_esperado <- dplyr::case_when(
      dp$codigo == "0" ~ "total",
      dp$codigo %in% c("A", "B") ~ "grupo",
      TRUE ~ "detalle"
    )
    expect_equal(dp$nivel, nivel_esperado)
    expect_true(all(dp$nivel[dp$codigo == "A"] == "grupo"))
    expect_true(all(dp$nivel[dp$codigo == "B"] == "grupo"))
  })

  test_that("year y trimestre son consistentes con fecha", {
    expect_equal(dp$year, lubridate::year(dp$fecha))
    expect_equal(dp$trimestre, lubridate::quarter(dp$fecha))
  })

  test_that("as_gdp_percent es consistente con monto y gdp", {
    esperado <- dp$monto / dp$gdp * 100
    expect_equal(dp$as_gdp_percent, esperado)
  })

  it("el total consolidado es mayor o igual que cada grupo, por fecha", {
    totales <- dp |>
      dplyr::filter(nivel == "total") |>
      dplyr::select(fecha, monto_total = monto)
    grupos <- dp |>
      dplyr::filter(nivel == "grupo") |>
      dplyr::select(fecha, monto)

    comparacion <- dplyr::left_join(grupos, totales, by = "fecha")
    expect_true(all(comparacion$monto <= comparacion$monto_total))
  })
})


