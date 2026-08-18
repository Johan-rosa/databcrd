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


describe("Testing deuda publica por sector", {
  ds <- deuda_publica_by_sector()

  it("returns a dataframe", expect_s3_class(ds, "data.frame"))

  it("has the correct structure", {
    expect_equal(ncol(ds), 11)
    # Orden real de columnas segun como se construyen en la funcion:
    # nivel/tipo_deuda/concepto se insertan con .before = concepto en el
    # primer mutate(); grupo/sector se insertan despues con
    # .before = tipo_deuda. Si esto falla, probablemente cambio el orden
    # de los mutate() en la funcion y hay que actualizar este vector.
    expect_named(
      ds,
      c(
        "nivel", "grupo", "sector", "tipo_deuda", "concepto", "fecha",
        "year", "trimestre", "monto", "gdp", "as_gdp_percent"
      )
    )
    expect_s3_class(ds$fecha, "Date")
    expect_type(ds$nivel, "character")
    expect_type(ds$grupo, "character")
    expect_type(ds$sector, "character")
    expect_type(ds$tipo_deuda, "character")
    expect_type(ds$concepto, "character")
    expect_type(ds$year, "double")
    expect_type(ds$trimestre, "integer")
    expect_type(ds$monto, "double")
    expect_type(ds$gdp, "double")
    expect_type(ds$as_gdp_percent, "double")
  })

  it("doesn't have dates in the future", {
    expect_true(max(ds$fecha) <= Sys.Date())
  })

  it("doesn't have empty rows", {
    expect_equal(
      dplyr::filter(ds, dplyr::if_all(dplyr::everything(), is.na)) |>
        nrow(),
      0
    )
  })

  it("doesn't have NA en monto, gdp, concepto, nivel ni fecha", {
    expect_false(anyNA(ds$monto))
    expect_false(anyNA(ds$gdp))
    expect_false(anyNA(ds$concepto))
    expect_false(anyNA(ds$nivel))
    expect_false(anyNA(ds$fecha))
  })

  test_that("no hay filas duplicadas para la misma combinacion nivel+grupo+sector+tipo_deuda+fecha", {
    counts <- dplyr::count(ds, nivel, grupo, sector, tipo_deuda, fecha, sort = TRUE)
    expect_equal(max(counts$n), 1L)
  })

  it("nivel solo toma los cinco valores esperados", {
    expect_setequal(
      unique(ds$nivel),
      c("total", "grupo", "sector", "fuente", "detalle")
    )
  })

  it("tipo_deuda solo toma los valores esperados (incluyendo NA)", {
    expect_setequal(
      unique(ds$tipo_deuda),
      c("Consolidada", "Externa", "Interna")
    )
  })

  it("grupo solo toma los valores esperados (incluyendo NA)", {
    expect_setequal(
      unique(ds$grupo),
      c(
        NA_character_,
        "Sector Público No Financiero",
        "Sector Público Financiero"
      )
    )
  })

  it("sector solo toma los valores esperados (incluyendo NA)", {
    expect_setequal(
      unique(ds$sector),
      c(NA_character_, "Gobierno Central", "Resto del SPNF", "Banco Central")
    )
  })

  test_that("grupo es NA unicamente en nivel 'total'", {
    expect_true(all(is.na(ds$grupo[ds$nivel == "total"])))
    expect_false(anyNA(ds$grupo[ds$nivel != "total"]))
  })

  test_that("sector es NA unicamente en nivel 'total' o 'grupo'", {
    expect_true(all(is.na(ds$sector[ds$nivel %in% c("total", "grupo")])))
    expect_false(anyNA(ds$sector[!ds$nivel %in% c("total", "grupo")]))
  })

  test_that("year y trimestre son consistentes con fecha", {
    expect_equal(ds$year, lubridate::year(ds$fecha))
    expect_equal(ds$trimestre, lubridate::quarter(ds$fecha))
  })

  test_that("as_gdp_percent es consistente con monto y gdp", {
    esperado <- ds$monto / ds$gdp * 100
    expect_equal(ds$as_gdp_percent, esperado)
  })

  test_that("el total consolidado por fecha es igual a la suma de los grupos, neto de la deuda intragubernamental", {
    # el BCRD resta la deuda intragubernamental al consolidar (nota 2/ del
    # archivo); sumar los grupos sin restarla la cuenta dos veces, ya que
    # es deuda del Gobierno Central frente al Banco Central (aparece en
    # ambos grupos). Verificado contra el archivo: SPNF + SPF - intra ==
    # consolidada, mientras que SPNF + SPF por si solo NO coincide.
    grupos <- ds |>
      dplyr::filter(nivel == "grupo") |>
      dplyr::group_by(fecha) |>
      dplyr::summarise(monto_grupos = sum(monto), .groups = "drop")
    intra <- ds |>
      dplyr::filter(nivel == "detalle") |>
      dplyr::select(fecha, monto_intra = monto)
    total_consolidada <- ds |>
      dplyr::filter(nivel == "total", tipo_deuda == "Consolidada") |>
      dplyr::select(fecha, monto_total = monto)

    comparacion <- grupos |>
      dplyr::left_join(intra, by = "fecha") |>
      dplyr::left_join(total_consolidada, by = "fecha")
    expect_equal(
      comparacion$monto_grupos - comparacion$monto_intra,
      comparacion$monto_total,
      tolerance = 1e-6
    )
  })

  test_that("el total interna consolidada es igual a la deuda interna por sector, neta de intragubernamental", {
    interna_fuentes <- ds |>
      dplyr::filter(nivel == "fuente", tipo_deuda == "Interna") |>
      dplyr::group_by(fecha) |>
      dplyr::summarise(monto_interna_fuentes = sum(monto), .groups = "drop")
    intra <- ds |>
      dplyr::filter(nivel == "detalle") |>
      dplyr::select(fecha, monto_intra = monto)
    interna_total <- ds |>
      dplyr::filter(nivel == "total", tipo_deuda == "Interna") |>
      dplyr::select(fecha, monto_total = monto)

    comparacion <- interna_fuentes |>
      dplyr::left_join(intra, by = "fecha") |>
      dplyr::left_join(interna_total, by = "fecha")
    expect_equal(
      comparacion$monto_interna_fuentes - comparacion$monto_intra,
      comparacion$monto_total,
      tolerance = 1e-6
    )
  })

  test_that("el total externa consolidada es igual a la suma de la deuda externa por sector (sin ajuste)", {
    # a diferencia de la interna, la deuda externa no tiene componente
    # intragubernamental que restar
    externa_fuentes <- ds |>
      dplyr::filter(nivel == "fuente", tipo_deuda == "Externa") |>
      dplyr::group_by(fecha) |>
      dplyr::summarise(monto_externa_fuentes = sum(monto), .groups = "drop")
    externa_total <- ds |>
      dplyr::filter(nivel == "total", tipo_deuda == "Externa") |>
      dplyr::select(fecha, monto_total = monto)

    comparacion <- dplyr::left_join(externa_fuentes, externa_total, by = "fecha")
    expect_equal(
      comparacion$monto_externa_fuentes,
      comparacion$monto_total,
      tolerance = 1e-6
    )
  })

  test_that("cada grupo es igual a la suma de sus sectores", {
    sectores <- ds |>
      dplyr::filter(nivel == "sector") |>
      dplyr::group_by(grupo, fecha) |>
      dplyr::summarise(monto_sectores = sum(monto), .groups = "drop")
    grupo_total <- ds |>
      dplyr::filter(nivel == "grupo") |>
      dplyr::select(grupo, fecha, monto_grupo = monto)

    comparacion <- dplyr::left_join(sectores, grupo_total, by = c("grupo", "fecha"))
    expect_equal(comparacion$monto_sectores, comparacion$monto_grupo, tolerance = 1e-6)
  })

  test_that("cada sector es igual a la suma de sus fuentes (externa + interna)", {
    fuentes <- ds |>
      dplyr::filter(nivel == "fuente") |>
      dplyr::group_by(grupo, sector, fecha) |>
      dplyr::summarise(monto_fuentes = sum(monto), .groups = "drop")
    sector_total <- ds |>
      dplyr::filter(nivel == "sector") |>
      dplyr::select(grupo, sector, fecha, monto_sector = monto)

    comparacion <- dplyr::left_join(
      fuentes, sector_total,
      by = c("grupo", "sector", "fecha")
    )
    expect_equal(comparacion$monto_fuentes, comparacion$monto_sector, tolerance = 1e-6)
  })

  it("el total consolidado es igual a la suma de externa + interna", {
    total_wide <- ds |>
      dplyr::filter(nivel == "total") |>
      dplyr::select(fecha, tipo_deuda, monto) |>
      tidyr::pivot_wider(names_from = tipo_deuda, values_from = monto)

    expect_equal(
      total_wide$Consolidada,
      total_wide$Externa + total_wide$Interna,
      tolerance = 1e-6
    )
  })
})
