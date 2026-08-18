
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


describe("Testing balanza de pagos", {
  bp <- balanza_pagos()
  cols_catalogo <- c("code", "nivel", "code_padre", "concepto", "naturaleza", "cuenta")

  it("returns a dataframe", expect_s3_class(bp, "data.frame"))

  it("has the correct structure", {
    expect_true(all(cols_catalogo %in% names(bp)))
    expect_type(bp$code, "character")
    expect_type(bp$nivel, "integer")
    expect_type(bp$code_padre, "character")
    expect_type(bp$concepto, "character")
    expect_type(bp$naturaleza, "character")
    expect_type(bp$cuenta, "character")
  })

  it("los nombres de las columnas de fecha son fechas validas y no estan en el futuro", {
    fechas <- bp$fecha
    expect_false(anyNA(fechas))
    expect_true(max(fechas) <= Sys.Date())
  })

  it("doesn't have empty rows in the catalog columns", {
    expect_equal(
      dplyr::filter(bp, dplyr::if_all(dplyr::all_of(c("code", "concepto", "naturaleza", "cuenta")), is.na)) |>
        nrow(),
      0
    )
  })

  test_that("code es unico", {
    cnt <- bp |> dplyr::count(code, fecha)
    expect_equal(max(cnt$n), 1L)
  })

  it("naturaleza solo toma los tres valores esperados", {
    expect_setequal(unique(bp$naturaleza), c("Credito", "Debito", "Saldo"))
  })

  test_that("los codigos tienen formato jerarquico decimal (1, 1.1, 1.1.1.1, ...)", {
    expect_true(all(grepl("^[0-9]+(\\.[0-9]+)*$", bp$code)))
  })

  test_that("nivel coincide con la profundidad del codigo", {
    nivel_esperado <- stringr::str_count(bp$code, "\\.") + 1L
    expect_equal(bp$nivel, nivel_esperado)
  })

  test_that("code_padre es consistente con code (quitar el ultimo segmento) o NA en nivel 1", {
    tiene_padre <- stringr::str_detect(bp$code, "\\.")
    padre_esperado <- dplyr::if_else(
      tiene_padre,
      stringr::str_remove(bp$code, "\\.[^.]+$"),
      NA_character_
    )
    expect_equal(bp$code_padre, padre_esperado)
    # todo code_padre no-NA debe existir como code en algun lado
    expect_true(all(stats::na.omit(bp$code_padre) %in% bp$code))
  })

  test_that("cuenta solo toma los valores de nivel 1 esperados", {
    cuentas_esperadas <- c(
      "Cuenta Corriente", "Cuenta de Capital", "Prestamo Neto",
      "Cuenta Financiera", "Saldo", "Errores y Omisiones", "Financiamiento"
    )
    expect_setequal(unique(bp$cuenta), cuentas_esperadas)
  })
})

describe("Testing los filtros de balanza_pagos()", {
  bp <- balanza_pagos()

  test_that("filtro_codigo devuelve solo los codigos pedidos", {
    codigos <- c("1.1.1.1", "1.1.1.2")
    res <- balanza_pagos(filtro_codigo = codigos)
    expect_setequal(res$code, codigos)
    expect_equal(res, dplyr::filter(bp, code %in% codigos))
  })

  test_that("filtro_naturaleza devuelve solo la naturaleza pedida", {
    res <- balanza_pagos(filtro_naturaleza = "Credito")
    expect_true(all(res$naturaleza == "Credito"))
    expect_equal(res, dplyr::filter(bp, naturaleza == "Credito"))
  })

  test_that("filtro_cuenta devuelve solo la cuenta pedida", {
    res <- balanza_pagos(filtro_cuenta = "Cuenta Financiera")
    expect_true(all(res$cuenta == "Cuenta Financiera"))
    expect_equal(res, dplyr::filter(bp, cuenta == "Cuenta Financiera"))
  })

  test_that("filtro_nivel devuelve solo los niveles pedidos", {
    res <- balanza_pagos(filtro_nivel = 1:2)
    expect_true(all(res$nivel %in% 1:2))
    expect_equal(res, dplyr::filter(bp, nivel %in% 1:2))
  })

  test_that("filtro_concepto hace match parcial insensible a mayusculas", {
    res <- balanza_pagos(filtro_concepto = "invers")
    expect_true(nrow(res) > 1) # varias categorias distintas contienen "invers"
    expect_true(all(grepl("invers", res$concepto, ignore.case = TRUE)))
  })

  test_that("solo_hojas descarta las filas de subtotal", {
    res <- balanza_pagos(solo_hojas = TRUE)
    expect_true(nrow(res) < nrow(bp))
    expect_true(all(!res$code %in% res$code_padre))
    # ninguna fila hoja deberia ser code_padre de otra fila hoja
    expect_true(all(!(res$code %in% bp$code_padre)))
  })

  test_that("los filtros se combinan con AND, no OR", {
    res <- balanza_pagos(filtro_cuenta = "Cuenta Financiera", solo_hojas = TRUE)
    esperado <- bp |>
      dplyr::filter(cuenta == "Cuenta Financiera", !code %in% code_padre)
    expect_equal(res, esperado)
  })

  test_that("un filtro que no matchea nada devuelve 0 filas con warning", {
    expect_warning(
      res <- balanza_pagos(filtro_codigo = "99.99"),
      "no dejaron ninguna fila"
    )
    expect_equal(nrow(res), 0)
  })
})
