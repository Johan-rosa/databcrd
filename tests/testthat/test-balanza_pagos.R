# tests/testthat/test-balanza_servicios-integracion.R
#
# Tests de "contrato de datos": descargan el archivo REAL del BCRD y
# verifican tanto la estructura cruda del Excel (para detectar cambios en
# el layout fuente antes de que rompan la función en silencio) como la
# forma del resultado de balanza_servicios().
#
# No usan mocks a propósito: el objetivo es detectar cuando el BCRD
# cambia el archivo (agrega/quita columnas, renombra categorías, cambia
# el número de filas de encabezado, etc.). Por eso:
#   - se saltan en CRAN (skip_on_cran)
#   - se saltan si no hay conexión (skip_if_offline)
#   - conviene correrlas aparte (p. ej. un job de CI programado, no en
#     cada PR) porque dependen de la disponibilidad del sitio del BCRD.
#
# Requiere (Suggests): testthat (>= 3.0.0), curl (para skip_if_offline)

url_balanza <- paste0(
  "https://cdn.bancentral.gov.do/documents/estadisticas/",
  "sector-externo/documents/Balanza-de-Servicios-trimestral.xlsx"
)

# ---------------------------------------------------------------------
# Cache de la descarga: se descarga UNA sola vez para todo el archivo de
# tests (tanto la version cruda como el resultado ya transformado), no
# una vez por test_that().
# ---------------------------------------------------------------------
.cache <- new.env(parent = emptyenv())

obtener_raw <- function() {
  if (is.null(.cache$raw)) {
    testthat::skip_on_cran()
    testthat::skip_if_offline(host = "cdn.bancentral.gov.do")

    file_path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(file_path))
    download_file(url_balanza, file_path)

    .cache$raw <- readxl::read_excel(file_path, skip = 7, col_names = FALSE) |>
      suppressMessages()
  }
  .cache$raw
}

obtener_resultado <- function() {
  if (is.null(.cache$res)) {
    testthat::skip_on_cran()
    testthat::skip_if_offline(host = "cdn.bancentral.gov.do")
    .cache$res <- balanza_servicios()
  }
  .cache$res
}

# ---------------------------------------------------------------------
# Estructura CRUDA del archivo (antes de transformar) - detectan cambios
# de layout en la fuente del BCRD.
# ---------------------------------------------------------------------

test_that("el archivo del BCRD se descarga y tiene al menos una columna de concepto + datos", {
  raw <- obtener_raw()
  expect_gt(ncol(raw), 1)
  expect_gt(nrow(raw), 0)
})

test_that("la primera columna del archivo crudo es texto (los conceptos), no numerica", {
  raw <- obtener_raw()
  primera_col <- raw[[1]]
  expect_true(is.character(primera_col))
  expect_false(any(is.na(primera_col) | primera_col == ""))
})

test_that("el numero de columnas trimestrales esta dentro del rango esperado", {
  # dates se genera con length.out = ncol(raw_data) - 1 asumiendo que la
  # primera columna trimestral es 2010 T1. Si el BCRD inserta o elimina
  # una columna, este test lo detecta ANTES de que produzca fechas mal
  # alineadas en silencio.
  raw <- obtener_raw()
  n_trimestres_reportados <- ncol(raw) - 1

  inicio <- lubridate::make_date(2010, 1, 1)
  trimestres_transcurridos <- lubridate::interval(inicio, Sys.Date()) %/% months(3) + 1

  # margen: hasta 4 trimestres de rezago en publicacion, y 1 de margen
  # por si ya salio el trimestre en curso
  esperado_min <- trimestres_transcurridos - 4
  esperado_max <- trimestres_transcurridos + 1

  expect_gte(n_trimestres_reportados, esperado_min)
  expect_lte(n_trimestres_reportados, esperado_max)
})

test_that("las filas de bloque (CREDITO/DEBITO/SALDO) siguen presentes en el archivo crudo", {
  # Si el BCRD cambia el texto exacto de estas etiquetas, la extraccion
  # de naturaleza (str_extract "CREDITO|DEBITO|SALDO") deja de
  # funcionar en silencio (todo queda NA).
  raw <- obtener_raw()
  conceptos <- raw[[1]]

  expect_true(any(grepl("CREDITO", conceptos, fixed = TRUE)))
  expect_true(any(grepl("DEBITO", conceptos, fixed = TRUE)))
  expect_true(any(grepl("SALDO", conceptos, fixed = TRUE)))
})

# ---------------------------------------------------------------------
# Resultado de balanza_servicios() - forma y sanidad de los datos ya
# transformados.
# ---------------------------------------------------------------------

test_that("balanza_servicios() corre sin error contra el archivo real", {
  res <- obtener_resultado()
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
})

test_that("el resultado tiene exactamente las columnas esperadas, en tipo y cantidad", {
  res <- obtener_resultado()

  expect_equal(ncol(res), 8)
  expect_named(
    res,
    c("code", "naturaleza", "concepto", "fecha", "year", "trimestre",
      "monto", "monto_acumulado")
  )
  expect_s3_class(res$fecha, "Date")
  expect_type(res$year, "double")
  expect_type(res$trimestre, "double")
  expect_type(res$monto, "double")
  expect_type(res$monto_acumulado, "double")
})

test_that("no hay filas vacias: las columnas clave no tienen NA", {
  res <- obtener_resultado()

  expect_false(any(is.na(res$code)))
  expect_false(any(is.na(res$naturaleza)))
  expect_false(any(is.na(res$concepto) | res$concepto == ""))
  expect_false(any(is.na(res$fecha)))
  expect_false(any(is.na(res$monto)))
})

test_that("no hay fechas en el futuro", {
  res <- obtener_resultado()
  expect_true(all(res$fecha <= Sys.Date()))
})

test_that("no hay filas duplicadas para la misma combinacion code+naturaleza+fecha", {
  res <- obtener_resultado()
  claves <- paste(res$code, res$naturaleza, res$fecha)
  expect_equal(anyDuplicated(claves), 0L)
})

test_that("naturaleza solo toma los tres valores esperados", {
  res <- obtener_resultado()
  expect_setequal(unique(res$naturaleza), c("Credito", "Debito", "Saldo"))
})

test_that("los codigos de categoria tienen el formato esperado (letra+punto o 1/2/3)", {
  res <- obtener_resultado()
  codigos <- unique(res$code)
  formato_valido <- grepl("^[A-Z]{1,3}\\.$", codigos) | codigos %in% c("1", "2", "3")
  # si esto falla, el BCRD probablemente agrego una categoria con un
  # patron de codigo distinto
  expect_true(all(formato_valido))
})

test_that("year y trimestre son consistentes con fecha", {
  res <- obtener_resultado()
  expect_equal(res$year, lubridate::year(res$fecha))
  expect_equal(res$trimestre, lubridate::quarter(res$fecha))
})
