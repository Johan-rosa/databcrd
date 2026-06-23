tasas_activas <- get_tasas_activas()

test_that("Pasivas: there aren't dates in the future", {
  testthat::expect_true(max(tasas_activas$fecha) <= lubridate::today())
})

test_that("There aren't haps between dates", {
  test_nmonths <- tasas_activas |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = fecha == lag_fecha + months(1))

  testthat::expect_true(all(test_nmonths$one_month_diff))
})

test_that("No missing columns", {
  any_missing_columns <- tasas_activas |>
    sapply(function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(any_missing_columns)
})

test_that("No missing rows", {
  empty_rows <- tasas_activas |>
    apply(MARGIN = 1, FUN = function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(empty_rows)
})

tasas_pasivas <- get_tasas_pasivas()

test_that("Pasivas: there aren't dates in the future", {
  testthat::expect_true(max(tasas_pasivas$fecha) <= lubridate::today())
})

test_that("There aren't haps between dates", {
  test_nmonths <- tasas_pasivas |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
    dplyr::filter(!is.na(lag_fecha)) |>
    dplyr::mutate(one_month_diff = fecha == lag_fecha + months(1))

  testthat::expect_true(all(test_nmonths$one_month_diff))
})

test_that("No missing columns", {
  any_missing_columns <- tasas_pasivas |>
    sapply(function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(any_missing_columns)
})

test_that("No missing rows", {
  empty_rows <- tasas_pasivas |>
    apply(MARGIN = 1, FUN = function(x) all(is.na(x))) |>
    all()

  testthat::expect_false(empty_rows)
})





tasas <- get_tasas_diarias(2024)

# --- Estructura y columnas ---

test_that("Tiene las columnas esperadas", {
  expect_named(
    tasas,
    c("fecha", "year", "mes", "day", "tipo_tasa", "moneda", "grupo", "condicion", "detalle", "tasa"),
    ignore.order = TRUE
  )
})

test_that("No hay columnas completamente vacías", {
  any_empty <- tasas |> sapply(\(x) all(is.na(x))) |> any()
  expect_false(any_empty)
})

test_that("No hay filas completamente vacías", {
  any_empty <- tasas |> apply(1, \(x) all(is.na(x))) |> any()
  expect_false(any_empty)
})

# --- Fechas ---

test_that("No hay fechas en el futuro", {
  expect_true(max(tasas$fecha) <= lubridate::today())
})

test_that("Las fechas pertenecen al año solicitado", {
  expect_true(all(lubridate::year(tasas$fecha) == 2024))
})

# --- Valores permitidos ---

test_that("tipo_tasa solo contiene valores válidos", {
  expect_in(unique(tasas$tipo_tasa), c("Activa", "Pasiva"))
})

test_that("moneda solo contiene valores válidos", {
  expect_in(unique(tasas$moneda), c("DOP", "USD"))
})

test_that("condicion solo contiene valores válidos", {
  expect_in(unique(tasas$condicion), c("General", "Preferencial"))
})

test_that("Las cuatro combinaciones tipo_tasa x moneda están presentes", {
  combos <- tasas |>
    dplyr::distinct(tipo_tasa, moneda) |>
    nrow()
  expect_equal(combos, 4L)
})

# --- Tasas ---

test_that("Las tasas son numéricas y positivas", {
  expect_type(tasas$tasa, "double")
  expect_true(all(tasas$tasa > 0))
})

test_that("Las tasas están en rangos razonables (0-100)", {
  expect_true(all(dplyr::between(tasas$tasa, 0, 100)))
})

# --- Filtros ---

test_that("filtro_moneda filtra correctamente", {
  resultado <- get_tasas_diarias(2024, filtro_moneda = "DOP")
  expect_true(all(resultado$moneda == "DOP"))
})

test_that("filtro_tipo_tasa filtra correctamente", {
  resultado <- get_tasas_diarias(2024, filtro_tipo_tasa = "Activa")
  expect_true(all(resultado$tipo_tasa == "Activa"))
})

test_that("filtro_grupo filtra correctamente", {
  resultado <- get_tasas_diarias(2024, filtro_grupo = "Plazo")
  expect_true(all(resultado$grupo == "Plazo"))
})

test_that("filtro_condicion filtra correctamente", {
  resultado <- get_tasas_diarias(2024, filtro_condicion = "General")
  expect_true(all(resultado$condicion == "General"))
})

test_that("Argumentos inválidos lanzan error", {
  expect_error(get_tasas_diarias(2024, filtro_moneda    = "EUR"))
  expect_error(get_tasas_diarias(2024, filtro_tipo_tasa = "Nominal"))
  expect_error(get_tasas_diarias(2024, filtro_condicion = "Otro"))
  expect_error(get_tasas_diarias(2024, filtro_grupo     = "Otro"))
})
