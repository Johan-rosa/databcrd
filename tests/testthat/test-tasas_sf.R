tasas_activas_bm  <- get_tasas_activas("bm")
tasas_activas_aap <- get_tasas_activas("aap")
tasas_activas_bac <- get_tasas_activas("bac")
tasas_activas_cc  <- get_tasas_activas("cc")

tasas_pasivas_bm  <- get_tasas_pasivas("bm")
tasas_pasivas_aap <- get_tasas_pasivas("aap")
tasas_pasivas_bac <- get_tasas_pasivas("bac")
tasas_pasivas_cc  <- get_tasas_pasivas("cc")

test_that("Pasivas: there aren't dates in the future", {
  testthat::expect_true(max(tasas_activas_bm$fecha)  <= lubridate::today())
  testthat::expect_true(max(tasas_activas_aap$fecha) <= lubridate::today())
  testthat::expect_true(max(tasas_activas_bac$fecha) <= lubridate::today())
  testthat::expect_true(max(tasas_activas_cc$fecha)  <= lubridate::today())

  testthat::expect_true(max(tasas_pasivas_bm$fecha)  <= lubridate::today())
  testthat::expect_true(max(tasas_pasivas_aap$fecha) <= lubridate::today())
  testthat::expect_true(max(tasas_pasivas_bac$fecha) <= lubridate::today())
  testthat::expect_true(max(tasas_pasivas_cc$fecha)  <= lubridate::today())
})

test_that("There aren't haps between dates", {
  test_months <- \(tasas) {
    tasas |>
      dplyr::arrange(fecha) |>
      dplyr::mutate(lag_fecha = dplyr::lag(fecha)) |>
      dplyr::filter(!is.na(lag_fecha)) |>
      dplyr::mutate(one_month_diff = fecha == lag_fecha + months(1))
  }

  testthat::expect_true(all(test_months(tasas_activas_bm)$one_month_diff))
  testthat::expect_true(all(test_months(tasas_activas_aap)$one_month_diff))
  testthat::expect_true(all(test_months(tasas_activas_bac)$one_month_diff))
  testthat::expect_true(all(test_months(tasas_activas_cc)$one_month_diff))

  testthat::expect_true(all(test_months(tasas_pasivas_bm)$one_month_diff))
  testthat::expect_true(all(test_months(tasas_pasivas_aap)$one_month_diff))
  testthat::expect_true(all(test_months(tasas_pasivas_bac)$one_month_diff))

  # Este archivo not tiene valores para el 2018. Por ese ese test fallaría
  # testthat::expect_true(all(test_months(tasas_pasivas_cc)$one_month_diff))
})

test_that("No missing columns", {
  any_missing_columns <- \(tasas) {
    tasas |>
      sapply(function(x) all(is.na(x))) |>
      all()
  }

  testthat::expect_false(any_missing_columns(tasas_activas_bm))
  testthat::expect_false(any_missing_columns(tasas_activas_aap))
  testthat::expect_false(any_missing_columns(tasas_activas_bac))
  testthat::expect_false(any_missing_columns(tasas_activas_cc))

  testthat::expect_false(any_missing_columns(tasas_pasivas_bm))
  testthat::expect_false(any_missing_columns(tasas_pasivas_aap))
  testthat::expect_false(any_missing_columns(tasas_pasivas_bac))
  testthat::expect_false(any_missing_columns(tasas_pasivas_cc))
})

test_that("No missing rows", {
  empty_rows <- \(tasas) {
    tasas |>
      apply(MARGIN = 1, FUN = function(x) all(is.na(x))) |>
      all()
  }

  testthat::expect_false(empty_rows(tasas_activas_bm))
  testthat::expect_false(empty_rows(tasas_activas_aap))
  testthat::expect_false(empty_rows(tasas_activas_bac))
  testthat::expect_false(empty_rows(tasas_activas_cc))

  testthat::expect_false(empty_rows(tasas_pasivas_bm))
  testthat::expect_false(empty_rows(tasas_pasivas_aap))
  testthat::expect_false(empty_rows(tasas_pasivas_bac))
  testthat::expect_false(empty_rows(tasas_pasivas_cc))
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


# Tasas reales ---------------------------------------------------------

tasas_real_mensual_long <- get_tasas_reales("mensual", "long")
tasa_real_anual_wide  <- get_tasas_reales("anual", "wide")
tasas_real_anual_long <- get_tasas_reales("anual", "long")

describe('get_tasas_reales("mensual", "wide")', {
  tasa_real  <- get_tasas_reales("mensual", "wide")

  it("No tiene fechas duplicadas", {
    cnt <- dplyr::count(tasa_real, date)
    expect_equal(max(cnt$n), 1)
  })

  it("No tiene fechas en el futuro", {
    expect_true(max(tasa_real$date) < lubridate::today())
  })

  it("Tiene las columnas adecuadas", {
    names <- c(
      "date", "year", "mes",
      paste(
        rep(c("bm", "aap", "bac", "cc"), each = 2),
        rep(c("activa", "pasiva"), times = 4),
        sep = "_"
      ),
      "inflacion"
    )

    expect_named(tasa_real, names)
  })

  it("Tiene 12 meses por año", {
    current_year <- lubridate::year(Sys.Date())

    ns_complete <- tasa_real |>
      dplyr::filter(year < current_year) |>
      dplyr::count(year)

    expect_true(all(ns_complete$n == 12L))
  })
})

describe('get_tasas_reales("anual", "wide")', {
  tasa_real  <- get_tasas_reales("anual", "wide")

  it("No tiene fechas duplicadas", {
    cnt <- dplyr::count(tasa_real, year)
    expect_equal(max(cnt$n), 1)
  })

  it("No tiene fechas en el futuro", {
    expect_true(max(tasa_real$year) < lubridate::year(lubridate::today()))
  })

  it("No hay años perdidos", {
    ycnt <- tasa_real |>
      dplyr::arrange(year) |>
      dplyr::mutate(diff_year = year - dplyr::lag(year, default = 2007))

    expect_true(all(ycnt$diff_year == 1))
  })

  it("Tiene las columnas adecuadas", {
    names <- c(
      "year",
      paste(
        rep(c("bm", "aap", "bac", "cc"), each = 2),
        rep(c("activa", "pasiva"), times = 4),
        sep = "_"
      ),
      "inflacion"
    )

    expect_named(tasa_real, names)
  })
})
