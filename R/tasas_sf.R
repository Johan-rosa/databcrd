.download_bcrd_file <- function(url, dest_path) {
  tryCatch({
    utils::download.file(url, dest_path, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    stop(glue::glue("Error al descargar desde {url}. Verifique su conexión de red o si el Banco Central cambió el enlace. Detalle: {e$message}"), call. = FALSE)
  })
}

params <- dplyr::lst(
  bm_pasiva_2007 = dplyr::lst(
    endpoint = "tbm_pasiva-1991-2007.xls",
    fileext  = ".xls",
    col_names =  c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_m360d",
      "tp_ps", "tp_pp", "tp_dep_ahorros", "tp_preferencial", "tp_general",
      "tp_interbancarios"
    )
  ),
  bm_pasiva_2012 = list(
    endpoint = "tbm_pasivad-2008-2012.xls",
    fileext  = ".xls",
    col_names = c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_2a",
      "tp_pp", "tp_ps", "tp_dep_ahorros", "tp_general",
      "tp_preferencial", "tp_interbancarios"
    )
  ),
  bm_pasiva_2016 = list(
    endpoint = "tbm_pasivad-2013-2016.xlsx",
    fileext = ".xlsx",
    col_names = c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_2a",
      "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros", "tp_general",
      "tp_preferencial", "tp_interbancarios"
    )
  ),
  bm_pasiva_today = list(
    endpoint = "tbm_pasivad.xlsx",
    fileext  = ".xlsx",
    col_names = c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_2a",
      "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros", "tp_general",
      "tp_preferencial", "tp_interbancarios"
    )
  ),
  bm_activa_2007 = list(
    endpoint = "tbm_activa-1991-2007.xls",
    fileext  = ".xls",
    col_names = c(
      "mes", "ta_90d", "ta_180d", "ta_360d", "ta_m360d", "ta_2a", "ta_5a", "ta_m5a",
      "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio", "ta_consumo", "ta_hipotecario"
    )
  ),
  bm_activa_2012 = list(
    endpoint = "tbm_activad-2008-2012.xls",
    fileext  = ".xls",
    col_names = c(
      "mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a",
      "ta_pp", "ta_ps", "ta_preferencial", "ta_comercio", "ta_consumo",
      "ta_hipotecario"
    )
  ),
  bm_activa_2016 = list(
    endpoint = "tbm_activad-2013-2016.xlsx",
    fileext  = ".xlsx",
    col_names = c(
      "mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a",
      "ta_pp", "ta_ps", "ta_preferencial", "ta_comercio", "ta_consumo",
      "ta_hipotecario"
    )
  ),
  bm_activa_today = list(
    endpoint = "tbm_activad.xlsx",
    fileext = ".xlsx",
    col_names = c(
      "mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a",
      "ta_pp", "ta_ps", "ta_comercio", "ta_consumo", "ta_hipotecario",
      "ta_preferencial", "ta_preferencial_comercio", "ta_preferencial_consumo",
      "ta_preferencial_hipotecario"
    )
  ),

  aap_pasiva_2007 = list(
    endpoint = "taap_pasiva.xls",
    fileext = ".xls",
    col_names = c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_m360d", "tp_pp")
  ),
  # Reusing some names after this point
  aap_pasiva_2012 = list(
    endpoint = "taap_pasivad-2008-2012.xls",
    fileext = ".xls",
    col_names = bm_pasiva_2012$col_names
  ),
  aap_pasiva_2016 = list(
    endpoint = "taap_pasivad-2013-2016.xlsx",
    fileext = ".xlsx",
    col_names = bm_pasiva_2016$col_names
  ),
  aap_pasiva_today = list(
    endpoint = "taap_pasivad.xlsx",
    fileext = ".xlsx",
    col_names = bm_pasiva_today$col_names
  ),

  aap_activa_2007 = list(
    endpoint = "taap_activa.xls",
    fileext  = ".xls",
    col_names =  c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a", "ta_pp")
  ),
  aap_activa_2012 = list(
    endpoint = "taap_activad-2008-2012.xls",
    fileext  = ".xls",
    col_names = c(
      "mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a",
      "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio", "ta_consumo", "ta_hipotecario"
    )
  ),
  aap_activa_2016 = list(
    endpoint = "taap_activad-2013-2016.xlsx",
    fileext  = ".xlsx",
    col_names = aap_activa_2012$col_names
  ),
  aap_activa_today = list(
    endpoint = "taap_activad.xlsx",
    fileext = ".xlsx",
    col_names = bm_activa_today$col_name
  ),

  bac_activa_2007 = list(
    endpoint = "tbd_activa.xls",
    fileext  = ".xls",
    col_names = aap_activa_2007$col_names
  ),
  bac_activa_2012 = list(
    endpoint = "tbd_activad-2008-2012.xls",
    fileext  = ".xls",
    col_names = aap_activa_2012$col_names
  ),
  bac_activa_2016 = list(
    endpoint = "tbac_activad_2013_2016.xls",
    fileext  = ".xls",
    col_names = aap_activa_2016$col_names
  ),
  bac_activa_today = list(
    endpoint = "tbac_activad.xlsx",
    fileext  = ".xlsx",
    # Este archivo tiene una columna para las tarjetas de crédito. Es el único que lo tiene.
    col_names = c(
      "mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a",
      "ta_pp", "ta_ps", "ta_tc", "ta_comercio", "ta_consumo", "ta_hipotecario",
      "ta_preferencial", "ta_preferencial_comercio", "ta_preferencial_consumo",
      "ta_preferencial_hipotecario"
    )
  ),

  bac_pasiva_2007 = list(
    endpoint = "tbd_pasiva.xls",
    fileext  = ".xls",
    col_names = aap_pasiva_2007$col_names
  ),
  bac_pasiva_2012 = list(
    endpoint = "tbd_pasivad-2008-2012.xls",
    fileext = ".xls",
    # la tasas simple y la ponderada están invertidas.
    col_names = c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_2a",
      "tp_ps", "tp_pp", "tp_dep_ahorros", "tp_general",
      "tp_preferencial", "tp_interbancarios"
    )
  ),
  bac_pasiva_2016 = list(
    endpoint = "tbac_pasivad_2013_2016.xls",
    fileext = "xls",
    # la tasas simple y la ponderada están invertidas.
    col_names = c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_2a",
      "tp_5a", "tp_m5a",
      "tp_ps", "tp_pp", "tp_dep_ahorros", "tp_general",
      "tp_preferencial", "tp_interbancarios"
    )
  ),
  bac_pasiva_today = list(
    endpoint = "tbac_pasivad.xlsx",
    fileext = "xlsx",
    col_names = bac_pasiva_2016$col_names
  ),

  cc_activa_2012 = list(
    endpoint = "tf_activad-2008-2011.xls",
    fileext = ".xls",
    col_names = c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a", "ta_pp")
  ),
  cc_activa_2016 = list(
    endpoint = "tf_activad_2013-2016.xlsx",
    fileext = ".xlsx",
    col_names = c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a", "ta_pp")
  ),
  cc_activa_today = list(
    endpoint = "tf_activad.xls",
    fileext  = ".xls",
    col_name = c(
      "mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a", "ta_m5a",
      "ta_pp", "ta_ps", "ta_comercio", "ta_consumo", "ta_hipotecario",
      "ta_preferencial", "ta_preferencial_comercio", "ta_preferencial_consumo",
      "ta_preferencial_hipotecario"
    )
  ),

  cc_pasiva_today = list(
    endpoint = "tf_pasivad.xlsx",
    fileext  = ".xlsx",
    col_names = c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_2a",
      "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_general", "tp_preferencial",
      "tp_interbancarios"
    )
  ),
  cc_pasiva_2016 = list(
    endpoint = "tf_pasivad_2013_2016.xlsx",
    fileext = ".xlsx",
    col_names = c(
      "mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_2a", "tp_5a", "tp_m5a",
      "tp_pp"
    )
  )
)

.get_historical_rates <- function(endpoint, fileext, col_names) {
  cdn_url <- "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/documents/"
  file_url <- paste0(cdn_url, endpoint)

  temp_file <- tempfile(fileext = fileext)
  .download_bcrd_file(file_url, temp_file)

  month_pattern <- purrr::map_chr(1:12, ~ crear_mes(.x, "number_to_text")) |>
    paste(collapse = "|")

  temp_file |>
    readxl::read_excel(col_names = FALSE) |>
    suppressMessages() |>
    janitor::clean_names() |>
    dplyr::filter(stringr::str_detect(x1, paste0("^[\\*]?\\d{4}|", month_pattern))) |>
    janitor::remove_empty("cols") |>
    purrr::set_names(col_names) |>
    dplyr::mutate(
      year = stringr::str_extract(mes, "\\d{4}"),
      mes  = stringr::str_extract(mes, "[a-zA-Z]+")
    ) |>
    tidyr::fill(year) |>
    dplyr::filter(stringr::str_detect(mes, "\\d{4}", negate = TRUE)) |>
    dplyr::mutate(
      dplyr::across(-c(mes, year), as.numeric),
      mes = crear_mes(mes),
      fecha = lubridate::make_date(year, mes, 1)
    ) |>
    dplyr::filter(dplyr::if_any(dplyr::matches("^(ta|tp)"), ~ !is.na(.))) |>
    dplyr::relocate(fecha, year, mes)
}


# ==============================================================================
# 1. METADATOS Y CONFIGURACIONES GLOBALES
# ==============================================================================

.tasas_col_names_diarias <- list(
  `ACTRD$` =  c(
    "day_mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
    "ta_m5a", "ta_pp", "ta_ps", "ta_comercio", "ta_consumo",
    "ta_hipotecario", "ta_preferencial", "ta_preferencial_comercio",
    "ta_preferencial_consumo", "ta_preferencial_hipotecario"
  ),
  `PASRD$` = c(
    "day_mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
    "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps",  "tp_dep_ahorros", "tp_general",
    "tp_preferencial", "tp_interbancarios"
  ),
  `ACTUS$` = c(
    "day_mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
    "ta_m5a", "ta_pp", "ta_ps", "ta_comercio", "ta_consumo",
    "ta_hipotecario", "ta_preferencial", "ta_preferencial_comercio",
    "ta_preferencial_consumo", "ta_preferencial_hipotecario"
  ),
  `PASUS$` = c(
    "day_mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
    "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps",  "tp_dep_ahorros", "tp_general"
  )
)

.tasas_detalles_labels <- c(
  "30d"            = "0 a 30 días",
  "60d"            = "31 a 60 días",
  "180d"           = "91 a 180 días",
  "360d"           = "181 a 360 días",
  "m360"           = "Más de 360 días",
  "2a"             = "361 días a 2 años",
  "5a"             = "2 a 5 años",
  "m5a"            = "Más de 5 años",
  "pp"             = "Promedio ponderado",
  "ps"             = "Promedio simple",
  "comercio"       = "Comercio",
  "consumo"        = "Consumo",
  "hipotecario"    = "Hipotecario",
  "dep_ahorros"    = "Depositos",
  "general"        = "General",
  "preferencial"   = "Preferencial PP",
  "interbancarios" = "Interbancaria",
  "tc" = "Tarjeta de crédito"
)

# ==============================================================================
# 2. FUNCIONES AUXILIARES INTERNAS
# ==============================================================================


#' Pivot wide formats of interest rates data frames to long format
#'
#' Tidies interest rate datasets from wide matrix forms down into structured tidy key-value combinations.
#'
#' @param tasas_wide `<data.frame>` Wide structured table array parsed from source files.
#' @param filtro_tipo_tasa `<character>` Filter by type: `"Activa"` or `"Pasiva"`.
#' @param filtro_moneda `<character>` Filter by currency: `"DOP"` or `"USD"`.
#' @param filtro_condicion `<character>` Filter by rate condition: `"General"` or `"Preferencial"`.
#' @param filtro_grupo `<character>` Filter by grouping: `"Plazo"`, `"Promedio"`, or `"Sector"`.
#' @param filtro_detalle `<character>` Filter by specific descriptive category.
#'
#' @return A tidy mapped structure long configuration `tibble`.
#' @export
tasas_to_long <- function(
    tasas_wide,
    filtro_tipo_tasa = NULL,
    filtro_moneda    = NULL,
    filtro_condicion = NULL,
    filtro_grupo     = NULL,
    filtro_detalle   = NULL
) {
  validate <- \(values, choices) {
    if (!is.null(values)) match.arg(values, choices, several.ok = TRUE) else values
  }

  filtro_moneda    <- validate(filtro_moneda,    c("DOP", "USD"))
  filtro_condicion <- validate(filtro_condicion, c("General", "Preferencial"))
  filtro_tipo_tasa <- validate(filtro_tipo_tasa, c("Activa", "Pasiva"))
  filtro_grupo     <- validate(filtro_grupo,     c("Plazo", "Promedio", "Sector"))

  tasas_wide |>
    tidyr::pivot_longer(cols = dplyr::matches("^ta|^tp")) |>
    dplyr::mutate(
      grupo = dplyr::case_when(
        stringr::str_detect(name, "\\d[da]$") ~ "Plazo",
        stringr::str_detect(name, "pp$|ps|preferencial$") ~ "Promedio",
        stringr::str_detect(name, "comercio|consumo|hipotecario|tc") ~ "Sector"
      ),
      condicion = dplyr::if_else(stringr::str_detect(name, "preferencial"), "Preferencial", "General"),
      # Reemplazado dplyr::recode por un mapeo directo moderno de strings (Punto 5)
      name = stringr::str_replace_all(name, c("ta_90d" = "0 a 90 días", "tp_90d" = "61 a 90 días")),
      detalle_raw = stringr::str_remove(name, "^ta_preferencial_|^ta_|^tp_"),
      detalle = stringr::str_replace_all(detalle_raw, .tasas_detalles_labels)
    ) |>
    dplyr::filter(is.null(filtro_tipo_tasa) | type      %in% filtro_tipo_tasa) |>
    dplyr::filter(is.null(filtro_moneda)    | moneda    %in% filtro_moneda) |>
    dplyr::filter(is.null(filtro_condicion) | condicion %in% filtro_condicion) |>
    dplyr::filter(is.null(filtro_grupo)     | grupo     %in% filtro_grupo) |>
    dplyr::filter(is.null(filtro_detalle)   | detalle   %in% filtro_detalle) |>
    dplyr::select(
      dplyr::any_of(c("fecha", "year", "mes", "day")),
      tipo_tasa = type, moneda, grupo, condicion, detalle, tasa = value
    ) |>
    dplyr::filter(!is.na(tasa))
}

# ==============================================================================
# 3. FUNCIONES PÚBLICAS EXPORTADAS
# ==============================================================================

#' Download interest rates for savings (Pasivas)
#'
#' Downloads and aggregates historic monthly interest rates for savings (tasas pasivas)
#' published by the Central Bank of the Dominican Republic (BCRD).
#'
#' @param long `<logical>` If `TRUE`, converts data frame into long format. Defaults to `FALSE`.
#' @param filtro_condicion `<character>` Filter by rate condition: `"General"` or `"Preferencial"`. Only used if `long = TRUE`.
#' @param filtro_grupo `<character>` Filter by grouping: `"Plazo"`, `"Promedio"`, or `"Sector"`. Only used if `long = TRUE`.
#' @param filtro_detalle `<character>` Filter by specific descriptive category. Only used if `long = TRUE`.
#'
#' @return A `tibble` containing monthly series data.
#' @export
get_tasas_pasivas <- function(
    entidad = c("bm", "aap", "bac", "cc"),
    long = FALSE,
    filtro_condicion = NULL,
    filtro_grupo     = NULL,
    filtro_detalle   = NULL
) {
  entidad <- rlang::arg_match(entidad)
  toread <- stringr::str_subset(names(params), paste0(entidad, "_pasiva"))
  files_to_read <- params[toread]

  tasas <- purrr::map(
    files_to_read,
    \(x) do.call(.get_historical_rates, x)
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(fecha)

  if (entidad == "bac") {
    # Este archivo tiene un problema, los años 2012 y 2017 están repetidos
    # en dos archivos
    tasas <- tasas |>
      dplyr::slice(1, .by = c(year, mes))
  }

  if (long) {
    tasas <- tasas |>
      dplyr::mutate(type = "Pasiva", moneda = "DOP") |>
      tasas_to_long(
        filtro_condicion = filtro_condicion,
        filtro_grupo     = filtro_grupo,
        filtro_detalle   = filtro_detalle
      )
  }

  tasas
}


#' Download interest rates for loans (Activas)
#'
#' Downloads and aggregates historic monthly interest rates for loans (tasas activas)
#' published by the Central Bank of the Dominican Republic (BCRD).
#'
#' @param long `<logical>` If `TRUE`, converts data frame into long format. Defaults to `FALSE`.
#' @param filtro_condicion `<character>` Filter by rate condition: `"General"` or `"Preferencial"`. Only used if `long = TRUE`.
#' @param filtro_grupo `<character>` Filter by grouping: `"Plazo"`, `"Promedio"`, or `"Sector"`. Only used if `long = TRUE`.
#' @param filtro_detalle `<character>` Filter by specific descriptive category. Only used if `long = TRUE`.
#'
#' @return A `tibble` containing monthly series data.
#' @export
get_tasas_activas <- function(
    entidad = c("bm", "aap", "bac", "cc"),
    long             = FALSE,
    filtro_condicion = NULL,
    filtro_grupo     = NULL,
    filtro_detalle   = NULL
) {
  entidad <- rlang::arg_match(entidad)
  toread <- stringr::str_subset(names(params), paste0(entidad, "_activa"))
  files_to_read <- params[toread]

  tasas <- purrr::map(
    files_to_read,
    \(x) do.call(.get_historical_rates, x)
  ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(fecha)

  if (entidad == "bac") {
    # Este archivo tiene un problema, los años 2012 y 2017 están repetidos
    # en dos archivos
    tasas <- tasas |>
      dplyr::slice(1, .by = c(year, mes))
  }

  if (long) {
    tasas <- tasas |>
      dplyr::mutate(type = "Activa", moneda = "DOP") |>
      tasas_to_long(
        filtro_condicion = filtro_condicion,
        filtro_grupo     = filtro_grupo,
        filtro_detalle   = filtro_detalle
      )
  }

  tasas
}


#' Daily Interest Rates from the Central Bank
#'
#' Downloads and processes the daily interest rates published by the
#' Central Bank of the Dominican Republic (BCRD) for a given year.
#'
#' @param year `<integer>` Year to download. Defaults to `2025`.
#' @param filtro_tipo_tasa `<character>` Filter by rate type: `"Activa"`
#'   or `"Pasiva"`.
#' @param filtro_moneda `<character>` Filter by currency: `"DOP"` or `"USD"`.
#' @param filtro_condicion `<character>` Filter by rate condition:
#'   `"General"` or `"Preferencial"`.
#' @param filtro_grupo `<character>` Filter by grouping: `"Plazo"`,
#'   `"Promedio"`, or `"Sector"`.
#' @param filtro_detalle `<character>` Filter by detail category (see the
#'   `detalle` column in the output, e.g. `"Comercio"`, `"0 a 30 días"`).
#'
#' @return A `tibble` with one row per rate and date, containing the
#'   following columns:
#'   \describe{
#'     \item{fecha}{Observation date (`Date`).}
#'     \item{year}{Year (`integer`).}
#'     \item{mes}{Month (`integer`).}
#'     \item{day}{Day of the month (`integer`).}
#'     \item{tipo_tasa}{`"Activa"` or `"Pasiva"`.}
#'     \item{moneda}{`"DOP"` or `"USD"`.}
#'     \item{grupo}{`"Plazo"`, `"Promedio"`, or `"Sector"`.}
#'     \item{condicion}{`"General"` or `"Preferencial"`.}
#'     \item{detalle}{Instrument, maturity, or sector description.}
#'     \item{tasa}{Interest rate value (`double`).}
#'   }
#'
#' @examples
#' # All rates for 2024
#' get_tasas_diarias(2024)
#'
#' # Only active rates in DOP grouped by term
#' get_tasas_diarias(
#'   year             = 2024,
#'   filtro_tipo_tasa = "Activa",
#'   filtro_moneda    = "DOP",
#'   filtro_grupo     = "Plazo"
#' )
#'
#' @export
#'
get_tasas_diarias <- function(
    year             = 2025,
    filtro_tipo_tasa = NULL,
    filtro_moneda    = NULL,
    filtro_condicion = NULL,
    filtro_grupo     = NULL,
    filtro_detalle   = NULL
) {

  file_url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-monetario-y-financiero/",
    glue::glue("documents/tasas_diariasBM-{year}.xlsx")
  )

  file_path <- tempfile(pattern = as.character(year), fileext = ".xlsx")
  .download_bcrd_file(file_url, file_path) # Control de errores inyectado aquí

  sheets <- readxl::excel_sheets(file_path)

  # Generar patrón de meses dinámicamente con fallback seguro
  month_pattern <- purrr::map_chr(1:12, ~ crear_mes(.x, "number_to_text")) |>
    paste(collapse = "|")

  data <- purrr::map(
    purrr::set_names(sheets),
    \(sheet) {
      tasas <- readxl::read_excel(
        path = file_path,
        skip = 11,
        trim_ws = TRUE,
        col_names = FALSE,
        sheet = sheet,
        col_types = "text" # Forzado tipado explícito para evitar fallos de coercion silenciosa
      ) |>
        suppressMessages()

      type   <- dplyr::if_else(stringr::str_detect(sheet, "^ACT"), "Activa", "Pasiva")
      moneda <- dplyr::if_else(stringr::str_detect(sheet, "RD\\$$"), "DOP", "USD")

      tasas |>
        purrr::set_names(.tasas_col_names_diarias[[sheet]]) |>
        janitor::remove_empty(which = c("cols", "rows")) |>
        dplyr::mutate(
          mes_raw = stringr::str_extract(day_mes, month_pattern),
          mes     = crear_mes(mes_raw, "text_to_number")
        ) |>
        dplyr::select(-mes_raw) |>
        dplyr::relocate(mes) |>
        tidyr::fill(mes) |>
        dplyr::filter(dplyr::if_any(dplyr::ends_with("90d"), \(x) !is.na(x))) |>
        dplyr::filter(stringr::str_detect(day_mes, "^\\d{1,2}$")) |>
        dplyr::mutate(
          dplyr::across(-c(mes, day_mes), \(x) as.numeric(stringr::str_remove_all(x, ","))),
          year    = !!year,
          day_mes = as.numeric(day_mes),
          fecha   = lubridate::make_date(year, mes, day_mes),
          type    = !!type,
          moneda  = !!moneda
        ) |>
        dplyr::relocate(fecha, year, mes, day = day_mes, type, moneda)
    }
  ) |>
    dplyr::bind_rows() |>
    tasas_to_long(
      filtro_tipo_tasa = filtro_tipo_tasa,
      filtro_moneda    = filtro_moneda,
      filtro_condicion = filtro_condicion,
      filtro_grupo     = filtro_grupo,
      filtro_detalle   = filtro_detalle
    )

  data
}
