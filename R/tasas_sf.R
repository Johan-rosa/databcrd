# ==============================================================================
# Internal Helper Metadata Definitions
# ==============================================================================

.tasas_col_names <- list(
  `ACTRD$` = c(
    "day_mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
    "ta_m5a", "ta_pp", "ta_ps", "ta_comercio", "ta_consumo",
    "ta_hipotecario", "ta_preferencial", "ta_preferencial_comercio",
    "ta_preferencial_consumo", "ta_preferencial_hipotecario"
  ),
  `PASRD$` = c(
    "day_mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
    "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros", "tp_general",
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
    "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros", "tp_general"
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
  "interbancarios" = "Interbancaria"
)

# ==============================================================================
# Exported Core Functions
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
#'
#' @examples
#' \init{
#' # Get wide format data frame
#' get_tasas_pasivas(long = FALSE)
#'
#' # Get long format filtered data frame
#' get_tasas_pasivas(long = TRUE, filtro_grupo = "Plazo")
#' }
get_tasas_pasivas <- function(
    long = FALSE,
    filtro_condicion = NULL,
    filtro_grupo = NULL,
    filtro_detalle = NULL
) {
  file_names <- c(
    "Tasas 1991-2007",
    "Tasas 2008-2012",
    "Tasas 2013-2016",
    "Tasas 2017-today"
  )

  urls <- c(
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_pasiva-1991-2007.xls"
    ),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_pasivad-2008-2012.xls"
    ),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_pasivad-2013-2016.xlsx"
    ),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_pasivad.xlsx"
    )
  )

  names(urls) <- file_names

  temp_files <- base::sapply(
    c(".xls", ".xls", ".xlsx", ".xlsx"),
    \(x) tempfile(fileext = x)
  ) |>
    stats::setNames(file_names)

  purrr::walk2(
    urls, temp_files,
    utils::download.file, mode = "wb", quiet = TRUE
  )

  params <- list(
    list(range = "A157:M266"),
    list(range = "A14:O88"),
    list(range = "A11:O67"),
    list(skip = 10)
  ) |>
    purrr::map2(
      temp_files,
      \(x, y) append(x, list(path = y, col_names = FALSE))
    )

  suppressMessages(
    tasas_list <- purrr::map(
      seq_along(file_names),
      \(index) do.call(readxl::read_excel, params[[index]])
    ) |>
      stats::setNames(file_names)
  )

  prepare_data <- function(df, names, start_date) {
    df |>
      stats::setNames(names) |>
      dplyr::filter(stringr::str_detect(mes, "^[\\*]?[A-Z]")) |>
      dplyr::filter(!is.na(tp_30d)) |>
      dplyr::mutate(
        fecha = seq(as.Date(start_date), by = "month", length.out = dplyr::n()),
        year = lubridate::year(fecha),
        mes = lubridate::month(fecha),
        mes = crear_mes(mes, "number_to_text")
      ) |>
      dplyr::select(fecha, year, mes, dplyr::everything())
  }

  df_names <- list(
    c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d", "tp_m360",
      "tp_ps", "tp_pp", "tp_dep_ahorros", "tp_preferencial", "tp_general",
      "tp_interbancarios"),
    c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
      "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
      "tp_general", "tp_preferencial", "tp_interbancarios")
  )

  df_names <- list(df_names[[1]], df_names[[2]], df_names[[2]], df_names[[2]])

  df_cleaning_params <- dplyr::lst(
    df = tasas_list,
    names = df_names,
    start_date = c("2000/01/01", "2008/01/01", "2013/01/01", "2017/01/01")
  )

  tasas <- purrr::pmap(
    df_cleaning_params,
    prepare_data
  ) |>
    purrr::list_rbind()

  if (long) {
    tasas <- tasas |>
      dplyr::mutate(
        type = "Pasiva",
        moneda = "DOP"
      ) |>
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
#'
#' @examples
#' \init{
#' # Get active interest rates wide format
#' get_tasas_activas(long = FALSE)
#' }
get_tasas_activas <- function(
    long = FALSE,
    filtro_condicion = NULL,
    filtro_grupo = NULL,
    filtro_detalle = NULL
) {
  file_names <- c(
    "Tasas 1991-2007",
    "Tasas 2008-2012",
    "Tasas 2013-2016",
    "Tasas 2017-today"
  )

  urls <- c(
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activa-1991-2007.xls"
    ),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activad-2008-2012.xls"
    ),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activad-2013-2016.xlsx"
    ),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activad.xlsx"
    )
  )

  names(urls) <- file_names

  temp_files <- base::sapply(
    c(".xls", ".xls", ".xlsx", ".xlsx"),
    \(x) tempfile(fileext = x)
  ) |>
    stats::setNames(file_names)

  purrr::walk2(
    urls, temp_files,
    utils::download.file, mode = "wb", quiet = TRUE
  )

  params <- list(
    list(range = "A157:O266"),
    list(range = "A14:M85"),
    list(range = "A10:M66"),
    list(skip = 9)
  ) |>
    purrr::map2(
      temp_files,
      \(x, y) append(x, list(path = y, col_names = FALSE))
    )

  suppressMessages(
    tasas_list <- purrr::map(
      seq_along(file_names),
      \(index) do.call(readxl::read_excel, params[[index]])
    ) |>
      stats::setNames(file_names)
  )

  prepare_data <- function(df, names, start_date) {
    df |>
      janitor::remove_empty(which = "cols") |>
      janitor::remove_empty(which = "rows") |>
      stats::setNames(names) |>
      dplyr::filter(stringr::str_detect(mes, "^[\\*]?[A-Z]")) |>
      dplyr::filter(!is.na(ta_90d)) |>
      dplyr::mutate(
        fecha = seq(as.Date(start_date), by = "month", length.out = dplyr::n()),
        year = lubridate::year(fecha),
        mes = lubridate::month(fecha),
        mes = crear_mes(mes, "number_to_text")
      ) |>
      dplyr::select(fecha, year, mes, dplyr::everything())
  }

  df_names <- list(
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
      "ta_consumo", "ta_hipotecario"),
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
      "ta_consumo", "ta_hipotecario"),
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
      "ta_consumo", "ta_hipotecario"),
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_pp", "ta_ps", "ta_comercio", "ta_consumo",
      "ta_hipotecario", "ta_preferencial", "ta_preferencial_comercio",
      "ta_preferencial_consumo", "ta_preferencial_hipotecario")
  )

  df_cleaning_params <- dplyr::lst(
    df = tasas_list,
    names = df_names,
    start_date = c("2000/01/01", "2008/01/01", "2013/01/01", "2017/01/01")
  )

  tasas <- purrr::pmap(
    df_cleaning_params,
    prepare_data
  ) |>
    purrr::list_rbind()

  if (long) {
    tasas <- tasas |>
      dplyr::mutate(
        type = "Activa",
        moneda = "DOP"
      ) |>
      tasas_to_long(
        filtro_condicion = filtro_condicion,
        filtro_grupo     = filtro_grupo,
        filtro_detalle   = filtro_detalle
      )
  }

  tasas
}

#' Pivot wide formats of interest rates data frames to long format
#'
#' Tidies interest rate datasets from wide matrix forms down into structured tidy key-value combinations.
#' Includes conditional programmatic matching validation parameters.
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
    filtro_moneda = NULL,
    filtro_condicion = NULL,
    filtro_grupo = NULL,
    filtro_detalle = NULL
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
        stringr::str_detect(name, "pp$|ps|preferencial$")   ~ "Promedio",
        stringr::str_detect(name, "comercio|consumo|hipotecario")   ~ "Sector"
      ),
      condicion = dplyr::if_else(stringr::str_detect(name, "preferencial"), "Preferencial", "General"),
      name = dplyr::recode(
        name,
        "ta_90d" = "0 a 90 días",
        "tp_90d" = "61 a 90 días"
      ),
      detalle = dplyr::recode(
        stringr::str_remove(name, "^ta_preferencial_|^ta_|^tp_"),
        !!!.tasas_detalles_labels
      )
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

#' Daily Interest Rates from the Central Bank
#'
#' Downloads and processes the daily interest rates published by the
#' Central Bank of the Dominican Republic (BCDR) for a given year.
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
#'    year             = 2024,
#'    filtro_tipo_tasa = "Activa",
#'    filtro_moneda    = "DOP",
#'    filtro_grupo     = "Plazo"
#' )
#'
#' @export
get_tasas_diarias <- function(
    year = 2025,
    filtro_tipo_tasa = NULL,
    filtro_moneda = NULL,
    filtro_condicion = NULL,
    filtro_grupo = NULL,
    filtro_detalle = NULL
) {
  validate <- \(values, choices) {
    if (!is.null(values)) match.arg(values, choices, several.ok = TRUE) else values
  }

  filtro_moneda    <- validate(filtro_moneda,    c("DOP", "USD"))
  filtro_condicion <- validate(filtro_condicion, c("General", "Preferencial"))
  filtro_tipo_tasa <- validate(filtro_tipo_tasa, c("Activa", "Pasiva"))
  filtro_grupo     <- validate(filtro_grupo,     c("Plazo", "Promedio", "Sector"))

  file_url <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/",
    glue::glue("documents/tasas_diariasBM-{year}.xlsx")
  )

  file_path <- tempfile(pattern = as.character(year), fileext = ".xlsx")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  sheets <- readxl::excel_sheets(file_path)

  month_pattern <- crear_mes(1:12, "number_to_text") |>
    paste(collapse = "|")

  data <- purrr::map(
    purrr::set_names(sheets),
    \(sheet) {
      suppressMessages(
        tasas <- readxl::read_excel(
          path = file_path,
          skip = 11,
          trim_ws = TRUE,
          col_names = FALSE,
          sheet = sheet
        )
      )

      type <- dplyr::if_else(stringr::str_detect(sheet, "^ACT"), "Activa", "Pasiva")
      moneda <- dplyr::if_else(stringr::str_detect(sheet, "RD\\$$"), "DOP", "USD")

      tasas |>
        purrr::set_names(.tasas_col_names[[sheet]]) |>
        janitor::remove_empty(which = "cols") |>
        janitor::remove_empty(which = "rows") |>
        dplyr::mutate(
          mes = stringr::str_extract(day_mes, month_pattern),
          mes = crear_mes(mes)
        ) |>
        dplyr::relocate(mes) |>
        tidyr::fill(mes) |>
        dplyr::filter(dplyr::if_any(dplyr::ends_with("90d"), \(x) !is.na(x))) |>
        dplyr::filter(stringr::str_detect(day_mes, "^\\d{1,2}$")) |>
        dplyr::mutate(
          year = year,
          day_mes = as.numeric(day_mes),
          fecha = lubridate::make_date(year, mes, day_mes),
          type = type,
          moneda = moneda
        ) |>
        dplyr::relocate(fecha, year, mes, day = day_mes, type, moneda)
    }
  ) |>
    dplyr::bind_rows() |>
    tasas_to_long()

  data
}
