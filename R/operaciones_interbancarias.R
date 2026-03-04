get_tasa_interbancaria <- function() {
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/",
    "documents/Tasas_Interbancarias_Promedio_por_Plazos.xlsm"
  )

  file <- tempfile(fileext = ".xlsm")

  tryCatch(
    download.file(url, file, mode = "wb", quiet = TRUE),
    error = function(e) {
      stop("No se pudo descargar el archivo de tasas interbancarias.")
    }
  )

  headers_oi <- c(
    "year_mes",
    "monto_operaciones_interbancarias",
    "tasa_promedio_ponderado",
    "monto_depositos_vista",
    "tasa_depositos_vista",
    "monto_d_1_7",        "tasa_d_1_7",
    "monto_d_8_30",       "tasa_d_8_30",
    "monto_d_31_60",      "tasa_d_31_60",
    "monto_d_61_90",      "tasa_d_61_90",
    "monto_d_91_120",     "tasa_d_91_120",
    "monto_d_121_180",    "tasa_d_121_180",
    "monto_d_181_365",    "tasa_d_181_365",
    "monto_mas_365_dias", "tasa_mas_365_dias"
  )

  readxl::read_excel(
    file,
    skip = 11,
    col_names = FALSE
  ) |>
    stats::setNames(headers_oi) |>
    dplyr::filter(!is.na(year_mes)) |>
    dplyr::mutate(
      year = as.integer(stringr::str_extract(year_mes, "20\\d{2}")),
      year_mes = stringr::str_remove(year_mes, "\\*")
    ) |>
    tidyr::fill(year) |>
    dplyr::filter(!stringr::str_detect(year_mes, "Acumulad")) |>
    dplyr::mutate(
      dplyr::across(
        -c(year_mes, year),
        readr::parse_number
      )
    ) |>
    dplyr::filter(!is.na(monto_operaciones_interbancarias)) |>
    dplyr::distinct(year, year_mes, .keep_all = TRUE) |>
    dplyr::mutate(
      mes = databcrd::crear_mes(year_mes),
      date = lubridate::make_date(year, mes, 1)
    ) |>
    dplyr::relocate(date, year, mes) |>
    suppressMessages() |>
    suppressWarnings()
}
