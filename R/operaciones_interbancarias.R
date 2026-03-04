#' Get average interbank interest rates by maturity
#'
#' Downloads and processes the official Average Interbank Rates by Maturity
#' file published by the Central Bank of the Dominican Republic.
#'
#' The function:
#' \itemize{
#'   \item Downloads the .xlsm file from the institutional CDN.
#'   \item Cleans headers and aggregated rows (cumulative values).
#'   \item Converts amounts and interest rates to numeric format.
#'   \item Builds time variables (`year`, `mes`, `date`).
#' }
#'
#' @return A monthly-frequency `tibble` including:
#' \describe{
#'   \item{date}{Date object (first day of the month).}
#'   \item{year}{Numeric year.}
#'   \item{mes}{Numeric month (1–12).}
#'   \item{monto_operaciones_interbancarias}{Total interbank transaction amount.}
#'   \item{tasa_promedio_ponderado}{Weighted average interbank interest rate.}
#'   \item{monto_depositos_vista}{Demand deposits amount.}
#'   \item{tasa_depositos_vista}{Demand deposits interest rate.}
#'   \item{monto_d_*}{Amounts by maturity bucket (days).}
#'   \item{tasa_d_*}{Interest rates by maturity bucket (days).}
#' }
#'
#' @details
#' Requires the source file structure to remain unchanged (11 initial rows skipped
#' and fixed column order). If the format published by the Central Bank changes,
#' the function may fail.
#'
#' @source Central Bank of the Dominican Republic.
#'
#' @examples
#' \dontrun{
#' rates <- get_tasa_interbancaria()
#' dplyr::glimpse(rates)
#' }
#'
#' @export
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
