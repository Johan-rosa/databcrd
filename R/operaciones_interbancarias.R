#' Obtener tasas interbancarias promedio por plazos
#'
#' Descarga y procesa el archivo oficial de Tasas Interbancarias Promedio
#' por Plazos publicado por el Banco Central de la República Dominicana.
#'
#' La función:
#' \itemize{
#'   \item Descarga el archivo .xlsm desde el CDN institucional.
#'   \item Limpia encabezados y filas agregadas (acumulados).
#'   \item Convierte montos y tasas a formato numérico.
#'   \item Construye variables temporales (`year`, `mes`, `date`).
#' }
#'
#' @return Un `tibble` con frecuencia mensual que incluye:
#' \describe{
#'   \item{date}{Fecha tipo Date (primer día del mes).}
#'   \item{year}{Año numérico.}
#'   \item{mes}{Mes numérico (1–12).}
#'   \item{monto_operaciones_interbancarias}{Monto total de operaciones interbancarias.}
#'   \item{tasa_promedio_ponderado}{Tasa promedio ponderada de operaciones interbancarias.}
#'   \item{monto_depositos_vista}{Monto de depósitos a la vista.}
#'   \item{tasa_depositos_vista}{Tasa de depósitos a la vista.}
#'   \item{monto_d_*}{Montos por tramo de plazo (días).}
#'   \item{tasa_d_*}{Tasas por tramo de plazo (días).}
#' }
#'
#' @details
#' Requiere que la estructura del archivo fuente no cambie (salto de 11 filas
#' iniciales y orden de columnas fijo). Si el formato publicado por el Banco
#' Central cambia, la función podría fallar.
#'
#' @source Banco Central de la República Dominicana.
#'
#' @examples
#' \dontrun{
#' tasas <- get_tasa_interbancaria()
#' dplyr::glimpse(tasas)
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
