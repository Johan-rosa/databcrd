#' Monthly Economic Activity Indicator
#'
#' Get the data series for the monthly Economic Activity Indicator of the
#' Dominican Republic. It includes seasonal and non seasonal adjusted series,
#' as well as monthly and year over year variations.
#'
#' @param variaciones Boolean indicating if variations should be included or
#' index only
#'
#' @return a tibble
#' @export
#'
#' @examples
#' get_imae(variaciones = FALSE)
get_imae <- function(variaciones = TRUE) {
  checkmate::assert_logical(variaciones)

  url <- paste0(
    "https://cdn.bancentral.gov.do/",
    "documents/estadisticas/sector-real/documents/imae.xlsx"
    )

  temp_path <- base::tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url, temp_path, quiet = TRUE, mode = "wb")

  header_imae <- c(
    "mes",
    "indice_original", "original_vi", "origianl_va", "original_p12m",
    "indice_desestacionalizado", "desestacionalizado_vm",
    "desestacionalizado_vi", "desestacionalizado_va",
    "desestacionalizado_p12m", "indice_tc", "tc_vm",
    "tc_vi", "tc_va", "tc_p12m"
  )

  suppressMessages(
    imae <- readxl::read_excel(
      path = temp_path,
      skip = 9,
      col_names = FALSE
    )
  )

  imae <- imae |>
    janitor::clean_names() |>
    dplyr::select(-1) |>
    dplyr::select(where(~!all(is.na(.)))) |>
    stats::setNames(header_imae) |>
    dplyr::filter(!is.na(mes)) |>
    dplyr::mutate(
      fecha = seq(as.Date("2007-01-01"), by = "month", length.out = dplyr::n()),
      year = lubridate::year(fecha)
    ) |>
    dplyr::select(fecha, year, mes, dplyr::everything())

  if (!variaciones) {
    imae <- imae |>
      dplyr::select(fecha, year, mes, dplyr::contains("indice"))
  }

  imae
}
