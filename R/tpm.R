#' Series of the Monetary Policy Rate of the Dominican Republic
#'
#' Monetary Policy Rate as well as some short term rates for deposits or loans
#'
#' @return a tibble
#' @export
get_tpm <- function() {
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-monetario-y-financiero/documents/Serie_TPM.xlsx")

  temp_file <- tempfile(fileext = ".xlsx")

  utils::download.file(url, temp_file, mode = "wb", quiet = TRUE)

  suppressMessages(
    data_tpm <- readxl::read_excel(temp_file, skip = 6, col_names = FALSE)
  )

  data_tpm |>
    stats::setNames(
      c("year", "mes", "tpm", "facilidades_depositos",
        "facilidades_prestamos", "facilidades_lombarda")) |>
    tidyr::fill(year) |>
    dplyr::filter(!is.na(mes)) |>
    dplyr::mutate(
      year = as.numeric(year),
      # En febrero 2013 hay un superindice par auna nota
      # R lo leel como Feb1
      mes = stringr::str_remove(mes, "[0-9]$"),
      mes = crear_mes(mes),
      fecha = lubridate::make_date(year, mes, "01")
    ) |>
    dplyr::select(fecha, year, mes, dplyr::everything())
}
