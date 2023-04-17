#' Download the CPI series
#'
#' Download the CPI inflacion series for the Dominican Republic in any
#' disaggregation
#'
#' @param desagregacion string with the desired disaggregation. options:
#' "general", "grupos", "regiones", "subyacente", "tnt" (transable y no
#' transable),
#' "articulos"
#'
#' @export
#'
#' @return a tibble
#' @examples
#' get_ipc_data()
# Funcion para descargar data del IPC
get_ipc_data <- function(desagregacion = "general") {
  checkmate::assert_character(desagregacion)
  checkmate::assert_choice(desagregacion, choices = c("general"))

  if (desagregacion == "general") {
    return(get_ipc_general())
  }

}

#' To get the general CPI data
#'
#' You can get the CPI index and the monthly, year over year and
#' throughout the year variations, as well as the 12 month average
get_ipc_general <- function() {
    url_descarga <- paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/precios/documents/",
      "ipc_base_2019-2020.xls")

    file_path <- tempfile(pattern = "", fileext = ".xls")

    utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

    suppressMessages(
      ipc_general <- readxl::read_excel(
        file_path,
        sheet = 1,
        col_names = FALSE,
        skip = 7)
    )

    var_names <- c(
      "year", "mes", "ipc", "ipc_vm", "ipc_vd", "ipc_vi", "ipc_p12")

    ipc_general <- ipc_general |>
      janitor::clean_names() |>
      dplyr::select(1:7) |>
      stats::setNames(var_names) |>
      dplyr::filter(!is.na(mes)) |>
      dplyr::mutate(
        fecha = seq(
          lubridate::ymd("1984/01/01"),
          by = "month",
          length.out = dplyr::n()),
        year = lubridate::year(fecha),
        mes = crear_mes(mes)
      ) |>
      dplyr::select(fecha, year, mes, dplyr::everything())

    ipc_general
}
