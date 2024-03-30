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
#' get_ipc_data("general")
#' get_ipc_data("grupos")
#' get_ipc_data("subyacente")
#' get_ipc_data("regiones")
#' get_ipc_data("tnt")
get_ipc_data <- function(desagregacion) {
  checkmate::assert_character(desagregacion)
  checkmate::assert_choice(
    desagregacion,
    choices = c("general", "grupos", "regiones", "subyacente", "tnt")
  )

  result <- switch(desagregacion,
                   "general" = get_ipc_general(),
                   "grupos" = get_ipc_grupos(),
                   "regiones" = get_ipc_regiones(),
                   "subyacente" = get_ipc_subyacente(),
                   "tnt" = get_ipc_tnt()
  )

  return(result)
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
      tidyr::fill(year) |>
      dplyr::mutate(
        mes = crear_mes(mes),
        fecha = lubridate::make_date(year, mes)) |>
      dplyr::select(fecha, year, mes, dplyr::everything())

    ipc_general
}

#' To get the CPI data by group of goods and services
#'
#' You can get the CPI index and the monthly, year over year and
#' throughout the year variations, as well as the 12 month average
get_ipc_grupos <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "precios/documents/ipc_grupos_base_2019-2020.xls"
  )

  file_path <- tempfile(pattern = "", fileext = ".xls")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  header_ipc_grupos <- c(
    "fecha", "ipc_ayb", "ipc_ayb_vm", "ipc_alcohol_tabaco",
    "ipc_alcohol_tabaco_vm", "ipc_ropa_calzado", "ipc_ropa_calzado_vm",
    "ipc_vivienda", "ipc_vivienda_vm",
    "ipc_muebles", "ipc_muebles_vm", "ipc_salud", "ipc_salud_vm",
    "ipc_transporte", "ipc_transporte_vm", "ipc_comunicaciones",
    "ipc_comunicaciones_vm", "ipc_cultura", "ipc_cultura_vm", "ipc_educacion",
    "ipc_educacion_vm", "ipc_hotel_restaurantes", "ipc_hotel_restaurantes_vm",
    "ipc_bines_servicios", "ipc_bienes_servicios_vm"
  )

  suppressMessages(
    ipc_grupos <- readxl::read_excel(
      file_path,
      skip = 6,
      col_names = FALSE,
      na = "-"
    ))

  ipc_grupos <-
    ipc_grupos |>
    janitor::clean_names() |>
    dplyr::select(1:24) |>
    stats::setNames(header_ipc_grupos) |>
    dplyr::filter(!is.na(fecha)) |>
    dplyr::mutate(
      year = stringr::str_extract(
        string = fecha,
        pattern = "\\d{4}"
        )
      ) |>
    tidyr::fill(year) |>
    dplyr::filter(!is.na(ipc_ayb)) |>
    dplyr::mutate(
      mes = crear_mes(fecha),
      fecha = lubridate::make_date(year, mes)) |>
    dplyr::select(fecha, year, mes, dplyr::everything())

  ipc_grupos

}

#' To get the CPI data by geographic region
#'
#' You can get the CPI index and the monthly, year over year and
#' throughout the year variations, as well as the 12 month average
get_ipc_regiones <- function() {

  header_ipc_regiones <- c(
    "year", "mes", "ipc_ozama", "ipc_ozama_vm", "ipc_cibao",
    "ipc_cibao_vm", "ipc_este", "ipc_este_vm", "ipc_sur",
    "ipc_sur_vm")

  url_descarga <-  base::paste0(
    "https://cdn.bancentral.gov.do/",
    "documents/estadisticas/precios/documents/",
    "ipc_regiones_base_2019-2020.xls"
  )

  file_path <- base::tempfile(pattern = "", fileext = ".xls")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  base::suppressMessages(
    ipc_region <- readxl::read_excel(
      file_path,
      skip = 7,
      col_names = FALSE
    ))

  ipc_region <-
    ipc_region |>
    stats::setNames(header_ipc_regiones) |>
    dplyr::filter(!is.na(mes)) |>
    tidyr::fill(year) |>
    dplyr::mutate(
      mes = crear_mes(mes),
      fecha = lubridate::make_date(year, mes)) |>
    dplyr::select(fecha, year, mes, dplyr::everything())

  ipc_region
}

#' To get the CPI core inflation data
#'
#' You can get the CPI index and the monthly, year over year and
#' throughout the year variations, as well as the 12 month average
get_ipc_subyacente <- function() {
  header_ipc_subyacente <- c(
    "year", "mes", "ipc_subyacente", "ipc_subyacente_vm",
    "ipc_subyacente_vd", "ipc_subyacente_vi"
  )

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/precios/documents/",
    "ipc_subyacente_base_2019-2020.xlsx"
  )

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  base::suppressMessages(
    ipc_subyacente <- readxl::read_excel(
      file_path,
      skip = 25,
      col_names = FALSE, na = c("-")
    ))

  ipc_subyacente <-
    ipc_subyacente[complete.cases(ipc_subyacente$`...2`), ] |>
    janitor::clean_names() |>
    dplyr::select(1:6) |>
    stats::setNames(header_ipc_subyacente) |>
    tidyr::fill(year) |>
    dplyr::mutate(
      mes = crear_mes(mes),
      fecha = lubridate::make_date(year, mes),
      across(c("year","ipc_subyacente", "ipc_subyacente_vm",
               "ipc_subyacente_vd", "ipc_subyacente_vi"),
             as.numeric)
    ) |>
    dplyr::select(fecha, year, mes, dplyr::everything()) |>
    dplyr::filter(!is.na(ipc_subyacente))

  ipc_subyacente
}

#' To get the CPI data by transferable or not
#'
#' You can get the CPI index and the monthly, year over year and
#' throughout the year variations, as well as the 12 month average
get_ipc_tnt <- function() {
  header_ipc_tnt <- c(
    "year", "mes", "ipc", "ipc_vm", "ipc_vd",
    "ipc_t", "ipc_t_vm", "ipc_t_vd", "ipc_nt",
    "ipc_nt_vm", "ipc_nt_vd"
  )

  url_descarga <- base::paste0(
    "https://cdn.bancentral.gov.do/",
    "documents/estadisticas/precios/",
    "documents/ipc_tnt_base_2019-2020.xls"
  )

  file_path <- base::tempfile(pattern = "", fileext = ".xls")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  suppressMessages(
    ipc_tnt <- readxl::read_excel(
      file_path,
      skip = 31,
      col_names = FALSE,
      na = "-"
    )
  )

  ipc_tnt <- ipc_tnt |>
    janitor::clean_names() |>
    tidyr::fill(x1) |>
    dplyr::filter(!is.na(x1), !is.na(x2)) |>
    stats::setNames(header_ipc_tnt) |>
    dplyr::filter(!is.na(mes)) |>
    tidyr::fill(year) |>
    dplyr::mutate(
      mes = crear_mes(mes),
      fecha = lubridate::make_date(year, mes)) |>
    dplyr::select(fecha, year, mes, dplyr::everything())

  ipc_tnt
}

