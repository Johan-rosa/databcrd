#' Foreing direct investment to the Dominican Republic
#'
#' This function returns the  Foreing direct investment to the
#' Dominican Republicthe based on the specified frequency.
#'
#' @param modalidad A character string. Options are "pais_origen" or
#' "sector_destino".
#' @param frecuencia A character string that specifies the frequency of the
#' exchange rates to be downloaded. Valid options are "trimestral",  or "anual".
#'
#' @return A data frame
#'
#' @export
#'
#' @examples
#'get_tc_eif("mensual")
#'get_tc_eif("anual")

get_ied <- function(
    modalidad = "pais_origen",
    frecuencia = "trimestral"
  ) {
  checkmate::assert_choice(modalidad, c("pais_origen", "sector_destino"))
  checkmate::assert_choice(frecuencia, c("trimestral", "anual"))

  if (modalidad == "pais_origen") return(get_ied_pais(frecuencia))
  if (modalidad == "sector_destino") return(get_ied_sector(frecuencia))

}

# Get Foreign Direct Investment by country of origin
get_ied_pais <- function(frecuencia = "trimestral") {
  checkmate::assert_choice(frecuencia, c("trimestral", "anual"))

  file_url <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/",
    "documents/inversion_ext_pais_6.xls"
  )

  file_path <- base::tempfile(fileext = ".xls")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  suppressMessages(
    ied_data <- readxl::read_excel(
      path = file_path,
      skip = 7,
      n_max = 20
    )
  )

  quarters <- base::seq(
    as.Date("2010-01-01"),
    by = "quarter",
    length.out = base::ncol(ied_data) - 1
  )

  ied_data <- ied_data |>
    stats::setNames(c("pais", quarters)) |>
    tidyr::pivot_longer(
      cols = -pais,
      names_to = "trimestre",
      values_to = "monto") |>
    dplyr::mutate(
      trimestre = as.numeric(trimestre),
      fecha = lubridate::as_date(trimestre),
      year = lubridate::year(fecha),
      trimestre = lubridate::quarter(fecha)
    ) |>
    dplyr::select(pais, year, trimestre, fecha, monto)

  if (frecuencia == "anual") {
    ied_anual <- suppressMessages(
      ied_data |>
        dplyr::select(-c(trimestre, fecha)) |>
        dplyr::group_by(pais, year) |>
        dplyr::summarise(monto = sum(monto))
    )
    return(ied_anual)
  }

  ied_data
}

# Get Foreign Direct Investment by sector
get_ied_sector <- function(frecuencia = "trimestral") {
  checkmate::assert_choice(frecuencia, c("trimestral", "anual"))

  file_url <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/",
    "documents/inversion_ext_sector_6.xls"
  )
  file_path <- base::tempfile(fileext = ".xls")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  suppressMessages(
    ied_data <- readxl::read_excel(
      path = file_path,
      skip = 7,
      n_max = 10
    )
  )

  quarters <- base::seq(
    as.Date("2010-01-01"),
    by = "quarter",
    length.out = base::ncol(ied_data) - 1
  )

  ied_data <- ied_data |>
    stats::setNames(c("sector", quarters)) |>
    tidyr::pivot_longer(
      cols = -sector,
      names_to = "trimestre",
      values_to = "monto") |>
    dplyr::mutate(
      trimestre = as.numeric(trimestre),
      fecha = lubridate::as_date(trimestre),
      year = lubridate::year(fecha),
      trimestre = lubridate::quarter(fecha)
    )

  if (frecuencia == "anual") {
    ied_anual <- suppressMessages(
      ied_data |>
        dplyr::select(-c(trimestre, fecha)) |>
        dplyr::group_by(sector, year) |>
        dplyr::summarise(monto = sum(monto))
    )
    return(ied_anual)
  }

  ied_data
}
