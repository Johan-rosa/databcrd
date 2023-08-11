#' Remittances to the Dominican Republic
#'
#' @param modalidad string indicating which perspective of remittances is asked.
#' Options are: "mensual", "por_pais_emisor", "por_provincia_receptora",
#' "cantidad_de_transacciones", "promedio_transacciones", "segun_moneda",
#' "entidad_pagadora", "genero_receptor"
#'
#' @return a data frame
#' @export
#' @examples
#' get_remesas("mensual")
#' get_remesas("por_pais_emisor")
#' get_remesas("por_provincia_receptora")
#' get_remesas("cantidad_de_transacciones")
#' get_remesas("promedio_transacciones")
#' get_remesas("segun_moneda")
#' get_remesas("entidad_pagadora")
#' get_remesas("genero_receptor")
get_remesas <- function(modalidad = "mensual") {
  checkmate::assert_choice(
    modalidad,
    choices = c(
      "mensual", "por_pais_emisor", "por_provincia_receptora",
      "cantidad_de_transacciones", "promedio_transacciones",
      "segun_moneda", "entidad_pagadora", "genero_receptor"
    )
  )

  remesas_function <- switch(
    modalidad,
    "mensual" = get_remesas_mensuales,
    "por_pais_emisor" = get_remesas_pais,
    "por_provincia_receptora" = get_remesas_provincias,
    "cantidad_de_transacciones" = get_remesas_cnt,
    "promedio_transacciones" = get_remesas_avg,
    "segun_moneda" = get_remesas_currency,
    "entidad_pagadora" = get_remesas_epa,
    "genero_receptor" = get_remesas_genero
  )

  remesas_function()
}

#' Get the monthly remittances
get_remesas_mensuales <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_6.xlsx")

  file_path <- tempfile(fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- readxl::read_excel(file_path, skip = 7, n_max = 12) |>
    dplyr::rename(mes = "PERIODOS") |>
    tidyr::pivot_longer(cols = -mes, names_to = "year", values_to = "monto") |>
    dplyr::mutate(
      mes = crear_mes(mes, type = "text_to_number"),
      year = as.numeric(year),
      fecha = lubridate::make_date(year, mes, "1")
    ) |>
    stats::na.omit() |>
    dplyr::arrange(fecha)

  data
}

read_remesa_file <- function(url, skip, n_max, values_to = "proporcion") {
  file_path <- tempfile(fileext = ".xlsx")

  utils::download.file(url, file_path, mode = "wb", quiet = TRUE)

  readxl::read_excel(file_path, skip = skip, n_max = n_max) |>
    dplyr::rename(partida = 1) |>
    dplyr::slice(-1) |> # merged header adds an extra row
    dplyr::mutate(dplyr::across(-partida, as.numeric)) |>
    stats::na.omit() |>
    tidyr::pivot_longer(-partida, names_to = "year", values_to = values_to) |>
    dplyr::mutate(year = as.numeric(year))
}

#' Get remittances by country
get_remesas_pais <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PE.xlsx"
  )

  read_remesa_file(url_descarga, skip = 5, n_max = 13)
}

#' Get remittances by provinces
get_remesas_provincias <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PR.xlsx")

  read_remesa_file(url_descarga, skip = 5, n_max = 17)
}

#' Get quantity of transactions of remittances
get_remesas_cnt <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_TR.xlsx"
  )

  read_remesa_file(url_descarga, skip = 5, n_max = 13, values_to = "cantidad")
}

#' Get average of remittances
get_remesas_avg <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PT.xlsx"
  )

  read_remesa_file(url_descarga, skip = 5, n_max = 12, values_to = "monto")
}

#' Get remittances by currency
get_remesas_currency <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_MP.xlsx"
  )

  read_remesa_file(url_descarga, skip = 5, n_max = 4)

}

#' Get remittances by payment company
get_remesas_epa <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PP.xlsx"
  )

  read_remesa_file(url_descarga, skip = 5, n_max = 4)
}

#' Get remittances by gender
get_remesas_genero <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_GR.xlsx")

  read_remesa_file(url_descarga, skip = 5, n_max = 4)
}
