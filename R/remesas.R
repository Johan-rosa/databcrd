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
    choices = c("mensual", "por_pais_emisor",
                "por_provincia_receptora", "cantidad_de_transacciones",
                "promedio_transacciones", "segun_moneda", "entidad_pagadora",
                "genero_receptor")
  )

  if (modalidad == "mensual") return(get_remesas_mensuales())
  if (modalidad == "por_pais_emisor") return(get_remesas_pais())
  if (modalidad == "por_provincia_receptora") return(get_remesas_provincias())
  if (modalidad == "cantidad_de_transacciones") return(get_remesas_cnt())
  if (modalidad == "promedio_transacciones") return(get_remesas_avg())
  if (modalidad == "segun_moneda") return(get_remesas_currency())
  if (modalidad == "entidad_pagadora") return(get_remesas_epa())
  if (modalidad == "genero_receptor") return(get_remesas_genero())

}

#' Get the monthly remittances

get_remesas_mensuales <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_6.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressMessages(
    readxl::read_excel(file_path,
                             skip = 7,
                             n_max = 12)
    ) |>
    tidyr::pivot_longer(!PERIODOS,
                        names_to = "year",
                        values_to = "monto") |>
    dplyr::mutate(mes = crear_mes(PERIODOS, type = "text_to_number"),
                  fecha = lubridate::make_date(year, mes, "1")) |>
    dplyr::select(-PERIODOS) |>
    na.omit()

  data
}

#' Get remittances by country

get_remesas_pais <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PE.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressWarnings(
    suppressMessages(
      readxl::read_excel(file_path,
                       skip = 5,
                       n_max = 13)
      ) |>
      dplyr::mutate(dplyr::across(!País, as.numeric)) |>
      na.omit() |>
      tidyr::pivot_longer(!País,
                        names_to = "year",
                        values_to = "proporcion") |>
      dplyr::rename(partida = "País") |>
      dplyr::mutate(year = as.numeric(year))
  )

  data

}

#' Get remittances by provinces

get_remesas_provincias <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PR.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressWarnings(
    suppressMessages(
      readxl::read_excel(file_path,
                         skip = 5,
                         n_max = 17) |>
        dplyr::mutate(dplyr::across(!Provincia, as.numeric)) |>
        na.omit() |>
        tidyr::pivot_longer(!Provincia,
                            names_to = "year",
                            values_to = "proporcion") |>
        dplyr::rename(partida = "Provincia") |>
        dplyr::mutate(year = as.numeric(year))
    )
  )

  data

}

#' Get quantity of transactions of remittances

get_remesas_cnt <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_TR.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressWarnings(
    suppressMessages(
      readxl::read_excel(file_path,
                         skip = 5,
                         n_max = 13) |>
        dplyr::mutate(dplyr::across(!País, as.numeric)) |>
        na.omit() |>
        tidyr::pivot_longer(!País,
                            names_to = "year",
                            values_to = "cantidad") |>
        dplyr::rename(partida = "País") |>
        dplyr::mutate(year = as.numeric(year))
    )
  )

  data

}

#' Get average of remittances

get_remesas_avg <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PT.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressWarnings(
    suppressMessages(
      readxl::read_excel(file_path,
                         skip = 5,
                         n_max = 12) |>
        dplyr::mutate(dplyr::across(!País, as.numeric)) |>
        na.omit() |>
        tidyr::pivot_longer(!País,
                            names_to = "year",
                            values_to = "monto") |>
        dplyr::rename(partida = "País") |>
        dplyr::mutate(year = as.numeric(year))
    )
  )

  data

}

#' Get remittances by currency
get_remesas_currency <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_MP.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressWarnings(
    suppressMessages(
      readxl::read_excel(file_path,
                         skip = 5,
                         n_max = 4) |>
        dplyr::mutate(dplyr::across(!Detalle, as.numeric)) |>
        na.omit() |>
        tidyr::pivot_longer(!Detalle,
                            names_to = "year",
                            values_to = "proporcion") |>
        dplyr::rename(partida = "Detalle") |>
        dplyr::mutate(year = as.numeric(year))
    )
  )

  data

}

#' Get remittances by payment company
get_remesas_epa <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_PP.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressWarnings(
    suppressMessages(
      readxl::read_excel(file_path,
                         skip = 5,
                         n_max = 4) |>
        dplyr::mutate(dplyr::across(!Detalle, as.numeric)) |>
        na.omit() |>
        tidyr::pivot_longer(!Detalle,
                            names_to = "year",
                            values_to = "proporcion") |>
        dplyr::rename(partida = "Detalle") |>
        dplyr::mutate(year = as.numeric(year))
    )
  )

  data

}

#' Get remittances by gender
get_remesas_genero <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/documents/",
    "Remesas_GR.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  data <- suppressWarnings(
    suppressMessages(
      readxl::read_excel(file_path,
                         skip = 5,
                         n_max = 4) |>
        dplyr::mutate(dplyr::across(!Genero, as.numeric)) |>
        na.omit() |>
        tidyr::pivot_longer(!Genero,
                            names_to = "year",
                            values_to = "proporcion") |>
        dplyr::rename(partida = "Genero") |>
        dplyr::mutate(year = as.numeric(year))
    )
  )

  data

}
