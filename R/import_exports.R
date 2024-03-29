#' Total exports by sectors
#'
#' This function returns total exports by sectors in the  Dominican Republic
#' based on the specified frequency.
#'
#' @param frecuencia A character string that specifies the frequency of the
#' data to be downloaded. Valid options are "mensual",
#' "trimestral",  or "anual".
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_exportaciones("mensual")
#' get_exportaciones("trimestral")
#' get_exportaciones("anual")

get_exportaciones <- function(frecuencia = "mensual") {
  checkmate::assert_choice(
    frecuencia,
    choices = c("mensual", "trimestral", "anual"))

  years <- 2010:lubridate::year(Sys.Date())

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-externo/documents/Exportaciones_Mensuales_",
    years, "_6.xls"
  )

  files_path <- tempfile(pattern = as.character(years), fileext = ".xls")

  save_download <- purrr::possibly(utils::download.file, otherwise = NA) # nolint

  purrr::walk2(
    url_descarga,
    files_path,
    \(url, file) save_download(url, file, mode = "wb", quiet = TRUE)
  ) |> suppressWarnings()

  files_path <- files_path[file.exists(files_path)]

  suppressMessages(
    suppressWarnings(
      exportaciones <- purrr::map(
        files_path,
        readxl::read_excel,
        col_names = TRUE, skip = 8, na = "n.d.",
        n_max = 70)  |>
        stats::setNames(years[seq_along(files_path)])
    )
  )

  exportaciones1 <- exportaciones |>
    purrr::map(
      ~.x |>
        janitor::clean_names() |>
        dplyr::slice(-1) |>
        tidyr::drop_na(x2) |>
        dplyr::select(-x1, -x2, -dplyr::last_col()) |>
        dplyr::bind_cols(exports_details) |>
        tidyr::pivot_longer(names_to = "mes",
                            values_to = "valor_expor",
                            cols = -c(original_names, labels, short_names,
                                      categoria, nivel, direct_parent)
                            )
      ) |>
        dplyr::bind_rows(.id = "year") |>
        dplyr::mutate(mes = crear_mes(mes,
                                      type = "text_to_number"),
                      fecha = lubridate::make_date(year, mes, "1"),
                      trimestre = lubridate::quarter(fecha, with_year = TRUE))

  if (frecuencia == "mensual") {
    data <- exportaciones1 |>
      dplyr::select(-c(year, mes, trimestre))
  } else if (frecuencia == "trimestral") {
    data <- exportaciones1 |>
      dplyr::select(-c(year, mes, fecha)) |>
      dplyr::group_by(trimestre, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_expor = sum(valor_expor)) |>
      suppressMessages()
  } else if (frecuencia == "anual") {
    data <- exportaciones1 |>
      dplyr::select(-c(trimestre, mes, fecha)) |>
      dplyr::group_by(year, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_expor = sum(valor_expor)) |>
      suppressMessages()
  }

  return(data)
}

#' Free Trade Exports
#'
#' This function returns Free Trade exports from the Dominican Republic
#' by goods
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_exportaciones_zf()
get_exportaciones_zf <- function() {
  file_url <- base::paste0("https://cdn.bancentral.gov.do/documents/",
                     "estadisticas/sector-externo/",
                     "documents/Exportaciones_Zonas_Francas_6.xls")
  file_path <- base::tempfile(pattern = "", fileext = ".xls")

  utils::download.file(file_url,
                       file_path,
                       mode = "wb",
                       quiet = TRUE)


  data <- suppressMessages(
    readxl::read_excel(path = file_path,
                       skip = 8) |>
      janitor::clean_names() |>
      dplyr::filter(!is.na(confecciones_textiles),
                    !grepl("^2", x3)) |>
      dplyr::mutate(fecha = seq(
        as.Date("2010-01-01"),
        length.out = dplyr::n(),
        by = "month")) |>
      dplyr::select(-c(x1, x2, x3)) |>
      tidyr::pivot_longer(!fecha,
                          names_to = "partida",
                          values_to = "valor_expor")
  )

  return(data)

}

#' Total imports by sectors
#'
#' This function returns total imports by sectors in the  Dominican Republic
#' based on the specified frequency.
#'
#' @param frecuencia A character string that specifies the frequency of the
#' data to be downloaded. Valid options are "mensual",
#' "trimestral",  or "anual".
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_importaciones("mensual")
#' get_importaciones("trimestral")
#' get_importaciones("anual")

get_importaciones <- function(frecuencia = "mensual") {
  checkmate::assert_choice(
    frecuencia,
    choices = c("mensual", "trimestral", "anual"))

  years <- 2010:lubridate::year(Sys.Date())

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-externo/documents/Importaciones_Mensuales_",
    years, "_6.xls")

  files_path <- tempfile(pattern = as.character(years), fileext = ".xls")

  save_download <- purrr::possibly(utils::download.file, otherwise = NA) # nolint

  purrr::walk2(
    url_descarga,
    files_path,
    \(url, file) save_download(url, file, mode = "wb", quiet = TRUE)
  ) |> suppressWarnings()

  files_path <- files_path[file.exists(files_path)]

  suppressMessages(
    suppressWarnings(
      importaciones <- purrr::map(
        files_path,
        readxl::read_excel,
        col_names = TRUE, skip = 8, na = "n.d.",
        n_max = 70)  |>
        stats::setNames(years[seq_along(files_path)])
    )
  )

  importaciones1 <- importaciones |>
    purrr::map(
      ~.x |>
        janitor::clean_names() |>
        dplyr::slice(-1) |>
        tidyr::drop_na(ene) |>
        dplyr::select(-x1, -x2, -dplyr::last_col()) |>
        dplyr::bind_cols(imports_details) |>
        tidyr::pivot_longer(names_to = "mes",
                            values_to = "valor_impor",
                            cols = -c(original_names, labels, short_names,
                                      categoria, nivel, direct_parent)
        )
    ) |>
    dplyr::bind_rows(.id = "year") |>
    dplyr::filter(!grepl("^x|^total", mes)) |>
    dplyr::mutate(mes = crear_mes(mes,
                                  type = "text_to_number"),
                  fecha = lubridate::make_date(year, mes, "1"),
                  trimestre = lubridate::quarter(fecha, with_year = TRUE))

  if (frecuencia == "mensual") {
    data <- importaciones1 |>
      dplyr::select(-c(year, mes, trimestre))
  } else if (frecuencia == "trimestral") {
    data <- importaciones1 |>
      dplyr::select(-c(year, mes, fecha)) |>
      dplyr::group_by(trimestre, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_impor = sum(valor_impor)) |>
      suppressMessages()
  } else if (frecuencia == "anual") {
    data <- importaciones1 |>
      dplyr::select(-c(trimestre, mes, fecha)) |>
      dplyr::group_by(year, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_impor = sum(valor_impor)) |>
      suppressMessages()
  }

  return(data)
}

#' Oil Imports
#'
#' This function returns oil imports to the Dominican Republic
#' by type
#'
#' @return A data frame
#' @export
#'
#' @examples
#' get_exportaciones_zf()
get_importaciones_petroleo <- function() {
  file_url <- base::paste0("https://cdn.bancentral.gov.do/documents/",
                           "estadisticas/sector-externo/",
                           "documents/Importaciones_Crudo_6.xls")
  file_path <- base::tempfile(pattern = "", fileext = ".xls")

  utils::download.file(file_url,
                       file_path,
                       mode = "wb",
                       quiet = TRUE)

  headers <- c("Fecha",
               "PetroleoCrudoXVolumen", "PetroleoCrudoXPrecio",
               "PetroleoCrudoXValor",
               "GasolinaXVolumen", "GasolinaXPrecio", "GasolinaXValor",
               "GasoilXVolumen", "GasoilXPrecio", "GasoilXValor",
               "GLPXVolumen", "GLPXPrecio", "GLPXValor",
               "GasNaturalXVolumen", "GasNaturalXPrecio", "GasNaturalXValor",
               "FuelOilXVolumen", "FuelOilXPrecio", "FuelOilXValor",
               "GasolinadeAviacionXVolumen", "GasolinadeAviacionXPrecio",
               "GasolinadeAviacionXValor",
               "AvturXVolumen", "AvturXPrecio", "AvturXValor",
               "OtrosXVolumen", "OtrosXPrecio", "OtrosXValor",
               "TotalXVolumen", "TotalXPrecio", "TotalXValor")

  data <- readxl::read_excel(path = file_path,
                             skip = 8,
                             col_names = headers) |>
    dplyr::filter(!is.na(PetroleoCrudoXPrecio),
                  !grepl("^2", Fecha)) |>
    dplyr::mutate(fecha = seq(as.Date("2010-01-01"),
                              length.out = dplyr::n(),
                              by = "month"),
                  PetroleoCrudoXVolumen = as.numeric(PetroleoCrudoXVolumen)) |>
    dplyr::select(-Fecha) |>
    tidyr::pivot_longer(!fecha,
                        names_to = "partida",
                        values_to = "valor_impor") |>
    tidyr::separate_wider_delim(cols = partida,
                                delim = "X",
                                names = c("categoria", "partida"))

  return(data)

}
