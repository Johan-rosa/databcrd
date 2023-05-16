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

get_exportaciones <- function(frecuencia = 'mensual') {
  checkmate::assert_choice(
    frecuencia,
    choices = c("mensual", "trimestral", "anual"))

  periodo <- c(2010:lubridate::year(Sys.Date()))

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-externo/documents/Exportaciones_Mensuales_",
    periodo,"_6.xls?v=1571253704905"
  )

  files_path <- purrr::map_chr(
    periodo,
    ~tempfile(pattern = "", fileext = ".xls")
  )

  download <- purrr::possibly(download.file, otherwise = -1)

  suppressWarnings(
    descarga <- purrr::map2(
      url_descarga,
      files_path,
      ~download(.x, .y, mode = "wb", quiet = TRUE))
  )

  files_path <- files_path[as.logical(unlist(descarga) + 1)]

  suppressMessages(
    suppressWarnings(
      # importar archivos
      exportaciones <- purrr::map(
        files_path,
        readxl::read_excel, sheet = 1,
        col_names = TRUE, skip = 8, na = "n.d.",
        n_max = 70)  |>
        setNames(periodo[as.logical(unlist(descarga) + 1)])
    )
  )

  exportaciones1 <- exportaciones |>
    purrr:::map(
      ~.x |>
        janitor::clean_names() |>
        dplyr::slice(-1) |>
        tidyr::drop_na(x2) |>
        dplyr::select(-x1, -x2, -dplyr::last_col()) |>
        dplyr::bind_cols(exports_details) |>
        tidyr::gather('mes', "valor_expor", -c(original_names, labels, short_names,
                                               categoria, nivel, direct_parent))
      ) |>
        dplyr::bind_rows(.id = "year") |>
        dplyr::mutate(mes = crear_mes(stringr::str_to_title(mes),
                                      type = 'text_to_number'),
                      fecha = as.Date(paste(year, mes, '1', sep = '-')),
                      trimestre = lubridate::quarter(fecha, with_year = TRUE))

  if(frecuencia == 'mensual') {
    data <- exportaciones1 |>
      dplyr::select(-c(year, mes, trimestre))
  } else if(frecuencia == 'trimestral') {
    data <- exportaciones1 |>
      dplyr::select(-c(year, mes, fecha)) |>
      dplyr::group_by(trimestre, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_expor = sum(valor_expor)) |>
      suppressMessages()
  } else if(frecuencia == 'anual') {
    data <- exportaciones1 |>
      dplyr::select(-c(trimestre, mes, fecha)) |>
      dplyr::group_by(year, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_expor = sum(valor_expor)) |>
      suppressMessages()
  }

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

get_importaciones <- function(frecuencia = 'mensual') {
  checkmate::assert_choice(
    frecuencia,
    choices = c("mensual", "trimestral", "anual"))

  periodo <- c(2010:lubridate::year(Sys.Date()))

  url_descarga <- paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                         "sector-externo/documents/Importaciones_Mensuales_",
                         periodo,
                         "_6.xls?v=1571324085432")

  files_path <- purrr::map_chr(
    periodo,
    ~tempfile(pattern = "", fileext = ".xls")
  )

  download <- purrr::possibly(download.file, otherwise = -1)

  suppressWarnings(
    descarga <- purrr::map2(
      url_descarga,
      files_path,
      ~download(.x, .y, mode = "wb", quiet = TRUE))
  )

  files_path <- files_path[as.logical(unlist(descarga) + 1)]

  suppressMessages(
    suppressWarnings(
      # importar archivos
      importaciones <- purrr::map(
        files_path,
        readxl::read_excel, sheet = 1,
        col_names = TRUE, skip = 8, na = "n.d.",
        n_max = 70)  |>
        setNames(periodo[as.logical(unlist(descarga) + 1)])
    )
  )

  importaciones1 <- importaciones |>
    purrr:::map(
      ~.x |>
        janitor::clean_names() |>
        dplyr::slice(-1) |>
        tidyr::drop_na(ene) |>
        dplyr::select(-x1, -x2, -dplyr::last_col()) |>
        dplyr::bind_cols(imports_details) |>
        tidyr::gather('mes', "valor_impor", -c(original_names, labels, short_names,
                                               categoria, nivel, direct_parent))
    ) |>
    dplyr::bind_rows(.id = "year") |>
    dplyr::filter(!grepl("^x|^total", mes)) |>
    dplyr::mutate(mes = crear_mes(stringr::str_to_title(mes),
                                  type = 'text_to_number'),
                  fecha = as.Date(paste(year, mes, '1', sep = '-')),
                  trimestre = lubridate::quarter(fecha, with_year = TRUE))

  if(frecuencia == 'mensual') {
    data <- importaciones1 |>
      dplyr::select(-c(year, mes, trimestre))
  } else if(frecuencia == 'trimestral') {
    data <- importaciones1 |>
      dplyr::select(-c(year, mes, fecha)) |>
      dplyr::group_by(trimestre, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_impor = sum(valor_impor)) |>
      suppressMessages()
  } else if(frecuencia == 'anual') {
    data <- importaciones1 |>
      dplyr::select(-c(trimestre, mes, fecha)) |>
      dplyr::group_by(year, original_names, labels, short_names,
                      categoria, nivel, direct_parent) |>
      dplyr::summarize(valor_impor = sum(valor_impor)) |>
      suppressMessages()
  }

  return(data)
}
