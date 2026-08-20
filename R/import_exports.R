#' Construye el catalogo jerarquico de exportaciones a partir del archivo raw
#'
#' Funcion interna que limpia el encabezado descriptivo de las hojas de
#' exportaciones del Banco Central y deriva `id`, `categoria`, `nivel` y
#' `regimen` para cada fila, a partir de la numeracion presente en el label
#' original. El bloque de "Total" no trae numeracion en el archivo fuente,
#' asi que se le asignan ids sinteticos consistentes con el esquema
#' existente (Total = "4" implicito, dado que Minerales/Agropecuarios/
#' Industriales son 1/2/3), y sus desgloses (Nacionales, Zonas Francas,
#' Bienes Adquiridos en Puerto) se etiquetan como categoria "Subtotal" en
#' vez de "Total", ya que son subtotales del gran total y no el total en si.
#'
#' @param raw_exportaciones tibble crudo con la primera columna como `id`
#'   y la segunda como el label original de cada fila (`og_label`)
#'
#' @return Un tibble con columnas `id`, `og_label`, `label`, `categoria`,
#'   `nivel` y `regimen`
#' @noRd
construir_catalogo_exportaciones <- function(raw_exportaciones) {
  raw_exportaciones |>
    dplyr::select(id = 1, og_label = 2) |>
    janitor::remove_empty(which = "rows") |>
    dplyr::mutate(
      id_in_label = stringr::str_extract(og_label, "^\\d[\\.\\d]+"),
      id = dplyr::coalesce(id, id_in_label) |>
        stringr::str_remove("\\.$"),
      # el bloque de "Total" no trae numeracion en el og_label ni id propio
      # en el raw -- se asignan ids sinteticos consistentes con el esquema
      # existente (Total = "4" implicito, ya que Minerales/Agro/Industrial son 1/2/3)
      id = dplyr::coalesce(
        id,
        dplyr::case_when(
          og_label == "Nacionales" ~ "4.1",
          og_label == "Zonas Francas" ~ "4.2",
          og_label == "Bienes Adquiridos en Puerto" ~ "4.3",
          og_label == "Combustibles para aeronaves" ~ "4.3.1",
          og_label == "Alimentos para aeronaves" ~ "4.3.2",
          TRUE ~ NA_character_
        )
      ),
      label = stringr::str_remove_all(og_label, "[\\d\\./]|\\(.+\\)") |>
        stringr::str_squish(),
      categoria = dplyr::case_when(
        stringr::str_detect(id, "^\\d$") ~ label,
        label == "Total" ~ "Total",
        TRUE ~ NA
      ),
      nivel = dplyr::case_when(
        stringr::str_length(id) == 1 ~ 2,
        stringr::str_length(id) == 3 ~ 3,
        stringr::str_length(id)  > 3 ~ 4,
        is.na(id) ~ 1
      )
    ) |>
    dplyr::select(-id_in_label) |>
    tidyr::fill(categoria) |>
    dplyr::mutate(
      # las filas bajo Total son subtotales del gran total, no el total en si
      categoria = dplyr::if_else(categoria == "Total" & nivel %in% c(3, 4), "Subtotal", categoria),
      regimen = dplyr::if_else(nivel == 3, label, NA)
    ) |>
    tidyr::fill(regimen) |>
    dplyr::mutate(
      regimen = dplyr::if_else(nivel < 4, NA, regimen),
      label = dplyr::if_else(nivel == 3, paste(categoria, label), label)
    )
}

#' Exportaciones totales por sector
#'
#' Descarga y consolida las cifras de exportaciones totales de Republica
#' Dominicana por sector (minerales, agropecuarios, industriales) segun la
#' periodicidad solicitada, a partir de los archivos publicados por el
#' Banco Central en su portal de estadisticas del sector externo.
#'
#' @param frecuencia Cadena de texto con la periodicidad de los datos.
#'   Valores validos: "mensual", "trimestral" o "anual".
#' @param filtro_categoria Vector de caracteres opcional para filtrar por
#'   categoria (p. ej. "Minerales", "Agropecuarios", "Industriales",
#'   "Subtotal", "Total"). Si es `NULL` (por defecto) no se filtra.
#' @param filtro_nivel Vector numerico opcional para filtrar por nivel
#'   jerarquico (1 a 4). Si es `NULL` (por defecto) no se filtra.
#' @param filtro_regimen Vector de caracteres opcional para filtrar por
#'   regimen ("Nacionales" o "Zonas Francas"). Si es `NULL` (por defecto)
#'   no se filtra.
#'
#' @return Un tibble con las exportaciones por sector, con columnas de
#'   fecha (o year/trimestre segun la frecuencia), categoria, nivel,
#'   regimen y valor exportado.
#' @export
#'
#' @examples
#' \dontrun{
#' get_exportaciones("mensual")
#' get_exportaciones("anual", filtro_categoria = "Industriales")
#' get_exportaciones("trimestral", filtro_regimen = "Zonas Francas")
#' }
get_exportaciones <- function(
    frecuencia = c("mensual", "trimestral", "anual"),
    filtro_categoria = NULL,
    filtro_nivel = NULL,
    filtro_regimen = NULL
) {
  frecuencia <- rlang::arg_match(frecuencia)

  years <- 2010:lubridate::year(Sys.Date())

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-externo/documents/Exportaciones_Mensuales_",
    years, "_6.xls"
  )

  files_path <- tempfile(pattern = as.character(years), fileext = ".xls")

  save_download <- purrr::possibly(utils::download.file, otherwise = NA) # nolint
  on.exit(unlink(files_path), add = TRUE)

  purrr::walk2(
    url_descarga,
    files_path,
    \(url, file) {
      save_download(url, file, mode = "wb", quiet = TRUE)
    },
    .progress = TRUE
  ) |> suppressWarnings()

  files_path <- files_path[file.exists(files_path)]

  suppressMessages(
    suppressWarnings(
      exportaciones <- purrr::map(
        files_path,
        readxl::read_excel,
        col_names = TRUE,
        skip = 8,
        na = "n.d."
      )  |>
        stats::setNames(years[seq_along(files_path)])
    )
  )

  exportaciones1 <- exportaciones |>
    purrr::map(
      \(year_data) {
        meses <- crear_mes(1:12, "number_to_shorttext") |> tolower()

        year_data_clean <-  year_data |>
          janitor::clean_names() |>
          janitor::remove_empty(which = "rows") |>
          dplyr::filter(dplyr::if_all(dplyr::any_of(meses), \(x) !is.na(x)))

        catalogo <- year_data_clean |> construir_catalogo_exportaciones()

        catalogo |>
          dplyr::bind_cols(
            dplyr::select(year_data_clean, dplyr::any_of(meses))
          ) |>
          # Algunas veces dejan los meses futuros y solo los ocultan
          # hay que removerlos.
          dplyr::select(where(~ any(.x > 0, na.rm = TRUE))) |>
          tidyr::pivot_longer(
            names_to = "mes",
            values_to = "valor_expor",
            cols = -c(id, og_label, label, categoria, nivel, regimen)
          )
      }) |>
        dplyr::bind_rows(.id = "year") |>
        dplyr::mutate(
          mes = crear_mes(mes,
          type = "text_to_number"),
          fecha = lubridate::make_date(year, mes, "1"),
          trimestre = lubridate::quarter(fecha),
          .after = regimen
        )

  data <- if (frecuencia == "mensual") {
    exportaciones1 |>
      dplyr::select(-trimestre)
  } else if (frecuencia == "trimestral") {
    exportaciones1 |>
      dplyr::select(-c(mes, fecha)) |>
      dplyr::group_by(year, trimestre, label, categoria, nivel, regimen) |>
      dplyr::summarize(valor_expor = sum(valor_expor)) |>
      dplyr::ungroup()
  } else if (frecuencia == "anual") {
    exportaciones1 |>
      dplyr::select(-c(trimestre, mes, fecha)) |>
      dplyr::group_by(year, label, categoria, nivel, regimen) |>
      dplyr::summarize(valor_expor = sum(valor_expor)) |>
      dplyr::ungroup()
  }

  data |>
    dplyr::filter(
      (is.null(filtro_categoria) | categoria %in% filtro_categoria) &
      (is.null(filtro_nivel)     | nivel %in% filtro_nivel) &
      (is.null(filtro_regimen)   | regimen %in% filtro_regimen)
    )
}

#' Exportaciones de zonas francas por partida
#'
#' Descarga y consolida las exportaciones de zonas francas de la Republica
#' Dominicana desagregadas por partida (tipo de bien), a partir del archivo
#' publicado por el Banco Central en su portal de estadisticas del sector
#' externo. Los valores estan expresados en millones de USD.
#'
#' @details
#' La funcion identifica las columnas de partidas a partir del encabezado
#' del archivo original, removiendo las notas al pie (p. ej. "1/", "2/").
#' Se descartan las filas que ya vienen acumuladas por anio (aquellas cuya
#' etiqueta de mes contiene un anio de 4 digitos, usadas como totales en el
#' archivo fuente). Las fechas se generan de forma secuencial, un mes por
#' fila, comenzando en enero de 2010.
#'
#' @return Un tibble en formato largo con columnas `fecha`, `year`, `mes`,
#'   `partida` y `valor` (en millones de USD).
#'   \describe{
#'     \item{fecha}{Fecha del periodo (primer dia del mes)}
#'     \item{year}{Anio del periodo}
#'     \item{mes}{Mes del periodo (1-12)}
#'     \item{partida}{Tipo de bien exportado desde zonas francas}
#'     \item{valor}{Valor exportado, en millones de USD}
#'   }
#'
#' @source \url{https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Exportaciones_Zonas_Francas_6.xls}
#' @export
#'
#' @examples
#' \dontrun{
#' get_exportaciones_zf()
#' }
get_exportaciones_zf <- function() {
  file_url <- base::paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/",
    "documents/Exportaciones_Zonas_Francas_6.xls"
  )
  file_path <- base::tempfile(pattern = "", fileext = ".xls")

  download_file(file_url, file_path)

  raw_data <- readxl::read_excel(path = file_path, skip = 8) |>
    suppressMessages()

  rubros <- names(raw_data) |>
    stringr::str_remove_all(" \\d/") |>
    stringr::str_subset("^\\w")

  headers <- c("year", "mes_number", "mes_label", rubros)

  usethis::ui_info("Valores en millones de USD")

  raw_data |>
    purrr::set_names(headers) |>
    dplyr::filter(
      dplyr::if_any(dplyr::any_of(rubros), \(x) x > 0),
      stringr::str_detect(mes_label, "\\d{4}", negate = TRUE)
    ) |>
    dplyr::mutate(
      fecha = seq(
        lubridate::ymd("2010-01-01"),
        by = "month",
        length.out = dplyr::n()
      ),
      year = lubridate::year(fecha),
      mes  = lubridate::month(fecha),
      .before = year
    ) |>
    dplyr::select(-c(mes_number, mes_label)) |>
    tidyr::pivot_longer(-c(fecha, year, mes), names_to = "partida", values_to = "valor")
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
        dplyr::select(where(~ any(.x > 0, na.rm = TRUE))) |>
        dplyr::bind_cols(imports_details) |>
        tidyr::pivot_longer(
          names_to = "mes",
          values_to = "valor_impor",
          cols = -c(original_names, labels, short_names, categoria, nivel, direct_parent)
        )
    ) |>
    dplyr::bind_rows(.id = "year") |>
    dplyr::filter(!grepl("^x|^total", mes)) |>
    dplyr::mutate(
      mes = crear_mes(mes, type = "text_to_number"),
      fecha = lubridate::make_date(year, mes, "1"),
      trimestre = lubridate::quarter(fecha, with_year = TRUE)
    )

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
      dplyr::group_by(
        year, original_names, labels, short_names,
        categoria, nivel, direct_parent
      ) |>
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
  file_url <- base::paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-externo/",
    "documents/Importaciones_Crudo_6.xls"
  )

  file_path <- base::tempfile(pattern = "", fileext = ".xls")

  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

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

  data <- readxl::read_excel(
    path = file_path,
    skip = 8,
    col_names = headers
  ) |>
    dplyr::filter(!is.na(PetroleoCrudoXPrecio), !grepl("^2", Fecha)) |>
    dplyr::mutate(
      fecha = seq(as.Date("2010-01-01"), length.out = dplyr::n(), by = "month"),
      PetroleoCrudoXVolumen = as.numeric(PetroleoCrudoXVolumen)
    ) |>
    dplyr::filter(dplyr::if_any(where(is.numeric), ~ .x > 0)) |>
    dplyr::select(-Fecha) |>
    tidyr::pivot_longer(!fecha, names_to = "partida", values_to = "valor_impor") |>
    tidyr::separate_wider_delim(
      cols = partida,
      delim = "X",
      names = c("categoria", "partida")
    )

  return(data)

}
