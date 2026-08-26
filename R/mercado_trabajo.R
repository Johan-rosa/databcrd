#' Población en Edad de Trabajar (PET) y Fuerza de Trabajo (PEA)
#'
#' Descarga desde el Banco Central de la República Dominicana (BCRD) el
#' archivo trimestral de Población en Edad de Trabajar por Condición de
#' Actividad (PET, PEA, Ocupados, Desocupados e Inactivos) y lo organiza en
#' un tibble homogéneo, con una desagregación por sexo (Total, Masculino,
#' Femenino) tomada de las distintas hojas del archivo.
#'
#' @param filtro_desagregacion Vector de caracteres opcional. Uno o más
#'   valores entre "total", "masculino" y "femenino" (no distingue entre
#'   mayúsculas y minúsculas). Si es `NULL` (por defecto), se retornan las
#'   tres desagregaciones.
#'
#' @return Un tibble con las columnas `fecha`, `year`, `trimestre`,
#'   `desagregacion`, `pet`, `pea`, `ocupados`, `desocupados` e `inactivos`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pea()
#' pea("total")
#' pea(c("masculino", "femenino"))
#' }
pea <- function(filtro_desagregacion = NULL) {
  url <- "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/01_PET.xlsx"
  file_path <- tempfile(fileext = ".xlsx")
  download_file(url, file_path)

  data_pea <- readxl::excel_sheets(file_path) |>
    purrr::set_names() |>
    purrr::map(
      \(sheet) {
        readxl::read_excel(file_path, sheet = sheet, skip = 9) |>
          suppressMessages() |>
          purrr::set_names(c("year", "trimestre", "pet", "pea", "ocupados", "desocupados", "inactivos")) |>
          tidyr::fill(year) |>
          # Se descartan filas de notas al pie / totales / filas en blanco
          # que trae el Excel debajo de la tabla de datos: cualquier fila
          # donde alguna columna de valores (todo menos year/trimestre)
          # esté vacía se considera "no es un dato".
          dplyr::filter(dplyr::if_all(-c(year, trimestre), \(x) !is.na(x))) |>
          dplyr::mutate(
            trimestre = stringr::str_extract(trimestre, "[IV]+"),
            trimestre = dplyr::recode(trimestre, "I" = 1, "II" = 2, "III" = 3, "IV" = 4),
            fecha = lubridate::make_date(year, trimestre * 3, 1),
            year = as.numeric(year)
          ) |>
          dplyr::relocate(fecha, year, trimestre)
      }
    ) |>
    purrr::list_rbind(names_to = "desagregacion") |>
    # Normaliza el nombre de la hoja "PET Total" a "Total". Si el BCRD
    # nombra las otras hojas de otra forma en el futuro, este recode no
    # las va a "arreglar" silenciosamente.`
    dplyr::mutate(
      desagregacion = dplyr::recode(desagregacion, "PET Total" = "Total")
    )

  if (is.null(filtro_desagregacion)) return(data_pea)

  to_keep <- tolower(filtro_desagregacion)
  all_correct_values <- all(to_keep %in% c("total", "masculino", "femenino"))

  if (!all_correct_values) {
    rlang::abort("El filtro debe contener los valores `total`, `masculino` y/o `femenino`")
  }

  data_pea |>
    dplyr::filter(tolower(desagregacion) %in% to_keep)
}


#' Las primeras 2 filas leídas traen el encabezado combinado (año arriba,
#' número romano del trimestre abajo); `ml_fechas()` las combina en un
#' vector de fechas/etiquetas, una por columna de datos.
ml_fechas <- function(raw_data) {
  years <- dplyr::tibble(values = unlist(raw_data[1,], use.names = FALSE)[-1]) |>
    tidyr::fill(values) |>
    dplyr::pull(values)

  trimestres <- dplyr::tibble(values = unlist(raw_data[2,], use.names = FALSE)[-1]) |>
    dplyr::mutate(
      values = stringr::str_extract(values, "[IV]+"),
      values = dplyr::recode(values, "I" = 1, "II" = 2, "III" = 3, "IV" = 4)
    ) |>
    dplyr::pull(values)

  lubridate::make_date(years, trimestres * 3, 1) |> as.character()
}

#' Parsear una tabla de indicadores del mercado laboral del BCRD
#'
#' Función interna que toma el archivo ya descargado `00_Indicadores.xlsx`
#' y extrae una de sus dos tablas (niveles o tasas), para una de las tres
#' hojas del archivo (Indicadores/Total, Masculino, Femenino). Convierte la
#' tabla de formato ancho (un indicador por fila, un trimestre por columna)
#' a formato largo.
#'
#' @param file_path Ruta al archivo `00_Indicadores.xlsx` ya descargado.
#' @param sheet Nombre de la hoja a leer ("Indicadores", "Masculino" o
#'   "Femenino").
#' @param skip Número de filas a saltar antes del encabezado de la tabla
#'   (difiere entre la tabla de niveles y la de tasas, ya que comparten hoja).
#' @param n_max Número de filas a leer, incluyendo las 2 filas de encabezado
#'   (año + trimestre en números romanos) y las filas de indicadores.
#' @param values_name Nombre de la columna de valores en el resultado
#'   (`"poblacion"` para niveles, `"tasa"` para tasas).
#' @param filtro_indicador Vector de caracteres opcional. Cada elemento se
#'   trata como un patrón de texto (no distingue mayúsculas/minúsculas) y se
#'   usa para hacer *coincidencia parcial* sobre el nombre del indicador
#'   (ej. `"desocup"` coincide con "Desocupados Abiertos" y con "Desocupados
#'   (Abiertos con iniciadores)"). No es una búsqueda exacta.
#'
#' @return Un tibble en formato largo con las columnas `indicador`, `fecha`
#'   y la columna indicada en `values_name`.
#'
#' @noRd
parse_indicadores_laborales <- function(file_path, sheet, skip, n_max, values_name, filtro_indicador = NULL) {
  raw_data <- readxl::read_excel(
    file_path,
    sheet = sheet,
    skip = skip,
    n_max = n_max,
    col_names = FALSE
  ) |>
    suppressMessages()

  # Las primeras 2 filas leídas traen el encabezado combinado (año arriba,
  # número romano del trimestre abajo); `ml_fechas()` las combina en un
  # vector de fechas/etiquetas, una por columna de datos.
  fechas <- ml_fechas(raw_data)

  wide_data <- raw_data[-c(1, 2), ] |>
    purrr::set_names(c("indicador", fechas)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(fechas), as.numeric),
      # Quita las referencias a notas al pie del nombre del indicador
      # (ej. "Sector Formal 2/" -> "Sector Formal").
      indicador = stringr::str_remove(indicador, "\\d/") |>
        stringr::str_squish()
    )

  if (!is.null(filtro_indicador)) {
    # ADVERTENCIA: `filtro_indicador` se usa tal cual como patrón de regex.
    # Si algún indicador o filtro contiene caracteres especiales de regex
    # (ej. paréntesis o "+", como en "SU4: Desocupación + Subocupación...").
    pattern <- paste(filtro_indicador, collapse = "|") |>
      tolower()

    wide_data <- wide_data |>
      dplyr::filter(stringr::str_detect(tolower(indicador), pattern))
  }

  if (nrow(wide_data) == 0) {
    rlang::abort("El `filtro_indicador` no devuelve ningún resultado")
  }

  wide_data |>
    tidyr::pivot_longer(
      dplyr::all_of(fechas),
      names_to = "fecha",
      values_to = values_name
    )
}

#' Principales indicadores del mercado laboral en niveles (personas)
#'
#' Descarga y organiza, en formato largo, la tabla de niveles (personas) de
#' los Principales Indicadores del Mercado Laboral publicados por el BCRD:
#' Población Total, PET, PEA, Ocupados, Subocupados, formalidad, Cesantes,
#' Nuevos, Fuerza de Trabajo Potencial, Inactivos, etc. Incluye la
#' desagregación por sexo (Total, Masculino, Femenino).
#'
#' @param filtro_indicador Vector de caracteres opcional para filtrar por
#'   nombre de indicador. La coincidencia es parcial y no distingue
#'   mayúsculas/minúsculas (ver detalles en `parse_indicadores_laborales()`).
#'
#' @return Un tibble con las columnas `desagregacion`, `indicador`, `fecha`
#'   y `poblacion`.
#'
#' @export
mercado_laboral_niveles <- function(filtro_indicador = NULL) {
  url <- "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/00_Indicadores.xlsx"
  file_path <- tempfile(fileext = ".xlsx")
  download_file(url, file_path)

  c("Indicadores", "Masculino", "Femenino") |>
    purrr::set_names() |>
    purrr::map(
      \(sheet) {
        parse_indicadores_laborales(
          file_path = file_path,
          sheet = sheet,
          skip = 7,
          n_max = 22,
          filtro_indicador = filtro_indicador,
          values_name = "poblacion"
        )
      }
    ) |>
    purrr::list_rbind(names_to = "desagregacion") |>
    dplyr::mutate(desagregacion = dplyr::recode(desagregacion, "Indicadores" = "Total"))
}

#' Principales indicadores del mercado laboral en tasas (porcentajes)
#'
#' Descarga y organiza, en formato largo, la tabla de tasas (porcentajes) de
#' los Principales Indicadores del Mercado Laboral publicados por el BCRD:
#' Tasa Global de Participación, Tasa de Ocupación, Tasa de Desocupación
#' (SU1-SU4), Tasa de Inactividad, etc. Incluye la desagregación por sexo
#' (Total, Masculino, Femenino).
#'
#' @param filtro_indicador Vector de caracteres opcional para filtrar por
#'   nombre de indicador. La coincidencia es parcial y no distingue
#'   mayúsculas/minúsculas (ver detalles en `parse_indicadores_laborales()`).
#'
#' @return Un tibble con las columnas `desagregacion`, `indicador`, `fecha`
#'   y `tasa`.
#'
#' @export
mercado_laboral_tasas <- function(filtro_indicador = NULL) {
  url <- "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-de-trabajo/documents/00_Indicadores.xlsx"
  file_path <- tempfile(fileext = ".xlsx")
  download_file(url, file_path)

  c("Indicadores", "Masculino", "Femenino") |>
    purrr::set_names() |>
    purrr::map(
      \(sheet) {
        parse_indicadores_laborales(
          file_path = file_path,
          sheet = sheet,
          skip = 30,
          n_max = 17,
          filtro_indicador = filtro_indicador,
          values_name = "tasa"
        )
      }
    ) |>
    purrr::list_rbind(names_to = "desagregacion") |>
    dplyr::mutate(desagregacion = dplyr::recode(desagregacion, "Indicadores" = "Total"))
}
