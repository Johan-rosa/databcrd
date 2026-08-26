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
            fecha = lubridate::make_date(year, trimestre * 3, 1)
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

