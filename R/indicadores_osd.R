#' Obtener indicadores de Otras Sociedades de Depósito (OSD)
#'
#' Descarga la serie de indicadores de OSD publicada por el Banco Central
#' de la República Dominicana, la transforma a formato largo (tidy) y la
#' enriquece con los metadatos de clasificación provenientes de
#' `detalles_indicadores_osd`.
#'
#' @param filter_by_variable Vector de caracteres opcional. Filtra por
#'   una o más de las siguientes categorías:
#'   \itemize{
#'     \item `"Activos externos"`
#'     \item `"Pasivos externos"`
#'     \item `"Inversiones"`
#'     \item `"Préstamos"`
#'     \item `"Depósitos"`
#'     \item `"Composición depósitos"`
#'     \item `"Tasa de cambio"`
#'   }
#'   `NULL` (por defecto) no aplica filtro.
#' @param filter_by_sector Vector de caracteres opcional. Filtra por
#'   uno o más de los siguientes valores:
#'   \itemize{
#'     \item `"Total"`
#'     \item `"Sector público"`
#'     \item `"Sector privado"`
#'     \item `"Sociedades financieras"`
#'     \item `"No residentes"`
#'     \item `"Depósitos transferibles"`
#'     \item `"Otros depósitos"`
#'     \item `"Valores distintos de acciones"`
#'   }
#'   Aplica solo a las variables `"Préstamos"`, `"Depósitos"` y
#'   `"Composición depósitos"`; para el resto de variables esta columna
#'   es `NA` (ver "Nota sobre valores `NA`" más abajo).
#' @param filter_by_moneda Vector de caracteres opcional. Filtra por
#'   uno o más de los siguientes valores: `"Total"`, `"DOP"`, `"USD"`.
#'   Aplica solo a `"Préstamos"` y `"Depósitos"`; para el resto de
#'   variables esta columna es `NA`.
#' @param filter_by_entidad Vector de caracteres opcional. Filtra por
#'   uno o más de los siguientes valores:
#'   \itemize{
#'     \item `"General"` (agregado, no desglosado por entidad)
#'     \item `"Bancos múltiples"`
#'     \item `"Resto OSD"`
#'   }
#' @param filter_by_nivel Vector entero opcional. Filtra por nivel de
#'   agregación:
#'   \itemize{
#'     \item `1` — Total agregado
#'     \item `2` — Por sector
#'     \item `3` — Por moneda
#'     \item `4` — Por entidad
#'   }
#'
#' @section Nota sobre valores `NA` en `sector` y `moneda`:
#' Variables como `"Activos externos"`, `"Pasivos externos"`,
#' `"Inversiones"` y `"Tasa de cambio"` no tienen desglose por sector
#' ni por moneda, por lo que esas columnas quedan en `NA` para esas
#' filas. Como `NA %in% x` siempre evalúa a `FALSE` (nunca `NA`),
#' aplicar `filter_by_sector` o `filter_by_moneda` excluye
#' automáticamente esas variables del resultado. Si quieres esas
#' variables junto con un filtro de sector/moneda, combina con
#' `filter_by_variable` en la misma llamada, o filtra en dos pasos.
#'
#' @return Un tibble en formato largo con columnas `row_id`, `indicador`,
#'   `date`, `value`, `variable`, `sector`, `moneda`, `entidad`, `nivel`,
#'   `nivel_tipo`.
#'
#' @examples
#' \dontrun{
#' indicadores_osd()
#'
#' indicadores_osd(
#'   filtro_variable = "Préstamos",
#'   filtro_sector = "Sector privado",
#'   filtro_moneda = "USD"
#' )
#'
#' indicadores_osd(filter_by_entidad = c("Bancos múltiples", "Resto OSD"))
#' }
#'
#' @export
indicadores_osd <- function(
    filtro_variable   = NULL,
    filtro_nivel_tipo = NULL,
    filtro_sector     = NULL,
    filtro_moneda     = NULL,
    filtro_entidad    = NULL
) {
    url <- paste0(
      "https://cdn.bancentral.gov.do/documents/estadisticas/",
      "sector-monetario-y-financiero/documents/serie_indicadores_osd.xlsx"
    )

    file_url <- tempfile(fileext = ".xlsx")
    download_file(url, file_url)

    indicadores_raw <- readxl::read_excel(file_url, skip = 4) |>
      dplyr::rename(indicador = 1)

    headers <- c(
      "indicador",
      seq(
        from = lubridate::ymd("1996-01-01"),
        by = "month",
        length.out = ncol(indicadores_raw) - 1
      ) |> as.character()
    )

    indicadores_raw |>
      dplyr::filter(dplyr::if_all(-indicador, ~ !is.na(.x))) |>
      purrr::set_names(headers) |>
      dplyr::mutate(
        indicador = stringr::str_remove_all(indicador, "\\(\\d\\)"),
        indicador = stringr::str_squish(indicador),
        row_id = dplyr::row_number(),
        .before = indicador
      ) |>
      tidyr::pivot_longer(-c(row_id, indicador), names_to = "date", values_to = "value") |>
      dplyr::mutate(date = lubridate::ymd(date)) |>
      dplyr::left_join(detalles_indicadores_osd) |>
      dplyr::filter(
        is.null(filtro_variable)   | variable   %in% filtro_variable,
        is.null(filtro_nivel_tipo) | nivel_tipo %in% filtro_nivel_tipo,
        is.null(filtro_sector)     | sector     %in% filtro_sector,
        is.null(filtro_moneda)     | moneda     %in% filtro_moneda,
        is.null(filtro_entidad)    | entidad       %in% filtro_entidad,
      ) |>
      dplyr::rename(id_indicador = row_id)
}

