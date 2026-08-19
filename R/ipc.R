#' Descarga el Índice de Precios al Consumidor (IPC) de República Dominicana
#'
#' Punto de entrada único para las distintas desagregaciones del IPC
#' publicadas por el Banco Central de la República Dominicana (BCRD).
#' Según el valor de `desagregacion`, delega en [get_ipc_general()],
#' [get_ipc_grupos()], [get_ipc_regiones()], [get_ipc_subyacente()] o
#' [get_ipc_tnt()].
#'
#' @details
#' Todas las series se descargan directamente desde los archivos Excel
#' publicados por el BCRD (base 2019-2020); no hay caché, así que cada
#' llamada dispara una descarga nueva.
#'
#' Para el detalle por artículo (grupo, subgrupo, clase, subclase y
#' artículo) hay que usar [get_ipc_articulos()] o [get_ipc_long()]: esta
#' función no acepta `"articulos"` como valor de `desagregacion`.
#'
#' @param desagregacion Cadena con la desagregación deseada. Una de:
#' \describe{
#'   \item{`"general"`}{Índice general del IPC, sin desagregar.}
#'   \item{`"grupos"`}{IPC por grupos de bienes y servicios (alimentos y
#'     bebidas, vivienda, transporte, etc.).}
#'   \item{`"regiones"`}{IPC por región geográfica (Ozama, Cibao, Este,
#'     Sur).}
#'   \item{`"subyacente"`}{IPC subyacente (inflación núcleo, excluye
#'     rubros volátiles).}
#'   \item{`"tnt"`}{IPC de bienes transables y no transables.}
#' }
#'
#' @return Un tibble; las columnas exactas dependen de `desagregacion`
#'   (ver [get_ipc_general()], [get_ipc_grupos()], [get_ipc_regiones()],
#'   [get_ipc_subyacente()] y [get_ipc_tnt()]). En todos los casos incluye
#'   al menos `fecha`, `year` y `mes`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_ipc_data("general")
#' get_ipc_data("grupos")
#' get_ipc_data("subyacente")
#' get_ipc_data("regiones")
#' get_ipc_data("tnt")
#' }
get_ipc_data <- function(
    desagregacion = c("general", "grupos", "regiones", "subyacente", "tnt")
) {
  desagregacion <- rlang::arg_match(desagregacion)

  result <- switch(
    desagregacion,
    "general" = get_ipc_general(),
    "grupos" = get_ipc_grupos(),
    "regiones" = get_ipc_regiones(),
    "subyacente" = get_ipc_subyacente(),
    "tnt" = get_ipc_tnt()
  )

  return(result)
}

#' Serie del IPC general de República Dominicana
#'
#' Descarga y limpia la serie mensual del Índice de Precios al Consumidor
#' (IPC) general, base 2019-2020, publicada por el Banco Central de la
#' República Dominicana (BCRD).
#'
#' @details
#' El archivo se descarga en cada llamada desde el Excel del BCRD; no hay
#' caché ni control de versión del archivo fuente. Se descarta el
#' encabezado del Excel (`skip = 7`) y el año se propaga hacia abajo con
#' [tidyr::fill()], porque en el archivo original solo aparece en la
#' primera fila de cada bloque de meses.
#'
#' Llamada internamente por [get_ipc_data()] con
#' `desagregacion = "general"`; no está exportada, así que para uso
#' normal conviene usar `get_ipc_data("general")`.
#'
#' @return Un tibble con una fila por mes y las columnas:
#' \describe{
#'   \item{fecha}{`Date`. Primer día del mes de la observación.}
#'   \item{year}{Año.}
#'   \item{mes}{Mes (1-12).}
#'   \item{ipc}{Índice de precios al consumidor.}
#'   \item{ipc_vm}{Variación mensual, en \%.}
#'   \item{ipc_vd}{Variación acumulada respecto a diciembre del año
#'     anterior, en \%.}
#'   \item{ipc_vi}{Variación interanual, en \%.}
#'   \item{ipc_p12}{Promedio de la variación interanual de los últimos 12
#'     meses, en \%.}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_base_2019-2020.xls>
#'
#' @examples
#' \dontrun{
#' get_ipc_general()
#' }
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

#' IPC de República Dominicana por grupo de bienes y servicios
#'
#' Descarga y limpia la serie mensual del Índice de Precios al Consumidor
#' (IPC) desagregada por grupo de bienes y servicios, base 2019-2020,
#' publicada por el Banco Central de la República Dominicana (BCRD).
#'
#' @details
#' A diferencia de [get_ipc_general()], cada grupo solo trae el índice y
#' su variación mensual (`_vm`); no incluye variación interanual,
#' acumulada ni promedio de 12 meses.
#'
#' La columna del grupo "Bienes y servicios diversos" tiene un nombre
#' inconsistente en el origen: el índice se llama `ipc_bines_servicios`
#' (sin la "e" de "bienes") mientras que su variación mensual sí se llama
#' `ipc_bienes_servicios_vm`. Se documenta tal cual está para no romper
#' código existente que dependa de estos nombres.
#'
#' Llamada internamente por [get_ipc_data()] con
#' `desagregacion = "grupos"`; no está exportada, así que para uso normal
#' conviene usar `get_ipc_data("grupos")`.
#'
#' @return Un tibble con una fila por mes y las columnas:
#' \describe{
#'   \item{fecha}{`Date`. Primer día del mes de la observación.}
#'   \item{year}{Año.}
#'   \item{mes}{Mes (1-12).}
#'   \item{ipc_ayb, ipc_ayb_vm}{Alimentos y bebidas no alcohólicas:
#'     índice y variación mensual (\%).}
#'   \item{ipc_alcohol_tabaco, ipc_alcohol_tabaco_vm}{Bebidas
#'     alcohólicas y tabaco: índice y variación mensual (\%).}
#'   \item{ipc_ropa_calzado, ipc_ropa_calzado_vm}{Ropa y calzado: índice
#'     y variación mensual (\%).}
#'   \item{ipc_vivienda, ipc_vivienda_vm}{Vivienda: índice y variación
#'     mensual (\%).}
#'   \item{ipc_muebles, ipc_muebles_vm}{Muebles y artículos para el
#'     hogar: índice y variación mensual (\%).}
#'   \item{ipc_salud, ipc_salud_vm}{Salud: índice y variación mensual
#'     (\%).}
#'   \item{ipc_transporte, ipc_transporte_vm}{Transporte: índice y
#'     variación mensual (\%).}
#'   \item{ipc_comunicaciones, ipc_comunicaciones_vm}{Comunicaciones:
#'     índice y variación mensual (\%).}
#'   \item{ipc_cultura, ipc_cultura_vm}{Recreación y cultura: índice y
#'     variación mensual (\%).}
#'   \item{ipc_educacion, ipc_educacion_vm}{Educación: índice y
#'     variación mensual (\%).}
#'   \item{ipc_hotel_restaurantes, ipc_hotel_restaurantes_vm}{Restaurantes
#'     y hoteles: índice y variación mensual (\%).}
#'   \item{ipc_bines_servicios, ipc_bienes_servicios_vm}{Bienes y
#'     servicios diversos: índice (nombre con el typo del origen) y
#'     variación mensual (\%).}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_grupos_base_2019-2020.xls>
#'
#' @examples
#' \dontrun{
#' get_ipc_grupos()
#' }
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

#' IPC de República Dominicana por región geográfica
#'
#' Descarga y limpia la serie mensual del Índice de Precios al Consumidor
#' (IPC) desagregada por región geográfica, base 2019-2020, publicada por
#' el Banco Central de la República Dominicana (BCRD).
#'
#' @details
#' Al igual que [get_ipc_grupos()], cada región solo trae el índice y su
#' variación mensual (`_vm`); no incluye variación interanual, acumulada
#' ni promedio de 12 meses.
#'
#' Llamada internamente por [get_ipc_data()] con
#' `desagregacion = "regiones"`; no está exportada, así que para uso
#' normal conviene usar `get_ipc_data("regiones")`.
#'
#' @return Un tibble con una fila por mes y las columnas:
#' \describe{
#'   \item{fecha}{`Date`. Primer día del mes de la observación.}
#'   \item{year}{Año.}
#'   \item{mes}{Mes (1-12).}
#'   \item{ipc_ozama, ipc_ozama_vm}{Región Ozama: índice y variación
#'     mensual (\%).}
#'   \item{ipc_cibao, ipc_cibao_vm}{Región Cibao: índice y variación
#'     mensual (\%).}
#'   \item{ipc_este, ipc_este_vm}{Región Este: índice y variación
#'     mensual (\%).}
#'   \item{ipc_sur, ipc_sur_vm}{Región Sur: índice y variación mensual
#'     (\%).}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_regiones_base_2019-2020.xls>
#'
#' @examples
#' \dontrun{
#' get_ipc_regiones()
#' }
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

#' IPC subyacente (inflación núcleo) de República Dominicana
#'
#' Descarga y limpia la serie mensual del Índice de Precios al Consumidor
#' (IPC) subyacente, base 2019-2020, publicada por el Banco Central de la
#' República Dominicana (BCRD). El IPC subyacente excluye del cálculo los
#' rubros de precios más volátiles (p. ej. combustibles y algunos
#' alimentos frescos), como una medida de la tendencia inflacionaria de
#' fondo.
#'
#' @details
#' El archivo se descarga en cada llamada desde el Excel del BCRD; no hay
#' caché. Se descarta un encabezado largo (`skip = 25`) propio de este
#' archivo, y los guiones (`"-"`) del Excel se interpretan como `NA`.
#' Tras la limpieza, las columnas numéricas se convierten explícitamente
#' con [as.numeric()] porque llegan como texto desde
#' [readxl::read_excel()].
#'
#' Llamada internamente por [get_ipc_data()] con
#' `desagregacion = "subyacente"`; no está exportada, así que para uso
#' normal conviene usar `get_ipc_data("subyacente")`.
#'
#' @return Un tibble con una fila por mes y las columnas:
#' \describe{
#'   \item{fecha}{`Date`. Primer día del mes de la observación.}
#'   \item{year}{Año.}
#'   \item{mes}{Mes (1-12).}
#'   \item{ipc_subyacente}{Índice de precios al consumidor subyacente.}
#'   \item{ipc_subyacente_vm}{Variación mensual, en \%.}
#'   \item{ipc_subyacente_vd}{Variación acumulada respecto a diciembre
#'     del año anterior, en \%.}
#'   \item{ipc_subyacente_vi}{Variación interanual, en \%.}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_subyacente_base_2019-2020.xlsx>
#'
#' @examples
#' \dontrun{
#' get_ipc_subyacente()
#' }
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

  ipc_subyacente <- ipc_subyacente |>
    dplyr::filter(!is.na(ipc_subyacente[[2]])) |>
    janitor::clean_names() |>
    dplyr::select(1:6) |>
    stats::setNames(header_ipc_subyacente) |>
    tidyr::fill(year) |>
    dplyr::mutate(
      mes = crear_mes(mes),
      fecha = lubridate::make_date(year, mes),
      dplyr::across(c("year","ipc_subyacente", "ipc_subyacente_vm",
                      "ipc_subyacente_vd", "ipc_subyacente_vi"),
                    as.numeric)
    ) |>
    dplyr::select(fecha, year, mes, dplyr::everything()) |>
    dplyr::filter(!is.na(ipc_subyacente))

  ipc_subyacente
}

#' IPC de República Dominicana por bienes transables y no transables
#'
#' Descarga y limpia la serie mensual del Índice de Precios al Consumidor
#' (IPC) desagregada en bienes y servicios transables (`_t`, expuestos a
#' competencia internacional) y no transables (`_nt`), base 2019-2020,
#' publicada por el Banco Central de la República Dominicana (BCRD).
#'
#' @details
#' El archivo se descarga en cada llamada desde el Excel del BCRD; no hay
#' caché. Se descarta un encabezado largo (`skip = 31`) propio de este
#' archivo y los guiones (`"-"`) se interpretan como `NA`. A diferencia
#' de [get_ipc_general()] y [get_ipc_subyacente()], ninguno de los tres
#' bloques (general, transable, no transable) incluye variación
#' interanual (`_vi`) ni promedio de 12 meses (`_p12`); solo variación
#' mensual (`_vm`) y acumulada (`_vd`).
#'
#' Llamada internamente por [get_ipc_data()] con
#' `desagregacion = "tnt"`; no está exportada, así que para uso normal
#' conviene usar `get_ipc_data("tnt")`.
#'
#' @return Un tibble con una fila por mes y las columnas:
#' \describe{
#'   \item{fecha}{`Date`. Primer día del mes de la observación.}
#'   \item{year}{Año.}
#'   \item{mes}{Mes (1-12).}
#'   \item{ipc, ipc_vm, ipc_vd}{Índice general, variación mensual (\%) y
#'     variación acumulada respecto a diciembre del año anterior (\%).}
#'   \item{ipc_t, ipc_t_vm, ipc_t_vd}{Bienes y servicios transables:
#'     índice, variación mensual (\%) y variación acumulada (\%).}
#'   \item{ipc_nt, ipc_nt_vm, ipc_nt_vd}{Bienes y servicios no
#'     transables: índice, variación mensual (\%) y variación acumulada
#'     (\%).}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_tnt_base_2019-2020.xls>
#'
#' @examples
#' \dontrun{
#' get_ipc_tnt()
#' }
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
