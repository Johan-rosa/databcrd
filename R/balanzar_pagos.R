#' Balanza de Servicios - datos trimestrales
#'
#' Descarga y transforma el archivo "Balanza de Servicios trimestral.xlsx"
#' publicado por el Banco Central de la República Dominicana (BCRD), y lo
#' convierte en un tibble en formato largo (tidy) con una fila por
#' categoría de servicio, trimestre y naturaleza contable (crédito, débito
#' o saldo).
#'
#' @details
#' El archivo fuente organiza los datos en tres bloques de filas: CREDITO,
#' DEBITO y SALDO (I-II). Dentro de cada bloque hay filas de categoría
#' (bienes, transporte, viajes, telecomunicaciones, seguros, etc.) y
#' algunas filas de detalle sin código propio (p. ej. "De los cuales
#' servicios de ZF:"). Por eso `code` y `naturaleza` se completan hacia
#' abajo con [tidyr::fill()], heredando el valor de la última fila no
#' faltante.
#'
#' Las fechas de las columnas trimestrales **no se leen del archivo**: se
#' asume que la primera columna de datos corresponde a T1-2010 y se genera
#' una secuencia trimestral consecutiva con
#' `length.out = ncol(raw_data) - 1`. Si el BCRD inserta, elimina o
#' reordena columnas, o cambia el trimestre inicial, las fechas quedarán
#' mal alineadas sin que la función lo detecte.
#'
#' @return Un tibble con una fila por combinación de categoría, trimestre
#'   y naturaleza contable, con las columnas:
#' \describe{
#'   \item{code}{`chr`. Código de la categoría tal como aparece en el
#'     Excel: letras `"A."`–`"J."` para las líneas de servicios, o
#'     numerales `"1"`, `"2"`, `"3"` para las filas
#'     agregadas de Crédito, Débito y Saldo. Las filas de
#'     detalle sin código propio heredan el de la categoría anterior.}
#'   \item{naturaleza}{`chr`. `"Credito"`, `"Debito"` o `"Saldo"`,
#'     heredado hacia abajo desde la fila de encabezado de cada bloque.}
#'   \item{concepto}{`chr`. Descripción de la categoría de servicio, sin
#'     el código inicial ni guiones/dos puntos sobrantes (p. ej.
#'     `"Viajes"`, `"Servicios financieros"`, `"De los cuales servicios
#'     de ZF:"`). Nota: para las filas agregadas queda `"Credito"`,
#'     `"Debito"` y `"Saldo"`.}
#'   \item{fecha}{`Date`. Primer día del trimestre (`2010-01-01`,
#'     `2010-04-01`, ...). Ver advertencia en `@details`.}
#'   \item{year}{`dbl`. Año extraído de `fecha`.}
#'   \item{trimestre}{`dbl`. Trimestre (1 a 4) extraído de `fecha`.}
#'   \item{monto}{`dbl`. Valor trimestral reportado para esa
#'     categoría/naturaleza, en las unidades del archivo original (USD
#'     millones, según la convención habitual del BCRD para esta
#'     publicación). Las celdas vacías (`"-"`) se interpretan como `0`.}
#'   \item{monto_acumulado}{`dbl`. Suma acumulada de `monto` dentro de
#'     cada combinación `(year, concepto, code)`, ordenada por `fecha`;
#'     equivale al acumulado del año (year-to-date) para esa categoría, y
#'     se reinicia en enero de cada año.}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Balanza-de-Servicios-trimestral.xlsx>
#'
#' @examples
#' \dontrun{
#' balanza_servicios()
#'
#' # Serie de viajes (turismo), solo créditos
#' balanza_servicios() |>
#'   dplyr::filter(concepto == "Viajes", naturaleza == "Credito")
#' }
#'
#' @export
balanza_servicios <- function() {
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-externo/documents/Balanza-de-Servicios-trimestral.xlsx"
  )

  file_path <- tempfile(fileext = ".xlsx")
  download_file(url, file_path)
  on.exit(unlink(file_path))

  raw_data <- readxl::read_excel(file_path, skip = 7, col_names = F) |>
    suppressMessages()

  dates <- seq(
    lubridate::make_date(2010, 1, 1),
    by = "quarter",
    length.out = ncol(raw_data) - 1
  )

  headers <- c("concepto", as.character(dates))

  raw_data |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    purrr::set_names(headers) |>
    dplyr::mutate(
      code = stringr::str_extract(concepto, "^[A-Z]+\\."),
      naturaleza = stringr::str_extract(concepto, "CREDITO|DEBITO|SALDO"),
      concepto = stringr::str_remove(concepto, "^[A-Z]+\\.|-|:") |>
        stringr::str_squish(),
      .before = concepto
    ) |>
    tidyr::fill(code, naturaleza) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("^\\d{4}"),
        \(x) readr::parse_number(as.character(stringr::str_replace(x, "\\-", "0")))
      ),
      naturaleza = stringr::str_to_sentence(naturaleza)
    ) |>
    dplyr::mutate(
      code = dplyr::case_when(
        code == "I." & concepto == "CREDITO" ~ "1",
        code == "II." & concepto == "DEBITO" ~ "2",
        code == "III." & concepto == "SALDO ( I-II )" ~ "3",
        TRUE ~ code
      ),
      concepto = dplyr::recode(
        concepto,
        "SALDO ( I-II )" = "Saldo",
        "CREDITO" = "Credito",
        "DEBITO" = "Debito"
      )
    ) |>
    tidyr::pivot_longer(
      dplyr::matches("^\\d{4}"),
      names_to = "fecha",
      values_to = "monto"
    ) |>
    dplyr::filter(!is.na(monto)) |>
    dplyr::mutate(
      fecha = lubridate::ymd(fecha),
      year = lubridate::year(fecha),
      trimestre = lubridate::quarter(fecha)
    ) |>
    dplyr::arrange(fecha, naturaleza, code) |>
    dplyr::mutate(
      monto_acumulado = cumsum(monto),
      .by = c(year, concepto, code)
    ) |>
    dplyr::relocate(code, naturaleza, concepto, fecha, year, trimestre)
}
