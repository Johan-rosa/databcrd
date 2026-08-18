#' Deuda pública consolidada por fuente de financiamiento
#'
#' Descarga y transforma el archivo
#' "Deuda_Consolidada_Por_Fuente_Trimestral.xlsx" publicado por el Banco
#' Central de la República Dominicana (BCRD), y lo convierte en un tibble
#' en formato largo (tidy) con una fila por fuente de financiamiento y
#' trimestre, incluyendo el monto en millones de US$ y su equivalente como
#' porcentaje del PIB.
#'
#' @details
#' El archivo fuente organiza las filas en una jerarquía de tres niveles:
#' un total consolidado ("DEUDA PÚBLICA CONSOLIDADA (A+B)"), dos grupos
#' ("A. DEUDA EXTERNA" y "B. DEUDA INTERNA NETA"), y las líneas de detalle
#' dentro de cada grupo (Gobierno Central, Resto del SPNF, Banco Central,
#' Deuda Intragubernamental). El archivo tiene una inconsistencia de
#' formato entre los grupos: la fila "A. DEUDA EXTERNA" tiene un espacio
#' en blanco al inicio de la celda, mientras que "B. DEUDA INTERNA NETA"
#' no lo tiene. Por eso `fuente` se normaliza con [stringr::str_squish()]
#' *antes* de extraer `codigo`; hacerlo sobre el texto crudo deja sin
#' detectar el código de la fila "A." y la reclasifica incorrectamente
#' como `nivel = "total"`.
#'
#' Las fechas de las columnas trimestrales **no se leen del archivo**: se
#' asume que la primera columna de datos corresponde a T1-2013 y se genera
#' una secuencia trimestral consecutiva con
#' `length.out = ncol(raw_data) - 1`. Si el BCRD inserta, elimina o
#' reordena columnas, o cambia el trimestre inicial, las fechas quedarán
#' mal alineadas sin que la función lo detecte.
#'
#' El PIB nominal usado para calcular `as_gdp_percent` se toma de la fila
#' memo "Producto Interno Bruto ( Millones de USD)" en la hoja
#' "Fuente (%PIB)", no de una fuente de PIB independiente.
#'
#' @return Un tibble con una fila por combinación de fuente y trimestre,
#'   con las columnas:
#' \describe{
#'   \item{codigo}{`chr`. Código jerárquico tal como aparece en el Excel:
#'     `"0"` para el total consolidado, `"A"`/`"B"` para los grupos
#'     (externa/interna), o `"1"`-`"4"` para las líneas de detalle dentro
#'     de cada grupo.}
#'   \item{tipo_deuda}{`chr`. `"Consolidada"`, `"Externa"` o
#'     `"Interna neta"`, heredado hacia abajo con [tidyr::fill()] desde
#'     la fila de encabezado de cada bloque.}
#'   \item{nivel}{`chr`. Nivel jerárquico: `"total"`, `"grupo"` o
#'     `"detalle"`.}
#'   \item{fuente}{`chr`. Descripción de la fuente de financiamiento, sin
#'     el código inicial ni paréntesis/notas al pie sobrantes (p. ej.
#'     `"Gobierno Central"`, `"Banco Central"`).}
#'   \item{fecha}{`Date`. Primer día del trimestre (`2013-03-01`,
#'     `2013-06-01`, ...). Ver advertencia en `@details`.}
#'   \item{year}{`dbl`. Año extraído de `fecha`.}
#'   \item{trimestre}{`dbl`. Trimestre (1 a 4) extraído de `fecha`.}
#'   \item{monto}{`dbl`. Monto de deuda reportado para esa fuente y
#'     trimestre, en millones de US$.}
#'   \item{gdp}{`dbl`. PIB nominal trimestral en millones de US$, tomado
#'     de la fila memo de la hoja "Fuente (%PIB)".}
#'   \item{as_gdp_percent}{`dbl`. `monto / gdp * 100`.}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Deuda_Consolidada_Por_Fuente_Trimestral.xlsx>
#'
#' @examples
#' \dontrun{
#' deuda_publica_by_fuente()
#'
#' # Deuda externa como % del PIB, solo el grupo agregado
#' deuda_publica_by_fuente() |>
#'   dplyr::filter(codigo == "A", nivel == "grupo")
#' }
#'
#' @export
deuda_publica_by_fuente <- function() {
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/",
    "documents/Deuda_Consolidada_Por_Fuente_Trimestral.xlsx"
  )
  file_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file_path), add = TRUE)

  download_file(url, file_path)

  raw_data <- readxl::read_excel(
    file_path,
    sheet = "Fuente (US$)",
    skip = 10,
    col_names = FALSE
  ) |>
    suppressMessages() |>
    janitor::remove_empty(which = c("cols", "rows"))

  dates <- seq(
    lubridate::make_date(2013, 3, 1),
    by = "quarter",
    length.out = ncol(raw_data) - 1
  )
  headers <- c("fuente", as.character(dates))

  data_wide <- raw_data |>
    purrr::set_names(headers) |>
    dplyr::select(
      fuente,
      dplyr::where(~ is.numeric(.x) && sum(.x, na.rm = TRUE) != 0)
    ) |>
    dplyr::filter(
      dplyr::if_all(-fuente, \(x) !is.na(x))
    ) |>
    dplyr::mutate(
      # str_squish() up front: the source sheet has an inconsistent
      # leading space on " A. DEUDA EXTERNA..." (but not on "B. DEUDA
      # INTERNA NETA..."). Extracting/removing "^.\\." against the raw
      # string misses the "A." row entirely (regex is anchored at the
      # true start), which both drops its codigo (-> miscoded as
      # nivel = "total") and leaves "A." stuck in the cleaned fuente text.
      fuente = stringr::str_squish(fuente),
      codigo = stringr::str_extract(fuente, "^.\\."),
      tipo_deuda = stringr::str_extract(
        fuente, "CONSOLIDADA|EXTERNA|INTERNA NETA"
      ) |>
        stringr::str_to_sentence(),
      fuente = stringr::str_remove_all(fuente, "^.\\.|\\(.+\\)|\\d/") |>
        stringr::str_squish(),
      nivel = dplyr::case_when(
        is.na(codigo) ~ "total",
        codigo %in% c("A.", "B.") ~ "grupo",
        TRUE ~ "detalle"
      ),
      codigo = ifelse(is.na(codigo), "0.", codigo) |>
        stringr::str_remove("\\.$"),
      .before = fuente
    ) |>
    tidyr::fill(tipo_deuda)

  deuda_long <- data_wide |>
    tidyr::pivot_longer(
      dplyr::matches("\\d{4}"),
      names_to = "fecha",
      values_to = "monto"
    ) |>
    dplyr::mutate(
      fecha = lubridate::ymd(fecha),
      year = lubridate::year(fecha),
      trimestre = lubridate::quarter(fecha),
      .after = fecha
    )

  gdp_raw <- readxl::read_excel(
    file_path,
    sheet = "Fuente (%PIB)",
    skip = 10,
    col_names = FALSE
  ) |>
    suppressMessages()

  gdp <- gdp_raw |>
    dplyr::select(seq_along(headers)) |>
    purrr::set_names(headers) |>
    dplyr::filter(fuente == "Producto Interno Bruto ( Millones de USD)") |>
    tidyr::pivot_longer(
      -fuente,
      names_to = "fecha",
      values_to = "gdp"
    ) |>
    dplyr::mutate(fecha = lubridate::ymd(fecha)) |>
    dplyr::select(-fuente)

  deuda_long |>
    dplyr::left_join(gdp, by = "fecha") |>
    dplyr::mutate(as_gdp_percent = monto / gdp * 100)
}
