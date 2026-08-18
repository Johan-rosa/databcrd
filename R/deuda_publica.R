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

#' Deuda pública consolidada por sector institucional
#'
#' Descarga y transforma el archivo
#' "Deuda_Consolidada_Por_Sector_Trimestral.xlsx" publicado por el Banco
#' Central de la República Dominicana (BCRD), y lo convierte en un tibble
#' en formato largo (tidy) con una fila por concepto (sector, sub-sector o
#' fuente de financiamiento dentro de un sector) y trimestre, incluyendo el
#' monto en millones de US$ y su equivalente como porcentaje del PIB.
#'
#' @details
#' A diferencia de [deuda_publica_by_fuente()], este archivo organiza las
#' filas en una jerarquía de **cuatro niveles**. Las filas no tienen un
#' código único y consistente para distinguir todos los niveles, así que
#' la detección combina dos señales:
#'
#' \enumerate{
#'   \item `"total"`: tres filas al inicio, en MAYÚSCULA SOSTENIDA
#'     ("DEUDA PÚBLICA CONSOLIDADA", "...EXTERNA CONSOLIDADA",
#'     "...INTERNA CONSOLIDADA"), sin código ni sector asociado.
#'   \item `"grupo"`: dos filas con código de letra (A./B.), agrupando
#'     sectores institucionales (Sector Público No Financiero / Sector
#'     Público Financiero).
#'   \item `"sector"`: filas con código numérico (1./2./3.) dentro de cada
#'     grupo (Gobierno Central, Resto del SPNF, Banco Central).
#'   \item `"fuente"`: filas sin código propio que empiezan con
#'     "Deuda Externa" o "Deuda Interna" (mayúscula inicial únicamente,
#'     a diferencia de las filas "total"), desagregando cada sector.
#'   \item `"detalle"`: una fila memo que empieza con "De los cuales"
#'     ("De los cuales: Intragubernamental"), hija de la fila
#'     "Deuda Interna" del Gobierno Central.
#' }
#'
#' El texto original también tiene sangría (espacios en blanco al inicio
#' de la celda) que en teoría distinguiría estos niveles, pero
#' [readxl::read_excel()] usa `trim_ws = TRUE` por defecto y la elimina
#' antes de que la función pueda medirla. Por eso el nivel se detecta por
#' el patrón del propio texto (mayúscula sostenida vs. mayúscula inicial,
#' y el prefijo "De los cuales") en lugar de la indentación. Esto es más
#' robusto frente al comportamiento de lectura, pero depende de que el
#' BCRD mantenga esa convención de mayúsculas/minúsculas en el texto.
#'
#' Al igual que en `deuda_publica_by_fuente()`, la fila " A. SECTOR
#' PÚBLICO NO FINANCIERO..." tiene un espacio en blanco inicial que
#' "B. SECTOR PÚBLICO FINANCIERO..." no tiene; se resuelve aplicando
#' [stringr::str_squish()] antes de extraer el código.
#'
#' `grupo` y `sector` se completan hacia abajo (con relleno acotado, no
#' [tidyr::fill()] directo sobre toda la tabla) para que las filas de
#' `nivel` "fuente" y "detalle" hereden el sector al que pertenecen sin
#' que ese valor se filtre hacia las filas "total" siguientes. `tipo_deuda`
#' se deriva del texto en los niveles "total" y "fuente"; en el nivel
#' "detalle" se hereda de la fila "fuente" inmediatamente anterior (en
#' este archivo, siempre "Deuda Interna"), usando `dplyr::lag()` en lugar
#' de `fill()` para no arrastrar el valor más allá de esa fila.
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
#' "Sector (%PIB)", igual que en `deuda_publica_by_fuente()`.
#'
#' @return Un tibble con una fila por combinación de concepto y trimestre,
#'   con las columnas:
#' \describe{
#'   \item{grupo}{`chr`. `"Sector Público No Financiero"` o
#'     `"Sector Público Financiero"`. `NA` para las filas `nivel == "total"`.}
#'   \item{sector}{`chr`. `"Gobierno Central"`, `"Resto del SPNF"` o
#'     `"Banco Central"`. `NA` para `nivel %in% c("total", "grupo")`.}
#'   \item{tipo_deuda}{`chr`. `"Consolidada"`, `"Externa"` o `"Interna"`.
#'     `NA` para las filas agregadas (`nivel %in% c("grupo", "sector")`)
#'     que suman ambas fuentes.}
#'   \item{nivel}{`chr`. Nivel jerárquico: `"total"`, `"grupo"`,
#'     `"sector"`, `"fuente"` o `"detalle"`.}
#'   \item{concepto}{`chr`. Descripción de la fila tal como aparece en el
#'     Excel, sin código inicial ni paréntesis/notas al pie sobrantes.}
#'   \item{fecha}{`Date`. Primer día del trimestre (`2013-03-01`,
#'     `2013-06-01`, ...). Ver advertencia en `@details`.}
#'   \item{year}{`dbl`. Año extraído de `fecha`.}
#'   \item{trimestre}{`dbl`. Trimestre (1 a 4) extraído de `fecha`.}
#'   \item{monto}{`dbl`. Monto de deuda reportado para ese concepto y
#'     trimestre, en millones de US$.}
#'   \item{gdp}{`dbl`. PIB nominal trimestral en millones de US$.}
#'   \item{as_gdp_percent}{`dbl`. `monto / gdp * 100`.}
#' }
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Deuda_Consolidada_Por_Sector_Trimestral.xlsx>
#'
#' @examples
#' \dontrun{
#' deuda_publica_by_sector()
#'
#' # Deuda del Banco Central, solo la fuente externa
#' deuda_publica_by_sector() |>
#'   dplyr::filter(sector == "Banco Central", tipo_deuda == "Externa")
#' }
#'
#' @export
deuda_publica_by_sector <- function() {
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/",
    "documents/Deuda_Consolidada_Por_Sector_Trimestral.xlsx"
  )

  file_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file_path), add = TRUE)

  download_file(url, file_path)

  raw_data <- readxl::read_excel(
    file_path,
    sheet = "Sector (US$)",
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

  headers <- c("concepto", as.character(dates))

  data_wide <- raw_data |>
    purrr::set_names(headers) |>
    dplyr::select(
      concepto,
      dplyr::where(~ is.numeric(.x) && sum(.x, na.rm = TRUE) != 0)
    ) |>
    dplyr::filter(
      dplyr::if_all(-concepto, \(x) !is.na(x))
    ) |>
    dplyr::mutate(
      concepto = stringr::str_squish(concepto),
      codigo = stringr::str_extract(concepto, "^[A-Za-z0-9]\\."),
      # No se puede usar la indentacion del texto crudo para distinguir
      # nivel: readxl::read_excel() usa trim_ws = TRUE por defecto, asi
      # que los espacios iniciales de "Deuda Externa"/"Deuda Interna" y de
      # "De los cuales..." ya vienen recortados al llegar aqui. En su
      # lugar, el nivel se detecta por el patron del propio texto: las
      # filas "total" van en MAYUSCULA SOSTENIDA ("DEUDA PUBLICA... "),
      # mientras que las filas "fuente" usan version con mayuscula inicial
      # unicamente ("Deuda Externa"/"Deuda Interna"), y la fila "detalle"
      # siempre empieza con "De los cuales".
      nivel = dplyr::case_when(
        stringr::str_detect(codigo, "^[A-Za-z]\\.") ~ "grupo",
        stringr::str_detect(codigo, "^[0-9]\\.") ~ "sector",
        stringr::str_detect(concepto, "^Deuda (Externa|Interna)") ~ "fuente",
        stringr::str_detect(concepto, "^De los cuales") ~ "detalle",
        TRUE ~ "total"
      ),
      tipo_deuda = dplyr::case_when(
        nivel == "total" & stringr::str_detect(
          stringr::str_to_upper(concepto), "EXTERNA"
        ) ~ "Externa",
        nivel == "total" & stringr::str_detect(
          stringr::str_to_upper(concepto), "INTERNA"
        ) ~ "Interna",
        nivel == "total" ~ "Consolidada",
        nivel == "fuente" & stringr::str_detect(
          stringr::str_to_upper(concepto), "EXTERNA"
        ) ~ "Externa",
        nivel == "fuente" & stringr::str_detect(
          stringr::str_to_upper(concepto), "INTERNA"
        ) ~ "Interna",
        TRUE ~ NA_character_
      ),
      # nivel "detalle" no menciona la fuente en su propio texto; siempre
      # es hija directa de la fila "fuente" inmediatamente anterior.
      tipo_deuda = dplyr::if_else(
        nivel == "detalle", dplyr::lag(tipo_deuda), tipo_deuda
      ),
      concepto = stringr::str_remove_all(
        concepto, "^[A-Za-z0-9]\\.|\\(.+\\)|\\d/"
      ) |>
        stringr::str_squish(),
      .before = concepto
    ) |>
    dplyr::select(-codigo) |>
    dplyr::mutate(
      grupo_tmp = ifelse(nivel == "grupo", concepto, NA_character_),
      sector_tmp = ifelse(nivel == "sector", concepto, NA_character_)
    ) |>
    tidyr::fill(grupo_tmp, sector_tmp) |>
    dplyr::mutate(
      grupo = ifelse(
        nivel %in% c("grupo", "sector", "fuente", "detalle"),
        grupo_tmp,
        NA_character_
      ),
      sector = ifelse(
        nivel %in% c("sector", "fuente", "detalle"),
        sector_tmp,
        NA_character_
      ),
      .before = tipo_deuda
    ) |>
    dplyr::select(-grupo_tmp, -sector_tmp)

  deuda_long <- data_wide |>
    dplyr::mutate(
      tipo_deuda = ifelse(is.na(tipo_deuda), "Consolidada", tipo_deuda),
      dplyr::across(c(concepto, grupo), stringr::str_to_title)
    ) |>
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
    sheet = "Sector (%PIB)",
    skip = 10,
    col_names = FALSE
  ) |>
    suppressMessages()

  gdp <- gdp_raw |>
    dplyr::select(seq_along(headers)) |>
    purrr::set_names(headers) |>
    dplyr::filter(concepto == "Producto Interno Bruto ( Millones de USD)") |>
    tidyr::pivot_longer(
      -concepto,
      names_to = "fecha",
      values_to = "gdp"
    ) |>
    dplyr::mutate(fecha = lubridate::ymd(fecha)) |>
    dplyr::select(-concepto)

  deuda_long |>
    dplyr::left_join(gdp, by = "fecha") |>
    dplyr::mutate(as_gdp_percent = monto / gdp * 100)
}
