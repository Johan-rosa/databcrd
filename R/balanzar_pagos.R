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

#' Balanza de Pagos - datos trimestrales
#'
#' Descarga y transforma la hoja `BOP_TRIM` del archivo de Balanza de
#' Pagos publicado por el Banco Central de la Republica Dominicana
#' (BCRD), y la une contra [catalogo_balanza_pagos], que trae el codigo
#' jerarquico, nivel, naturaleza contable y cuenta de nivel 1 de cada
#' fila.
#'
#' @details
#' A diferencia de [balanza_servicios()], aqui el `code`/`naturaleza`
#' de cada fila no se extraen con expresiones regulares: se pegan por
#' POSICION contra [catalogo_balanza_pagos], porque las filas de
#' detalle (p. ej. "Exportaciones", "Credito") no traen ningun codigo
#' propio en el texto y no hay forma confiable de inferirlas solo del
#' texto. Esto exige que el archivo mantenga las mismas 57 filas de
#' datos, en el mismo orden, que cuando se construyo el catalogo; si
#' el conteo de filas no coincide, la funcion aborta con un mensaje
#' explicito en vez de pegar datos desalineados en silencio.
#'
#' Como salvaguarda adicional, se compara el texto crudo leido en cada
#' corrida (`og_concepto`) contra el texto crudo capturado al construir
#' el catalogo (columna `concepto_fuente` de [catalogo_balanza_pagos]),
#' usando solo una limpieza de espacios (`stringr::str_squish()`), sin
#' reconstruir el texto normalizado del catalogo. Esto evita falsos
#' positivos: la comparacion solo falla si el BCRD cambio el texto
#' fuente, no cuando el catalogo tiene un estilo distinto (acentos
#' removidos, abreviaturas expandidas, notas al pie separadas).
#'
#' Si se detectan conceptos con texto distinto, la funcion imprime la
#' tabla de diferencias y aborta, a menos que se llame con
#' `force_bind_by_position = TRUE`, en cuyo caso solo emite un warning
#' y continua (bajo responsabilidad de quien llama, ya que no se puede
#' garantizar que el orden de las filas siga correspondiendo al
#' catalogo).
#'
#' Los filtros (`filtro_*`, `solo_hojas`) se aplican al final, despues
#' de unir los datos crudos contra el catalogo, y son todos opcionales
#' (`NULL` o `FALSE` = sin filtrar). Se pueden combinar libremente.
#'
#' `concepto` NO es unico en el catalogo: valores como `"Credito"`,
#' `"Debito"`, `"Otros"`, `"Nacionales"`, `"Zonas Francas"`, `"Remesas
#' Familiares"`, `"Otras Transferencias"`, `"Inversion de Cartera"` y
#' `"Activos de Reservas"` aparecen repetidos bajo distintas cuentas
#' padre. `filtro_concepto` hace match parcial insensible a mayusculas
#' (via `stringr::str_detect()`), asi que por si solo puede devolver
#' varias categorias no relacionadas; combinalo con `filtro_cuenta` o
#' `filtro_codigo` para desambiguar.
#'
#' @param force_bind_by_position `logical`. Si `FALSE` (por defecto) y
#'   se detectan conceptos cuyo texto crudo cambio respecto al
#'   catalogo, la funcion aborta. Si `TRUE`, solo advierte e igual
#'   devuelve el resultado.
#' @param filtro_codigo `character`. Vector de `code` a conservar
#'   (match exacto), p. ej. `c("1.1.1.1", "1.1.1.2")`. `NULL` (por
#'   defecto) no filtra.
#' @param filtro_naturaleza `character`. Vector de `naturaleza` a
#'   conservar (`"Credito"`, `"Debito"`, `"Saldo"`). `NULL` no filtra.
#' @param filtro_concepto `character`. Vector de patrones a buscar en
#'   `concepto` (match parcial, insensible a mayusculas/minusculas;
#'   basta con que coincida uno de los patrones). `NULL` no filtra.
#' @param filtro_cuenta `character`. Vector de `cuenta` (nivel 1) a
#'   conservar, p. ej. `"Cuenta Financiera"`. `NULL` no filtra.
#' @param filtro_nivel `integer`. Vector de `nivel` a conservar, p. ej.
#'   `1:2` para quedarse solo con el resumen. `NULL` no filtra.
#' @param solo_hojas `logical`. Si `TRUE`, descarta las filas de
#'   subtotal (aquellas cuyo `code` aparece como `code_padre` de otra
#'   fila), dejando solo las categorias mas finas. Util para sumar
#'   `monto` sin duplicar cifras. Por defecto `FALSE`.
#'
#' @return Un tibble en formato ancho con una fila por cuenta/subcuenta
#'   (igual a [catalogo_balanza_pagos], sin las columnas `nota` y
#'   `concepto_fuente`) y una columna adicional por cada trimestre
#'   publicado (nombrada con la fecha de inicio del trimestre, p. ej.
#'   `"2010-01-01"`), en millones de USD, filtrado segun los
#'   argumentos `filtro_*`/`solo_hojas` provistos.
#'
#' @source
#' <https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos_trim_6.xlsx>
#'
#' @seealso [catalogo_balanza_pagos], [balanza_servicios()]
#'
#' @examples
#' \dontrun{
#' balanza_pagos()
#'
#' # Solo la Cuenta Financiera, sin subtotales
#' balanza_pagos(filtro_cuenta = "Cuenta Financiera", solo_hojas = TRUE)
#'
#' # Categorias de credito en Ingreso Primario, resumen (nivel 1-3)
#' balanza_pagos(
#'   filtro_cuenta = "Cuenta Corriente",
#'   filtro_naturaleza = "Credito",
#'   filtro_nivel = 1:3
#' )
#' }
#'
#' @export
balanza_pagos <- function(
    force_bind_by_position = FALSE,
    filtro_codigo     = NULL,
    filtro_naturaleza = NULL,
    filtro_concepto   = NULL,
    filtro_cuenta     = NULL,
    filtro_nivel      = NULL,
    solo_hojas        = FALSE
) {
  url <- "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos_trim_6.xlsx"
  file_path <- tempfile(fileext = ".xlsx")
  download_file(url, file_path)
  on.exit(unlink(file_path))

  raw_data <- readxl::read_excel(
    file_path,
    sheet = "BOP_TRIM",
    skip = 10,
    col_names = FALSE
  ) |>
    suppressMessages()

  dates <- seq(
    lubridate::make_date(2010, 1, 1),
    by = "quarter",
    length.out = ncol(raw_data) - 1
  )

  headers <- c("og_concepto", as.character(dates))

  raw_data <- raw_data |>
    purrr::set_names(headers) |>
    janitor::remove_empty(which = c("cols", "rows")) |>
    dplyr::filter(!dplyr::if_all(-og_concepto, is.na))

  # El catalogo debe tener exactamente una fila por cada fila de datos
  # del archivo, en el mismo orden. Si esto no se cumple, es mas seguro
  # abortar con un mensaje claro que dejar que dplyr::bind_cols() tire
  # un error generico de tamanos distintos.
  if (nrow(raw_data) != nrow(catalogo_balanza_pagos)) {
    rlang::abort(paste0(
      "El archivo tiene fuente", nrow(raw_data), " filas de datos, se esperaban ",
      nrow(catalogo_balanza_pagos), " segun catalogo_balanza_pagos. ",
      "Es probable que el BCRD haya agregado, quitado o reordenado una ",
      "categoria; revisa el archivo antes de continuar."
    ))
  }

  wide_data <- catalogo_balanza_pagos |>
      dplyr::bind_cols(raw_data)

  # Comparar los conceptos originales con los del catalogo
  catalogos_erroneos <- wide_data |>
    dplyr::filter(og_concepto != concepto_fuente)

  # Comparar el texto crudo leido ahora contra el texto crudo capturado
  # al construir el catalogo (concepto_fuente), no contra `concepto`
  # (que ya viene normalizado con el estilo propio del paquete y nunca
  # va a coincidir con el texto original del Excel).
  conceptos_cambiados <- wide_data |>
    dplyr::select(code, concepto, concepto_fuente, og_concepto) |>
    dplyr::mutate(
      og_concepto = stringr::str_squish(og_concepto),
      concepto_fuente = stringr::str_squish(concepto_fuente)
    ) |>
    dplyr::filter(og_concepto != concepto_fuente)

  if (nrow(conceptos_cambiados) > 0) {
    rlang::warn(paste0(
      "Se detecto que algunos conceptos cambiaron en el archivo original ",
      "desde que se programo esta funcion. Favor inspeccionar los cambios ",
      "y, si no son relevantes, correr la funcion con ",
      "`force_bind_by_position = TRUE`."
    ))

    print(conceptos_cambiados)
    if (!force_bind_by_position) {
      rlang::abort(paste0(
        "Hay conceptos que cambiaron en el archivo original y no se ",
        "garantiza la integridad de los resultados."
      ))
    }
  }

  # patron para filtro_concepto (match parcial): se precalcula porque
  # stringr::str_detect() no acepta un patron character(0) sin error,
  # a diferencia de %in%, que si tolera un filtro NULL sin problema.
  patron_concepto <- if (is.null(filtro_concepto)) {
    NA_character_
  } else {
    stringr::str_c(filtro_concepto, collapse = "|")
  }

  resultado <- wide_data |>
    dplyr::select(-og_concepto, -concepto_fuente, -nota) |>
    dplyr::filter(
      is.null(filtro_codigo)     | code       %in% filtro_codigo,
      is.null(filtro_naturaleza) | naturaleza %in% filtro_naturaleza,
      is.null(filtro_cuenta)     | cuenta      %in% filtro_cuenta,
      is.null(filtro_nivel)      | nivel       %in% filtro_nivel,
      is.null(filtro_concepto)   | stringr::str_detect(concepto, stringr::regex(patron_concepto, ignore_case = TRUE)),
      !isTRUE(solo_hojas)        | !(code %in% code_padre)
    )

  if (nrow(resultado) == 0) {
    rlang::warn("Los filtros aplicados no dejaron ninguna fila en el resultado.")
    return(resultado)
  }

  resultado |>
    tidyr::pivot_longer(
      dplyr::matches("\\d{4}"),
      names_to = "fecha",
      values_to = "monto"
    ) |>
    dplyr::mutate(
      fecha = lubridate::ymd(fecha),
      year  = lubridate::year(fecha),
      trimestre = lubridate::quarter(fecha),
      .after = fecha
    )
}
