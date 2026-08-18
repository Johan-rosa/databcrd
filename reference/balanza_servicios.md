# Balanza de Servicios - datos trimestrales

Descarga y transforma el archivo "Balanza de Servicios trimestral.xlsx"
publicado por el Banco Central de la República Dominicana (BCRD), y lo
convierte en un tibble en formato largo (tidy) con una fila por
categoría de servicio, trimestre y naturaleza contable (crédito, débito
o saldo).

## Usage

``` r
balanza_servicios()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Balanza-de-Servicios-trimestral.xlsx>

## Value

Un tibble con una fila por combinación de categoría, trimestre y
naturaleza contable, con las columnas:

- code:

  `chr`. Código de la categoría tal como aparece en el Excel: letras
  `"A."`–`"J."` para las líneas de servicios, o numerales `"1"`, `"2"`,
  `"3"` para las filas agregadas de Crédito, Débito y Saldo. Las filas
  de detalle sin código propio heredan el de la categoría anterior.

- naturaleza:

  `chr`. `"Credito"`, `"Debito"` o `"Saldo"`, heredado hacia abajo desde
  la fila de encabezado de cada bloque.

- concepto:

  `chr`. Descripción de la categoría de servicio, sin el código inicial
  ni guiones/dos puntos sobrantes (p. ej. `"Viajes"`,
  `"Servicios financieros"`, `"De los cuales servicios de ZF:"`). Nota:
  para las filas agregadas queda `"Credito"`, `"Debito"` y `"Saldo"`.

- fecha:

  `Date`. Primer día del trimestre (`2010-01-01`, `2010-04-01`, ...).
  Ver advertencia en `@details`.

- year:

  `dbl`. Año extraído de `fecha`.

- trimestre:

  `dbl`. Trimestre (1 a 4) extraído de `fecha`.

- monto:

  `dbl`. Valor trimestral reportado para esa categoría/naturaleza, en
  las unidades del archivo original (USD millones, según la convención
  habitual del BCRD para esta publicación). Las celdas vacías (`"-"`) se
  interpretan como `0`.

- monto_acumulado:

  `dbl`. Suma acumulada de `monto` dentro de cada combinación
  `(year, concepto, code)`, ordenada por `fecha`; equivale al acumulado
  del año (year-to-date) para esa categoría, y se reinicia en enero de
  cada año.

## Details

El archivo fuente organiza los datos en tres bloques de filas: CREDITO,
DEBITO y SALDO (I-II). Dentro de cada bloque hay filas de categoría
(bienes, transporte, viajes, telecomunicaciones, seguros, etc.) y
algunas filas de detalle sin código propio (p. ej. "De los cuales
servicios de ZF:"). Por eso `code` y `naturaleza` se completan hacia
abajo con
[`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html),
heredando el valor de la última fila no faltante.

Las fechas de las columnas trimestrales **no se leen del archivo**: se
asume que la primera columna de datos corresponde a T1-2010 y se genera
una secuencia trimestral consecutiva con
`length.out = ncol(raw_data) - 1`. Si el BCRD inserta, elimina o
reordena columnas, o cambia el trimestre inicial, las fechas quedarán
mal alineadas sin que la función lo detecte.

## Examples

``` r
if (FALSE) { # \dontrun{
balanza_servicios()

# Serie de viajes (turismo), solo créditos
balanza_servicios() |>
  dplyr::filter(concepto == "Viajes", naturaleza == "Credito")
} # }
```
