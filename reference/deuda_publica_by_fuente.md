# Deuda pública consolidada por fuente de financiamiento

Descarga y transforma el archivo
"Deuda_Consolidada_Por_Fuente_Trimestral.xlsx" publicado por el Banco
Central de la República Dominicana (BCRD), y lo convierte en un tibble
en formato largo (tidy) con una fila por fuente de financiamiento y
trimestre, incluyendo el monto en millones de US\$ y su equivalente como
porcentaje del PIB.

## Usage

``` r
deuda_publica_by_fuente()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Deuda_Consolidada_Por_Fuente_Trimestral.xlsx>

## Value

Un tibble con una fila por combinación de fuente y trimestre, con las
columnas:

- codigo:

  `chr`. Código jerárquico tal como aparece en el Excel: `"0"` para el
  total consolidado, `"A"`/`"B"` para los grupos (externa/interna), o
  `"1"`-`"4"` para las líneas de detalle dentro de cada grupo.

- tipo_deuda:

  `chr`. `"Consolidada"`, `"Externa"` o `"Interna neta"`, heredado hacia
  abajo con
  [`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html)
  desde la fila de encabezado de cada bloque.

- nivel:

  `chr`. Nivel jerárquico: `"total"`, `"grupo"` o `"detalle"`.

- fuente:

  `chr`. Descripción de la fuente de financiamiento, sin el código
  inicial ni paréntesis/notas al pie sobrantes (p. ej.
  `"Gobierno Central"`, `"Banco Central"`).

- fecha:

  `Date`. Primer día del trimestre (`2013-03-01`, `2013-06-01`, ...).
  Ver advertencia en `@details`.

- year:

  `dbl`. Año extraído de `fecha`.

- trimestre:

  `dbl`. Trimestre (1 a 4) extraído de `fecha`.

- monto:

  `dbl`. Monto de deuda reportado para esa fuente y trimestre, en
  millones de US\$.

- gdp:

  `dbl`. PIB nominal trimestral en millones de US\$, tomado de la fila
  memo de la hoja "Fuente (%PIB)".

- as_gdp_percent:

  `dbl`. `monto / gdp * 100`.

## Details

El archivo fuente organiza las filas en una jerarquía de tres niveles:
un total consolidado ("DEUDA PÚBLICA CONSOLIDADA (A+B)"), dos grupos
("A. DEUDA EXTERNA" y "B. DEUDA INTERNA NETA"), y las líneas de detalle
dentro de cada grupo (Gobierno Central, Resto del SPNF, Banco Central,
Deuda Intragubernamental). El archivo tiene una inconsistencia de
formato entre los grupos: la fila "A. DEUDA EXTERNA" tiene un espacio en
blanco al inicio de la celda, mientras que "B. DEUDA INTERNA NETA" no lo
tiene. Por eso `fuente` se normaliza con
[`stringr::str_squish()`](https://stringr.tidyverse.org/reference/str_trim.html)
*antes* de extraer `codigo`; hacerlo sobre el texto crudo deja sin
detectar el código de la fila "A." y la reclasifica incorrectamente como
`nivel = "total"`.

Las fechas de las columnas trimestrales **no se leen del archivo**: se
asume que la primera columna de datos corresponde a T1-2013 y se genera
una secuencia trimestral consecutiva con
`length.out = ncol(raw_data) - 1`. Si el BCRD inserta, elimina o
reordena columnas, o cambia el trimestre inicial, las fechas quedarán
mal alineadas sin que la función lo detecte.

El PIB nominal usado para calcular `as_gdp_percent` se toma de la fila
memo "Producto Interno Bruto ( Millones de USD)" en la hoja "Fuente
(%PIB)", no de una fuente de PIB independiente.

## Examples

``` r
if (FALSE) { # \dontrun{
deuda_publica_by_fuente()

# Deuda externa como % del PIB, solo el grupo agregado
deuda_publica_by_fuente() |>
  dplyr::filter(codigo == "A", nivel == "grupo")
} # }
```
