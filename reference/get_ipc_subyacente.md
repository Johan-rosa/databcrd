# IPC subyacente (inflación núcleo) de República Dominicana

## Usage

``` r
get_ipc_subyacente()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_subyacente_base_2019-2020.xlsx>

## Value

Un tibble con una fila por mes y las columnas:

- fecha:

  `Date`. Primer día del mes de la observación.

- year:

  Año.

- mes:

  Mes (1-12).

- ipc_subyacente:

  Índice de precios al consumidor subyacente.

- ipc_subyacente_vm:

  Variación mensual, en \\ ipc_subyacente_vdVariación acumulada respecto
  a diciembre del año anterior, en \\ ipc_subyacente_viVariación
  interanual, en \\ Descarga y limpia la serie mensual del Índice de
  Precios al Consumidor (IPC) subyacente, base 2019-2020, publicada por
  el Banco Central de la República Dominicana (BCRD). El IPC subyacente
  excluye del cálculo los rubros de precios más volátiles (p. ej.
  combustibles y algunos alimentos frescos), como una medida de la
  tendencia inflacionaria de fondo. El archivo se descarga en cada
  llamada desde el Excel del BCRD; no hay caché. Se descarta un
  encabezado largo (`skip = 25`) propio de este archivo, y los guiones
  (`"-"`) del Excel se interpretan como `NA`. Tras la limpieza, las
  columnas numéricas se convierten explícitamente con
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) porque llegan
  como texto desde
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html).Llamada
  internamente por
  [`get_ipc_data()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_data.md)
  con `desagregacion = "subyacente"`; no está exportada, así que para
  uso normal conviene usar `get_ipc_data("subyacente")`.
