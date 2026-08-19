# Serie del IPC general de República Dominicana

## Usage

``` r
get_ipc_general()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_base_2019-2020.xls>

## Value

Un tibble con una fila por mes y las columnas:

- fecha:

  `Date`. Primer día del mes de la observación.

- year:

  Año.

- mes:

  Mes (1-12).

- ipc:

  Índice de precios al consumidor.

- ipc_vm:

  Variación mensual, en \\ ipc_vdVariación acumulada respecto a
  diciembre del año anterior, en \\ ipc_viVariación interanual, en \\
  ipc_p12Promedio de la variación interanual de los últimos 12 meses, en
  \Descarga y limpia la serie mensual del Índice de Precios al
  Consumidor (IPC) general, base 2019-2020, publicada por el Banco
  Central de la República Dominicana (BCRD).El archivo se descarga en
  cada llamada desde el Excel del BCRD; no hay caché ni control de
  versión del archivo fuente. Se descarta el encabezado del Excel
  (`skip = 7`) y el año se propaga hacia abajo con
  [`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html),
  porque en el archivo original solo aparece en la primera fila de cada
  bloque de meses.Llamada internamente por
  [`get_ipc_data()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_data.md)
  con `desagregacion = "general"`; no está exportada, así que para uso
  normal conviene usar `get_ipc_data("general")`.
