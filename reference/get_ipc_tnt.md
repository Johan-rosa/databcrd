# IPC de República Dominicana por bienes transables y no transables

## Usage

``` r
get_ipc_tnt()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_tnt_base_2019-2020.xls>

## Value

Un tibble con una fila por mes y las columnas:

- fecha:

  `Date`. Primer día del mes de la observación.

- year:

  Año.

- mes:

  Mes (1-12).

- ipc, ipc_vm, ipc_vd:

  Índice general, variación mensual (\\ variación acumulada respecto a
  diciembre del año anterior (\\ ipc_t, ipc_t_vm, ipc_t_vdBienes y
  servicios transables: índice, variación mensual (\\ ipc_nt, ipc_nt_vm,
  ipc_nt_vdBienes y servicios no transables: índice, variación mensual
  (\\ (\\ Descarga y limpia la serie mensual del Índice de Precios al
  Consumidor (IPC) desagregada en bienes y servicios transables (`_t`,
  expuestos a competencia internacional) y no transables (`_nt`), base
  2019-2020, publicada por el Banco Central de la República Dominicana
  (BCRD). El archivo se descarga en cada llamada desde el Excel del
  BCRD; no hay caché. Se descarta un encabezado largo (`skip = 31`)
  propio de este archivo y los guiones (`"-"`) se interpretan como `NA`.
  A diferencia de
  [`get_ipc_general()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_general.md)
  y
  [`get_ipc_subyacente()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_subyacente.md),
  ninguno de los tres bloques (general, transable, no transable) incluye
  variación interanual (`_vi`) ni promedio de 12 meses (`_p12`); solo
  variación mensual (`_vm`) y acumulada (`_vd`).Llamada internamente por
  [`get_ipc_data()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_data.md)
  con `desagregacion = "tnt"`; no está exportada, así que para uso
  normal conviene usar `get_ipc_data("tnt")`.
