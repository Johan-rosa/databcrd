# IPC de República Dominicana por región geográfica

## Usage

``` r
get_ipc_regiones()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_regiones_base_2019-2020.xls>

## Value

Un tibble con una fila por mes y las columnas:

- fecha:

  `Date`. Primer día del mes de la observación.

- year:

  Año.

- mes:

  Mes (1-12).

- ipc_ozama, ipc_ozama_vm:

  Región Ozama: índice y variación mensual (\\ ipc_cibao,
  ipc_cibao_vmRegión Cibao: índice y variación mensual (\\ ipc_este,
  ipc_este_vmRegión Este: índice y variación mensual (\\ ipc_sur,
  ipc_sur_vmRegión Sur: índice y variación mensual (\Descarga y limpia
  la serie mensual del Índice de Precios al Consumidor (IPC) desagregada
  por región geográfica, base 2019-2020, publicada por el Banco Central
  de la República Dominicana (BCRD).Al igual que
  [`get_ipc_grupos()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_grupos.md),
  cada región solo trae el índice y su variación mensual (`_vm`); no
  incluye variación interanual, acumulada ni promedio de 12
  meses.Llamada internamente por
  [`get_ipc_data()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_data.md)
  con `desagregacion = "regiones"`; no está exportada, así que para uso
  normal conviene usar `get_ipc_data("regiones")`.
