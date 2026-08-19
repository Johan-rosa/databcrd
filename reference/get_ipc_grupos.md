# IPC de República Dominicana por grupo de bienes y servicios

## Usage

``` r
get_ipc_grupos()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/ipc_grupos_base_2019-2020.xls>

## Value

Un tibble con una fila por mes y las columnas:

- fecha:

  `Date`. Primer día del mes de la observación.

- year:

  Año.

- mes:

  Mes (1-12).

- ipc_ayb, ipc_ayb_vm:

  Alimentos y bebidas no alcohólicas: índice y variación mensual (\\
  ipc_alcohol_tabaco, ipc_alcohol_tabaco_vmBebidas alcohólicas y tabaco:
  índice y variación mensual (\\ ipc_ropa_calzado,
  ipc_ropa_calzado_vmRopa y calzado: índice y variación mensual (\\
  ipc_vivienda, ipc_vivienda_vmVivienda: índice y variación mensual (\\
  ipc_muebles, ipc_muebles_vmMuebles y artículos para el hogar: índice y
  variación mensual (\\ ipc_salud, ipc_salud_vmSalud: índice y variación
  mensual (\\ ipc_transporte, ipc_transporte_vmTransporte: índice y
  variación mensual (\\ ipc_comunicaciones,
  ipc_comunicaciones_vmComunicaciones: índice y variación mensual (\\
  ipc_cultura, ipc_cultura_vmRecreación y cultura: índice y variación
  mensual (\\ ipc_educacion, ipc_educacion_vmEducación: índice y
  variación mensual (\\ ipc_hotel_restaurantes,
  ipc_hotel_restaurantes_vmRestaurantes y hoteles: índice y variación
  mensual (\\ ipc_bines_servicios, ipc_bienes_servicios_vmBienes y
  servicios diversos: índice (nombre con el typo del origen) y variación
  mensual (\Descarga y limpia la serie mensual del Índice de Precios al
  Consumidor (IPC) desagregada por grupo de bienes y servicios, base
  2019-2020, publicada por el Banco Central de la República Dominicana
  (BCRD).A diferencia de
  [`get_ipc_general()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_general.md),
  cada grupo solo trae el índice y su variación mensual (`_vm`); no
  incluye variación interanual, acumulada ni promedio de 12 meses.La
  columna del grupo "Bienes y servicios diversos" tiene un nombre
  inconsistente en el origen: el índice se llama `ipc_bines_servicios`
  (sin la "e" de "bienes") mientras que su variación mensual sí se llama
  `ipc_bienes_servicios_vm`. Se documenta tal cual está para no romper
  código existente que dependa de estos nombres.Llamada internamente por
  [`get_ipc_data()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_data.md)
  con `desagregacion = "grupos"`; no está exportada, así que para uso
  normal conviene usar `get_ipc_data("grupos")`.
