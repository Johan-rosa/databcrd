# Descargar y consolidar las tasas de interés semanales del BCRD

`get_tasas_semanales()` descarga el archivo de tasas de interés activas
y pasivas semanales publicado por el Banco Central de la República
Dominicana (BCRD) para una entidad financiera y un año determinados,
procesa todas las hojas del libro de Excel, y devuelve los datos
consolidados en formato largo (long) listos para análisis.

## Usage

``` r
get_tasas_semanales(
  year = 2025,
  entidad = c("bm", "aap", "bac"),
  filtro_tipo_tasa = NULL,
  filtro_moneda = NULL,
  filtro_condicion = NULL,
  filtro_grupo = NULL,
  filtro_detalle = NULL
)
```

## Arguments

- year:

  Numeric. Año de la serie a descargar (p. ej. `2025`).

- entidad:

  Character. Entidad financiera cuyo archivo de tasas se desea
  descargar. Uno de `"bm"`, `"aap"`, `"bac"`. Se valida con
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).

- filtro_tipo_tasa, filtro_moneda, filtro_condicion, filtro_grupo,
  filtro_detalle:

  Filtros opcionales que se pasan directamente a
  [`tasas_to_long()`](https://johan-rosa.github.io/databcrd/reference/tasas_to_long.md)
  para filtrar el resultado final en formato largo. `NULL` (por defecto)
  equivale a no filtrar.

## Value

Un tibble en formato largo con las tasas de interés semanales de la
entidad y año solicitados, incluyendo columnas como `start_date`,
`end_date`, `year`, `type`, `moneda`, y las columnas resultantes de
[`tasas_to_long()`](https://johan-rosa.github.io/databcrd/reference/tasas_to_long.md)
(p. ej. `tipo_tasa`, `condicion`, `grupo`, `detalle`, `valor`, según
corresponda).

## See also

[`tasas_to_long()`](https://johan-rosa.github.io/databcrd/reference/tasas_to_long.md)
para la lógica de transformación a formato largo y sus filtros.

## Examples

``` r
if (FALSE) { # \dontrun{
get_tasas_semanales(year = 2025, entidad = "bm")
get_tasas_semanales(year = 2024, entidad = "aap", filtro_moneda = "DOP")
} # }
```
