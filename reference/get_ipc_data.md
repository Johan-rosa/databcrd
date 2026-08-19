# Descarga el Índice de Precios al Consumidor (IPC) de República Dominicana

Punto de entrada único para las distintas desagregaciones del IPC
publicadas por el Banco Central de la República Dominicana (BCRD). Según
el valor de `desagregacion`, delega en
[`get_ipc_general()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_general.md),
[`get_ipc_grupos()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_grupos.md),
[`get_ipc_regiones()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_regiones.md),
[`get_ipc_subyacente()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_subyacente.md)
o
[`get_ipc_tnt()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_tnt.md).

## Usage

``` r
get_ipc_data(
  desagregacion = c("general", "grupos", "regiones", "subyacente", "tnt")
)
```

## Arguments

- desagregacion:

  Cadena con la desagregación deseada. Una de:

  `"general"`

  :   Índice general del IPC, sin desagregar.

  `"grupos"`

  :   IPC por grupos de bienes y servicios (alimentos y bebidas,
      vivienda, transporte, etc.).

  `"regiones"`

  :   IPC por región geográfica (Ozama, Cibao, Este, Sur).

  `"subyacente"`

  :   IPC subyacente (inflación núcleo, excluye rubros volátiles).

  `"tnt"`

  :   IPC de bienes transables y no transables.

## Value

Un tibble; las columnas exactas dependen de `desagregacion` (ver
[`get_ipc_general()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_general.md),
[`get_ipc_grupos()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_grupos.md),
[`get_ipc_regiones()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_regiones.md),
[`get_ipc_subyacente()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_subyacente.md)
y
[`get_ipc_tnt()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_tnt.md)).
En todos los casos incluye al menos `fecha`, `year` y `mes`.

## Details

Todas las series se descargan directamente desde los archivos Excel
publicados por el BCRD (base 2019-2020); no hay caché, así que cada
llamada dispara una descarga nueva.

Para el detalle por artículo (grupo, subgrupo, clase, subclase y
artículo) hay que usar
[`get_ipc_articulos()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_articulos.md)
o
[`get_ipc_long()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_long.md):
esta función no acepta `"articulos"` como valor de `desagregacion`.

## Examples

``` r
if (FALSE) { # \dontrun{
get_ipc_data("general")
get_ipc_data("grupos")
get_ipc_data("subyacente")
get_ipc_data("regiones")
get_ipc_data("tnt")
} # }
```
