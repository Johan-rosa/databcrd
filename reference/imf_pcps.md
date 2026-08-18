# Precios de materias primas del FMI (PCPS)

Descarga series mensuales del Primary Commodity Price System (PCPS) del
FMI para uno o más indicadores, y las une con sus nombres descriptivos
obtenidos de
[`imf_pcps_catalogo()`](https://johan-rosa.github.io/databcrd/reference/imf_pcps_catalogo.md).

## Usage

``` r
imf_pcps(
  indicadores = default_pcps_indicators,
  data_transformation = "INDEX",
  start_period = "2019-01"
)
```

## Source

<https://data.imf.org/en/datasets/IMF.RES:PCPS>

## Arguments

- indicadores:

  Vector de códigos de indicador PCPS (ver
  [`imf_pcps_catalogo()`](https://johan-rosa.github.io/databcrd/reference/imf_pcps_catalogo.md)
  para la lista completa). Por defecto, `default_pcps_indicators`: una
  selección curada de 16 índices y precios de alimentos, bebidas,
  cereales y fertilizantes.

- data_transformation:

  Código SDMX de transformación de datos. `"INDEX"` por defecto (series
  en forma de índice); otras transformaciones disponibles dependen del
  indicador.

- start_period:

  Periodo inicial de la consulta, como cadena `"YYYY-MM"` (p. ej.
  `"2019-01"`). Se convierte con
  [`as.character()`](https://rdrr.io/r/base/character.html) antes de
  enviarse a la API.

## Value

Un tibble con las columnas:

- id:

  `chr`. Código del indicador PCPS.

- date:

  `Date`. Primer día del mes de la observación.

- value:

  `dbl`. Valor de la observación, en la transformación solicitada
  (`data_transformation`).

- indicador_name:

  `chr`. Nombre descriptivo del indicador, obtenido de
  [`imf_pcps_catalogo()`](https://johan-rosa.github.io/databcrd/reference/imf_pcps_catalogo.md).
  `NA` si el código no aparece en el catálogo actual.

## Details

La consulta siempre filtra `COUNTRY = "G001"` (agregado mundial): PCPS
es un sistema de precios internacionales de referencia y no reporta
series por país, así que este es el único código de país que existe para
este dataflow. Se deja explícito en el filtro en vez de depender del
comportamiento por defecto de
[`imf.data::get_data()`](https://pedrobtz.github.io/imf.data/reference/get_data.html)
(que ya devuelve únicamente `"G001"` si se omite el filtro), para que la
consulta sea autoexplicativa y no dependa de un default no documentado.

El catálogo
([`imf_pcps_catalogo()`](https://johan-rosa.github.io/databcrd/reference/imf_pcps_catalogo.md))
se descarga en cada llamada a esta función; implica una llamada de red
adicional solo para obtener los nombres de los indicadores.

## Examples

``` r
if (FALSE) { # \dontrun{
imf_pcps()

# Solo el precio del petróleo Brent, desde 2015
imf_pcps(indicadores = "POILBRE", start_period = "2015-01")
} # }
```
