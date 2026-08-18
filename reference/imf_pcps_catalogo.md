# Catálogo de indicadores del Primary Commodity Price System (PCPS)

Descarga el glosario de códigos `CL_PCPS_INDICATOR` publicado por el
Departamento de Investigación del FMI (IMF.RES) en su API SDMX, y lo
convierte en un tibble con un código de indicador por fila. El PCPS
cubre precios e índices de precios de más de 100 materias primas
(energía, agricultura, fertilizantes y metales).

## Usage

``` r
imf_pcps_catalogo()
```

## Source

<https://data.imf.org/platform/rest/v1/registry/sdmx-plus/structure/glossary/IMF.RES/CL_PCPS_INDICATOR/3.0.0>

## Value

Un tibble con las columnas:

- id:

  `chr`. Código del indicador (p. ej. `"PALLFNF"`, `"POILBRE"`), usado
  para filtrar en
  [`imf_pcps()`](https://johan-rosa.github.io/databcrd/reference/imf_pcps.md).

- name:

  `chr`. Nombre corto del indicador en inglés (único idioma disponible
  en el glosario), p. ej.
  `"Brent Crude, US dollars per barrel, Unit prices"`.

- description:

  `chr`. Descripción larga del indicador: fuente de la serie,
  metodología y unidad de medida.

## Examples

``` r
if (FALSE) { # \dontrun{
imf_pcps_catalogo()
} # }
```
