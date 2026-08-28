# Principales indicadores del mercado laboral en tasas (porcentajes)

Descarga y organiza, en formato largo, la tabla de tasas (porcentajes)
de los Principales Indicadores del Mercado Laboral publicados por el
BCRD: Tasa Global de Participación, Tasa de Ocupación, Tasa de
Desocupación (SU1-SU4), Tasa de Inactividad, etc. Incluye la
desagregación por sexo (Total, Masculino, Femenino).

## Usage

``` r
mercado_laboral_tasas(filtro_indicador = NULL)
```

## Arguments

- filtro_indicador:

  Vector de caracteres opcional para filtrar por nombre de indicador. La
  coincidencia es parcial y no distingue mayúsculas/minúsculas (ver
  detalles en `parse_indicadores_laborales()`).

## Value

Un tibble con las columnas `desagregacion`, `indicador`, `fecha` y
`tasa`.
