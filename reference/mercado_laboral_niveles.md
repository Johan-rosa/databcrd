# Principales indicadores del mercado laboral en niveles (personas)

Descarga y organiza, en formato largo, la tabla de niveles (personas) de
los Principales Indicadores del Mercado Laboral publicados por el BCRD:
Población Total, PET, PEA, Ocupados, Subocupados, formalidad, Cesantes,
Nuevos, Fuerza de Trabajo Potencial, Inactivos, etc. Incluye la
desagregación por sexo (Total, Masculino, Femenino).

## Usage

``` r
mercado_laboral_niveles(filtro_indicador = NULL)
```

## Arguments

- filtro_indicador:

  Vector de caracteres opcional para filtrar por nombre de indicador. La
  coincidencia es parcial y no distingue mayúsculas/minúsculas (ver
  detalles en `parse_indicadores_laborales()`).

## Value

Un tibble con las columnas `desagregacion`, `indicador`, `fecha` y
`poblacion`.
