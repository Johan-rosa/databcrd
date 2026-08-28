# Población en Edad de Trabajar (PET) y Fuerza de Trabajo (PEA)

Descarga desde el Banco Central de la República Dominicana (BCRD) el
archivo trimestral de Población en Edad de Trabajar por Condición de
Actividad (PET, PEA, Ocupados, Desocupados e Inactivos) y lo organiza en
un tibble homogéneo, con una desagregación por sexo (Total, Masculino,
Femenino) tomada de las distintas hojas del archivo.

## Usage

``` r
pea(filtro_desagregacion = NULL)
```

## Arguments

- filtro_desagregacion:

  Vector de caracteres opcional. Uno o más valores entre "total",
  "masculino" y "femenino" (no distingue entre mayúsculas y minúsculas).
  Si es `NULL` (por defecto), se retornan las tres desagregaciones.

## Value

Un tibble con las columnas `fecha`, `year`, `trimestre`,
`desagregacion`, `pet`, `pea`, `ocupados`, `desocupados` e `inactivos`.

## Examples

``` r
if (FALSE) { # \dontrun{
pea()
pea("total")
pea(c("masculino", "femenino"))
} # }
```
