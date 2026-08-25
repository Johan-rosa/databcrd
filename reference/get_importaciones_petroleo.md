# Importaciones mensuales de petróleo y derivados

Descarga y organiza en formato ordenado (tidy) la serie de importaciones
mensuales de petróleo crudo y sus derivados publicada por el Banco
Central de la República Dominicana (BCRD), con datos desde enero de 2010
en adelante.

## Usage

``` r
get_importaciones_petroleo()
```

## Value

Un tibble con las columnas:

- fecha:

  Fecha correspondiente al mes de la observación (`Date`).

- combustible:

  Tipo de combustible. Uno de: "Petroleo Crudo", "Gasolina", "Gasoil",
  "GLP", "Gas Natural", "Fuel-Oil", "Gasolina de Aviación", "Avtur",
  "Otros", "Total".

- volumen:

  Volumen importado, en barriles (BB).

- precio:

  Precio promedio, en US\$/BB.

- valor:

  Valor de la importación, en US\$.

## Details

La función descarga el archivo `Importaciones_Crudo_6.xls` publicado por
el BCRD en su sección de estadísticas del sector externo, reconstruye
los encabezados (que combinan el nombre del combustible con la métrica
en filas separadas) y transforma el resultado a formato largo, con una
fila por combinación de fecha y combustible.

## Examples

``` r
if (FALSE) { # \dontrun{
get_importaciones_petroleo()
} # }
```
