# Exportaciones totales por sector

Descarga y consolida las cifras de exportaciones totales de Republica
Dominicana por sector (minerales, agropecuarios, industriales) segun la
periodicidad solicitada, a partir de los archivos publicados por el
Banco Central en su portal de estadisticas del sector externo.

## Usage

``` r
get_exportaciones(
  frecuencia = c("mensual", "trimestral", "anual"),
  filtro_categoria = NULL,
  filtro_nivel = NULL,
  filtro_regimen = NULL
)
```

## Arguments

- frecuencia:

  Cadena de texto con la periodicidad de los datos. Valores validos:
  "mensual", "trimestral" o "anual".

- filtro_categoria:

  Vector de caracteres opcional para filtrar por categoria (p. ej.
  "Minerales", "Agropecuarios", "Industriales", "Subtotal", "Total"). Si
  es `NULL` (por defecto) no se filtra.

- filtro_nivel:

  Vector numerico opcional para filtrar por nivel jerarquico (1 a 4). Si
  es `NULL` (por defecto) no se filtra.

- filtro_regimen:

  Vector de caracteres opcional para filtrar por regimen ("Nacionales" o
  "Zonas Francas"). Si es `NULL` (por defecto) no se filtra.

## Value

Un tibble con las exportaciones por sector, con columnas de fecha (o
year/trimestre segun la frecuencia), categoria, nivel, regimen y valor
exportado.

## Examples

``` r
if (FALSE) { # \dontrun{
get_exportaciones("mensual")
get_exportaciones("anual", filtro_categoria = "Industriales")
get_exportaciones("trimestral", filtro_regimen = "Zonas Francas")
} # }
```
