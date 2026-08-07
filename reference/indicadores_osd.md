# Obtener indicadores de Otras Sociedades de Depósito (OSD)

Descarga la serie de indicadores de OSD publicada por el Banco Central
de la República Dominicana, la transforma a formato largo (tidy) y la
enriquece con los metadatos de clasificación provenientes de
`detalles_indicadores_osd`.

## Usage

``` r
indicadores_osd(
  filtro_variable = NULL,
  filtro_nivel_tipo = NULL,
  filtro_sector = NULL,
  filtro_moneda = NULL,
  filtro_entidad = NULL
)
```

## Arguments

- filter_by_variable:

  Vector de caracteres opcional. Filtra por una o más de las siguientes
  categorías:

  - `"Activos externos"`

  - `"Pasivos externos"`

  - `"Inversiones"`

  - `"Préstamos"`

  - `"Depósitos"`

  - `"Composición depósitos"`

  - `"Tasa de cambio"`

  `NULL` (por defecto) no aplica filtro.

- filter_by_sector:

  Vector de caracteres opcional. Filtra por uno o más de los siguientes
  valores:

  - `"Total"`

  - `"Sector público"`

  - `"Sector privado"`

  - `"Sociedades financieras"`

  - `"No residentes"`

  - `"Depósitos transferibles"`

  - `"Otros depósitos"`

  - `"Valores distintos de acciones"`

  Aplica solo a las variables `"Préstamos"`, `"Depósitos"` y
  `"Composición depósitos"`; para el resto de variables esta columna es
  `NA` (ver "Nota sobre valores `NA`" más abajo).

- filter_by_moneda:

  Vector de caracteres opcional. Filtra por uno o más de los siguientes
  valores: `"Total"`, `"DOP"`, `"USD"`. Aplica solo a `"Préstamos"` y
  `"Depósitos"`; para el resto de variables esta columna es `NA`.

- filter_by_entidad:

  Vector de caracteres opcional. Filtra por uno o más de los siguientes
  valores:

  - `"General"` (agregado, no desglosado por entidad)

  - `"Bancos múltiples"`

  - `"Resto OSD"`

- filter_by_nivel:

  Vector entero opcional. Filtra por nivel de agregación:

  - `1` — Total agregado

  - `2` — Por sector

  - `3` — Por moneda

  - `4` — Por entidad

## Value

Un tibble en formato largo con columnas `row_id`, `indicador`, `date`,
`value`, `variable`, `sector`, `moneda`, `entidad`, `nivel`,
`nivel_tipo`.

## Nota sobre valores `NA` en `sector` y `moneda`

Variables como `"Activos externos"`, `"Pasivos externos"`,
`"Inversiones"` y `"Tasa de cambio"` no tienen desglose por sector ni
por moneda, por lo que esas columnas quedan en `NA` para esas filas.
Como `NA %in% x` siempre evalúa a `FALSE` (nunca `NA`), aplicar
`filter_by_sector` o `filter_by_moneda` excluye automáticamente esas
variables del resultado. Si quieres esas variables junto con un filtro
de sector/moneda, combina con `filter_by_variable` en la misma llamada,
o filtra en dos pasos.

## Examples

``` r
if (FALSE) { # \dontrun{
indicadores_osd()

indicadores_osd(
  filtro_variable = "Préstamos",
  filtro_sector = "Sector privado",
  filtro_moneda = "USD"
)

indicadores_osd(filter_by_entidad = c("Bancos múltiples", "Resto OSD"))
} # }
```
