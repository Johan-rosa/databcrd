# Download interest rates for loans (Activas)

Downloads and aggregates historic monthly interest rates for loans
(tasas activas) published by the Central Bank of the Dominican Republic
(BCRD).

## Usage

``` r
get_tasas_activas(
  long = FALSE,
  filtro_condicion = NULL,
  filtro_grupo = NULL,
  filtro_detalle = NULL
)
```

## Arguments

- long:

  `<logical>` If `TRUE`, converts data frame into long format. Defaults
  to `FALSE`.

- filtro_condicion:

  `<character>` Filter by rate condition: `"General"` or
  `"Preferencial"`. Only used if `long = TRUE`.

- filtro_grupo:

  `<character>` Filter by grouping: `"Plazo"`, `"Promedio"`, or
  `"Sector"`. Only used if `long = TRUE`.

- filtro_detalle:

  `<character>` Filter by specific descriptive category. Only used if
  `long = TRUE`.

## Value

A `tibble` containing monthly series data.
