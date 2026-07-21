# Download interest rates for savings (Pasivas)

Downloads and aggregates historic monthly interest rates for savings
(tasas pasivas) published by the Central Bank of the Dominican Republic
(BCRD).

## Usage

``` r
get_tasas_pasivas(
  entidad = c("bm", "aap", "bac", "cc"),
  long = FALSE,
  filtro_condicion = NULL,
  filtro_grupo = NULL,
  filtro_detalle = NULL
)
```

## Arguments

- entidad:

  `<character>` Entity type to fetch. One of: ' \* `"bm"`: Multiple
  Banks. ' \* `"aap"`: Savings and Loan Associations. ' \* `"bac"`:
  Savings and Credit Banks. ' \* `"cc"`: Credit Corporations

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
