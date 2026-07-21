# Daily Interest Rates from the Central Bank

Downloads and processes the daily interest rates published by the
Central Bank of the Dominican Republic (BCRD) for a given year.

## Usage

``` r
get_tasas_diarias(
  year = 2025,
  filtro_tipo_tasa = NULL,
  filtro_moneda = NULL,
  filtro_condicion = NULL,
  filtro_grupo = NULL,
  filtro_detalle = NULL
)
```

## Arguments

- year:

  `<integer>` Year to download. Defaults to `2025`.

- filtro_tipo_tasa:

  `<character>` Filter by rate type: `"Activa"` or `"Pasiva"`.

- filtro_moneda:

  `<character>` Filter by currency: `"DOP"` or `"USD"`.

- filtro_condicion:

  `<character>` Filter by rate condition: `"General"` or
  `"Preferencial"`.

- filtro_grupo:

  `<character>` Filter by grouping: `"Plazo"`, `"Promedio"`, or
  `"Sector"`.

- filtro_detalle:

  `<character>` Filter by detail category (see the `detalle` column in
  the output, e.g. `"Comercio"`, `"0 a 30 días"`).

## Value

A `tibble` with one row per rate and date, containing the following
columns:

- fecha:

  Observation date (`Date`).

- year:

  Year (`integer`).

- mes:

  Month (`integer`).

- day:

  Day of the month (`integer`).

- tipo_tasa:

  `"Activa"` or `"Pasiva"`.

- moneda:

  `"DOP"` or `"USD"`.

- grupo:

  `"Plazo"`, `"Promedio"`, or `"Sector"`.

- condicion:

  `"General"` or `"Preferencial"`.

- detalle:

  Instrument, maturity, or sector description.

- tasa:

  Interest rate value (`double`).

## Examples

``` r
# All rates for 2024
get_tasas_diarias(2024)
#> # A tibble: 13,219 × 10
#>    fecha       year   mes   day tipo_tasa moneda grupo   condicion detalle  tasa
#>    <date>     <dbl> <dbl> <dbl> <chr>     <chr>  <chr>   <chr>     <chr>   <dbl>
#>  1 2024-01-02  2024     1     2 Activa    DOP    Plazo   General   0 a 90… 18.2 
#>  2 2024-01-02  2024     1     2 Activa    DOP    Plazo   General   91 a 1…  9.96
#>  3 2024-01-02  2024     1     2 Activa    DOP    Plazo   General   331 a … 15.6 
#>  4 2024-01-02  2024     1     2 Activa    DOP    Plazo   General   361 dí… 14.6 
#>  5 2024-01-02  2024     1     2 Activa    DOP    Plazo   General   2 a 5 … 18.8 
#>  6 2024-01-02  2024     1     2 Activa    DOP    Plazo   General   m2 a 5… 14.3 
#>  7 2024-01-02  2024     1     2 Activa    DOP    Promed… General   Promed… 15.4 
#>  8 2024-01-02  2024     1     2 Activa    DOP    Promed… General   Promed… 15.3 
#>  9 2024-01-02  2024     1     2 Activa    DOP    Sector  General   Comerc… 12.7 
#> 10 2024-01-02  2024     1     2 Activa    DOP    Sector  General   Consumo 18.8 
#> # ℹ 13,209 more rows

# Only active rates in DOP grouped by term
get_tasas_diarias(
  year             = 2024,
  filtro_tipo_tasa = "Activa",
  filtro_moneda    = "DOP",
  filtro_grupo     = "Plazo"
)
#> # A tibble: 1,512 × 10
#>    fecha       year   mes   day tipo_tasa moneda grupo condicion detalle    tasa
#>    <date>     <dbl> <dbl> <dbl> <chr>     <chr>  <chr> <chr>     <chr>     <dbl>
#>  1 2024-01-02  2024     1     2 Activa    DOP    Plazo General   0 a 90 d… 18.2 
#>  2 2024-01-02  2024     1     2 Activa    DOP    Plazo General   91 a 180…  9.96
#>  3 2024-01-02  2024     1     2 Activa    DOP    Plazo General   331 a 60… 15.6 
#>  4 2024-01-02  2024     1     2 Activa    DOP    Plazo General   361 días… 14.6 
#>  5 2024-01-02  2024     1     2 Activa    DOP    Plazo General   2 a 5 añ… 18.8 
#>  6 2024-01-02  2024     1     2 Activa    DOP    Plazo General   m2 a 5 a… 14.3 
#>  7 2024-01-03  2024     1     3 Activa    DOP    Plazo General   0 a 90 d… 11.4 
#>  8 2024-01-03  2024     1     3 Activa    DOP    Plazo General   91 a 180… 11.5 
#>  9 2024-01-03  2024     1     3 Activa    DOP    Plazo General   331 a 60… 15.2 
#> 10 2024-01-03  2024     1     3 Activa    DOP    Plazo General   361 días… 15.3 
#> # ℹ 1,502 more rows
```
