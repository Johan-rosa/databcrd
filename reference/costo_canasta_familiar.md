# Family Basket Cost

Downloads and returns the monthly cost of the Dominican Republic's
family basket (`Canasta Familiar`) published by the Central Bank of the
Dominican Republic. Data are available either by income quintile or by
geographic region.

## Usage

``` r
costo_canasta_familiar(
  by = c("regiones", "quintiles"),
  format = c("long", "wide")
)
```

## Arguments

- by:

  Character. Level of disaggregation. One of:

  "regiones"

  :   Family basket cost by geographic region.

  "quintiles"

  :   Family basket cost by household income quintile.

- format:

  Character. Output format:

  "long"

  :   Returns a tidy data frame with one observation per date and group.

  "wide"

  :   Returns the original table in wide format.

## Value

A tibble containing the monthly family basket cost.

When `format = "long"`, the returned data frame contains:

- date:

  Date corresponding to the first day of each month.

- year:

  Year.

- mes:

  Month.

- regiones or quintiles:

  Grouping variable specified by `by`.

- costo:

  Family basket cost in Dominican pesos (DOP).

When `format = "wide"`, the original publication layout is returned with
cleaned column names.

## Details

The data are downloaded directly from the Central Bank of the Dominican
Republic and correspond to the 2019–2020 reference base.

## Examples

``` r
# Family basket cost by income quintile
costo_canasta_familiar(by = "quintiles")
#> # A tibble: 612 × 5
#>    date        year   mes quintiles  costo
#>    <date>     <dbl> <dbl> <chr>      <dbl>
#>  1 2018-01-01  2018     1 Quintil 1 19402.
#>  2 2018-01-01  2018     1 Quintil 2 25439.
#>  3 2018-01-01  2018     1 Quintil 3 30445.
#>  4 2018-01-01  2018     1 Quintil 4 35856.
#>  5 2018-01-01  2018     1 Quintil 5 56196.
#>  6 2018-01-01  2018     1 Nacional  33464.
#>  7 2018-02-01  2018     2 Quintil 1 19345.
#>  8 2018-02-01  2018     2 Quintil 2 25384.
#>  9 2018-02-01  2018     2 Quintil 3 30393.
#> 10 2018-02-01  2018     2 Quintil 4 35820.
#> # ℹ 602 more rows

# Family basket cost by region
costo_canasta_familiar(by = "regiones")
#> # A tibble: 276 × 5
#>    date        year   mes regiones              costo
#>    <date>     <dbl> <dbl> <chr>                 <dbl>
#>  1 2020-10-01  2020    10 Región Ozama         42976.
#>  2 2020-10-01  2020    10 Región Norte o Cibao 34392.
#>  3 2020-10-01  2020    10 Región Este          33583.
#>  4 2020-10-01  2020    10 Región Sur           28750.
#>  5 2020-11-01  2020    11 Región Ozama         43229.
#>  6 2020-11-01  2020    11 Región Norte o Cibao 34598.
#>  7 2020-11-01  2020    11 Región Este          33719.
#>  8 2020-11-01  2020    11 Región Sur           28979.
#>  9 2020-12-01  2020    12 Región Ozama         43424.
#> 10 2020-12-01  2020    12 Región Norte o Cibao 34774.
#> # ℹ 266 more rows

# Wide format
costo_canasta_familiar(
  by = "quintiles",
  format = "wide"
)
#> # A tibble: 102 × 9
#>    date        year   mes quintil_1 quintil_2 quintil_3 quintil_4 quintil_5
#>    <date>     <dbl> <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1 2018-01-01  2018     1    19402.    25439.    30445.    35856.    56196.
#>  2 2018-02-01  2018     2    19345.    25384.    30393.    35820.    56200.
#>  3 2018-03-01  2018     3    19439.    25497.    30515.    35951.    56379.
#>  4 2018-04-01  2018     4    19492.    25571.    30619.    36087.    56650.
#>  5 2018-05-01  2018     5    19515.    25620.    30694.    36188.    56837.
#>  6 2018-06-01  2018     6    19571.    25692.    30769.    36257.    56922.
#>  7 2018-07-01  2018     7    19532.    25663.    30748.    36257.    56976.
#>  8 2018-08-01  2018     8    19476.    25614.    30724.    36271.    57105.
#>  9 2018-09-01  2018     9    19426.    25579.    30720.    36302.    57265.
#> 10 2018-10-01  2018    10    19492.    25657.    30799.    36380.    57350.
#> # ℹ 92 more rows
#> # ℹ 1 more variable: nacional <dbl>
```
