# Average exchange rate of the operations in financial institutions

This function returns the average exchange rates of the operations
conducted by the banks and exchange operators in the Dominican Republic
based on the specified frequency.

## Usage

``` r
get_tc(frecuencia = "diaria", entidad = "spot", average_or_fp = "average")
```

## Arguments

- frecuencia:

  A character string that specifies the frequency of the exchange rates
  to be downloaded. Valid options are "diaria", "mensual", "trimestral",
  or "anual".

- entidad:

  Valid options are "eif" for financial institutions, "ac" for foreign
  currency exchange agencies or "spot" for reference exchange rate

- average_or_fp:

  A character string that specifies if the average or the value for the
  last day of the period is desired. valid options are "average" and
  "fp" for end of period.

## Value

A data frame with columns: compra: for the buying rates venta: selling
rates

## Examples

``` r
get_tc(frecuencia = "diaria", entidad = "spot", average_or_fp = "average")
#> # A tibble: 8,807 × 3
#>    fecha      compra venta
#>    <date>      <dbl> <dbl>
#>  1 1991-01-02   11.2  11.5
#>  2 1991-01-03   11.2  11.5
#>  3 1991-01-04   11.2  11.5
#>  4 1991-01-07   11.2  11.5
#>  5 1991-01-08   11.2  11.5
#>  6 1991-01-09   11.2  11.5
#>  7 1991-01-10   11.2  11.5
#>  8 1991-01-11   11.2  11.5
#>  9 1991-01-14   11.2  11.5
#> 10 1991-01-15   11.2  11.5
#> # ℹ 8,797 more rows
get_tc(frecuencia = "mensual", entidad = "eif", average_or_fp = "average")
#> # A tibble: 409 × 3
#>    fecha      compra venta
#>    <date>      <dbl> <dbl>
#>  1 1992-01-01   12.5  12.6
#>  2 1992-02-01   12.5  12.6
#>  3 1992-03-01   12.6  12.7
#>  4 1992-04-01   12.6  12.7
#>  5 1992-05-01   12.7  12.8
#>  6 1992-06-01   12.7  12.8
#>  7 1992-07-01   12.7  12.8
#>  8 1992-08-01   12.6  12.8
#>  9 1992-09-01   12.5  12.6
#> 10 1992-10-01   12.5  12.6
#> # ℹ 399 more rows
get_tc(frecuencia = "trimestral", entidad = "ac", average_or_fp = "fp")
#> # A tibble: 109 × 3
#>    fecha      compra venta
#>    <date>      <dbl> <dbl>
#>  1 1999-03-31   16.1  16.2
#>  2 1999-06-30   15.8  15.9
#>  3 1999-09-30   16.0  16.1
#>  4 1999-12-31   16.0  16.1
#>  5 2000-03-31   16.3  16.3
#>  6 2000-06-30   16.5  16.5
#>  7 2000-09-29   16.5  16.5
#>  8 2000-12-27   16.6  16.7
#>  9 2001-03-30   16.8  16.9
#> 10 2001-06-29   16.8  16.8
#> # ℹ 99 more rows
```
