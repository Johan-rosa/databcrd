# Average exchange rate of the operations in financial institutions

This function returns the average exchange rates of the operations
conducted by the banks and exchange operators in the Dominican Republic
based on the specified frequency.

## Usage

``` r
get_tc_eif(frecuencia = "diaria")
```

## Arguments

- frecuencia:

  A character string that specifies the frequency of the exchange rates
  to be downloaded. Valid options are "diaria", "mensual", "trimestral",
  or "anual".

## Value

A data frame with columns: compra: for the buying rates venta: selling
rates

## Examples

``` r
get_tc_eif("mensual")
#> # A tibble: 410 × 5
#>    fecha       year   mes compra venta
#>    <date>     <dbl> <dbl>  <dbl> <dbl>
#>  1 1992-01-01  1992     1   12.5  12.6
#>  2 1992-02-01  1992     2   12.5  12.6
#>  3 1992-03-01  1992     3   12.6  12.7
#>  4 1992-04-01  1992     4   12.6  12.7
#>  5 1992-05-01  1992     5   12.7  12.8
#>  6 1992-06-01  1992     6   12.7  12.8
#>  7 1992-07-01  1992     7   12.7  12.8
#>  8 1992-08-01  1992     8   12.6  12.8
#>  9 1992-09-01  1992     9   12.5  12.6
#> 10 1992-10-01  1992    10   12.5  12.6
#> # ℹ 400 more rows
get_tc_eif("anual")
#> # A tibble: 34 × 3
#>     year compra venta
#>    <dbl>  <dbl> <dbl>
#>  1  1992   12.6  12.7
#>  2  1993   12.5  12.6
#>  3  1994   12.8  13.0
#>  4  1995   13.5  13.6
#>  5  1996   13.6  13.7
#>  6  1997   14.2  14.3
#>  7  1998   15.2  15.2
#>  8  1999   16.0  16.0
#>  9  2000   16.3  16.4
#> 10  2001   16.8  16.9
#> # ℹ 24 more rows
```
