# Exchange rates in the spot market of the Dominican Republic

This function returns the average exchange rates of the operations
conducted by the banks and exchange operators in the Dominican Republic
based on the specified frequency.

## Usage

``` r
get_tc_spot(frecuencia = "mensual", average_or_fp = "average")
```

## Arguments

- frecuencia:

  A character string that specifies the frequency of the exchange rates
  to be downloaded. Valid options are "diaria", "mensual", "trimestral",
  or "anual".

- average_or_fp:

  A character string that specifies if the average or the value for the
  last day of the period is desired. valid options are "average" and
  "fp"

## Value

A data frame with columns: compra: for the buying rates venta: selling
rates

## Examples

``` r
get_tc_spot("mensual", "average")
#> # A tibble: 494 × 5
#>    fecha       year   mes compra venta
#>    <date>     <dbl> <dbl>  <dbl> <dbl>
#>  1 1985-01-01  1985     1   3.26  3.28
#>  2 1985-02-01  1985     2   3.26  3.28
#>  3 1985-03-01  1985     3   3.31  3.33
#>  4 1985-04-01  1985     4   3.21  3.24
#>  5 1985-05-01  1985     5   3.17  3.2 
#>  6 1985-06-01  1985     6   3.11  3.13
#>  7 1985-07-01  1985     7   2.99  3.01
#>  8 1985-08-01  1985     8   2.98  2.99
#>  9 1985-09-01  1985     9   2.98  2.99
#> 10 1985-10-01  1985    10   2.99  3   
#> # ℹ 484 more rows
get_tc_spot("mensual", "fp")
#> # A tibble: 422 × 5
#>    fecha       year   mes compra venta
#>    <date>     <dbl> <dbl>  <dbl> <dbl>
#>  1 1991-01-01  1991     1   11.7  12  
#>  2 1991-02-01  1991     2   12.7  13  
#>  3 1991-03-01  1991     3   12.7  13  
#>  4 1991-04-01  1991     4   12.7  13  
#>  5 1991-05-01  1991     5   12.7  13  
#>  6 1991-06-01  1991     6   12.7  13  
#>  7 1991-07-01  1991     7   12.5  12.5
#>  8 1991-08-01  1991     8   12.5  12.5
#>  9 1991-09-01  1991     9   12.5  12.5
#> 10 1991-10-01  1991    10   12.5  12.5
#> # ℹ 412 more rows
get_tc_spot("trimestral", "average")
#> # A tibble: 164 × 5
#>    fecha       year trimestre compra venta
#>    <date>     <dbl>     <int>  <dbl> <dbl>
#>  1 1992-01-01  1985         1   3.19  3.21
#>  2 1992-04-01  1985         2   3.16  3.19
#>  3 1992-07-01  1985         3   2.98  3.00
#>  4 1992-10-01  1985         4   2.98  2.99
#>  5 1993-01-01  1986         1   2.81  2.83
#>  6 1993-04-01  1986         2   2.82  2.83
#>  7 1993-07-01  1986         3   2.83  2.84
#>  8 1993-10-01  1986         4   3.02  3.03
#>  9 1994-01-01  1987         1   3.12  3.13
#> 10 1994-04-01  1987         2   3.42  3.44
#> # ℹ 154 more rows
get_tc_spot("trimestral", "fp")
#> # A tibble: 140 × 5
#>    fecha       year trimestre compra venta
#>    <date>     <dbl>     <int>  <dbl> <dbl>
#>  1 1992-01-01  1991         1   12.7  13  
#>  2 1992-04-01  1991         2   12.7  13  
#>  3 1992-07-01  1991         3   12.5  12.5
#>  4 1992-10-01  1991         4   12.5  12.5
#>  5 1993-01-01  1992         1   12.5  12.5
#>  6 1993-04-01  1992         2   12.5  12.5
#>  7 1993-07-01  1992         3   12.5  12.5
#>  8 1993-10-01  1992         4   12.5  12.5
#>  9 1994-01-01  1993         1   12.5  12.5
#> 10 1994-04-01  1993         2   12.5  12.5
#> # ℹ 130 more rows
```
