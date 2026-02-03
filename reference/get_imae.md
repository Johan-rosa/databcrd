# Monthly Economic Activity Indicator, base 2018

Get the data series for the monthly Economic Activity Indicator of the
Dominican Republic. It includes seasonal and non seasonal adjusted
series, as well as monthly and year over year variations.

## Usage

``` r
get_imae(variaciones = TRUE)
```

## Arguments

- variaciones:

  Boolean indicating if variations should be included or index only

## Value

a tibble

## Examples

``` r
get_imae(variaciones = FALSE)
#> # A tibble: 228 × 6
#>    fecha       year mes        indice_original indice_desestacionali…¹ indice_tc
#>    <date>     <dbl> <chr>                <dbl>                   <dbl>     <dbl>
#>  1 2007-01-01  2007 Enero                 54.0                    55.3      55.7
#>  2 2007-02-01  2007 Febrero               55.9                    56.2      55.9
#>  3 2007-03-01  2007 Marzo                 58.7                    56.7      56.4
#>  4 2007-04-01  2007 Abril                 55.9                    53.1      56.9
#>  5 2007-05-01  2007 Mayo                  60.6                    59.4      57.5
#>  6 2007-06-01  2007 Junio                 57.4                    57.5      58.3
#>  7 2007-07-01  2007 Julio                 58.9                    58.8      59.1
#>  8 2007-08-01  2007 Agosto                59.5                    61.0      59.9
#>  9 2007-09-01  2007 Septiembre            57.2                    61.2      60.5
#> 10 2007-10-01  2007 Octubre               60.4                    60.2      60.8
#> # ℹ 218 more rows
#> # ℹ abbreviated name: ¹​indice_desestacionalizado
```
