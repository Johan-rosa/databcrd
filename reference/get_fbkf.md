# Download GROSS FIXED CAPITAL FORMATION BY SECTOR AND TYPE OF GOOD

Get the series of the gross fixed capital formation by sector and type
of good of the Dominican Republic.

## Usage

``` r
get_fbkf()
```

## Value

a data frame with monthly series

## Examples

``` r
get_fbkf()
#> # A tibble: 85 × 5
#>     year categoria              monto variacion_interanual proporcion
#>    <dbl> <chr>                  <dbl>                <dbl>      <dbl>
#>  1  2007 total                394318.                 NA        100  
#>  2  2007 privado              338643.                 NA         85.9
#>  3  2007 publico               55675.                 NA         14.1
#>  4  2007 construccion         290672.                 NA         73.7
#>  5  2007 maquinaria_y_equipos 103646.                 NA         26.3
#>  6  2008 total                467113.                 18.5      100  
#>  7  2008 privado              387612.                 14.5       83.0
#>  8  2008 publico               79501.                 42.8       17.0
#>  9  2008 construccion         351466.                 20.9       75.2
#> 10  2008 maquinaria_y_equipos 115648.                 11.6       24.8
#> # ℹ 75 more rows
```
