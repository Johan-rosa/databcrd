# Free Trade Exports

This function returns Free Trade exports from the Dominican Republic by
goods

## Usage

``` r
get_exportaciones_zf()
```

## Value

A data frame

## Examples

``` r
get_exportaciones_zf()
#> # A tibble: 1,701 × 3
#>    fecha      partida                                     valor_expor
#>    <date>     <chr>                                             <dbl>
#>  1 2010-01-01 confecciones_textiles                              38.3
#>  2 2010-01-01 productos_electricos                               36.4
#>  3 2010-01-01 articulos_de_joyeria_y_conexos                     23.1
#>  4 2010-01-01 productos_farmaceuticos_2                           0.9
#>  5 2010-01-01 fabricacion_equipos_medicos_y_quirurgicos_2        53.1
#>  6 2010-01-01 manufacturas_de_calzados                           19.8
#>  7 2010-01-01 manufacturas_de_tabaco                             19.5
#>  8 2010-01-01 otros                                              23.4
#>  9 2010-01-01 total                                             214. 
#> 10 2010-02-01 confecciones_textiles                              60.2
#> # ℹ 1,691 more rows
```
