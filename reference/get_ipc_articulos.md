# Download the CPI series at item level

Download the CPI inflacion series for the Dominican Republic by item
level with details about the group, subgroup, class and subclass

## Usage

``` r
get_ipc_articulos()
```

## Value

a tibble

## Examples

``` r
get_ipc_articulos()
#> # A tibble: 114,444 × 13
#>    id      agregacion nombre   grupo subgrupo clase subclase articulo date      
#>    <chr>   <chr>      <chr>    <chr> <chr>    <chr> <chr>    <chr>    <date>    
#>  1 01      Grupo      Aliment… Alim… NA       NA    NA       NA       2010-12-01
#>  2 011     Subgrupo   Aliment… Alim… Aliment… NA    NA       NA       2010-12-01
#>  3 0111    Clase      Pan y c… Alim… Aliment… Pan … NA       NA       2010-12-01
#>  4 01111   SubClase   Pan      Alim… Aliment… Pan … Pan      NA       2010-12-01
#>  5 0111101 Articulo   Pan sob… Alim… Aliment… Pan … Pan      Pan sob… 2010-12-01
#>  6 0111102 Articulo   Pan de … Alim… Aliment… Pan … Pan      Pan de … 2010-12-01
#>  7 0111104 Articulo   Pan Int… Alim… Aliment… Pan … Pan      Pan Int… 2010-12-01
#>  8 01112   SubClase   Galleta… Alim… Aliment… Pan … Galleta… NA       2010-12-01
#>  9 0111201 Articulo   Galleta… Alim… Aliment… Pan … Galleta… Galleta… 2010-12-01
#> 10 0111202 Articulo   Bizcoch… Alim… Aliment… Pan … Galleta… Bizcoch… 2010-12-01
#> # ℹ 114,434 more rows
#> # ℹ 4 more variables: year <dbl>, mes <dbl>, ponderacion <dbl>, indice <dbl>
```
