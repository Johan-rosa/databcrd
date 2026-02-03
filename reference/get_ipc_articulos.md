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
#> # A tibble: 110,772 × 13
#>       id nombre     agregacion grupo subgrupo clase subclase articulo date      
#>    <dbl> <chr>      <chr>      <chr> <chr>    <chr> <chr>    <chr>    <date>    
#>  1     1 Indice Ge… General    NA    NA       NA    NA       NA       2010-12-01
#>  2     2 Alimentos… Grupo      Alim… NA       NA    NA       NA       2010-12-01
#>  3     3 Alimentos  Subgrupo   Alim… Aliment… NA    NA       NA       2010-12-01
#>  4     4 Pan y cer… Clase      Alim… Aliment… Pan … NA       NA       2010-12-01
#>  5     5 Pan        SubClase   Alim… Aliment… Pan … Pan      NA       2010-12-01
#>  6     6 Pan sobado Articulo   Alim… Aliment… Pan … Pan      Pan sob… 2010-12-01
#>  7     7 Pan de ag… Articulo   Alim… Aliment… Pan … Pan      Pan de … 2010-12-01
#>  8     8 Pan Integ… Articulo   Alim… Aliment… Pan … Pan      Pan Int… 2010-12-01
#>  9     9 Galletas … SubClase   Alim… Aliment… Pan … Galleta… NA       2010-12-01
#> 10    10 Galletas … Articulo   Alim… Aliment… Pan … Galleta… Galleta… 2010-12-01
#> # ℹ 110,762 more rows
#> # ℹ 4 more variables: year <dbl>, mes <dbl>, ponderacion <dbl>, indice <dbl>
```
