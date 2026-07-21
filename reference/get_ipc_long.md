# Download the CPI series in long format

Download the CPI inflation series for the Dominican Republic in any
disaggregation

## Usage

``` r
get_ipc_long(
  desagregacion = c("general", "grupo", "subgrupo", "clase", "subclase", "articulo")
)
```

## Arguments

- desagregacion:

  string with the desired disaggregation. options: "general", "grupo",
  "subgrupo", "clase", "subclase", "articulo"

## Value

a tibble

## Examples

``` r
get_ipc_long("general")
#> # A tibble: 187 × 7
#>    id    agregacion nombre         date        year   mes indice
#>    <chr> <chr>      <chr>          <date>     <dbl> <dbl>  <dbl>
#>  1 Z0001 General    Indice General 2010-12-01  2010    12   73.6
#>  2 Z0001 General    Indice General 2011-01-01  2011     1   74.5
#>  3 Z0001 General    Indice General 2011-02-01  2011     2   75.4
#>  4 Z0001 General    Indice General 2011-03-01  2011     3   76.3
#>  5 Z0001 General    Indice General 2011-04-01  2011     4   76.9
#>  6 Z0001 General    Indice General 2011-05-01  2011     5   77.1
#>  7 Z0001 General    Indice General 2011-06-01  2011     6   77.9
#>  8 Z0001 General    Indice General 2011-07-01  2011     7   78.6
#>  9 Z0001 General    Indice General 2011-08-01  2011     8   79.0
#> 10 Z0001 General    Indice General 2011-09-01  2011     9   79.1
#> # ℹ 177 more rows
get_ipc_long("grupo")
#> # A tibble: 2,244 × 9
#>    id    agregacion nombre       grupo date        year   mes ponderacion indice
#>    <chr> <chr>      <chr>        <chr> <date>     <dbl> <dbl>       <dbl>  <dbl>
#>  1 01    Grupo      Alimentos y… Alim… 2010-12-01  2010    12       23.8    65.2
#>  2 02    Grupo      Bebidas Alc… Bebi… 2010-12-01  2010    12        2.36   55.0
#>  3 03    Grupo      Prendas de … Pren… 2010-12-01  2010    12        4.19  123. 
#>  4 04    Grupo      Vivienda     Vivi… 2010-12-01  2010    12       13.0    81.9
#>  5 05    Grupo      Muebles y A… Mueb… 2010-12-01  2010    12        5.17   79.8
#>  6 06    Grupo      Salud        Salud 2010-12-01  2010    12        4.74   69.4
#>  7 07    Grupo      Transporte   Tran… 2010-12-01  2010    12       16.6    76.0
#>  8 08    Grupo      Comunicacio… Comu… 2010-12-01  2010    12        5.06   89.4
#>  9 09    Grupo      Recreación … Recr… 2010-12-01  2010    12        3.03   86.9
#> 10 10    Grupo      Educación    Educ… 2010-12-01  2010    12        3.06   58.2
#> # ℹ 2,234 more rows
```
