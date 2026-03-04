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
#> # A tibble: 182 × 7
#>       id nombre         agregacion date        year   mes indice
#>    <dbl> <chr>          <chr>      <date>     <dbl> <dbl>  <dbl>
#>  1     1 Indice General General    2010-12-01  2010    12   73.6
#>  2     1 Indice General General    2011-01-01  2011     1   74.5
#>  3     1 Indice General General    2011-02-01  2011     2   75.4
#>  4     1 Indice General General    2011-03-01  2011     3   76.3
#>  5     1 Indice General General    2011-04-01  2011     4   76.9
#>  6     1 Indice General General    2011-05-01  2011     5   77.1
#>  7     1 Indice General General    2011-06-01  2011     6   77.9
#>  8     1 Indice General General    2011-07-01  2011     7   78.6
#>  9     1 Indice General General    2011-08-01  2011     8   79.0
#> 10     1 Indice General General    2011-09-01  2011     9   79.1
#> # ℹ 172 more rows
get_ipc_long("grupo")
#> # A tibble: 2,184 × 9
#>       id nombre       agregacion grupo date        year   mes ponderacion indice
#>    <dbl> <chr>        <chr>      <chr> <date>     <dbl> <dbl>       <dbl>  <dbl>
#>  1     2 Alimentos y… Grupo      Alim… 2010-12-01  2010    12       23.8    65.2
#>  2   180 Bebidas Alc… Grupo      Bebi… 2010-12-01  2010    12        2.36   55.0
#>  3   199 Prendas de … Grupo      Pren… 2010-12-01  2010    12        4.19  123. 
#>  4   246 Vivienda     Grupo      Vivi… 2010-12-01  2010    12       13.0    81.9
#>  5   284 Muebles y A… Grupo      Mueb… 2010-12-01  2010    12        5.17   79.8
#>  6   343 Salud        Grupo      Salud 2010-12-01  2010    12        4.74   69.4
#>  7   392 Transporte   Grupo      Tran… 2010-12-01  2010    12       16.6    76.0
#>  8   439 Comunicacio… Grupo      Comu… 2010-12-01  2010    12        5.06   89.4
#>  9   452 Recreación … Grupo      Recr… 2010-12-01  2010    12        3.03   86.9
#> 10   501 Educación    Grupo      Educ… 2010-12-01  2010    12        3.06   58.2
#> # ℹ 2,174 more rows
```
