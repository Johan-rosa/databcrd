# Macroeconomic Expectations

Get the data series for the monthly macroeconomic expectations of the
Dominican Republic.

## Usage

``` r
get_expectativas(modalidad = "eem")
```

## Arguments

- modalidad:

  string with the desired disaggregation. options: "eem", "eoe", "ecc",

## Value

a tibble

## Examples

``` r
get_expectativas("eem")
#> # A tibble: 9,045 × 9
#>    fecha       year   mes medida  short_names    variable_key variable horizonte
#>    <date>     <dbl> <dbl> <chr>   <chr>          <chr>        <fct>    <fct>    
#>  1 2009-06-01  2009     6 Mediana inf_anio_actu… inf          Inflaci… Año actu…
#>  2 2009-06-01  2009     6 Mediana inf_12m        inf          Inflaci… 12 meses 
#>  3 2009-06-01  2009     6 Mediana inf_anio_sigu… inf          Inflaci… Año sigu…
#>  4 2009-06-01  2009     6 Mediana inf_24m        inf          Inflaci… 24 meses 
#>  5 2009-06-01  2009     6 Mediana tc_anio_actual tc           Variaci… Año actu…
#>  6 2009-06-01  2009     6 Mediana tc_12m         tc           Variaci… 12 meses 
#>  7 2009-06-01  2009     6 Mediana tc_anio_sigui… tc           Variaci… Año sigu…
#>  8 2009-06-01  2009     6 Mediana tc_24m         tc           Variaci… 24 meses 
#>  9 2009-06-01  2009     6 Mediana pib_trim_actu… pib          Crecimi… Trimestr…
#> 10 2009-06-01  2009     6 Mediana pib_anio_actu… pib          Crecimi… Año actu…
#> # ℹ 9,035 more rows
#> # ℹ 1 more variable: expectativa <dbl>
get_expectativas("eoe")
#> # A tibble: 2,585 × 5
#>     year mes   descripcion                      valor fecha     
#>    <dbl> <chr> <chr>                            <dbl> <date>    
#>  1  2006 Julio situacion_economica              36.5  2006-07-01
#>  2  2006 Julio nivel_de_inventario               3.85 2006-07-01
#>  3  2006 Julio produccion                        5.77 2006-07-01
#>  4  2006 Julio pedidos                         -13.5  2006-07-01
#>  5  2006 Julio empleo                           NA    2006-07-01
#>  6  2006 Julio expectativa_produccion           44.2  2006-07-01
#>  7  2006 Julio expectativa_precios               9.62 2006-07-01
#>  8  2006 Julio expectativa_situacion_economica  44.2  2006-07-01
#>  9  2006 Julio expectativa_empleo               NA    2006-07-01
#> 10  2006 Julio indice_de_confianza_industrial   54.5  2006-07-01
#> # ℹ 2,575 more rows
```
