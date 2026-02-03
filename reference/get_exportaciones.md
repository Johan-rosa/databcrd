# Total exports by sectors

This function returns total exports by sectors in the Dominican Republic
based on the specified frequency.

## Usage

``` r
get_exportaciones(frecuencia = "mensual")
```

## Arguments

- frecuencia:

  A character string that specifies the frequency of the data to be
  downloaded. Valid options are "mensual", "trimestral", or "anual".

## Value

A data frame

## Examples

``` r
get_exportaciones("mensual")
#> # A tibble: 12,096 × 8
#>    original_names labels   short_names categoria nivel direct_parent valor_expor
#>    <chr>          <chr>    <chr>       <chr>     <dbl> <chr>               <dbl>
#>  1 1. Minerales   Mineral… minerales   minerales     1 NA                    8.8
#>  2 1. Minerales   Mineral… minerales   minerales     1 NA                   11.7
#>  3 1. Minerales   Mineral… minerales   minerales     1 NA                    7.9
#>  4 1. Minerales   Mineral… minerales   minerales     1 NA                    9.9
#>  5 1. Minerales   Mineral… minerales   minerales     1 NA                    8.5
#>  6 1. Minerales   Mineral… minerales   minerales     1 NA                   10.8
#>  7 1. Minerales   Mineral… minerales   minerales     1 NA                    6.3
#>  8 1. Minerales   Mineral… minerales   minerales     1 NA                   15.3
#>  9 1. Minerales   Mineral… minerales   minerales     1 NA                    7.8
#> 10 1. Minerales   Mineral… minerales   minerales     1 NA                    5  
#> # ℹ 12,086 more rows
#> # ℹ 1 more variable: fecha <date>
get_exportaciones("trimestral")
#> # A tibble: 4,032 × 8
#> # Groups:   trimestre, original_names, labels, short_names, categoria, nivel
#> #   [4,032]
#>    trimestre original_names    labels  short_names categoria nivel direct_parent
#>        <dbl> <chr>             <chr>   <chr>       <chr>     <dbl> <chr>        
#>  1     2010. 1. Minerales      Minera… minerales   minerales     1 NA           
#>  2     2010. 2. Agropecuarios  Agrope… agropecuar… agropecu…     1 NA           
#>  3     2010. 2.1 Nacionales    Agrope… agropecuar… agropecu…     2 Agropecuarios
#>  4     2010. 2.2 Zonas Francas Agrope… agropecuar… agropecu…     2 Agropecuarios
#>  5     2010. 3. Industriales   Indust… industrial… industri…     1 NA           
#>  6     2010. 3.1 Nacionales    Indust… industrial… industri…     2 Industriales 
#>  7     2010. 3.2 Zonas Francas Indust… industrial… industri…     2 industriales 
#>  8     2010. Aceite de soya    Aceite… aceite_de_… industri…     3 Industriales…
#>  9     2010. Aguacates         Aguaca… aguacates   agropecu…     3 Agropecuario…
#> 10     2010. Ajíes y pimientos Ajíes … ajíes_y_pi… agropecu…     3 Agropecuario…
#> # ℹ 4,022 more rows
#> # ℹ 1 more variable: valor_expor <dbl>
get_exportaciones("anual")
#> # A tibble: 1,024 × 8
#> # Groups:   year, original_names, labels, short_names, categoria, nivel [1,024]
#>    year  original_names    labels      short_names categoria nivel direct_parent
#>    <chr> <chr>             <chr>       <chr>       <chr>     <dbl> <chr>        
#>  1 2010  1. Minerales      Minerales   minerales   minerales     1 NA           
#>  2 2010  2. Agropecuarios  Agropecuar… agropecuar… agropecu…     1 NA           
#>  3 2010  2.1 Nacionales    Agropecuar… agropecuar… agropecu…     2 Agropecuarios
#>  4 2010  2.2 Zonas Francas Agropecuar… agropecuar… agropecu…     2 Agropecuarios
#>  5 2010  3. Industriales   Industrial… industrial… industri…     1 NA           
#>  6 2010  3.1 Nacionales    Industrial… industrial… industri…     2 Industriales 
#>  7 2010  3.2 Zonas Francas Industrial… industrial… industri…     2 industriales 
#>  8 2010  Aceite de soya    Aceite de … aceite_de_… industri…     3 Industriales…
#>  9 2010  Aguacates         Aguacates   aguacates   agropecu…     3 Agropecuario…
#> 10 2010  Ajíes y pimientos Ajíes y pi… ajíes_y_pi… agropecu…     3 Agropecuario…
#> # ℹ 1,014 more rows
#> # ℹ 1 more variable: valor_expor <dbl>
```
