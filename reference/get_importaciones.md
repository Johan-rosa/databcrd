# Total imports by sectors

This function returns total imports by sectors in the Dominican Republic
based on the specified frequency.

## Usage

``` r
get_importaciones(frecuencia = "mensual")
```

## Arguments

- frecuencia:

  A character string that specifies the frequency of the data to be
  downloaded. Valid options are "mensual", "trimestral", or "anual".

## Value

A data frame

## Examples

``` r
get_importaciones("mensual")
#> # A tibble: 10,368 × 8
#>    original_names   labels short_names categoria nivel direct_parent valor_impor
#>    <chr>            <chr>  <chr>       <chr>     <dbl> <chr>               <dbl>
#>  1 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   457 
#>  2 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   430.
#>  3 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   577.
#>  4 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   638.
#>  5 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   509.
#>  6 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   519.
#>  7 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   542.
#>  8 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   543.
#>  9 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   547 
#> 10 1.  Bienes de C… Biene… bienes_de_… Consumo       1 NA                   583.
#> # ℹ 10,358 more rows
#> # ℹ 1 more variable: fecha <date>
get_importaciones("trimestral")
#> # A tibble: 3,456 × 8
#> # Groups:   trimestre, original_names, labels, short_names, categoria, nivel
#> #   [3,456]
#>    trimestre original_names     labels short_names categoria nivel direct_parent
#>        <dbl> <chr>              <chr>  <chr>       <chr>     <dbl> <chr>        
#>  1     2010. 1.  Bienes de Con… Biene… bienes_de_… Consumo       1 NA           
#>  2     2010. 2.  Materias Prim… Mater… materias_p… Materias…     1 NA           
#>  3     2010. 2.1 Nacionales     Nacio… materias_p… Materias…     2 Materias Pri…
#>  4     2010. 2.2 Zonas Francas  Zonas… materias_p… Materias…     2 Materias Pri…
#>  5     2010. 3.  Bienes de Cap… Biene… bienes_de_… Capital       1 NA           
#>  6     2010. 3.1 Nacionales     Nacio… capital_na… Capital       2 Bienes de Ca…
#>  7     2010. 3.2 Zonas Francas  Zonas… capital_zo… Capital       2 Bienes de Ca…
#>  8     2010. Aceites vegetales… Aceit… aceites_ve… Materias…     3 Nacionales   
#>  9     2010. Arroz para consumo Arroz… arroz_para… Consumo       3 Bienes de co…
#> 10     2010. Azúcar cruda (par… Azúca… azúcar_cru… Materias…     3 Nacionales   
#> # ℹ 3,446 more rows
#> # ℹ 1 more variable: valor_impor <dbl>
get_importaciones("anual")
#> # A tibble: 864 × 8
#> # Groups:   year, original_names, labels, short_names, categoria, nivel [864]
#>    year  original_names         labels short_names categoria nivel direct_parent
#>    <chr> <chr>                  <chr>  <chr>       <chr>     <dbl> <chr>        
#>  1 2010  1.  Bienes de Consumo  Biene… bienes_de_… Consumo       1 NA           
#>  2 2010  2.  Materias Primas    Mater… materias_p… Materias…     1 NA           
#>  3 2010  2.1 Nacionales         Nacio… materias_p… Materias…     2 Materias Pri…
#>  4 2010  2.2 Zonas Francas      Zonas… materias_p… Materias…     2 Materias Pri…
#>  5 2010  3.  Bienes de Capital  Biene… bienes_de_… Capital       1 NA           
#>  6 2010  3.1 Nacionales         Nacio… capital_na… Capital       2 Bienes de Ca…
#>  7 2010  3.2 Zonas Francas      Zonas… capital_zo… Capital       2 Bienes de Ca…
#>  8 2010  Aceites vegetales ali… Aceit… aceites_ve… Materias…     3 Nacionales   
#>  9 2010  Arroz para consumo     Arroz… arroz_para… Consumo       3 Bienes de co…
#> 10 2010  Azúcar cruda (parda)   Azúca… azúcar_cru… Materias…     3 Nacionales   
#> # ℹ 854 more rows
#> # ℹ 1 more variable: valor_impor <dbl>
```
