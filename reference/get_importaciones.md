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
#> # A tibble: 10,530 × 10
#>    year  id    og_label     label categoria nivel regimen fecha        mes valor
#>    <chr> <chr> <chr>        <chr> <chr>     <dbl> <chr>   <date>     <dbl> <dbl>
#>  1 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-01-01     1  457 
#>  2 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-02-01     2  430.
#>  3 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-03-01     3  577.
#>  4 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-04-01     4  638.
#>  5 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-05-01     5  509.
#>  6 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-06-01     6  519.
#>  7 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-07-01     7  542.
#>  8 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-08-01     8  543.
#>  9 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-09-01     9  547 
#> 10 2010  1     1.  Bienes … Bien… Bienes d…     2 NA      2010-10-01    10  583.
#> # ℹ 10,520 more rows
get_importaciones("trimestral")
#> # A tibble: 3,510 × 7
#>    year  trimestre label                          categoria nivel regimen  valor
#>    <chr>     <int> <chr>                          <chr>     <dbl> <chr>    <dbl>
#>  1 2010          1 Aceites vegetales alimenticios Materias…     4 Nacion…   14.1
#>  2 2010          1 Arroz para consumo             Bienes d…     4 NA         6.5
#>  3 2010          1 Azúcar cruda                   Materias…     4 Nacion…    7.4
#>  4 2010          1 Azúcar refinada                Bienes d…     4 NA         0.4
#>  5 2010          1 Bienes de Capital              Bienes d…     2 NA       461  
#>  6 2010          1 Bienes de Capital Nacionales   Bienes d…     3 NA       419. 
#>  7 2010          1 Bienes de Consumo              Bienes d…     2 NA      1464  
#>  8 2010          1 Bienes de consumo duradero     Bienes d…     4 NA       164. 
#>  9 2010          1 Carbón mineral                 Materias…     4 Nacion…   22.1
#> 10 2010          1 Comercializadoras              Materias…     4 Zonas …    6.4
#> # ℹ 3,500 more rows
get_importaciones("anual")
#> # A tibble: 918 × 6
#>    year  label                          categoria         nivel regimen    valor
#>    <chr> <chr>                          <chr>             <dbl> <chr>      <dbl>
#>  1 2010  Aceites vegetales alimenticios Materias Primas       4 Nacional…  128. 
#>  2 2010  Arroz para consumo             Bienes de Consumo     4 NA          14.5
#>  3 2010  Azúcar cruda                   Materias Primas       4 Nacional…   22.7
#>  4 2010  Azúcar refinada                Bienes de Consumo     4 NA           6.4
#>  5 2010  Bienes de Capital              Bienes de Capital     2 NA        2233. 
#>  6 2010  Bienes de Capital Nacionales   Bienes de Capital     3 NA        2090. 
#>  7 2010  Bienes de Consumo              Bienes de Consumo     2 NA        6521. 
#>  8 2010  Bienes de consumo duradero     Bienes de Consumo     4 NA         876. 
#>  9 2010  Carbón mineral                 Materias Primas       4 Nacional…   86.4
#> 10 2010  Comercializadoras              Materias Primas       4 Zonas Fr…   28.8
#> # ℹ 908 more rows
```
