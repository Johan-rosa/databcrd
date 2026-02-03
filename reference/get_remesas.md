# Remittances to the Dominican Republic

Remittances to the Dominican Republic

## Usage

``` r
get_remesas(modalidad = "mensual")
```

## Arguments

- modalidad:

  string indicating which perspective of remittances is asked. Options
  are: "mensual", "por_pais_emisor", "por_provincia_receptora",
  "cantidad_de_transacciones", "promedio_transacciones", "segun_moneda",
  "entidad_pagadora", "genero_receptor"

## Value

a data frame

## Examples

``` r
get_remesas("mensual")
#> # A tibble: 192 × 4
#>    fecha        mes  year      monto
#>    <date>     <dbl> <dbl>      <dbl>
#>  1 2010-01-01     1  2010 280155041.
#>  2 2010-02-01     2  2010 304097868.
#>  3 2010-03-01     3  2010 339939225.
#>  4 2010-04-01     4  2010 290804738.
#>  5 2010-05-01     5  2010 303781236.
#>  6 2010-06-01     6  2010 280461644.
#>  7 2010-07-01     7  2010 337133293.
#>  8 2010-08-01     8  2010 314046413.
#>  9 2010-09-01     9  2010 294098116.
#> 10 2010-10-01    10  2010 306398494.
#> # ℹ 182 more rows
get_remesas("por_pais_emisor")
#> New names:
#> • `2023` -> `2023...15`
#> • `2023` -> `2023...16`
#> # A tibble: 165 × 3
#>    partida         year proporcion
#>    <chr>          <dbl>      <dbl>
#>  1 Estados Unidos  2010      0.664
#>  2 Estados Unidos  2011      0.640
#>  3 Estados Unidos  2012      0.654
#>  4 Estados Unidos  2013      0.603
#>  5 Estados Unidos  2014      0.641
#>  6 Estados Unidos  2015      0.711
#>  7 Estados Unidos  2016      0.704
#>  8 Estados Unidos  2017      0.739
#>  9 Estados Unidos  2018      0.774
#> 10 Estados Unidos  2019      0.766
#> # ℹ 155 more rows
get_remesas("por_provincia_receptora")
#> # A tibble: 225 × 3
#>    partida            year proporcion
#>    <chr>             <dbl>      <dbl>
#>  1 Distrito Nacional  2010     0.0903
#>  2 Distrito Nacional  2011     0.260 
#>  3 Distrito Nacional  2012     0.270 
#>  4 Distrito Nacional  2013     0.279 
#>  5 Distrito Nacional  2014     0.284 
#>  6 Distrito Nacional  2015     0.298 
#>  7 Distrito Nacional  2016     0.306 
#>  8 Distrito Nacional  2017     0.317 
#>  9 Distrito Nacional  2018     0.318 
#> 10 Distrito Nacional  2019     0.316 
#> # ℹ 215 more rows
get_remesas("cantidad_de_transacciones")
#> # A tibble: 165 × 3
#>    partida         year cantidad
#>    <chr>          <dbl>    <dbl>
#>  1 Estados Unidos  2010  9790684
#>  2 Estados Unidos  2011  9886064
#>  3 Estados Unidos  2012 10477067
#>  4 Estados Unidos  2013 10619710
#>  5 Estados Unidos  2014 12233723
#>  6 Estados Unidos  2015 14502245
#>  7 Estados Unidos  2016 15142141
#>  8 Estados Unidos  2017 18203876
#>  9 Estados Unidos  2018 20468645
#> 10 Estados Unidos  2019 21899462
#> # ℹ 155 more rows
get_remesas("promedio_transacciones")
#> # A tibble: 150 × 3
#>    partida         year monto
#>    <chr>          <dbl> <dbl>
#>  1 Estados Unidos  2010  200.
#>  2 Estados Unidos  2011  204.
#>  3 Estados Unidos  2012  196.
#>  4 Estados Unidos  2013  192.
#>  5 Estados Unidos  2014  194.
#>  6 Estados Unidos  2015  200.
#>  7 Estados Unidos  2016  204.
#>  8 Estados Unidos  2017  206.
#>  9 Estados Unidos  2018  211.
#> 10 Estados Unidos  2019  209.
#> # ℹ 140 more rows
get_remesas("segun_moneda")
#> # A tibble: 30 × 3
#>    partida               year proporcion
#>    <chr>                <dbl>      <dbl>
#>  1 Dólar Estadounidense  2010      0.347
#>  2 Dólar Estadounidense  2011      0.569
#>  3 Dólar Estadounidense  2012      0.616
#>  4 Dólar Estadounidense  2013      0.621
#>  5 Dólar Estadounidense  2014      0.616
#>  6 Dólar Estadounidense  2015      0.639
#>  7 Dólar Estadounidense  2016      0.638
#>  8 Dólar Estadounidense  2017      0.643
#>  9 Dólar Estadounidense  2018      0.660
#> 10 Dólar Estadounidense  2019      0.670
#> # ℹ 20 more rows
get_remesas("entidad_pagadora")
#> # A tibble: 30 × 3
#>    partida               year proporcion
#>    <chr>                <dbl>      <dbl>
#>  1 Empresas Remesadoras  2010      0.961
#>  2 Empresas Remesadoras  2011      0.890
#>  3 Empresas Remesadoras  2012      0.879
#>  4 Empresas Remesadoras  2013      0.733
#>  5 Empresas Remesadoras  2014      0.737
#>  6 Empresas Remesadoras  2015      0.742
#>  7 Empresas Remesadoras  2016      0.743
#>  8 Empresas Remesadoras  2017      0.751
#>  9 Empresas Remesadoras  2018      0.763
#> 10 Empresas Remesadoras  2019      0.790
#> # ℹ 20 more rows
get_remesas("genero_receptor")
#> New names:
#> • `2023` -> `2023...10`
#> • `2023` -> `2023...11`
#> # A tibble: 30 × 3
#>    partida    year proporcion
#>    <chr>     <dbl>      <dbl>
#>  1 Masculino  2015      0.395
#>  2 Masculino  2016      0.420
#>  3 Masculino  2017      0.442
#>  4 Masculino  2018      0.453
#>  5 Masculino  2019      0.509
#>  6 Masculino  2020      0.518
#>  7 Masculino  2021      0.532
#>  8 Masculino  2022      0.532
#>  9 Masculino  2023      0.526
#> 10 Masculino  2023      0.528
#> # ℹ 20 more rows
```
