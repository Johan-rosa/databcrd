# Download the CPI series

Download the CPI inflacion series for the Dominican Republic in any
disaggregation

## Usage

``` r
get_ipc_data(desagregacion)
```

## Arguments

- desagregacion:

  string with the desired disaggregation. options: "general", "grupos",
  "regiones", "subyacente", "tnt" (transable y no transable),
  "articulos"

## Value

a tibble

## Examples

``` r
get_ipc_data("general")
#> # A tibble: 504 × 8
#>    fecha      year    mes   ipc ipc_vm ipc_vd ipc_vi ipc_p12
#>    <date>     <chr> <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1 1984-01-01 1984      1  1.38  1.74    1.74   7.05    5.57
#>  2 1984-02-01 1984      2  1.42  2.81    4.60  11.2     6.01
#>  3 1984-03-01 1984      3  1.44  1.23    5.89  11.9     6.52
#>  4 1984-04-01 1984      4  1.46  1.57    7.55  14.9     7.36
#>  5 1984-05-01 1984      5  1.48  1.20    8.84  15.2     8.26
#>  6 1984-06-01 1984      6  1.54  4.31   13.5   19.6     9.49
#>  7 1984-07-01 1984      7  1.56  1.29   15.0   20.7    10.8 
#>  8 1984-08-01 1984      8  1.57  0.455  15.5   20.1    12.0 
#>  9 1984-09-01 1984      9  1.64  4.69   20.9   24.8    13.7 
#> 10 1984-10-01 1984     10  1.68  2.34   23.8   26.3    15.4 
#> # ℹ 494 more rows
get_ipc_data("grupos")
#> # A tibble: 324 × 26
#>    fecha      year    mes ipc_ayb ipc_ayb_vm ipc_alcohol_tabaco
#>    <date>     <chr> <dbl>   <dbl>      <dbl>              <dbl>
#>  1 1999-01-01 1999      1    21.0     NA                   11.5
#>  2 1999-02-01 1999      2    20.6     -1.63                11.5
#>  3 1999-03-01 1999      3    20.9      1.11                11.8
#>  4 1999-04-01 1999      4    20.9      0.286               12.0
#>  5 1999-05-01 1999      5    20.8     -0.694               12.1
#>  6 1999-06-01 1999      6    20.4     -2.11                12.1
#>  7 1999-07-01 1999      7    20.1     -1.26                12.1
#>  8 1999-08-01 1999      8    20.0     -0.380               12.2
#>  9 1999-09-01 1999      9    19.9     -0.569               12.2
#> 10 1999-10-01 1999     10    20.5      2.83                12.4
#> # ℹ 314 more rows
#> # ℹ 20 more variables: ipc_alcohol_tabaco_vm <dbl>, ipc_ropa_calzado <dbl>,
#> #   ipc_ropa_calzado_vm <dbl>, ipc_vivienda <chr>, ipc_vivienda_vm <dbl>,
#> #   ipc_muebles <dbl>, ipc_muebles_vm <dbl>, ipc_salud <dbl>,
#> #   ipc_salud_vm <dbl>, ipc_transporte <dbl>, ipc_transporte_vm <dbl>,
#> #   ipc_comunicaciones <dbl>, ipc_comunicaciones_vm <dbl>, ipc_cultura <dbl>,
#> #   ipc_cultura_vm <dbl>, ipc_educacion <dbl>, ipc_educacion_vm <dbl>, …
get_ipc_data("subyacente")
#> # A tibble: 312 × 7
#>    fecha       year   mes ipc_subyacente ipc_subyacente_vm ipc_subyacente_vd
#>    <date>     <dbl> <dbl>          <dbl>             <dbl>             <dbl>
#>  1 2000-01-01  2000     1           25.8             0.316             0.316
#>  2 2000-02-01  2000     2           25.9             0.357             0.673
#>  3 2000-03-01  2000     3           26.0             0.327             1.00 
#>  4 2000-04-01  2000     4           26.1             0.320             1.32 
#>  5 2000-05-01  2000     5           26.2             0.207             1.53 
#>  6 2000-06-01  2000     6           26.7             1.92              3.48 
#>  7 2000-07-01  2000     7           26.8             0.354             3.85 
#>  8 2000-08-01  2000     8           26.9             0.361             4.22 
#>  9 2000-09-01  2000     9           27.2             1.22              5.50 
#> 10 2000-10-01  2000    10           27.3             0.289             5.80 
#> # ℹ 302 more rows
#> # ℹ 1 more variable: ipc_subyacente_vi <dbl>
get_ipc_data("regiones")
#> # A tibble: 180 × 11
#>    fecha      year    mes ipc_ozama ipc_ozama_vm ipc_cibao ipc_cibao_vm ipc_este
#>    <date>     <chr> <dbl>     <dbl>        <dbl>     <dbl>        <dbl>    <dbl>
#>  1 2011-01-01 2011      1      76.5       1.01        73.7       1.34       73.5
#>  2 2011-02-01 2011      2      77.3       1.08        74.5       1.21       74.5
#>  3 2011-03-01 2011      3      78.1       1.06        75.5       1.24       75.6
#>  4 2011-04-01 2011      4      78.8       0.847       76.2       0.975      76.2
#>  5 2011-05-01 2011      5      79.0       0.241       76.3       0.168      76.4
#>  6 2011-06-01 2011      6      79.7       0.962       77.1       1.05       77.2
#>  7 2011-07-01 2011      7      80.3       0.699       77.9       0.983      77.9
#>  8 2011-08-01 2011      8      80.7       0.544       78.2       0.423      78.2
#>  9 2011-09-01 2011      9      80.9       0.161       78.4       0.198      78.4
#> 10 2011-10-01 2011     10      80.9       0.0485      78.4       0.0351     78.4
#> # ℹ 170 more rows
#> # ℹ 3 more variables: ipc_este_vm <dbl>, ipc_sur <dbl>, ipc_sur_vm <dbl>
get_ipc_data("tnt")
#> # A tibble: 323 × 12
#>    fecha      year    mes   ipc  ipc_vm ipc_vd ipc_t    ipc_t_vm ipc_t_vd ipc_nt
#>    <date>     <chr> <dbl> <dbl>   <dbl>  <dbl> <chr>    <chr>    <chr>    <chr> 
#>  1 1999-02-01 1999      2  20.9 NA          NA 21.6612… NA       NA       20.11…
#>  2 1999-03-01 1999      3  21.0  0.521      NA 21.7395… 0.36144… NA       20.25…
#>  3 1999-04-01 1999      4  21.0  0.259      NA 21.7960… 0.26010… NA       20.30…
#>  4 1999-05-01 1999      5  21.0 -0.0497     NA 21.7612… -0.1596… NA       20.31…
#>  5 1999-06-01 1999      6  20.9 -0.547      NA 21.6155… -0.6695… NA       20.22…
#>  6 1999-07-01 1999      7  20.8 -0.250      NA 21.5133… -0.4728… NA       20.22…
#>  7 1999-08-01 1999      8  20.9  0.281      NA 21.3741… -0.6469… NA       20.45…
#>  8 1999-09-01 1999      9  21.0  0.400      NA 21.2914… -0.3866… NA       20.69…
#>  9 1999-10-01 1999     10  21.7  3.51       NA 22.0287… 3.46271… NA       21.42…
#> 10 1999-11-01 1999     11  22.0  1.43       NA 22.4571… 1.94491… NA       21.63…
#> # ℹ 313 more rows
#> # ℹ 2 more variables: ipc_nt_vm <chr>, ipc_nt_vd <chr>
```
