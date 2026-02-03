# Download the loans by sector series

Download loans by sectors

## Usage

``` r
get_prestamos_osd(osd = "consolidado")
```

## Arguments

- osd:

  string with the desired societies options: "consolidado",
  "bancos_multiples", "resto_osd".

## Value

a tibble

## Examples

``` r
get_prestamos_osd("consolidado")
#> # A tibble: 5,776 × 5
#>    sectores                          fecha         mn     me consolidado
#>    <chr>                             <date>     <dbl>  <dbl>       <dbl>
#>  1 AGRICULTURA, SILVICULTURA Y PESCA 1996-01-01 2920.  0           2920.
#>  2 AGRICULTURA, SILVICULTURA Y PESCA 1996-02-01 2847. 50.9         2898.
#>  3 AGRICULTURA, SILVICULTURA Y PESCA 1996-03-01 2913.  8.59        2921.
#>  4 AGRICULTURA, SILVICULTURA Y PESCA 1996-04-01 3007.  0.672       3007.
#>  5 AGRICULTURA, SILVICULTURA Y PESCA 1996-05-01 3059.  0.994       3060.
#>  6 AGRICULTURA, SILVICULTURA Y PESCA 1996-06-01 3155.  1.34        3156.
#>  7 AGRICULTURA, SILVICULTURA Y PESCA 1996-07-01 3112.  1.34        3113.
#>  8 AGRICULTURA, SILVICULTURA Y PESCA 1996-08-01 2984.  1.40        2986.
#>  9 AGRICULTURA, SILVICULTURA Y PESCA 1996-09-01 2947.  1.21        2949.
#> 10 AGRICULTURA, SILVICULTURA Y PESCA 1996-10-01 2848.  6.13        2854.
#> # ℹ 5,766 more rows
get_prestamos_osd("bancos_multiples")
#> # A tibble: 5,776 × 5
#>    sectores                          fecha         mn     me consolidado
#>    <chr>                             <date>     <dbl>  <dbl>       <dbl>
#>  1 AGRICULTURA, SILVICULTURA Y PESCA 1996-01-01 2308.  0           2308.
#>  2 AGRICULTURA, SILVICULTURA Y PESCA 1996-02-01 2205. 50.9         2256.
#>  3 AGRICULTURA, SILVICULTURA Y PESCA 1996-03-01 2350.  8.59        2358.
#>  4 AGRICULTURA, SILVICULTURA Y PESCA 1996-04-01 2347.  0.672       2348.
#>  5 AGRICULTURA, SILVICULTURA Y PESCA 1996-05-01 2404.  0.994       2405.
#>  6 AGRICULTURA, SILVICULTURA Y PESCA 1996-06-01 2456.  1.34        2458.
#>  7 AGRICULTURA, SILVICULTURA Y PESCA 1996-07-01 2412.  1.34        2414.
#>  8 AGRICULTURA, SILVICULTURA Y PESCA 1996-08-01 2289.  1.40        2290.
#>  9 AGRICULTURA, SILVICULTURA Y PESCA 1996-09-01 2222.  1.21        2223.
#> 10 AGRICULTURA, SILVICULTURA Y PESCA 1996-10-01 2114.  6.13        2121.
#> # ℹ 5,766 more rows
get_prestamos_osd("resto_osd")
#> # A tibble: 5,776 × 5
#>    sectores                          fecha         mn    me consolidado
#>    <chr>                             <date>     <dbl> <dbl>       <dbl>
#>  1 AGRICULTURA, SILVICULTURA Y PESCA 1996-01-01  612.     0        612.
#>  2 AGRICULTURA, SILVICULTURA Y PESCA 1996-02-01  642.     0        642.
#>  3 AGRICULTURA, SILVICULTURA Y PESCA 1996-03-01  563.     0        563.
#>  4 AGRICULTURA, SILVICULTURA Y PESCA 1996-04-01  660.     0        660.
#>  5 AGRICULTURA, SILVICULTURA Y PESCA 1996-05-01  656.     0        656.
#>  6 AGRICULTURA, SILVICULTURA Y PESCA 1996-06-01  698.     0        698.
#>  7 AGRICULTURA, SILVICULTURA Y PESCA 1996-07-01  700.     0        700.
#>  8 AGRICULTURA, SILVICULTURA Y PESCA 1996-08-01  695.     0        695.
#>  9 AGRICULTURA, SILVICULTURA Y PESCA 1996-09-01  726.     0        726.
#> 10 AGRICULTURA, SILVICULTURA Y PESCA 1996-10-01  733.     0        733.
#> # ℹ 5,766 more rows
```
