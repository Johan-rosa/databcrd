# Download legal reserve serie

Download legal reserve serie

## Usage

``` r
get_encaje()
```

## Value

a tibble

## Examples

``` r
get_encaje()
#> # A tibble: 289 × 17
#>    fecha       year   mes mn_oblicaciones mn_requerido_absoluto
#>    <date>     <dbl> <dbl>           <dbl>                 <dbl>
#>  1 2001-12-01  2001    12          97427.                19485.
#>  2 2002-01-01  2002     1          99409.                19882.
#>  3 2002-02-01  2002     2          98948.                19790.
#>  4 2002-03-01  2002     3          99798.                19960.
#>  5 2002-04-01  2002     4         101935.                20387.
#>  6 2002-05-01  2002     5         102181.                20436.
#>  7 2002-06-01  2002     6         103356.                20671.
#>  8 2002-07-01  2002     7         103100.                20620.
#>  9 2002-08-01  2002     8         102291.                20458.
#> 10 2002-09-01  2002     9         100814.                20163.
#> # ℹ 279 more rows
#> # ℹ 12 more variables: mn_requerido_tasa <dbl>, mn_efectivo_absoluto <dbl>,
#> #   mn_efectivo_tasa <dbl>, mn_excedente_absoluto <dbl>,
#> #   mn_excedente_tasa <dbl>, me_oblicaciones <dbl>,
#> #   me_requerido_absoluto <dbl>, me_requerido_tasa <dbl>,
#> #   me_efectivo_absoluto <dbl>, me_efectivo_tasa <dbl>,
#> #   me_excedente_absoluto <dbl>, me_excedente_tasa <dbl>
```
