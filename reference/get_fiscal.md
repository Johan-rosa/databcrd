# Download Statement of Operations of the non-financial public sector

Download Statement of Operations of the non-financial public sector

## Usage

``` r
get_fiscal(frecuencia = "Mensual")
```

## Arguments

- frecuencia:

  A character string that specifies the frequency of the exchange rates
  to be downloaded. Valid options are "Mensual", or "Anual".

## Value

a tibble

## Examples

``` r
get_fiscal()
#> # A tibble: 31,722 × 8
#>    original_names labels  short_names categoria   nivel direct_parent fecha     
#>    <chr>          <chr>   <chr>       <chr>       <dbl> <chr>         <date>    
#>  1 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-01-01
#>  2 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-02-01
#>  3 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-03-01
#>  4 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-04-01
#>  5 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-05-01
#>  6 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-06-01
#>  7 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-07-01
#>  8 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-08-01
#>  9 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-09-01
#> 10 Ingreso        ingreso GC_1        gobierno_c…     1 NA            2000-10-01
#> # ℹ 31,712 more rows
#> # ℹ 1 more variable: valor <dbl>
```
