# Monetary Operations

Downloads and returns data on the Central Bank of the Dominican
Republic's monetary operations. The dataset reports the amounts
transacted through the Bank's liquidity absorption and liquidity
injection facilities.

## Usage

``` r
operaciones_monetarias()
```

## Value

A tibble with the following variables:

- date:

  Reference period.

- ventanilla_depositos:

  Short-term remunerated deposit facility (Spanish: *Ventanilla Directa
  de Depósitos Remunerados de Corto Plazo*).

- subasta_letras:

  One-day Central Bank bill auctions (Spanish: *Subasta de Letras a un
  día*).

- operaciones_contraccion:

  Subtotal of liquidity absorption operations (Spanish: *Subtotal de
  Operaciones de Contracción*).

- ventanilla_repos:

  Direct repo facility (Spanish: *Ventanilla Directa de Repos*).

- subasta_repos:

  One-day repo auctions (Spanish: *Subasta de Repos a un día*).

- operaciones_expansion:

  Subtotal of liquidity injection operations (Spanish: *Subtotal de
  Operaciones de Expansión*).

## Details

Data are downloaded directly from the Central Bank of the Dominican
Republic.

## Examples

``` r
operaciones_monetarias()
#> # A tibble: 2,913 × 10
#>    date    year   mes   day ventanilla_depositos subasta_letras
#>    <date> <dbl> <dbl> <int>                <dbl>          <dbl>
#>  1 NA        NA    NA    NA               15517.         14928.
#>  2 NA        NA    NA    NA               16842.         15528.
#>  3 NA        NA    NA    NA               16452.         16275.
#>  4 NA        NA    NA    NA               17063.         16432.
#>  5 NA        NA    NA    NA               20627          20719.
#>  6 NA        NA    NA    NA               22100.         20968.
#>  7 NA        NA    NA    NA               21386.         22378.
#>  8 NA        NA    NA    NA               23357.         21850 
#>  9 NA        NA    NA    NA               21189.         23058.
#> 10 NA        NA    NA    NA               29277.         13909.
#> # ℹ 2,903 more rows
#> # ℹ 4 more variables: operaciones_contraccion <dbl>, ventanilla_repos <dbl>,
#> #   subasta_repos <dbl>, operaciones_expansion <dbl>
```
