# Download EMBI

Download EMBI of different countries

## Usage

``` r
get_embi(periodicidad = "mensual")
```

## Arguments

- periodicidad:

  string with the desired periodicity options: "diario", "semanal",
  "mensual", "trimestral", "anual".

## Value

a tibble

## Examples

``` r
get_embi("diario")
#> # A tibble: 4,562 × 21
#>    Fecha      Global LATINO `REP DOM` Argentina Bolivia Brasil Chile Colombia
#>    <date>      <dbl>  <dbl>     <dbl>     <dbl>   <dbl>  <dbl> <dbl>    <dbl>
#>  1 2007-10-29   2.10   2.33      2.28      3.50     NaN   1.75  1.12     1.60
#>  2 2007-10-30   2.09   2.31      2.28      3.41     NaN   1.73  1.13     1.57
#>  3 2007-10-31   2.01   2.20      2.13      3.12     NaN   1.66  1.12     1.48
#>  4 2007-11-01   2.12   2.32      2.23      3.29     NaN   1.78  1.10     1.61
#>  5 2007-11-02   2.20   2.40      2.31      3.41     NaN   1.85  1.15     1.68
#>  6 2007-11-05   2.22   2.42      2.28      3.55     NaN   1.85  1.15     1.66
#>  7 2007-11-06   2.18   2.39      2.25      3.45     NaN   1.83  1.15     1.65
#>  8 2007-11-07   2.25   2.48      2.29      3.60     NaN   1.90  1.16     1.76
#>  9 2007-11-08   2.33   2.58      2.40      3.85     NaN   1.99  1.24     1.85
#> 10 2007-11-09   2.36   2.61      2.44      3.87     NaN   2.02  1.23     1.88
#> # ℹ 4,552 more rows
#> # ℹ 12 more variables: `Costa Rica` <dbl>, Ecuador <dbl>, `El Salvador` <dbl>,
#> #   Guatemala <dbl>, Honduras <dbl>, México <dbl>, Paraguay <dbl>, Perú <dbl>,
#> #   Panamá <dbl>, Uruguay <dbl>, Venezuela <dbl>, `RD-LATINO` <dbl>
get_embi("semanal")
#> # A tibble: 953 × 21
#>    Fecha      Global LATINO `REP DOM` Argentina Bolivia Brasil Chile Colombia
#>    <date>      <dbl>  <dbl>     <dbl>     <dbl>   <dbl>  <dbl> <dbl>    <dbl>
#>  1 2007-10-28   2.11   2.31      2.25      3.34     NaN   1.75  1.12     1.59
#>  2 2007-11-04   2.27   2.50      2.33      3.66     NaN   1.92  1.19     1.76
#>  3 2007-11-11   2.39   2.65      2.50      3.90     NaN   2.02  1.32     1.91
#>  4 2007-11-18   2.65   2.92      2.78      4.31     NaN   2.26  1.46     2.14
#>  5 2007-11-25   2.73   3.01      2.82      4.31     NaN   2.36  1.49     2.19
#>  6 2007-12-02   2.62   2.88      2.77      3.96     NaN   2.23  1.53     2.01
#>  7 2007-12-09   2.46   2.71      2.66      3.77     NaN   2.08  1.55     1.80
#>  8 2007-12-16   2.51   2.80      2.71      4.06     NaN   2.18  1.53     1.88
#>  9 2007-12-23   2.41   2.69      2.62      3.94     NaN   2.06  1.49     1.80
#> 10 2007-12-30   2.64   2.91      2.91      4.23     NaN   2.26  1.52     2.08
#> # ℹ 943 more rows
#> # ℹ 12 more variables: `Costa Rica` <dbl>, Ecuador <dbl>, `El Salvador` <dbl>,
#> #   Guatemala <dbl>, Honduras <dbl>, México <dbl>, Paraguay <dbl>, Perú <dbl>,
#> #   Panamá <dbl>, Uruguay <dbl>, Venezuela <dbl>, `RD-LATINO` <dbl>
get_embi("mensual")
#> # A tibble: 220 × 21
#>    Fecha      Global LATINO `REP DOM` Argentina Bolivia Brasil Chile Colombia
#>    <date>      <dbl>  <dbl>     <dbl>     <dbl>   <dbl>  <dbl> <dbl>    <dbl>
#>  1 2007-10-01   2.07   2.28      2.23      3.34     NaN   1.71  1.12     1.55
#>  2 2007-11-01   2.47   2.73      2.57      3.97     NaN   2.11  1.34     1.96
#>  3 2007-12-01   2.51   2.78      2.70      3.94     NaN   2.14  1.53     1.88
#>  4 2008-01-01   2.80   3.06      3.28      4.43     NaN   2.42  1.65     2.41
#>  5 2008-02-01   2.90   3.20      3.75      4.77     NaN   2.56  1.69     2.49
#>  6 2008-03-01   3.14   3.44      4.57      5.29     NaN   2.76  1.69     2.63
#>  7 2008-04-01   2.95   3.22      4.43      5.56     NaN   2.43  1.72     2.16
#>  8 2008-05-01   2.74   2.98      4.02      5.53     NaN   2.04  1.59     1.79
#>  9 2008-06-01   2.74   2.93      4.09      5.62     NaN   1.94  1.66     1.81
#> 10 2008-07-01   3.10   3.35      4.89      6.27     NaN   2.32  1.81     2.17
#> # ℹ 210 more rows
#> # ℹ 12 more variables: `Costa Rica` <dbl>, Ecuador <dbl>, `El Salvador` <dbl>,
#> #   Guatemala <dbl>, Honduras <dbl>, México <dbl>, Paraguay <dbl>, Perú <dbl>,
#> #   Panamá <dbl>, Uruguay <dbl>, Venezuela <dbl>, `RD-LATINO` <dbl>
get_embi("trimestral")
#> # A tibble: 74 × 21
#>    Fecha      Global LATINO `REP DOM` Argentina Bolivia Brasil Chile Colombia
#>    <date>      <dbl>  <dbl>     <dbl>     <dbl>   <dbl>  <dbl> <dbl>    <dbl>
#>  1 2007-10-01   2.46   2.72      2.61      3.91     NaN   2.10  1.41     1.90
#>  2 2008-01-01   2.94   3.23      3.86      4.82     NaN   2.58  1.68     2.51
#>  3 2008-04-01   2.81   3.05      4.18      5.57     NaN   2.14  1.66     1.92
#>  4 2008-07-01   3.38   3.67      5.12      6.97     NaN   2.53  1.81     2.37
#>  5 2008-10-01   7.11   7.60     15.3      17.0      NaN   4.82  3.36     5.40
#>  6 2009-01-01   6.81   7.33     12.4      17.0      NaN   4.30  3.56     4.88
#>  7 2009-04-01   5.03   5.72      8.93     14.6      NaN   3.24  2.40     3.46
#>  8 2009-07-01   3.83   4.18      7.52      9.22     NaN   2.54  1.47     2.70
#>  9 2009-10-01   3.19   3.63      4.66      7.18     NaN   2.16  1.26     2.14
#> 10 2010-01-01   2.97   3.42      4.18      7.33     NaN   2.03  1.23     2.10
#> # ℹ 64 more rows
#> # ℹ 12 more variables: `Costa Rica` <dbl>, Ecuador <dbl>, `El Salvador` <dbl>,
#> #   Guatemala <dbl>, Honduras <dbl>, México <dbl>, Paraguay <dbl>, Perú <dbl>,
#> #   Panamá <dbl>, Uruguay <dbl>, Venezuela <dbl>, `RD-LATINO` <dbl>
get_embi("anual")
#> # A tibble: 20 × 21
#>    Fecha      Global LATINO `REP DOM` Argentina Bolivia Brasil Chile Colombia
#>    <date>      <dbl>  <dbl>     <dbl>     <dbl>   <dbl>  <dbl> <dbl>    <dbl>
#>  1 2007-01-01   2.46   2.72      2.61      3.91  NaN      2.10 1.41      1.90
#>  2 2008-01-01   4.05   4.37      7.08      8.56  NaN      3.01 2.12      3.04
#>  3 2009-01-01   4.70   5.20      8.35     12.0   NaN      3.05 2.16      3.28
#>  4 2010-01-01   3.01   3.44      3.77      6.89  NaN      2.02 1.29      1.89
#>  5 2011-01-01   3.42   3.53      4.42      6.87  NaN      1.95 1.39      1.68
#>  6 2012-01-01   3.42   3.45      4.63      9.89    3.41   1.83 1.51      1.48
#>  7 2013-01-01   3.18   3.40      3.77     10.7     3.06   2.09 1.54      1.58
#>  8 2014-01-01   3.30   3.44      3.39      7.86    2.55   2.35 1.43      1.67
#>  9 2015-01-01   4.15   4.33      3.85      5.89    2.98   3.61 1.86      2.50
#> 10 2016-01-01   4.09   4.48      4.15      4.75    1.67   3.94 2.00      2.78
#> 11 2017-01-01   3.25   3.62      3.14      4.12    1.73   2.64 1.30      1.94
#> 12 2018-01-01   3.59   3.90      3.14      5.47    2.41   2.65 1.33      1.84
#> 13 2019-01-01   3.56   3.96      3.31     13.1     2.83   2.35 1.36      1.84
#> 14 2020-01-01   4.07   4.65      4.89     22.4     5.41   3.18 1.98      2.63
#> 15 2021-01-01   3.21   3.63      3.52     15.8     4.75   2.82 1.41      2.62
#> 16 2022-01-01   4.05   4.46      4.25     21.8     5.62   2.97 1.78      3.95
#> 17 2023-01-01   3.69   4.11      3.26     22.3    13.0    2.29 1.40      3.58
#> 18 2024-01-01   3.18   4.13      2.20     13.7    20.2    2.11 1.25      3.10
#> 19 2025-01-01   2.81   3.88      2.02      7.52   16.0    2.10 1.13      3.11
#> 20 2026-01-01   2.27   2.95      1.72      5.47    6.37   1.93 0.909     2.68
#> # ℹ 12 more variables: `Costa Rica` <dbl>, Ecuador <dbl>, `El Salvador` <dbl>,
#> #   Guatemala <dbl>, Honduras <dbl>, México <dbl>, Paraguay <dbl>, Perú <dbl>,
#> #   Panamá <dbl>, Uruguay <dbl>, Venezuela <dbl>, `RD-LATINO` <dbl>
```
