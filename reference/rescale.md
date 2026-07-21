# change the scale of a vector

With this function you can change the scale of a numeric vector
providing the new minimum and maximum values

## Usage

``` r
rescale(x, new_min = 0, new_max = 1)
```

## Arguments

- x:

  a numeric vector

- new_min:

  numeric scalar with the minimum value desired

- new_max:

  numeric scalar with the maximum value desired

## Value

a numeric vector

## Examples

``` r
rescale(1:10)
#>  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
#>  [8] 0.7777778 0.8888889 1.0000000
```
