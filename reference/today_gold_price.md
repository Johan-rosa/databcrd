# Retrieve the current gold spot price

Queries the GoldAPI service for the current spot price of one troy ounce
of gold (XAU) quoted in U.S. dollars (USD).

## Usage

``` r
today_gold_price(key = Sys.getenv("GOLDAPI_KEY"))
```

## Arguments

- key:

  A character string containing a valid GoldAPI key. Defaults to
  `Sys.getenv("GOLDAPI_KEY")`.

## Value

A tibble containing the JSON response returned by GoldAPI. Timestamp
variables are converted to POSIXct.

## Details

An active GoldAPI key is required. By default, the function reads the
key from the `GOLDAPI_KEY` environment variable.

This function queries the endpoint `https://www.goldapi.io/api/XAU/USD`.

## Examples

``` r
if (FALSE) { # \dontrun{
today_gold_price()
} # }
```
