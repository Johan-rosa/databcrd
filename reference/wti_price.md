# Download WTI crude oil spot prices from the EIA API

Retrieves monthly West Texas Intermediate (WTI) spot prices for Cushing,
Oklahoma from the U.S. Energy Information Administration (EIA) API.

## Usage

``` r
wti_price(
  frequency = c("monthly", "daily", "annual", "weekly"),
  start = "2022-01",
  key = Sys.getenv("EIA_KEY"),
  remove_metadata = TRUE
)
```

## Arguments

- start:

  First month to retrieve, in \`"YYYY-MM"\` format. Defaults to
  \`"2022-01"\`.

- key:

  EIA API key. Defaults to \`Sys.getenv("EIA_KEY")\`.

## Value

A tibble containing monthly WTI spot prices and associated metadata
returned by the EIA API. The `value` column contains the price in U.S.
dollars per barrel.

## Details

This function queries the EIA v2 API series `"RWTC"`, which corresponds
to the Cushing, Oklahoma WTI spot price (FOB).

## See also

Obtain an API key from <https://www.eia.gov/opendata/>. Try
<https://www.eia.gov/opendata/browser/petroleum/pri/spt>
