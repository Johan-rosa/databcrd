# Monthly Gold Prices Since 1833

Retrieves a monthly time series of gold prices in U.S. dollars (USD)
beginning in 1833. The data is sourced from the World Gold Council and
is derived from historical records compiled by Timothy Green,
supplemented with data provided by the World Bank.

## Usage

``` r
gold_price()
```

## Source

World Gold Council. *Gold Price History Since 1833*. Historical series
compiled by Timothy Green and supplemented with data from the World
Bank. <https://datahub.io/core/gold-prices>

## Value

A tibble with two columns:

- date:

  A `Date` representing the month of the observation.

- gold_price:

  Monthly gold price in U.S. dollars (USD).

## Details

The data is downloaded from the DataHub mirror of the World Gold
Council's historical gold price dataset each time the function is
called, ensuring that the latest available version is retrieved.
