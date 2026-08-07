# Download euro area inflation data

Retrieves the monthly Harmonised Index of Consumer Prices (HICP)
inflation series for the euro area from the European Central Bank (ECB).

## Usage

``` r
ze_inflation()
```

## Value

A tibble with two columns:

- date:

  Observation date.

- inflation:

  Monthly HICP inflation rate.

## Details

The data correspond to the ECB series `"HICP.M.U2.N.000000.4D0.ANR"`,
which reports the annual rate of change of the Harmonised Index of
Consumer Prices (HICP) for the euro area.

## See also

Obtain additional information about the ECB Data Portal at
<https://data.ecb.europa.eu/>.
