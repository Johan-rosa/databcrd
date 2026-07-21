# Download euro area unemployment data

Retrieves the monthly unemployment rate for the euro area from the
European Central Bank (ECB).

## Usage

``` r
ze_unemployment()
```

## Value

A tibble with two columns:

- date:

  Observation date.

- unemployment:

  Unemployment rate.

## Details

The data correspond to the ECB series
`"LFSI.M.I10.S.UNEHRT.TOTAL0.15_74.T"`, which reports the harmonized
monthly unemployment rate for persons aged 15 to 74 in the euro area.

## See also

Additional information is available from the ECB Data Portal:
<https://data.ecb.europa.eu/>.
