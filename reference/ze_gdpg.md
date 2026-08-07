# Download euro area real GDP growth data

Retrieves the quarterly growth rate of real gross domestic product (GDP)
for the euro area from the European Central Bank (ECB).

## Usage

``` r
ze_gdpg()
```

## Value

A tibble with two columns:

- date:

  Observation date.

- growth:

  Quarterly real GDP growth rate.

## Details

The data correspond to the ECB series
`"MNA.Q.Y.I10.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.G1"`, which reports
quarterly growth in real GDP (volume) for the euro area.

## See also

Additional information is available from the ECB Data Portal:
<https://data.ecb.europa.eu/>.
