# Get average interbank interest rates by maturity

Downloads and processes the official Average Interbank Rates by Maturity
file published by the Central Bank of the Dominican Republic.

## Usage

``` r
get_tasa_interbancaria()
```

## Source

Central Bank of the Dominican Republic.

## Value

A monthly-frequency `tibble` including:

- date:

  Date object (first day of the month).

- year:

  Numeric year.

- mes:

  Numeric month (1–12).

- monto_operaciones_interbancarias:

  Total interbank transaction amount.

- tasa_promedio_ponderado:

  Weighted average interbank interest rate.

- monto_depositos_vista:

  Demand deposits amount.

- tasa_depositos_vista:

  Demand deposits interest rate.

- monto_d\_Amounts by maturity bucket (days). tasa_d\_:

  Interest rates by maturity bucket (days).

## Details

The function:

- Downloads the .xlsm file from the institutional CDN.

- Cleans headers and aggregated rows (cumulative values).

- Converts amounts and interest rates to numeric format.

- Builds time variables (`year`, `mes`, `date`).

Requires the source file structure to remain unchanged (11 initial rows
skipped and fixed column order). If the format published by the Central
Bank changes, the function may fail.

## Examples

``` r
if (FALSE) { # \dontrun{
rates <- get_tasa_interbancaria()
dplyr::glimpse(rates)
} # }
```
