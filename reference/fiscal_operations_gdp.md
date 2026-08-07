# Fiscal operations as a percentage of GDP

Downloads the annual fiscal operations table published by the Central
Bank of the Dominican Republic and returns it in tidy (long) format.

## Usage

``` r
fiscal_operations_gdp()
```

## Source

Central Bank of the Dominican Republic (Banco Central de la República
Dominicana), *Operaciones del Sector Público como porcentaje del PIB
(Anual)*.
<https://cdn.bancentral.gov.do/documents/estadisticas/documents/Operaciones_PIB_Anual.xlsx>

## Value

A tibble with the following columns:

- codigo:

  Unique account code. Duplicate codes in the source are disambiguated
  by appending a numeric suffix.

- categoria:

  Institutional sector (e.g. "Central Government", "Rest of the Public
  Sector", or "Non-Financial Public Sector").

- cuenta:

  Account name.

- level:

  Hierarchical level inferred from the account code.

- year:

  Calendar year.

- value:

  Fiscal balance or transaction expressed as a percentage of GDP.

## Details

The source spreadsheet reports fiscal aggregates as percentages of GDP
for the Central Government, the Rest of the Public Sector, and the
Non-Financial Public Sector. The function cleans the original layout,
propagates category labels and account codes, removes empty rows,
derives the hierarchical level of each account from its code, and
reshapes the data into a long format with one observation per account
and year.

The source workbook is downloaded directly from the Central Bank of the
Dominican Republic each time the function is called.

## Examples

``` r
if (FALSE) { # \dontrun{
fiscal <- get_fiscal_as_gdp()

dplyr::filter(fiscal, codigo == "1")
} # }
```
