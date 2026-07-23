# Fiscal operations

Downloads fiscal operations data published by the Central Bank of the
Dominican Republic and returns it in tidy format at either monthly or
annual frequency.

## Usage

``` r
fiscal_operations(frecuencia = c("mensual", "anual"))
```

## Source

Central Bank of the Dominican Republic (Banco Central de la República
Dominicana), *Operaciones del Sector Público*.
<https://cdn.bancentral.gov.do/documents/estadisticas/documents/Operaciones_Mensual.xlsx>

## Arguments

- frecuencia:

  Character scalar indicating the desired frequency. Must be one of:

  "mensual"

  :   Return monthly observations (default).

  "anual"

  :   Return annual totals obtained by summing monthly values.

## Value

If `frecuencia = "mensual"`, a tibble with the following columns:

- codigo:

  Unique account code. Duplicate codes in the source are disambiguated
  by appending a numeric suffix.

- categoria:

  Institutional sector.

- cuenta:

  Account name.

- level:

  Hierarchical level inferred from the account code.

- fecha:

  Observation date.

- year:

  Calendar year.

- mes:

  Calendar month (1–12).

- value:

  Reported fiscal value.

If `frecuencia = "anual"`, a tibble with one observation per account and
year containing:

- codigo:

  Unique account code.

- categoria:

  Institutional sector.

- cuenta:

  Account name.

- level:

  Hierarchical level inferred from the account code.

- year:

  Calendar year.

- value:

  Annual total obtained by summing monthly values.

## Details

The source spreadsheet reports fiscal aggregates for the Central
Government, the Rest of the Public Sector, and the Non-Financial Public
Sector. The function cleans the original workbook, propagates category
labels and account codes, removes empty rows, derives the hierarchical
level of each account from its code, and reshapes the data into a long
format. Monthly observations can be returned directly, or aggregated to
annual totals.

The source workbook is downloaded directly from the Central Bank of the
Dominican Republic each time the function is called.

## Examples

``` r
if (FALSE) { # \dontrun{
# Monthly data
monthly <- get_fiscal()

# Annual totals
annual <- get_fiscal("anual")
} # }
```
