# Parse Bank Reserve and Asset Data from Excel

Reads a structured Excel file containing monthly banking data and
returns a tidy dataframe with year, variable, and modalidad columns.

## Usage

``` r
get_reservas_internacionales()
```

## Arguments

- file:

  Path to the Excel file.

## Value

A dataframe with the following columns:

- mes:

  Month number (integer).

- column:

  Column index used for joining (integer).

- value:

  Numeric value for the given period and category.

- year:

  Four-digit year extracted from the header (character).

- variable:

  Either `"Reservas"` or `"Activos"`.

- modalidad:

  Either `"Brutos"` or `"Netos"`.

- date:

  First day of the month as a `Date` object.
