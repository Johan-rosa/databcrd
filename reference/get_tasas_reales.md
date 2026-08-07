# Retrieve real interest rates of financial intermediaries

Downloads the real lending and deposit interest rates published by the
Banco Central de la República Dominicana (BCRD) for financial
intermediary institutions.

## Usage

``` r
get_tasas_reales(
  frecuencia = c("mensual", "anual"),
  format = c("wide", "long")
)
```

## Source

Banco Central de la República Dominicana (BCRD), "Tasas de Interés
Reales de las Entidades de Intermediación Financiera".
:contentReference\[oaicite:1\]index=1

## Arguments

- frecuencia:

  Frequency of the returned data. One of:

  "mensual"

  :   Monthly observations (default).

  "anual"

  :   Annual averages or december values, not sure what it is.

- format:

  Output format. One of:

  "wide"

  :   One column per institution and interest rate type.

  "long"

  :   Tidy format with one observation per institution, interest rate
      type, and period. Includes both the reported real interest rate
      and the implied nominal interest rate.

## Value

A tibble.

When `format = "wide"`, the returned data contain:

- date:

  Observation date (monthly only).

- year:

  Calendar year.

- mes:

  Month number (monthly only).

- bm_activa, bm_pasiva:

  Real lending and deposit rates for multiple banks.

- aap_activa, aap_pasiva:

  Real lending and deposit rates for savings and loan associations.

- bac_activa, bac_pasiva:

  Real lending and deposit rates for savings and credit banks.

- cc_activa, cc_pasiva:

  Real lending and deposit rates for credit corporations.

- inflacion:

  Expected inflation over the next 12 months used by the BCRD in the
  calculation of real interest rates.

When `format = "long"`, the data are returned in tidy format with the
variables `entidad`, `tipo_tasa`, `tasa_real`, `inflacion`, and
`tasa_nominal`, where `tasa_nominal` is computed as:

\$\$ \mathrm{tasa\\nominal} = \mathrm{tasa\\real} + \mathrm{inflacion}
\$\$

## Details

Real interest rates are calculated by the BCRD as the difference between
nominal interest rates and expected inflation over the following 12
months. The published series covers the period from 2008 onward.
:contentReference\[oaicite:0\]index=0

The data are downloaded directly from the official BCRD statistical
spreadsheets. The function parses the original workbook and returns a
tidy dataset suitable for analysis.

Institution abbreviations used in the wide format are:

- bm:

  Multiple banks.

- aap:

  Savings and loan associations.

- bac:

  Savings and credit banks.

- cc:

  Credit corporations.

## Examples

``` r
# Monthly data in wide format
tasas <- get_tasas_reales()

# Annual data in long format
tasas_long <- get_tasas_reales(
  frecuencia = "anual",
  format = "long"
)
```
