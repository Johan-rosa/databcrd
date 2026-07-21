# Get IMF CPI inflation series for a set of countries

Downloads the total CPI index (not seasonally adjusted, monthly
frequency) from the IMF's CPI dataset via
[`imf.data::get_data()`](https://pedrobtz.github.io/imf.data/reference/get_data.html),
and derives month-over-month and year-over-year inflation rates for each
country.

## Usage

``` r
imf_inflation(country_codes = c("DOM"), last_n_obs = 60)
```

## Arguments

- country_codes:

  Character vector of IMF/ISO3 country codes to query (e.g.
  `c("DOM", "COL", "MEX")`). Default is `"DOM"`.

- last_n_obs:

  Integer. Number of most recent observations to request per country
  from the API (passed through to
  [`imf.data::get_data()`](https://pedrobtz.github.io/imf.data/reference/get_data.html)'s
  `last_n_obs` argument). Default is 60.

## Value

A tibble with one row per country-period, containing:

- date:

  Observation date (first of month), parsed from the IMF `TIME_PERIOD`
  field via
  [`lubridate::ym()`](https://lubridate.tidyverse.org/reference/ymd.html).

- country:

  IMF/ISO3 country code.

- index:

  CPI index value for that period (renamed from `OBS_VALUE`).

- vm:

  Month-over-month inflation rate, in percent.

- vi:

  Year-over-year inflation rate, in percent.

## Details

The function queries the IMF CPI dataset with fixed filters for:

- `INDEX_TYPE = "CPI"` — the CPI index itself (as opposed to, e.g., core
  or other index types).

- `COICOP_1999 = "_T"` — the "all items" total basket, not a COICOP
  sub-category.

- `TYPE_OF_TRANSFORMATION = "IX"` — index values, not percent-change
  series.

- `FREQUENCY = "M"` — monthly data.

`SCALE` and `STATUS` attributes are requested from the API but are not
currently kept in the output (they're dropped by the final
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)).

Inflation rates are computed per country (via `.by = COUNTRY`) as:

- `vm`: month-over-month % change (index vs. 1 month prior)

- `vi`: year-over-year % change (index vs. 12 months prior)

Because `vm`/`vi` rely on lagged values within each country's series,
the first observations for a country will be `NA` unless `last_n_obs`
covers enough history before the period you actually need (at least 13
months of data for `vi` to be non-`NA` on the earliest requested date).

Column names are snake_cased at the end via
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# Latest 12-month inflation rate for a set of LatAm countries,
# ranked from highest to lowest as of March 2026
imf_inflation(
  country_codes = c("COL", "DOM", "MEX", "BRA", "PER",
                     "CHL", "GTM", "URY", "PRY", "CRI"),
  last_n_obs = 24
) |>
  dplyr::filter(date == "2026-03-01") |>
  dplyr::arrange(dplyr::desc(vi))
} # }
```
