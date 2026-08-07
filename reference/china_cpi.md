# Download China's Consumer Price Index (CPI) series

Downloads monthly CPI data (all categories available under the CPI node)
from the NBS data portal, for a given date range.

## Usage

``` r
china_cpi(
  start_period = 201601,
  end_period = NULL,
  cid = "5353d942c68f42c789c7d8c546510ff4"
)
```

## Arguments

- start_period:

  Integer or character in `"YYYYMM"` format. First month to request.
  Defaults to `201601`.

- end_period:

  Integer or character in `"YYYYMM"` format. Last month to request.
  Defaults to the current year and month.

- cid:

  Character. The data-portal node id for the CPI-by-category indicator
  group, passed to
  [`china_series_cpi_metadata()`](https://johan-rosa.github.io/databcrd/reference/china_series_cpi_metadata.md).
  Defaults to `"5353d942c68f42c789c7d8c546510ff4"`.

## Value

A tibble in long format with columns `id`, `value`, `date`, and `name`
(one row per indicator per month). Rows with a missing (`NA`) value are
dropped.

## Examples

``` r
if (FALSE) { # \dontrun{
china_cpi(start_period = 202001, end_period = 202312)
} # }
```
