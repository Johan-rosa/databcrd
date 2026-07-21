# Fetch metadata for China's CPI series

Retrieves the list of individual CPI indicators (id + display name)
available under a given data-portal node (`cid`) – e.g. "CPI, current
month=same month of previous year", "CPI, current month=previous month",
etc. The resulting ids are needed to query actual values via
[`china_cpi()`](https://johan-rosa.github.io/databcrd/reference/china_cpi.md).

## Usage

``` r
china_series_cpi_metadata(cid = "5353d942c68f42c789c7d8c546510ff4")
```

## Arguments

- cid:

  Character. The data-portal node id for the CPI-by-category indicator
  group. Defaults to `"5353d942c68f42c789c7d8c546510ff4"`, the value
  observed during development (see
  [`china_price_index_node_id()`](https://johan-rosa.github.io/databcrd/reference/china_price_index_node_id.md)
  to re-derive it if this ever stops working).

## Value

A tibble with columns `id` and `name`, one row per CPI indicator.

## Examples

``` r
if (FALSE) { # \dontrun{
china_series_cpi_metadata()
} # }
```
