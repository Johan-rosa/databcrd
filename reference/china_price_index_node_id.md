# Find the node id for China's CPI indicator category

Walks the NBS data-portal tree (root -\> sections -\> indicators) to
locate the id of the "Consumer Price Indices by Category" node under
"Price Index". This id (`cid`) is what
[`china_series_cpi_metadata()`](https://johan-rosa.github.io/databcrd/reference/china_series_cpi_metadata.md)
and
[`china_cpi()`](https://johan-rosa.github.io/databcrd/reference/china_cpi.md)
need in order to query the actual CPI series.

## Usage

``` r
china_price_index_node_id()
```

## Value

A character scalar with the node id.

## Details

During development this id was observed to be the fixed value
`"5353d942c68f42c789c7d8c546510ff4"`, which is what those two functions
use as their default. This function exists so that value can be
re-derived programmatically if the site ever changes it.

## Examples

``` r
if (FALSE) { # \dontrun{
china_price_index_node_id()
} # }
```
