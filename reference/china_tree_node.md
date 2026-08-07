# Navigate the NBS data-portal folder tree

The NBS data portal organizes its data series in a tree of "nodes"
(folders and leaves). This function queries one level of that tree given
a parent node id, and is the building block used to locate specific
indicators (e.g. CPI) by walking down from the root.

## Usage

``` r
china_tree_node(parent_id = NULL, code = 19)
```

## Arguments

- parent_id:

  Character. The `id` of the parent node to list children for. Use
  `NULL` (the default) to fetch the root of the tree.

- code:

  Integer/character. Which panel of the site to query: `1` = "Browse
  topics" / "advanced search" panel, `19` = "Visualized" panel (the
  default). During development, `code = 19` was the panel that reliably
  contained the Price Index section, but this is not guaranteed to
  remain stable if the site changes.

## Value

A tibble with (when available) columns `level`, `parent_id`, `id`,
`name`, and `dt` – one row per child node. Returns a 0-row tibble if the
node has no children.

## Examples

``` r
if (FALSE) { # \dontrun{
root <- china_tree_node()
sections <- china_tree_node(root$id)
} # }
```
