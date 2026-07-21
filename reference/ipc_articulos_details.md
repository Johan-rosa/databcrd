# Utility complement to handle the CPI by item

Utility complement to handle the CPI by item

## Usage

``` r
ipc_articulos_details
```

## Format

### `ipc_articulos_details`

A data frame with 612 rows and 26 columns:

- posicion:

  Order in the original excel

- nombre:

  Item name, it can be a group, class or other level

- agregacion:

  This is the level of the given item

- group:

  Group name

- subgrupo:

  Subgroup name

- clase:

  Class name

- subclase:

  Subclass name

- articulo:

  Item name

- is_grupo:

  Boolean indicating if is a group

- is_subgrupo:

  Boolean indicating if is a subgroup

- is_clase:

  Boolean indicating if is a class

- is_subclase:

  Boolean indicating if is a subclass

- is_articulo:

  Boolean: is an item?

- is_subyacente:

  Boolean: is part of the core inflation?

- is_no_subyacente:

  Boolean: is part of the non-core inflation items?

- is_transables:

  Boolean: is a tradable good?

- is_no_transables:

  Boolean: is a non-tradable good?

- is_bienes:

  Boolean: is a good?

- is_servicios:

  Boolean: is a service?

- is_alimentos:

  Boolean: is a food item?

- is_transporte:

  Boolean: is a transportation item?

- is_vivienda:

  Boolean: is housing?

- is_resto:

  Boolean: is others?

- is_nuevo:

  Boolean: was introduced in the most recent basket?

- codigo_articulo:

  id of the item

- ponderacion_ipc:

  weight of the element as proportion of the general basket
