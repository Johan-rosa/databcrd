# Pivot wide formats of interest rates data frames to long format

Tidies interest rate datasets from wide matrix forms down into
structured tidy key-value combinations.

## Usage

``` r
tasas_to_long(
  tasas_wide,
  filtro_tipo_tasa = NULL,
  filtro_moneda = NULL,
  filtro_condicion = NULL,
  filtro_grupo = NULL,
  filtro_detalle = NULL
)
```

## Arguments

- tasas_wide:

  `<data.frame>` Wide structured table array parsed from source files.

- filtro_tipo_tasa:

  `<character>` Filter by type: `"Activa"` or `"Pasiva"`.

- filtro_moneda:

  `<character>` Filter by currency: `"DOP"` or `"USD"`.

- filtro_condicion:

  `<character>` Filter by rate condition: `"General"` or
  `"Preferencial"`.

- filtro_grupo:

  `<character>` Filter by grouping: `"Plazo"`, `"Promedio"`, or
  `"Sector"`.

- filtro_detalle:

  `<character>` Filter by specific descriptive category.

## Value

A tidy mapped structure long configuration `tibble`.
