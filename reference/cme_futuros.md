# Cotizaciones de futuros de CME Group

Descarga las cotizaciones vigentes de un contrato de futuros publicado
por CME Group (Chicago Mercantile Exchange), a partir del identificador
numerico del producto. Por defecto consulta los futuros de oro (Gold,
`product_id = 437`).

## Usage

``` r
cme_futuros(product_id = 437)
```

## Source

CME Group. <https://www.cmegroup.com/>.

## Arguments

- product_id:

  Numero entero con el identificador del producto en CME Group (por
  ejemplo, `437` para oro). Por defecto `437`.

## Value

Un tibble con una fila por mes de vencimiento del contrato y las
siguientes columnas:

- date:

  Fecha (primer dia del mes de vencimiento del contrato).

- year:

  Anio de vencimiento del contrato.

- month:

  Mes de vencimiento del contrato.

- last:

  Ultimo precio negociado.

- prior_settle:

  Precio de liquidacion de la sesion anterior.

- open:

  Precio de apertura.

- high:

  Precio maximo de la sesion.

- low:

  Precio minimo de la sesion.

- volume:

  Volumen negociado.

## Details

La funcion consulta el endpoint publico
`https://www.cmegroup.com/CmeWS/mvc/quotes/v2/<product_id>`, el cual
devuelve, entre otra informacion, un listado de cotizaciones por mes de
vencimiento (`quotes`). Cada solicitud incluye un timestamp en
milisegundos (`_t`) para evitar respuestas cacheadas.

## Examples

``` r
if (FALSE) { # \dontrun{
cme_futuros()
cme_futuros(product_id = 437)
} # }
```
