# Catalogo jerarquico de cuentas de la Balanza de Pagos

Catalogo de referencia con la estructura de cuentas de la Balanza de
Pagos de la Republica Dominicana (formato MBP6), tal como la publica el
BCRD en el archivo "Balanza de Pagos" (hoja `BOP_TRIM`). Cada fila
representa una cuenta o subcuenta, con su codigo jerarquico decimal,
nivel de profundidad, codigo padre y naturaleza contable, para poder
unirse contra los datos crudos por posicion sin depender de parsear el
texto de cada fila con expresiones regulares.

## Usage

``` r
catalogo_balanza_pagos
```

## Format

Un tibble con 57 filas y 7 columnas:

- code:

  `chr`. Codigo jerarquico decimal (`"1"`, `"1.1"`, `"1.1.1.1"`, ...).
  Respeta la numeracion original de la fuente para los niveles 1 a 3 y
  la extiende hacia abajo para las filas sin numeral propio (p. ej.
  "Exportaciones", "Credito").

- nivel:

  `int`. Profundidad en la jerarquia, de 1 (cuentas principales: Cuenta
  Corriente, Cuenta de Capital, etc.) a 5 (el nivel de detalle mas
  fino, p. ej. "Nacionales" dentro de "Exportaciones").

- code_padre:

  `chr`. `code` de la fila padre en la jerarquia; `NA` en las cuentas de
  nivel 1. Permite reconstruir el arbol o hacer joins recursivos.

- concepto:

  `chr`. Descripcion de la cuenta, normalizada (ver `@details`).

- naturaleza:

  `chr`. `"Credito"`, `"Debito"` o `"Saldo"`.

- cuenta:

  `chr`. Nombre de la cuenta de nivel 1 a la que pertenece la fila (p.
  ej. `"Cuenta Corriente"`, `"Cuenta Financiera"`, `"Financiamiento"`),
  util para filtrar sin tener que interpretar el codigo.

- nota:

  `chr`. Llamada al pie (`"1/"`, `"2/"`, `"3/"`, `"4/"`) o formula
  (`"Formula: 3 = 1 + 2"`) asociada a la fila en la fuente; `NA` cuando
  no aplica.

## Source

Estructura tomada de "BALANZA DE PAGOS DE LA REPUBLICA DOMINICANA -
RESULTADOS CONFORME AL MBP6", Departamento Internacional, BCRD (hoja
`BOP_TRIM` del archivo de balanza de pagos). Catalogo construido y
validado manualmente contra esa hoja.

## Details

El orden de las 57 filas de este catalogo coincide exactamente, fila por
fila, con las 57 filas de datos de la hoja `BOP_TRIM` (columna A, filas
12-77, excluyendo separadores en blanco y notas al pie). Esa
correspondencia posicional fue verificada explicitamente contra el
archivo fuente y es la base de como
[`balanza_pagos()`](https://johan-rosa.github.io/databcrd/reference/balanza_pagos.md)
une los datos crudos con este catalogo: por posicion, no por texto.

`concepto` no es una copia literal del texto del Excel; se normalizo
para uso consistente en el paquete:

- Se removieron acentos (`"Credito"`, no `"Crédito"`).

- Las llamadas al pie (`1/`, `2/`, `3/`, `4/`) y la formula `(3=1+2)` se
  movieron a la columna `nota`.

- Se expandieron abreviaturas (p. ej.
  `"Deuda Pub. y Priv. Med. y LP (Neto)"` -\>
  `"Deuda Publica y Privada Mediano y Largo Plazo (Neto)"`).

- Se corrigio el typo de la fuente `"Transferecias"` -\>
  `"Transferencias"`.

`naturaleza` toma siempre uno de tres valores: `"Credito"`, `"Debito"` o
`"Saldo"`. Las filas de subtotal (que suman a sus hijos, p. ej.
`"Cuenta Corriente"`, `"Balanza de Bienes"`) y las cuentas que el BCRD
reporta ya netas sin desglose de credito/debito (p. ej. los componentes
de la Cuenta Financiera) quedan como `"Saldo"`.

"Exportaciones" se codifico como Credito e "Importaciones" como Debito
por convencion estandar de balanza de pagos; el archivo fuente no las
etiqueta explicitamente con esos terminos (a diferencia de Balanza de
Servicios, que si dice "Credito"/"Debito").

## See also

[`balanza_pagos()`](https://johan-rosa.github.io/databcrd/reference/balanza_pagos.md),
que une los datos trimestrales crudos contra este catalogo por posicion.

## Examples

``` r
# Todas las cuentas de la Cuenta Corriente
catalogo_balanza_pagos |>
  dplyr::filter(cuenta == "Cuenta Corriente")
#> # A tibble: 41 × 8
#>    code      nivel code_padre concepto   naturaleza cuenta nota  concepto_fuente
#>    <chr>     <int> <chr>      <chr>      <chr>      <chr>  <chr> <chr>          
#>  1 1             1 NA         Cuenta Co… Saldo      Cuent… NA    1. Cuenta Corr…
#>  2 1.1           2 1          Balanza d… Saldo      Cuent… NA    1.1 Balanza de…
#>  3 1.1.1         3 1.1        Balanza d… Saldo      Cuent… NA    1.1.1 Balanza …
#>  4 1.1.1.1       4 1.1.1      Exportaci… Credito    Cuent… NA    Exportaciones  
#>  5 1.1.1.1.1     5 1.1.1.1    Nacionales Credito    Cuent… NA    Nacionales     
#>  6 1.1.1.1.2     5 1.1.1.1    Zonas Fra… Credito    Cuent… NA    Zonas Francas  
#>  7 1.1.1.2       4 1.1.1      Importaci… Debito     Cuent… NA    Importaciones  
#>  8 1.1.1.2.1     5 1.1.1.2    Nacionales Debito     Cuent… NA    Nacionales     
#>  9 1.1.1.2.2     5 1.1.1.2    Zonas Fra… Debito     Cuent… NA    Zonas Francas  
#> 10 1.1.2         3 1.1        Balanza d… Saldo      Cuent… NA    1.1.2 Balanza …
#> # ℹ 31 more rows

# Solo las filas "hoja" (sin hijos), utiles para no doble-contar al sumar
catalogo_balanza_pagos |>
  dplyr::filter(!code %in% code_padre)
#> # A tibble: 37 × 8
#>    code      nivel code_padre concepto   naturaleza cuenta nota  concepto_fuente
#>    <chr>     <int> <chr>      <chr>      <chr>      <chr>  <chr> <chr>          
#>  1 1.1.1.1.1     5 1.1.1.1    Nacionales Credito    Cuent… NA    Nacionales     
#>  2 1.1.1.1.2     5 1.1.1.1    Zonas Fra… Credito    Cuent… NA    Zonas Francas  
#>  3 1.1.1.2.1     5 1.1.1.2    Nacionales Debito     Cuent… NA    Nacionales     
#>  4 1.1.1.2.2     5 1.1.1.2    Zonas Fra… Debito     Cuent… NA    Zonas Francas  
#>  5 1.1.2.1.1     5 1.1.2.1    Viajes     Credito    Cuent… NA    Viajes         
#>  6 1.1.2.1.2     5 1.1.2.1    Servicios… Credito    Cuent… NA    Servicios de M…
#>  7 1.1.2.1.3     5 1.1.2.1    Otros      Credito    Cuent… NA    Otros          
#>  8 1.1.2.2.1     5 1.1.2.2    Fletes     Debito     Cuent… NA    Fletes         
#>  9 1.1.2.2.2     5 1.1.2.2    Otros      Debito     Cuent… NA    Otros          
#> 10 1.2.1.1       4 1.2.1      Credito    Credito    Cuent… NA    Crédito        
#> # ℹ 27 more rows
```
