# Balanza de Pagos - datos trimestrales

Descarga y transforma la hoja `BOP_TRIM` del archivo de Balanza de Pagos
publicado por el Banco Central de la Republica Dominicana (BCRD), y la
une contra
[catalogo_balanza_pagos](https://johan-rosa.github.io/databcrd/reference/catalogo_balanza_pagos.md),
que trae el codigo jerarquico, nivel, naturaleza contable y cuenta de
nivel 1 de cada fila.

## Usage

``` r
balanza_pagos(
  force_bind_by_position = FALSE,
  filtro_codigo = NULL,
  filtro_naturaleza = NULL,
  filtro_concepto = NULL,
  filtro_cuenta = NULL,
  filtro_nivel = NULL,
  solo_hojas = FALSE
)
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/bpagos_trim_6.xlsx>

## Arguments

- force_bind_by_position:

  `logical`. Si `FALSE` (por defecto) y se detectan conceptos cuyo texto
  crudo cambio respecto al catalogo, la funcion aborta. Si `TRUE`, solo
  advierte e igual devuelve el resultado.

- filtro_codigo:

  `character`. Vector de `code` a conservar (match exacto), p. ej.
  `c("1.1.1.1", "1.1.1.2")`. `NULL` (por defecto) no filtra.

- filtro_naturaleza:

  `character`. Vector de `naturaleza` a conservar (`"Credito"`,
  `"Debito"`, `"Saldo"`). `NULL` no filtra.

- filtro_concepto:

  `character`. Vector de patrones a buscar en `concepto` (match parcial,
  insensible a mayusculas/minusculas; basta con que coincida uno de los
  patrones). `NULL` no filtra.

- filtro_cuenta:

  `character`. Vector de `cuenta` (nivel 1) a conservar, p. ej.
  `"Cuenta Financiera"`. `NULL` no filtra.

- filtro_nivel:

  `integer`. Vector de `nivel` a conservar, p. ej. `1:2` para quedarse
  solo con el resumen. `NULL` no filtra.

- solo_hojas:

  `logical`. Si `TRUE`, descarta las filas de subtotal (aquellas cuyo
  `code` aparece como `code_padre` de otra fila), dejando solo las
  categorias mas finas. Util para sumar `monto` sin duplicar cifras. Por
  defecto `FALSE`.

## Value

Un tibble en formato ancho con una fila por cuenta/subcuenta (igual a
[catalogo_balanza_pagos](https://johan-rosa.github.io/databcrd/reference/catalogo_balanza_pagos.md),
sin las columnas `nota` y `concepto_fuente`) y una columna adicional por
cada trimestre publicado (nombrada con la fecha de inicio del trimestre,
p. ej. `"2010-01-01"`), en millones de USD, filtrado segun los
argumentos `filtro_*`/`solo_hojas` provistos.

## Details

A diferencia de
[`balanza_servicios()`](https://johan-rosa.github.io/databcrd/reference/balanza_servicios.md),
aqui el `code`/`naturaleza` de cada fila no se extraen con expresiones
regulares: se pegan por POSICION contra
[catalogo_balanza_pagos](https://johan-rosa.github.io/databcrd/reference/catalogo_balanza_pagos.md),
porque las filas de detalle (p. ej. "Exportaciones", "Credito") no traen
ningun codigo propio en el texto y no hay forma confiable de inferirlas
solo del texto. Esto exige que el archivo mantenga las mismas 57 filas
de datos, en el mismo orden, que cuando se construyo el catalogo; si el
conteo de filas no coincide, la funcion aborta con un mensaje explicito
en vez de pegar datos desalineados en silencio.

Como salvaguarda adicional, se compara el texto crudo leido en cada
corrida (`og_concepto`) contra el texto crudo capturado al construir el
catalogo (columna `concepto_fuente` de
[catalogo_balanza_pagos](https://johan-rosa.github.io/databcrd/reference/catalogo_balanza_pagos.md)),
usando solo una limpieza de espacios
([`stringr::str_squish()`](https://stringr.tidyverse.org/reference/str_trim.html)),
sin reconstruir el texto normalizado del catalogo. Esto evita falsos
positivos: la comparacion solo falla si el BCRD cambio el texto fuente,
no cuando el catalogo tiene un estilo distinto (acentos removidos,
abreviaturas expandidas, notas al pie separadas).

Si se detectan conceptos con texto distinto, la funcion imprime la tabla
de diferencias y aborta, a menos que se llame con
`force_bind_by_position = TRUE`, en cuyo caso solo emite un warning y
continua (bajo responsabilidad de quien llama, ya que no se puede
garantizar que el orden de las filas siga correspondiendo al catalogo).

Los filtros (`filtro_*`, `solo_hojas`) se aplican al final, despues de
unir los datos crudos contra el catalogo, y son todos opcionales (`NULL`
o `FALSE` = sin filtrar). Se pueden combinar libremente.

`concepto` NO es unico en el catalogo: valores como `"Credito"`,
`"Debito"`, `"Otros"`, `"Nacionales"`, `"Zonas Francas"`,
`"Remesas Familiares"`, `"Otras Transferencias"`,
`"Inversion de Cartera"` y `"Activos de Reservas"` aparecen repetidos
bajo distintas cuentas padre. `filtro_concepto` hace match parcial
insensible a mayusculas (via
[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)),
asi que por si solo puede devolver varias categorias no relacionadas;
combinalo con `filtro_cuenta` o `filtro_codigo` para desambiguar.

## See also

[catalogo_balanza_pagos](https://johan-rosa.github.io/databcrd/reference/catalogo_balanza_pagos.md),
[`balanza_servicios()`](https://johan-rosa.github.io/databcrd/reference/balanza_servicios.md)

## Examples

``` r
if (FALSE) { # \dontrun{
balanza_pagos()

# Solo la Cuenta Financiera, sin subtotales
balanza_pagos(filtro_cuenta = "Cuenta Financiera", solo_hojas = TRUE)

# Categorias de credito en Ingreso Primario, resumen (nivel 1-3)
balanza_pagos(
  filtro_cuenta = "Cuenta Corriente",
  filtro_naturaleza = "Credito",
  filtro_nivel = 1:3
)
} # }
```
