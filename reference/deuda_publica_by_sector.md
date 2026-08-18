# Deuda pública consolidada por sector institucional

Descarga y transforma el archivo
"Deuda_Consolidada_Por_Sector_Trimestral.xlsx" publicado por el Banco
Central de la República Dominicana (BCRD), y lo convierte en un tibble
en formato largo (tidy) con una fila por concepto (sector, sub-sector o
fuente de financiamiento dentro de un sector) y trimestre, incluyendo el
monto en millones de US\$ y su equivalente como porcentaje del PIB.

## Usage

``` r
deuda_publica_by_sector()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Deuda_Consolidada_Por_Sector_Trimestral.xlsx>

## Value

Un tibble con una fila por combinación de concepto y trimestre, con las
columnas:

- grupo:

  `chr`. `"Sector Público No Financiero"` o
  `"Sector Público Financiero"`. `NA` para las filas `nivel == "total"`.

- sector:

  `chr`. `"Gobierno Central"`, `"Resto del SPNF"` o `"Banco Central"`.
  `NA` para `nivel %in% c("total", "grupo")`.

- tipo_deuda:

  `chr`. `"Consolidada"`, `"Externa"` o `"Interna"`. `NA` para las filas
  agregadas (`nivel %in% c("grupo", "sector")`) que suman ambas fuentes.

- nivel:

  `chr`. Nivel jerárquico: `"total"`, `"grupo"`, `"sector"`, `"fuente"`
  o `"detalle"`.

- concepto:

  `chr`. Descripción de la fila tal como aparece en el Excel, sin código
  inicial ni paréntesis/notas al pie sobrantes.

- fecha:

  `Date`. Primer día del trimestre (`2013-03-01`, `2013-06-01`, ...).
  Ver advertencia en `@details`.

- year:

  `dbl`. Año extraído de `fecha`.

- trimestre:

  `dbl`. Trimestre (1 a 4) extraído de `fecha`.

- monto:

  `dbl`. Monto de deuda reportado para ese concepto y trimestre, en
  millones de US\$.

- gdp:

  `dbl`. PIB nominal trimestral en millones de US\$.

- as_gdp_percent:

  `dbl`. `monto / gdp * 100`.

## Details

A diferencia de
[`deuda_publica_by_fuente()`](https://johan-rosa.github.io/databcrd/reference/deuda_publica_by_fuente.md),
este archivo organiza las filas en una jerarquía de **cuatro niveles**.
Las filas no tienen un código único y consistente para distinguir todos
los niveles, así que la detección combina dos señales:

1.  `"total"`: tres filas al inicio, en MAYÚSCULA SOSTENIDA ("DEUDA
    PÚBLICA CONSOLIDADA", "...EXTERNA CONSOLIDADA", "...INTERNA
    CONSOLIDADA"), sin código ni sector asociado.

2.  `"grupo"`: dos filas con código de letra (A./B.), agrupando sectores
    institucionales (Sector Público No Financiero / Sector Público
    Financiero).

3.  `"sector"`: filas con código numérico (1./2./3.) dentro de cada
    grupo (Gobierno Central, Resto del SPNF, Banco Central).

4.  `"fuente"`: filas sin código propio que empiezan con "Deuda Externa"
    o "Deuda Interna" (mayúscula inicial únicamente, a diferencia de las
    filas "total"), desagregando cada sector.

5.  `"detalle"`: una fila memo que empieza con "De los cuales" ("De los
    cuales: Intragubernamental"), hija de la fila "Deuda Interna" del
    Gobierno Central.

El texto original también tiene sangría (espacios en blanco al inicio de
la celda) que en teoría distinguiría estos niveles, pero
[`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
usa `trim_ws = TRUE` por defecto y la elimina antes de que la función
pueda medirla. Por eso el nivel se detecta por el patrón del propio
texto (mayúscula sostenida vs. mayúscula inicial, y el prefijo "De los
cuales") en lugar de la indentación. Esto es más robusto frente al
comportamiento de lectura, pero depende de que el BCRD mantenga esa
convención de mayúsculas/minúsculas en el texto.

Al igual que en
[`deuda_publica_by_fuente()`](https://johan-rosa.github.io/databcrd/reference/deuda_publica_by_fuente.md),
la fila " A. SECTOR PÚBLICO NO FINANCIERO..." tiene un espacio en blanco
inicial que "B. SECTOR PÚBLICO FINANCIERO..." no tiene; se resuelve
aplicando
[`stringr::str_squish()`](https://stringr.tidyverse.org/reference/str_trim.html)
antes de extraer el código.

`grupo` y `sector` se completan hacia abajo (con relleno acotado, no
[`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html)
directo sobre toda la tabla) para que las filas de `nivel` "fuente" y
"detalle" hereden el sector al que pertenecen sin que ese valor se
filtre hacia las filas "total" siguientes. `tipo_deuda` se deriva del
texto en los niveles "total" y "fuente"; en el nivel "detalle" se hereda
de la fila "fuente" inmediatamente anterior (en este archivo, siempre
"Deuda Interna"), usando
[`dplyr::lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html) en
lugar de `fill()` para no arrastrar el valor más allá de esa fila.

Las fechas de las columnas trimestrales **no se leen del archivo**: se
asume que la primera columna de datos corresponde a T1-2013 y se genera
una secuencia trimestral consecutiva con
`length.out = ncol(raw_data) - 1`. Si el BCRD inserta, elimina o
reordena columnas, o cambia el trimestre inicial, las fechas quedarán
mal alineadas sin que la función lo detecte.

El PIB nominal usado para calcular `as_gdp_percent` se toma de la fila
memo "Producto Interno Bruto ( Millones de USD)" en la hoja "Sector
(%PIB)", igual que en
[`deuda_publica_by_fuente()`](https://johan-rosa.github.io/databcrd/reference/deuda_publica_by_fuente.md).

## Examples

``` r
if (FALSE) { # \dontrun{
deuda_publica_by_sector()

# Deuda del Banco Central, solo la fuente externa
deuda_publica_by_sector() |>
  dplyr::filter(sector == "Banco Central", tipo_deuda == "Externa")
} # }
```
