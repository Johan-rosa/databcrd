# Exportaciones de zonas francas por partida

Descarga y consolida las exportaciones de zonas francas de la Republica
Dominicana desagregadas por partida (tipo de bien), a partir del archivo
publicado por el Banco Central en su portal de estadisticas del sector
externo. Los valores estan expresados en millones de USD.

## Usage

``` r
get_exportaciones_zf()
```

## Source

<https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/Exportaciones_Zonas_Francas_6.xls>

## Value

Un tibble en formato largo con columnas `fecha`, `year`, `mes`,
`partida` y `valor` (en millones de USD).

- fecha:

  Fecha del periodo (primer dia del mes)

- year:

  Anio del periodo

- mes:

  Mes del periodo (1-12)

- partida:

  Tipo de bien exportado desde zonas francas

- valor:

  Valor exportado, en millones de USD

## Details

La funcion identifica las columnas de partidas a partir del encabezado
del archivo original, removiendo las notas al pie (p. ej. "1/", "2/").
Se descartan las filas que ya vienen acumuladas por anio (aquellas cuya
etiqueta de mes contiene un anio de 4 digitos, usadas como totales en el
archivo fuente). Las fechas se generan de forma secuencial, un mes por
fila, comenzando en enero de 2010.

## Examples

``` r
if (FALSE) { # \dontrun{
get_exportaciones_zf()
} # }
```
