# databcrd

**databcrd** es un paquete de R para descargar, limpiar e importar los
datos que publican el Banco Central de la República Dominicana (BCRD) y
otras de fuentes internacionales que se usan a menudo junto a ellos
(FMI, EIA, BCE, China NBS y World Gold Council).

En lugar de enlaces a Excel, adivinar hojas y nombres de columnas, o
mantener scripts de scraping frágiles cada vez que el BCRD cambia el
formato de un archivo,
[databcrd](https://johan-rosa.github.io/databcrd/) expone una función
por serie con una interfaz consistente y datos ya limpios en formato
*tidy*.

## Instalación

El paquete todavía no está en CRAN. Instala la versión de desarrollo
desde GitHub:

``` r

# install.packages("remotes")
remotes::install_github("Johan-rosa/databcrd")
```

## Uso rápido

``` r

library(databcrd)
 
# Indicador Mensual de Actividad Económica (IMAE)
get_imae()
 
# Tipo de cambio spot
get_tc_spot()
 
# Tasa de Política Monetaria
get_tpm()
 
# Inflación (IPC), a nivel general o por desagregaciones
get_ipc_data("general")
get_ipc_data("subyacente")
```

Cada función retorna un `tibble`, listo para encadenar con `dplyr`,
graficar con `ggplot2` o convertir a serie de tiempo con `tsibble`.

## Fuentes de datos

| Fuente | Cobertura en el paquete |
|----|----|
| **BCRD** | Cuentas nacionales, precios, tipo de cambio, tasas de interés, sector externo, sector fiscal, sector monetario y financiero |
| **FMI** (`imf.data`) | Inflación (IPC) comparada entre países, vía SDMX-CSV |
| **EIA** (EE. UU.) | Precio spot del petróleo WTI |
| **BCE** | Inflación (HICP), crecimiento del PIB real y desempleo de la eurozona |
| **China NBS** (`data.stats.gov.cn`) | Índice de Precios al Consumidor de China |
| **World Gold Council** | Precio mensual del oro desde 1833, y precio spot actual |

## Referencia de funciones

### Cuentas nacionales y actividad real

| Función | Descripción |
|----|----|
| [`get_imae()`](https://johan-rosa.github.io/databcrd/reference/get_imae.md) | IMAE, base 2018 |
| [`get_imae_2007()`](https://johan-rosa.github.io/databcrd/reference/get_imae_2007.md) | IMAE, base 2007 |
| [`get_pib_gasto()`](https://johan-rosa.github.io/databcrd/reference/get_pib_gasto.md) | PIB por el enfoque del gasto (real y nominal) |
| [`get_pib_gasto_2007()`](https://johan-rosa.github.io/databcrd/reference/get_pib_gasto_2007.md) | PIB por el enfoque del gasto, base 2007 |
| [`get_pib_sectores()`](https://johan-rosa.github.io/databcrd/reference/get_pib_sectores.md) | PIB por sector de origen (real y nominal) |
| [`get_pib_sectores_2007()`](https://johan-rosa.github.io/databcrd/reference/get_pib_sectores_2007.md) | PIB por sector de origen, base 2007 |
| [`get_fbkf()`](https://johan-rosa.github.io/databcrd/reference/get_fbkf.md) | Formación bruta de capital fijo por sector y tipo de bien |
| [`costo_canasta_familiar()`](https://johan-rosa.github.io/databcrd/reference/costo_canasta_familiar.md) | Costo de la canasta familiar |

### Precios e inflación

| Función | Descripción |
|----|----|
| [`get_ipc_data()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_data.md) | IPC por desagregación: general, grupos, regiones, subyacente, TNT |
| [`get_ipc_articulos()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_articulos.md) | IPC a nivel de artículo |
| [`get_ipc_long()`](https://johan-rosa.github.io/databcrd/reference/get_ipc_long.md) | IPC en formato largo |
| [`imf_inflation()`](https://johan-rosa.github.io/databcrd/reference/imf_inflation.md) | Inflación (IPC) del FMI para uno o varios países |
| [`china_cpi()`](https://johan-rosa.github.io/databcrd/reference/china_cpi.md) | IPC de China (portal de la NBS) |
| [`ze_inflation()`](https://johan-rosa.github.io/databcrd/reference/ze_inflation.md) | Inflación (HICP) de la eurozona |
| [`get_expectativas()`](https://johan-rosa.github.io/databcrd/reference/get_expectativas.md) | Encuesta de expectativas macroeconómicas |

### Tipo de cambio

| Función | Descripción |
|----|----|
| [`get_tc()`](https://johan-rosa.github.io/databcrd/reference/get_tc.md) / [`get_tc_eif()`](https://johan-rosa.github.io/databcrd/reference/get_tc_eif.md) | Tipo de cambio promedio de las operaciones en entidades de intermediación financiera |
| [`get_tc_spot()`](https://johan-rosa.github.io/databcrd/reference/get_tc_spot.md) | Tipo de cambio en el mercado spot |

### Tasas de interés y política monetaria

| Función | Descripción |
|----|----|
| [`get_tpm()`](https://johan-rosa.github.io/databcrd/reference/get_tpm.md) | Tasa de Política Monetaria y tasas de corto plazo asociadas |
| [`get_tasas_activas()`](https://johan-rosa.github.io/databcrd/reference/get_tasas_activas.md) | Tasas de interés activas (préstamos) |
| [`get_tasas_pasivas()`](https://johan-rosa.github.io/databcrd/reference/get_tasas_pasivas.md) | Tasas de interés pasivas (ahorros) |
| [`get_tasas_diarias()`](https://johan-rosa.github.io/databcrd/reference/get_tasas_diarias.md) | Tasas de interés diarias del BCRD |
| [`get_tasas_semanales()`](https://johan-rosa.github.io/databcrd/reference/get_tasas_semanales.md) | Tasas de interés semanales del BCRD |
| [`get_tasas_reales()`](https://johan-rosa.github.io/databcrd/reference/get_tasas_reales.md) | Tasas de interés reales de los intermediarios financieros |
| [`get_tasa_interbancaria()`](https://johan-rosa.github.io/databcrd/reference/get_tasa_interbancaria.md) | Tasas promedio de operaciones interbancarias por plazo |
| [`tasas_to_long()`](https://johan-rosa.github.io/databcrd/reference/tasas_to_long.md) | Convierte tablas de tasas de formato ancho a largo |
| [`get_encaje()`](https://johan-rosa.github.io/databcrd/reference/get_encaje.md) | Serie de encaje legal |

### Sector externo

| Función | Descripción |
|----|----|
| [`get_exportaciones()`](https://johan-rosa.github.io/databcrd/reference/get_exportaciones.md) | Exportaciones totales por sector |
| [`get_exportaciones_zf()`](https://johan-rosa.github.io/databcrd/reference/get_exportaciones_zf.md) | Exportaciones de zonas francas |
| [`get_importaciones()`](https://johan-rosa.github.io/databcrd/reference/get_importaciones.md) | Importaciones totales por sector |
| [`get_importaciones_petroleo()`](https://johan-rosa.github.io/databcrd/reference/get_importaciones_petroleo.md) | Importaciones de petróleo |
| [`get_remesas()`](https://johan-rosa.github.io/databcrd/reference/get_remesas.md) | Remesas recibidas |
| [`get_ied()`](https://johan-rosa.github.io/databcrd/reference/get_ied.md) | Inversión extranjera directa |
| [`get_reservas_internacionales()`](https://johan-rosa.github.io/databcrd/reference/get_reservas_internacionales.md) | Reservas y activos del banco central |
| [`get_embi()`](https://johan-rosa.github.io/databcrd/reference/get_embi.md) | EMBI (riesgo país) a distintas periodicidades |

### Sector fiscal y monetario

| Función | Descripción |
|----|----|
| [`fiscal_operations()`](https://johan-rosa.github.io/databcrd/reference/fiscal_operations.md) | Operaciones fiscales |
| [`fiscal_operations_gdp()`](https://johan-rosa.github.io/databcrd/reference/fiscal_operations_gdp.md) | Operaciones fiscales como % del PIB |
| [`get_indicadores_monetarios_bcrd()`](https://johan-rosa.github.io/databcrd/reference/get_indicadores_monetarios_bcrd.md) | Indicadores monetarios del BCRD |
| [`operaciones_monetarias()`](https://johan-rosa.github.io/databcrd/reference/operaciones_monetarias.md) | Operaciones monetarias |
| [`indicadores_osd()`](https://johan-rosa.github.io/databcrd/reference/indicadores_osd.md) | Indicadores de Otras Sociedades de Depósito (OSD) |
| [`get_prestamos_osd()`](https://johan-rosa.github.io/databcrd/reference/get_prestamos_osd.md) | Préstamos por sector de las OSD |

### Series internacionales de referencia

| Función | Descripción |
|----|----|
| [`gold_price()`](https://johan-rosa.github.io/databcrd/reference/gold_price.md) | Precio mensual del oro desde 1833 |
| [`today_gold_price()`](https://johan-rosa.github.io/databcrd/reference/today_gold_price.md) | Precio spot del oro (requiere `GOLDAPI_KEY`) |
| [`wti_price()`](https://johan-rosa.github.io/databcrd/reference/wti_price.md) | Precio spot del petróleo WTI (requiere `EIA_KEY`) |
| [`ze_gdpg()`](https://johan-rosa.github.io/databcrd/reference/ze_gdpg.md) | Crecimiento del PIB real de la eurozona |
| [`ze_unemployment()`](https://johan-rosa.github.io/databcrd/reference/ze_unemployment.md) | Tasa de desempleo de la eurozona |

### Utilidades

| Función | Descripción |
|----|----|
| [`crear_mes()`](https://johan-rosa.github.io/databcrd/reference/crear_mes.md) | Recodifica meses |
| [`date_label()`](https://johan-rosa.github.io/databcrd/reference/date_label.md) | Construye etiquetas de fecha legibles |
| [`rescale()`](https://johan-rosa.github.io/databcrd/reference/rescale.md) | Reescala un vector |

## Variables de entorno

Algunas fuentes requieren una API KEY propia del usuario. Configúralas
en tu `.Renviron`:

    EIA_KEY=tu_llave_de_eia
    GOLDAPI_KEY=tu_llave_de_goldapi

- `EIA_KEY`: usada por
  [`wti_price()`](https://johan-rosa.github.io/databcrd/reference/wti_price.md).
  Se obtiene gratis en <https://www.eia.gov/opendata/register.php>.
- `GOLDAPI_KEY`: usada por
  [`today_gold_price()`](https://johan-rosa.github.io/databcrd/reference/today_gold_price.md).
  Se obtiene en <https://www.goldapi.io/>. El resto de las funciones no
  requiere autenticación.

## Artículos y documentación

El sitio del paquete, generado con `pkgdown`, incluye la referencia
completa de funciones y artículos de uso (por ejemplo,
[`vignette("ipc_data")`](https://johan-rosa.github.io/databcrd/articles/ipc_data.md)
para trabajar con las distintas desagregaciones del IPC):

<https://johan-rosa.github.io/databcrd/>

## Diseño del paquete

- Cada función de descarga sigue el mismo patrón: construir la solicitud
  con `httr2`, ejecutarla a través de un wrapper centralizado con manejo
  de errores ([`tryCatch()`](https://rdrr.io/r/base/conditions.html) +
  condiciones de
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)), y
  devolver un `tibble` ya limpio con `dplyr`/`janitor`/`lubridate`.
- Las funciones están documentadas con `roxygen2`, incluyendo ejemplos
  ejecutables.
- Cuando una fuente expone una API interna o no oficial (como el portal
  de datos de la NBS de China), la lógica de bajo nivel (encabezados,
  sesión, parsing del árbol de nodos) vive en funciones internas
  separadas de la función exportada, para que el punto de entrada
  público se mantenga simple.

## Contribuir

Los *issues* y *pull requests* son bienvenidos. Antes de abrir un PR,
corre:

``` r

devtools::document()
devtools::test()
lintr::lint_package()
```

## Licencia

MIT © Johan Rosa, Juan Quiñones
