# Proyecciones del World Economic Outlook (WEO) del FMI

Descarga las últimas observaciones de inflación o crecimiento del PIB
del World Economic Outlook (WEO) del FMI para uno o más países, en
formato largo o ancho.

## Usage

``` r
imf_weo_forecast(
  indicador = c("inflacion", "gdp"),
  last_n_obs = 6,
  countries = c("DOM", "CHN", "USA"),
  format = c("long", "wide")
)
```

## Source

<https://data.imf.org/en/datasets/IMF.RES:WEO>

## Arguments

- indicador:

  `"inflacion"` (por defecto) o `"gdp"`. Ver `@details` para el código
  WEO exacto al que traduce cada etiqueta.

- last_n_obs:

  Número de observaciones más recientes a traer por país (por defecto
  6).

- countries:

  Vector de códigos de país ISO-3 del FMI (por defecto
  `c("DOM", "CHN", "USA")`).

- format:

  `"long"` (por defecto, una fila por país y año) o `"wide"` (una
  columna por país, indexada por año).

## Value

En formato `"long"`, un tibble con (al menos) las columnas:

- country:

  `chr`. Código de país ISO-3.

- indicator:

  `chr`. Código WEO real al que tradujo `indicador` (`"PCPIEPCH"` o
  `"NGDP_RPCH"`).

- year:

  `int`. Año de la observación.

- value:

  `dbl`. Valor de la observación: % para ambos indicadores disponibles
  actualmente.

En formato `"wide"`, un tibble con columna `year` y una columna
adicional por cada país en `countries`, con el valor de `indicador`.

## Details

El WEO se publica dos veces al año (abril y octubre) y mezcla, en la
misma serie, años de dato real (histórico) y años de proyección, sin que
la respuesta de la API indique cuál es cuál. No encontramos un atributo
SDMX confiable para separar automáticamente lo real de lo proyectado con
el paquete `imf.data` (se intentó `LATEST_ACTUAL_ANNUAL_DATA`, pero no
existe en este dataflow). Por ahora, quien use esta función debe
determinar el corte real/proyectado por su cuenta, por ejemplo
contrastando el año contra la fecha de publicación del vintage de WEO
consultado.

`indicador` traduce dos etiquetas amigables a sus códigos WEO
correspondientes:

- `"inflacion"`:

  → `PCPIEPCH`, inflación de **fin de periodo**, %. No es la inflación
  promedio (`PCPIPCH`), que es la cifra que habitualmente se cita como
  "la" inflación anual; esta función no da acceso a `PCPIPCH` por ahora.

- `"gdp"`:

  → `NGDP_RPCH`, crecimiento del PIB **real**, %. Esta función no da
  acceso al PIB nominal (`NGDPD`) por ahora.

Para usar otro código WEO (`PCPIPCH`, `NGDPD`, o cualquier otro), hay
que agregar el caso correspondiente al
[`switch()`](https://rdrr.io/r/base/switch.html) interno; con la
interfaz actual no es posible pasar un código SDMX directamente.

Solo se puede pedir un indicador por llamada; si se necesitan varios (p.
ej. inflación y PIB juntos), hay que llamar la función dos veces y
combinar los resultados con
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
o un `dplyr::*_join()` según el formato.

`year` se convierte a entero (antes quedaba como texto, heredado de
`TIME_PERIOD`).

El formato `"wide"` solo tiene sentido con un único indicador por
llamada: pivota a una columna por país, indexada por año.

## Examples

``` r
if (FALSE) { # \dontrun{
imf_weo_forecast()

# Crecimiento del PIB real, formato ancho
imf_weo_forecast(indicador = "gdp", format = "wide")
} # }
```
