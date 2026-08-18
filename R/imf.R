#' Get IMF CPI inflation series for a set of countries
#'
#' Downloads the total CPI index (not seasonally adjusted, monthly frequency)
#' from the IMF's CPI dataset via \code{imf.data::get_data()}, and derives
#' month-over-month and year-over-year inflation rates for each country.
#'
#' @param country_codes Character vector of IMF/ISO3 country codes to query
#'   (e.g. \code{c("DOM", "COL", "MEX")}). Default is \code{"DOM"}.
#' @param last_n_obs Integer. Number of most recent observations to request
#'   per country from the API (passed through to \code{imf.data::get_data()}'s
#'   \code{last_n_obs} argument). Default is 60.
#'
#' @details
#' The function queries the IMF CPI dataset with fixed filters for:
#' \itemize{
#'   \item \code{INDEX_TYPE = "CPI"} — the CPI index itself (as opposed to,
#'     e.g., core or other index types).
#'   \item \code{COICOP_1999 = "_T"} — the "all items" total basket, not a
#'     COICOP sub-category.
#'   \item \code{TYPE_OF_TRANSFORMATION = "IX"} — index values, not
#'     percent-change series.
#'   \item \code{FREQUENCY = "M"} — monthly data.
#' }
#' \code{SCALE} and \code{STATUS} attributes are requested from the API but
#' are not currently kept in the output (they're dropped by the final
#' \code{dplyr::select()}).
#'
#' Inflation rates are computed per country (via \code{.by = COUNTRY}) as:
#' \itemize{
#'   \item \code{vm}: month-over-month % change (index vs. 1 month prior)
#'   \item \code{vi}: year-over-year % change (index vs. 12 months prior)
#' }
#' Because \code{vm}/\code{vi} rely on lagged values within each country's
#' series, the first observations for a country will be \code{NA} unless
#' \code{last_n_obs} covers enough history before the period you actually need
#' (at least 13 months of data for \code{vi} to be non-\code{NA} on the
#' earliest requested date).
#'
#' Column names are snake_cased at the end via \code{janitor::clean_names()}.
#'
#' @return A tibble with one row per country-period, containing:
#' \describe{
#'   \item{date}{Observation date (first of month), parsed from the IMF
#'     \code{TIME_PERIOD} field via \code{lubridate::ym()}.}
#'   \item{country}{IMF/ISO3 country code.}
#'   \item{index}{CPI index value for that period (renamed from
#'     \code{OBS_VALUE}).}
#'   \item{vm}{Month-over-month inflation rate, in percent.}
#'   \item{vi}{Year-over-year inflation rate, in percent.}
#' }
#'
#' @examples
#' \dontrun{
#' # Latest 12-month inflation rate for a set of LatAm countries,
#' # ranked from highest to lowest as of March 2026
#' imf_inflation(
#'   country_codes = c("COL", "DOM", "MEX", "BRA", "PER",
#'                      "CHL", "GTM", "URY", "PRY", "CRI"),
#'   last_n_obs = 24
#' ) |>
#'   dplyr::filter(date == "2026-03-01") |>
#'   dplyr::arrange(dplyr::desc(vi))
#' }
#'
#' @export
imf_inflation <- function(
    country_codes = c("DOM"),
    last_n_obs = 60
) {
  imf.data::get_data(
    "CPI",
    filters = list(
      COUNTRY = country_codes,
      INDEX_TYPE = "CPI",
      COICOP_1999 = "_T",
      TYPE_OF_TRANSFORMATION = "IX",
      FREQUENCY = "M"
    ),
    last_n_obs = last_n_obs + 12,
    attributes = c("SCALE", "STATUS")
  ) |>
    dplyr::mutate(
      vi = OBS_VALUE / dplyr::lag(OBS_VALUE, 12) * 100 - 100,
      vm = OBS_VALUE / dplyr::lag(OBS_VALUE,  1) * 100 - 100,
      date = lubridate::ym(TIME_PERIOD),
      .by = COUNTRY
    ) |>
    dplyr::select(date, COUNTRY, index = OBS_VALUE, vm, vi) |>
    janitor::clean_names() |>
    dplyr::arrange(country, date) |>
    dplyr::slice_tail(n = last_n_obs, by = country)
}


imf_policy_rate <- function(
    country_codes = c("DOM"),
    last_n_obs = 60
) {
  imf.data::get_data(
    "MFS_IR",
    filters = list(
      COUNTRY = country_codes,
      INDICATOR = "MFS166_RT_PT_A_PT",
      FREQUENCY = "M"
    ),
    last_n_obs = last_n_obs
  ) |>
    dplyr::mutate(
      date = lubridate::ym(TIME_PERIOD),
      .by = COUNTRY
    ) |>
    dplyr::select(date, COUNTRY, policy_rate = OBS_VALUE) |>
    janitor::clean_names()
}



#' Catálogo de indicadores del Primary Commodity Price System (PCPS)
#'
#' Descarga el glosario de códigos `CL_PCPS_INDICATOR` publicado por el
#' Departamento de Investigación del FMI (IMF.RES) en su API SDMX, y lo
#' convierte en un tibble con un código de indicador por fila. El PCPS
#' cubre precios e índices de precios de más de 100 materias primas
#' (energía, agricultura, fertilizantes y metales).
#'
#' @return Un tibble con las columnas:
#' \describe{
#'   \item{id}{`chr`. Código del indicador (p. ej. `"PALLFNF"`,
#'     `"POILBRE"`), usado para filtrar en [imf_pcps()].}
#'   \item{name}{`chr`. Nombre corto del indicador en inglés (único idioma
#'     disponible en el glosario), p. ej. `"Brent Crude, US dollars per
#'     barrel, Unit prices"`.}
#'   \item{description}{`chr`. Descripción larga del indicador: fuente de
#'     la serie, metodología y unidad de medida.}
#' }
#'
#' @source
#' <https://data.imf.org/platform/rest/v1/registry/sdmx-plus/structure/glossary/IMF.RES/CL_PCPS_INDICATOR/3.0.0>
#'
#' @examples
#' \dontrun{
#' imf_pcps_catalogo()
#' }
#'
#' @export
imf_pcps_catalogo <- function() {
  url <- "https://data.imf.org/platform/rest/v1/registry/sdmx-plus/structure/glossary/IMF.RES/CL_PCPS_INDICATOR/3.0.0?references=hierarchy&detail=referencepartial"

  resp <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  resp$data$glossaries[[1]]$terms |>
    purrr::map(
      \(x) {
        dplyr::tibble(
          id = x$id,
          name = x$names$en,
          description = x$description
        )
      }
    ) |>
    purrr::list_rbind()
}

#' Códigos PCPS por defecto para [imf_pcps()]
#'
#' Selección curada de 16 indicadores del PCPS (alimentos, bebidas,
#' cereales y fertilizantes) usada como valor por defecto del argumento
#' `indicadores` en [imf_pcps()]. Ver [imf_pcps_catalogo()] para el
#' catálogo completo de códigos disponibles.
#'
#' @noRd
default_pcps_indicators <- c(
  "PALLFNF", "PALLMETA", "PALUM", "PBEEF", "PBEVE", "PCERE",
  "PCOCO", "PCOFF", "PFANDB", "PFERT", "PMAIZMT", "PSALM", "PSORG",
  "PSOYB", "PSUGA", "PWHEAMT"
)

#' Códigos PCPS por defecto para [imf_pcps()]
#'
#' Selección curada de 16 indicadores del PCPS (alimentos, bebidas,
#' cereales y fertilizantes) usada como valor por defecto del argumento
#' `indicadores` en [imf_pcps()]. Ver [imf_pcps_catalogo()] para el
#' catálogo completo de códigos disponibles.
#'
#' @noRd
default_pcps_indicators <- c(
  "PALLFNF", "PALLMETA", "PALUM", "PBEEF", "PBEVE", "PCERE",
  "PCOCO", "PCOFF", "PFANDB", "PFERT", "PMAIZMT", "PSALM", "PSORG",
  "PSOYB", "PSUGA", "PWHEAMT"
)
#' Precios de materias primas del FMI (PCPS)
#'
#' Descarga series mensuales del Primary Commodity Price System (PCPS) del
#' FMI para uno o más indicadores, y las une con sus nombres descriptivos
#' obtenidos de [imf_pcps_catalogo()].
#'
#' @details
#' La consulta siempre filtra `COUNTRY = "G001"` (agregado mundial): PCPS
#' es un sistema de precios internacionales de referencia y no reporta
#' series por país, así que este es el único código de país que existe
#' para este dataflow. Se deja explícito en el filtro en vez de depender
#' del comportamiento por defecto de [imf.data::get_data()] (que ya
#' devuelve únicamente `"G001"` si se omite el filtro), para que la
#' consulta sea autoexplicativa y no dependa de un default no documentado.
#'
#' El catálogo (`imf_pcps_catalogo()`) se descarga en cada llamada a esta
#' función; implica una llamada de red adicional solo para obtener los
#' nombres de los indicadores.
#'
#' @param indicadores Vector de códigos de indicador PCPS (ver
#'   [imf_pcps_catalogo()] para la lista completa). Por defecto,
#'   `default_pcps_indicators`: una selección curada de 16 índices y
#'   precios de alimentos, bebidas, cereales y fertilizantes.
#' @param data_transformation Código SDMX de transformación de datos.
#'   `"INDEX"` por defecto (series en forma de índice); otras
#'   transformaciones disponibles dependen del indicador.
#' @param start_period Periodo inicial de la consulta, como cadena
#'   `"YYYY-MM"` (p. ej. `"2019-01"`). Se convierte con `as.character()`
#'   antes de enviarse a la API.
#'
#' @return Un tibble con las columnas:
#' \describe{
#'   \item{id}{`chr`. Código del indicador PCPS.}
#'   \item{date}{`Date`. Primer día del mes de la observación.}
#'   \item{value}{`dbl`. Valor de la observación, en la transformación
#'     solicitada (`data_transformation`).}
#'   \item{indicador_name}{`chr`. Nombre descriptivo del indicador,
#'     obtenido de [imf_pcps_catalogo()]. `NA` si el código no aparece en
#'     el catálogo actual.}
#' }
#'
#' @source
#' <https://data.imf.org/en/datasets/IMF.RES:PCPS>
#'
#' @examples
#' \dontrun{
#' imf_pcps()
#'
#' # Solo el precio del petróleo Brent, desde 2015
#' imf_pcps(indicadores = "POILBRE", start_period = "2015-01")
#' }
#'
#' @export
imf_pcps <- function(
    indicadores = default_pcps_indicators,
    data_transformation = "INDEX",
    start_period = "2019-01"
) {
  catalogo <- imf_pcps_catalogo()

  raw_data <- imf.data::get_data(
    dataflow = "PCPS",
    agency_id = "IMF.RES",
    filters = list(
      COUNTRY = "G001",
      FREQUENCY = "M",
      INDICATOR = indicadores,
      DATA_TRANSFORMATION = data_transformation
    ),
    start_period = as.character(start_period)
  )

  raw_data |>
    janitor::clean_names() |>
    dplyr::mutate(
      fecha = lubridate::ym(time_period)
    ) |>
    dplyr::select(id = indicator, fecha, value = obs_value) |>
    dplyr::left_join(
      dplyr::select(catalogo, id, indicador_name = name),
      by = "id"
    )
}

#' Proyecciones del World Economic Outlook (WEO) del FMI
#'
#' Descarga las últimas observaciones de inflación o crecimiento del PIB
#' del World Economic Outlook (WEO) del FMI para uno o más países, en
#' formato largo o ancho.
#'
#' @details
#' El WEO se publica dos veces al año (abril y octubre) y mezcla, en la
#' misma serie, años de dato real (histórico) y años de proyección, sin
#' que la respuesta de la API indique cuál es cuál. No encontramos un
#' atributo SDMX confiable para separar automáticamente lo real de lo
#' proyectado con el paquete `imf.data` (se intentó
#' `LATEST_ACTUAL_ANNUAL_DATA`, pero no existe en este dataflow). Por
#' ahora, quien use esta función debe determinar el corte real/proyectado
#' por su cuenta, por ejemplo contrastando el año contra la fecha de
#' publicación del vintage de WEO consultado.
#'
#' `indicador` traduce dos etiquetas amigables a sus códigos WEO
#' correspondientes:
#' \describe{
#'   \item{`"inflacion"`}{→ `PCPIEPCH`, inflación de **fin de periodo**,
#'     %. No es la inflación promedio (`PCPIPCH`), que es la cifra que
#'     habitualmente se cita como "la" inflación anual; esta función no
#'     da acceso a `PCPIPCH` por ahora.}
#'   \item{`"gdp"`}{→ `NGDP_RPCH`, crecimiento del PIB **real**, %. Esta
#'     función no da acceso al PIB nominal (`NGDPD`) por ahora.}
#' }
#' Para usar otro código WEO (`PCPIPCH`, `NGDPD`, o cualquier otro),
#' hay que agregar el caso correspondiente al `switch()` interno; con la
#' interfaz actual no es posible pasar un código SDMX directamente.
#'
#' Solo se puede pedir un indicador por llamada; si se necesitan varios
#' (p. ej. inflación y PIB juntos), hay que llamar la función dos veces y
#' combinar los resultados con `dplyr::bind_rows()` o un `dplyr::*_join()`
#' según el formato.
#'
#' `year` se convierte a entero (antes quedaba como texto, heredado de
#' `TIME_PERIOD`).
#'
#' El formato `"wide"` solo tiene sentido con un único indicador por
#' llamada: pivota a una columna por país, indexada por año.
#'
#' @param indicador `"inflacion"` (por defecto) o `"gdp"`. Ver `@details`
#'   para el código WEO exacto al que traduce cada etiqueta.
#' @param last_n_obs Número de observaciones más recientes a traer por
#'   país (por defecto 6).
#' @param countries Vector de códigos de país ISO-3 del FMI (por defecto
#'   `c("DOM", "CHN", "USA")`).
#' @param format `"long"` (por defecto, una fila por país y año) o
#'   `"wide"` (una columna por país, indexada por año).
#'
#' @return
#' En formato `"long"`, un tibble con (al menos) las columnas:
#' \describe{
#'   \item{country}{`chr`. Código de país ISO-3.}
#'   \item{indicator}{`chr`. Código WEO real al que tradujo `indicador`
#'     (`"PCPIEPCH"` o `"NGDP_RPCH"`).}
#'   \item{year}{`int`. Año de la observación.}
#'   \item{value}{`dbl`. Valor de la observación: % para ambos
#'     indicadores disponibles actualmente.}
#' }
#' En formato `"wide"`, un tibble con columna `year` y una columna
#' adicional por cada país en `countries`, con el valor de `indicador`.
#'
#' @source
#' <https://data.imf.org/en/datasets/IMF.RES:WEO>
#'
#' @examples
#' \dontrun{
#' imf_weo_forecast()
#'
#' # Crecimiento del PIB real, formato ancho
#' imf_weo_forecast(indicador = "gdp", format = "wide")
#' }
#'
#' @export
imf_weo_forecast <- function(
    indicador = c("inflacion", "gdp"),
    last_n_obs = 6,
    countries = c("DOM", "CHN", "USA"),
    format = c("long", "wide")
) {
  indicador <- rlang::arg_match(indicador)
  format <- rlang::arg_match(format)

  indicador <- switch (indicador,
    gdp = "NGDP_RPCH",
    inflacion = "PCPIEPCH"
  )

  data <- imf.data::get_data(
    dataflow = "WEO",
    agency_id = "IMF.RES",
    filters = list(
      INDICATOR = indicador,
      COUNTRY   = countries
    ),
    last_n_obs = last_n_obs
  ) |>
    janitor::clean_names() |>
    dplyr::rename(value = obs_value, year = time_period) |>
    dplyr::mutate(year = as.integer(year))
  if (format == "wide") {
    data <- data |>
      dplyr::select(year, country, value) |>
      tidyr::pivot_wider(names_from = country, values_from = value)
  }
  data
}
