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
