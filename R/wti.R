#' Download WTI crude oil spot prices from the EIA API
#'
#' Retrieves monthly West Texas Intermediate (WTI) spot prices for Cushing,
#' Oklahoma from the U.S. Energy Information Administration (EIA) API.
#'
#' @param start <character>
#'   First month to retrieve, in `"YYYY-MM"` format.
#'   Defaults to `"2022-01"`.
#' @param key <character>
#'   EIA API key. Defaults to `Sys.getenv("EIA_KEY")`.
#'
#' @return
#' A tibble containing monthly WTI spot prices and associated metadata returned
#' by the EIA API. The `value` column contains the price in U.S. dollars per
#' barrel.
#'
#' @details
#' This function queries the EIA v2 API series `"RWTC"`, which corresponds to
#' the Cushing, Oklahoma WTI spot price (FOB).
#'
#' @seealso
#' Obtain an API key from <https://www.eia.gov/opendata/>.
#' Try <https://www.eia.gov/opendata/browser/petroleum/pri/spt>
#'
#' @export
wti_price <- function(
    frequency = c("monthly", "daily", "annual", "weekly"),
    start = "2022-01",
    key = Sys.getenv("EIA_KEY"),
    remove_metadata = TRUE
) {
  if (is.null(key) | key == "") {
    rlang::abort("Provide an active EIA API key. Obtain an API key from <https://www.eia.gov/opendata/>.")
  }
  frequency <- rlang::arg_match(frequency)

  resp <- httr2::request("https://api.eia.gov") |>
    httr2::req_url_path_append("v2", "petroleum", "pri", "spt", "data") |>
    httr2::req_url_query(
      api_key = key,
      frequency = frequency,
      "start" = start,
      "data[0]" = "value",
      "facets[series][]" = "RWTC",
      offset = 0,
      length = 5000
    ) |>
    httr2::req_perform()

  data <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    (\(x) x$response$data)() |>
    janitor::clean_names() |>
    dplyr::mutate(value = as.numeric(value)) |>
    dplyr::as_tibble()


  parse_date <- list(
    monthly = lubridate::ym,
    annual  = \(x) lubridate::ymd(paste0(x, "-01-01")),
    weekly  = lubridate::ymd,
    daily   = lubridate::ymd
  )

  if (remove_metadata) {
    data <- data |>
      dplyr::select(period, product_name, value)
  }

  data |>
    dplyr::mutate(date = parse_date[[frequency]](period)) |>
    dplyr::relocate(dplyr::any_of(c("date", "year"))) |>
    dplyr::select(-period)
}
