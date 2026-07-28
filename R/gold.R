#' Monthly Gold Prices Since 1833
#'
#' Retrieves a monthly time series of gold prices in U.S. dollars (USD)
#' beginning in 1833. The data is sourced from the World Gold Council and
#' is derived from historical records compiled by Timothy Green, supplemented
#' with data provided by the World Bank.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{date}{A `Date` representing the month of the observation.}
#'   \item{gold_price}{Monthly gold price in U.S. dollars (USD).}
#' }
#'
#' @details
#' The data is downloaded from the DataHub mirror of the World Gold Council's
#' historical gold price dataset each time the function is called, ensuring
#' that the latest available version is retrieved.
#'
#' @source
#' World Gold Council. *Gold Price History Since 1833*.
#' Historical series compiled by Timothy Green and supplemented with data
#' from the World Bank.
#' <https://datahub.io/core/gold-prices>
#'
#' @export
gold_price <- function() {
  url <- "https://datahub.io/core/gold-prices/_r/-/data/monthly-processed.csv"
  file <- tempfile(fileext = ".csv")
  download.file(url, file, mode = "wb", quiet = TRUE)

  readr::read_csv(file) |>
    dplyr::rename(date = Date, gold_price = Price)
}

#' Retrieve the current gold spot price
#'
#' Queries the GoldAPI service for the current spot price of one troy ounce
#' of gold (XAU) quoted in U.S. dollars (USD).
#'
#' An active GoldAPI key is required. By default, the function reads the key
#' from the `GOLDAPI_KEY` environment variable.
#'
#' @param key A character string containing a valid GoldAPI key.
#'   Defaults to `Sys.getenv("GOLDAPI_KEY")`.
#'
#' @return
#' A tibble containing the JSON response returned by GoldAPI. Timestamp
#' variables are converted to POSIXct.
#'
#' @details
#' This function queries the endpoint
#' `https://www.goldapi.io/api/XAU/USD`.
#'
#' @examples
#' \dontrun{
#' today_gold_price()
#' }
#'
#' @export
today_gold_price <- function(key = Sys.getenv("GOLDAPI_KEY")) {

  if (is.null(key) || key == "") {
    rlang::abort(
      "Provide an active GoldAPI key. Obtain an API key from <https://www.goldapi.io/>."
    )
  }

  result <- tryCatch(
    httr2::request("https://www.goldapi.io/api/XAU/USD") |>
      httr2::req_headers(
        "x-access-token" = key
      ) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),

    error = function(e) {
      rlang::abort(
        paste(
          "Unable to retrieve the current gold price.",
          conditionMessage(e)
        ),
        parent = e
      )
    }
  )

  if (httr2::resp_status(result) >= 400) {

    message <- tryCatch(
      httr2::resp_body_json(result)$error,
      error = function(...) httr2::resp_status_desc(result)
    )

    rlang::abort(
      paste0(
        "GoldAPI request failed (",
        httr2::resp_status(result),
        "): ",
        message
      )
    )
  }

  httr2::resp_body_json(result) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      timestamp = lubridate::as_datetime(timestamp),
      open_time = lubridate::as_datetime(open_time)
    )
}
