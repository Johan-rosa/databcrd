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
