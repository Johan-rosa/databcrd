#' Download euro area inflation data
#'
#' Retrieves the monthly Harmonised Index of Consumer Prices (HICP) inflation
#' series for the euro area from the European Central Bank (ECB).
#'
#' @return
#' A tibble with two columns:
#' \describe{
#'   \item{date}{Observation date.}
#'   \item{inflation}{Monthly HICP inflation rate.}
#' }
#'
#' @details
#' The data correspond to the ECB series
#' `"HICP.M.U2.N.000000.4D0.ANR"`, which reports the annual rate of change of
#' the Harmonised Index of Consumer Prices (HICP) for the euro area.
#'
#' @seealso
#' Obtain additional information about the ECB Data Portal at
#' <https://data.ecb.europa.eu/>.
#'
#' @export
ze_inflation <- function() {
  endpoint <- "https://data.ecb.europa.eu/data-detail-api/HICP.M.U2.N.000000.4D0.ANR"

  resp <- httr2::request(endpoint) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  tibble::as_tibble(data) |>
    dplyr::transmute(
      date = as.Date(PERIOD),
      inflation = as.numeric(OBS)
    ) |>
    dplyr::arrange(date)
}

#' Download euro area real GDP growth data
#'
#' Retrieves the quarterly growth rate of real gross domestic product (GDP) for
#' the euro area from the European Central Bank (ECB).
#'
#' @return
#' A tibble with two columns:
#' \describe{
#'   \item{date}{Observation date.}
#'   \item{growth}{Quarterly real GDP growth rate.}
#' }
#'
#' @details
#' The data correspond to the ECB series
#' `"MNA.Q.Y.I10.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.G1"`, which reports quarterly
#' growth in real GDP (volume) for the euro area.
#'
#' @seealso
#' Additional information is available from the ECB Data Portal:
#' <https://data.ecb.europa.eu/>.
#'
#' @export
ze_gdpg <- function() {
  endpoint <- "https://data.ecb.europa.eu/data-detail-api/MNA.Q.Y.I10.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.G1"

  resp <- httr2::request(endpoint) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  tibble::as_tibble(data) |>
    dplyr::transmute(
      date = as.Date(PERIOD),
      growth = as.numeric(OBS)
    ) |>
    dplyr::arrange(date)
}

#' Download euro area unemployment data
#'
#' Retrieves the monthly unemployment rate for the euro area from the European
#' Central Bank (ECB).
#'
#' @return
#' A tibble with two columns:
#' \describe{
#'   \item{date}{Observation date.}
#'   \item{unemployment}{Unemployment rate.}
#' }
#'
#' @details
#' The data correspond to the ECB series
#' `"LFSI.M.I10.S.UNEHRT.TOTAL0.15_74.T"`, which reports the harmonized monthly
#' unemployment rate for persons aged 15 to 74 in the euro area.
#'
#' @seealso
#' Additional information is available from the ECB Data Portal:
#' <https://data.ecb.europa.eu/>.
#'
#' @examples
#' unemployment <- euro_area_unemployment()
#'
#' @export
ze_unemployment  <- function() {
  endpoint <- "https://data.ecb.europa.eu/data-detail-api/LFSI.M.I10.S.UNEHRT.TOTAL0.15_74.T"

  resp <- httr2::request(endpoint) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  tibble::as_tibble(data) |>
    dplyr::transmute(
      date = as.Date(PERIOD),
      unemployment = as.numeric(OBS)
    ) |>
    dplyr::arrange(date)
}
