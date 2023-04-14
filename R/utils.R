
#' Create date label
#'
#' Create label in "Month Year" format out of a date object
#'
#' @param date a vector of class Date
#'
#' @return a character string
#' @export
date_label <- function(date = Sys.Date()) {
  checkmate::assert_date(date)
  paste(stringr::str_to_title(lubridate::month(date, label = TRUE)), lubridate::year(date))
}
