
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
  paste(
    stringr::str_to_title(lubridate::month(date, label = TRUE)),
    lubridate::year(date)
  )
}

#' Rescale a vector
#'
#' With this function you can change the scale of a numeric vector providing the
#' new minimum and maximum values
#'
#' @param x a numeric vector
#' @param new_min numeric scalar with the minimum value desired
#' @param new_max numeric scalar with the maximum value desired
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' rescale(1:10)
rescale <- function(x, new_min = 0, new_max = 1) {
  checkmate::assert_numeric(x)
  checkmate::assert_numeric(new_min)
  checkmate::assert_numeric(new_max)

  old_range <-  (max(x) - min(x))
  new_range <-  (new_max - (new_min))
  new_value <-  (((x - min(x)) * new_range) / old_range) + (new_min)
  return(new_value)
}
