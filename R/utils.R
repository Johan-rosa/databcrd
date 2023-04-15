
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

#' change the scale of a vector
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

#' Change month encoding
#'
#' Take month from text to number or form number to text. This work with any
#' month name (Spanish or English) to create a number. From number to text only
#' creates Spanish months
#'
#' @param mes a number or character with the month
#' @param type a character indicating the type of conversion,
#' can be any of these:
#' \code{c("text_to_number", "number_to_text", "number_to_shorttext")}
#'
#' @export
#'
#' @examples
#' crear_mes("Enero", "text_to_number")
crear_mes <- function(mes, type = "text_to_number") {
  # Input validation
  checkmate::assertChoice(
    type, c("text_to_number", "number_to_text", "number_to_shorttext"))

  if (is.character(mes)) {
    checkmate::assert_choice(type, c("text_to_number"))
  } else if (is.numeric(mes)) {
    checkmate::assert(
      checkmate::check_choice(type, c("number_to_text", "number_to_shorttext")),
      all(mes %in% 1:12),
      combine = "and"
    )
  }

  if (type == "number_to_text") {
    new_mes <- dplyr::recode(
      mes,
      `1` = "Enero",
      `2` = "Febrero",
      `3` = "Marzo",
      `4` = "Abril",
      `5` = "Mayo",
      `6` = "Junio",
      `7` = "Julio",
      `8` = "Agosto",
      `9` = "Septiembre",
      `10` = "Octubre",
      `11` = "Noviembre",
      `12` = "Diciembre")
  }

  if (type == "number_to_shorttext") {
    new_mes <- dplyr::recode(
      mes,
      `1` = "Ene",
      `2` = "Feb",
      `3` = "Mar",
      `4` = "Abr",
      `5` = "May",
      `6` = "Jun",
      `7` = "Jul",
      `8` = "Ago",
      `9` = "Sep",
      `10` = "Oct",
      `11` = "Nov",
      `12` = "Dic")
  }

  if (type == "text_to_number") {
    mes  <-  stringr::str_to_title(mes)
    new_mes <- dplyr::recode(
      mes,
      "Jan" = 01,
      "Ene" = 01,
      "Feb" = 02,
      "Mar" = 03,
      "Abr" = 04,
      "May" = 05,
      "Jun" = 06,
      "Jul" = 07,
      "Ago" = 08,
      "Sep" = 09,
      "Sept" = 09,
      "Oct" = 10,
      "Nov" = 11,
      "Dic" = 12,

      "Enero" = 01,
      "Febrero" = 02,
      "Marzo" = 03,
      "Abril" = 04,
      "Mayo" = 05,
      "Junio" = 06,
      "Julio" = 07,
      "Agosto" = 08,
      "Septiembre" = 09,
      "Octubre" = 10,
      "Noviembre" = 11,
      "Diciembre" = 12,

      "January" = 01,
      "February" = 02,
      "March" = 03,
      "April" = 04,
      "May" = 05,
      "June" = 06,
      "July" = 07,
      "August" = 08,
      "September" = 09,
      "October" = 10,
      "November" = 11,
      "December" = 12)
  }

  return(new_mes)
}
