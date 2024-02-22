
#' Download daily interest rate
#'
#' Get the series of daily interest rate of the Dominican Republic.
#'
#' @return a data frame with monthly series
#' @export
#'
#' @examples
#' get_fbkf()

get_tasas_diarias <- function() {
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "estadisticas/sector-monetario-y-financiero/",
                     "documents/tasas_diariasBM-2023.xlsx")
  file_path <- tempfile(pattern = "", fileext = "")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  suppressMessages(
    data <- readxl::read_excel(
      path = file_path,
      skip = 11,
      trim_ws = TRUE,
      col_names = FALSE
    )
  )

  data2 <- data |>
    apply()


    dplyr::mutate(month = readr::parse_vector(`...1`, readr::col_character()))


}
