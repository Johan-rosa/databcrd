#' Download EMBI
#'
#' Download EMBI of different countries
#'
#' @param periodicidad string with the desired periodicity options:
#' "diario", "semanal", "mensual", "trimestral", "anual".
#'
#' @export
#'
#' @return a tibble
#' @examples
#' get_embi("diario")
#' get_embi("semanal")
#' get_embi("mensual")
#' get_embi("trimestral")
#' get_embi("anual")
get_embi <- function(periodicidad) {
  checkmate::assert_choice(
    periodicidad,
    choices = c("diario", "semanal", "mensual", "trimestral", "anual")
  )
  
  per_table <- tibble::tibble(x = c("diario", "semanal", 
                                    "mensual", "trimestral", "anual"),
                              y = c("day", "week", "month", "quarter", "year"))
  
  periodicidad <- per_table |> 
    dplyr::filter(x == periodicidad) |> 
    dplyr::pull(y)

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "entorno-internacional/documents/",
    "Serie_Historica_Spread_del_EMBI.xlsx")
  
  file_path <- tempfile(pattern = "", fileext = ".xlsx")
  
  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)
  
  suppressWarnings(
    suppressMessages(
      embi <- readxl::read_excel(
        file_path,
        sheet = 1,
        col_names = TRUE,
        col_types = c("text", rep("numeric", 20)),
        skip = 1,
        na = "N/A") |> 
        dplyr::mutate(
          Fecha = dplyr::case_when(
            stringr::str_detect(Fecha, "^\\d+$") ~ janitor::excel_numeric_to_date(as.numeric(Fecha)),
            TRUE ~ lubridate::dmy(Fecha)
          )
        )
    )
  )
  
  
  embi <- embi |> 
    dplyr::mutate(Fecha = lubridate::floor_date(Fecha, periodicidad)) |> 
    dplyr::group_by(Fecha) |> 
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   .f = \(x) mean(x, na.rm = TRUE)))
  
}