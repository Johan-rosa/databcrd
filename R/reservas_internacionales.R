#' Parse Bank Reserve and Asset Data from Excel
#'
#' Reads a structured Excel file containing monthly banking data and returns
#' a tidy dataframe with year, variable, and modalidad columns.
#'
#' @param file Path to the Excel file.
#'
#' @return A dataframe with the following columns:
#'   \describe{
#'     \item{mes}{Month number (integer).}
#'     \item{column}{Column index used for joining (integer).}
#'     \item{value}{Numeric value for the given period and category.}
#'     \item{year}{Four-digit year extracted from the header (character).}
#'     \item{variable}{Either \code{"Reservas"} or \code{"Activos"}.}
#'     \item{modalidad}{Either \code{"Brutos"} or \code{"Netos"}.}
#'     \item{date}{First day of the month as a \code{Date} object.}
#'   }
#'
#' @export
get_reservas_internacionales <- function() {
  URL <- "https://cdn.bancentral.gov.do/documents/estadisticas/sector-externo/documents/reservas_internacionales.xlsx"
  file <- tempfile(fileext = ".xlsx")
  download.file(url = URL, destfile = file, mode = "wb", quiet = TRUE)

  headers <- readxl::read_excel(
    file,
    skip = 5,
    n_max = 3,
    col_names = FALSE,
    col_types = "text"
  ) |>
    suppressMessages()

  years <- headers[1, ] |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "colum", values_to = "year") |>
    dplyr::select(-colum) |>
    tibble::rowid_to_column("column") |>
    tidyr::fill(year) |>
    dplyr::mutate(year = stringr::str_extract(year, "\\d{4}")) |>
    dplyr::slice(-1)

  variable <- headers[2, ] |>
    dplyr::mutate(dplyr::across(2, \(x) "RESERVAS")) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "colum", values_to = "variable") |>
    dplyr::select(-colum) |>
    tibble::rowid_to_column("column") |>
    tidyr::fill(variable) |>
    dplyr::slice(-1)

  modalidad <- headers[3, ] |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "colum", values_to = "modalidad") |>
    dplyr::select(-colum) |>
    tibble::rowid_to_column("column") |>
    dplyr::slice(-1)

  usethis::ui_info("Valores expresados en millones de USD.")

  readxl::read_excel(file, skip = 8, n_max = 12, col_names = FALSE) |>
    suppressMessages() |>
    tidyr::pivot_longer(-c(1), names_to = "column", values_to = "value") |>
    dplyr::mutate(column = readr::parse_number(stringr::str_extract(column, "\\d+"))) |>
    dplyr::rename(mes = 1) |>
    dplyr::left_join(years,   , by = "column") |>
    dplyr::left_join(variable , by = "column") |>
    dplyr::left_join(modalidad, by = "column") |>
    dplyr::mutate(
      dplyr::across(c(variable, modalidad), stringr::str_to_title),
      mes = crear_mes(mes),
      date = lubridate::make_date(year, mes, 1),
      modalidad = stringr::str_extract(modalidad, "\\w+"),
      modalidad = dplyr::case_match(
        modalidad,
        "Brutas" ~ "Brutos",
        "Netas"  ~ "Netos",
        .default = modalidad
      ),
    ) |>
    dplyr::select(-column) |>
    dplyr::relocate(fecha = date, year, mes, variable, modalidad) |>
    dplyr::arrange(fecha)
}
