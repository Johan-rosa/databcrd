#' Family Basket Cost
#'
#' Downloads and returns the monthly cost of the Dominican Republic's
#' family basket (`Canasta Familiar`) published by the Central Bank of the
#' Dominican Republic. Data are available either by income quintile or by
#' geographic region.
#'
#' @param by Character. Level of disaggregation. One of:
#' \describe{
#'   \item{"regiones"}{Family basket cost by geographic region.}
#'   \item{"quintiles"}{Family basket cost by household income quintile.}
#' }
#'
#' @param format Character. Output format:
#' \describe{
#'   \item{"long"}{Returns a tidy data frame with one observation per
#'   date and group.}
#'   \item{"wide"}{Returns the original table in wide format.}
#' }
#'
#' @return
#' A tibble containing the monthly family basket cost.
#'
#' When `format = "long"`, the returned data frame contains:
#' \describe{
#'   \item{date}{Date corresponding to the first day of each month.}
#'   \item{year}{Year.}
#'   \item{mes}{Month.}
#'   \item{regiones or quintiles}{Grouping variable specified by `by`.}
#'   \item{costo}{Family basket cost in Dominican pesos (DOP).}
#' }
#'
#' When `format = "wide"`, the original publication layout is returned with
#' cleaned column names.
#'
#' @details
#' The data are downloaded directly from the Central Bank of the Dominican
#' Republic and correspond to the 2019--2020 reference base.
#'
#' @examples
#' # Family basket cost by income quintile
#' costo_canasta_familiar(by = "quintiles")
#'
#' # Family basket cost by region
#' costo_canasta_familiar(by = "regiones")
#'
#' # Wide format
#' costo_canasta_familiar(
#'   by = "quintiles",
#'   format = "wide"
#' )
#'
#' @export
costo_canasta_familiar <- function(
    by = c("regiones", "quintiles"),
    format = c("long", "wide")
) {
  by     <- rlang::arg_match(by)
  format <- rlang::arg_match(format)

  params <- list(
    quintiles = list(
      url = paste0(
        "https://cdn.bancentral.gov.do/documents/estadisticas/precios/documents/",
        "Costo_Canasta_quintiles_base_2019-2020.xlsx"
      ),
      ext = ".xlsx",
      skip = 3
    ),
    regiones = list(
      url = paste0(
        "https://cdn.bancentral.gov.do/documents/estadisticas/precios/",
        "documents/Costo_Canasta_regiones_base_2019-2020.xls"
      ),
      ext = ".xls",
      skip = 5
    )
  )

  cfg <- params[[by]]
  file_path <- tempfile(fileext = cfg$ext)

  download_file(cfg$url, file_path)
  on.exit(unlink(file_path), add = TRUE)

  data <- readxl::read_excel(file_path, skip = cfg$skip) |>
    suppressMessages() |>
    dplyr::rename("year" = 1, "mes" = 2) |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    tidyr::fill(year) |>
    dplyr::filter(stringr::str_detect(year, "^\\d{4}")) |>
    dplyr::mutate(
      year = as.numeric(year),
      mes = crear_mes(mes),
      date = lubridate::make_date(year, mes, 1)
    ) |>
    dplyr::relocate(date)

  if (format == "wide") {
    data <- data |>
      janitor::clean_names()

    return(data)
  }

  data |>
    tidyr::pivot_longer(-c(date, year, mes), names_to = by, values_to = "costo") |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(by), \(x) stringr::str_remove(x, "\\*"))
    )
}
