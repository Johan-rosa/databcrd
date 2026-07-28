
#' Monetary Indicators of the Central Bank of the Dominican Republic
#'
#' @return a data frame
#' @export
get_indicadores_monetarios_bcrd <- function() { #nolint
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-monetario-y-financiero/documents/serie_indicadores_bcrd.xlsx"
  )

  temp_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url, temp_path, quiet = TRUE, mode = "wb")

  suppressMessages(
    indicadores_bcrd <- readxl::read_excel(
      path = temp_path,
      skip = 4,
      col_names = TRUE, n_max = 53, na = c("n.d.")
    )
  )

  names(indicadores_bcrd) <- c("descripcion", names(indicadores_bcrd)[-1])

  indicadores_bcrd <- indicadores_bcrd |>
    dplyr::filter(!is.na(descripcion)) |>
    dplyr::rowwise() |>
    dplyr::mutate(remove = any(!is.na(dplyr::c_across(-descripcion)))) |>
    dplyr::ungroup() |>
    dplyr::filter(remove) |>
    dplyr::select(-remove) |>
    dplyr::bind_cols(indicadores_bcrd_details)

  indicadores_bcrd <- indicadores_bcrd |>
    tidyr::pivot_longer(
      cols = -c(
        descripcion, short_names, categoria, nivel, original_names, labels,
        direct_parent
        ),
      names_to = "fecha", values_to = "values") |>
    dplyr::group_by(short_names, nivel) |>
    dplyr::mutate(
      fecha = seq(
        as.Date("1996-01-01"),
        length.out = dplyr::n(),
        by = "month")
      ) |>
    dplyr::ungroup()

  indicadores_bcrd
}

#' Monetary Operations
#'
#' Downloads and returns data on the Central Bank of the Dominican Republic's
#' monetary operations. The dataset reports the amounts transacted through
#' the Bank's liquidity absorption and liquidity injection facilities.
#'
#' @return
#' A tibble with the following variables:
#' \describe{
#'   \item{date}{Reference period.}
#'   \item{ventanilla_depositos}{Short-term remunerated deposit facility (Spanish: *Ventanilla Directa de Depósitos Remunerados de Corto Plazo*).}
#'   \item{subasta_letras}{One-day Central Bank bill auctions (Spanish: *Subasta de Letras a un día*).}
#'   \item{operaciones_contraccion}{Subtotal of liquidity absorption operations (Spanish: *Subtotal de Operaciones de Contracción*).}
#'   \item{ventanilla_repos}{Direct repo facility (Spanish: *Ventanilla Directa de Repos*).}
#'   \item{subasta_repos}{One-day repo auctions (Spanish: *Subasta de Repos a un día*).}
#'   \item{operaciones_expansion}{Subtotal of liquidity injection operations (Spanish: *Subtotal de Operaciones de Expansión*).}
#' }
#'
#' @details
#' Data are downloaded directly from the Central Bank of the Dominican
#' Republic.
#'
#' @examples
#' operaciones_monetarias()
#'
#' @export
operaciones_monetarias <- function() {
  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "sector-monetario-y-financiero/documents/operaciones_monetarias.xlsx"
  )

  file_path <- tempfile(fileext = ".xlsx")
  download_file(url, file_path)

  columns <- c(
    "periodo", "ventanilla_depositos", "subasta_letras",
    "operaciones_contraccion", "ventanilla_repos",
    "subasta_repos", "operaciones_expansion"
  )

  readxl::read_excel(file_path, skip = 1008, col_names = FALSE) |>
    suppressMessages() |>
    janitor::clean_names() |>
    janitor::remove_empty(c("rows", "cols")) |>
    dplyr::filter(!dplyr::if_all(-x1, ~ . == 0)) |>
    purrr::set_names(columns) |>
    dplyr::mutate(
      periodo = stringr::str_remove(periodo, "[\\*\\.]"),
      date = lubridate::dmy(periodo, locale = "ES") |> suppressWarnings(),
      date = dplyr::coalesce(date, janitor::excel_numeric_to_date(as.numeric(periodo))) |>
        suppressWarnings(),
      year = lubridate::year(date),
      mes  = lubridate::month(date),
      day  = lubridate::day(date)
    ) |>
    dplyr::relocate(date, year, mes, day) |>
    dplyr::select(-periodo)
}
