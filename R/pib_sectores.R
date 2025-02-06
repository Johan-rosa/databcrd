#' GDP by the sector of origin, both real and nominal
#'
#' @param modalidad string indicating if "real" or "nominal"
#' @param acumulado logical, by default FALSE
#' @param homogenea_91 logical, by default FALSE
#'
#' @return a data frame
#' @export
get_pib_sectores <- function(
    modalidad    = "real",
    acumulado    = FALSE,
    homogenea_91 = FALSE
) {
  checkmate::assert_choice(modalidad, c("nominal", "real"))
  checkmate::assert_logical(acumulado, any.missing = FALSE, max.len = 1)
  checkmate::assert_logical(homogenea_91, any.missing = FALSE, max.len = 1)
  # nolint start
  url_2007 <- "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_origen_2007.xlsx"
  url_retro <- "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_origen_retro.xlsx"
  # nolint end

  temp_file <- tempfile(fileext = ".xlsx")
  url <- if (homogenea_91) url_retro else url_2007
  start_date <- if (homogenea_91) "1991-01-01" else "2007-01-01"

  utils::download.file(url, temp_file, mode = "wb", quiet = TRUE)

  sheet <- dplyr::case_when(
    modalidad == "nominal" & acumulado  ~ "PIB$_Trim_Acum",
    modalidad == "nominal" & !acumulado ~ "PIB$_Trim",
    modalidad == "real"    & acumulado  ~ "PIBK_Trim_Acum",
    modalidad == "real"    & !acumulado ~ "PIBK_Trim"
  )

  to_read <- list(
    nominal = list(
      list(skip = 9,  nmax = 31,  indicator_name = "pib_nominal"),
      list(skip = 45, nmax = 31,  indicator_name = "ponderacion")
    ),
    real = list(
      list(skip = 9,  nmax = 31, indicator_name = "indice"),
      list(skip = 45, nmax = 31, indicator_name = "crecimiento_interanual"),
      list(skip = 81, nmax = 31, indicator_name = "incidencia")
    )
  )

  read_pib_indicator <- function(skip, nmax, indicator_name) {
    suppressMessages(
      pib_sectores <- readxl::read_excel(
        path = temp_file,
        sheet = sheet,
        skip = skip,
        n_max = nmax,
        col_names = FALSE
      )
    )

    quarters <- seq(
      as.Date(start_date),
      by = "quarter",
      length.out = ncol(pib_sectores) - 1
    )

    pib_sectores |>
      stats::setNames(c("sector", quarters)) |>
      dplyr::filter(!is.na(sector)) |>
      tidyr::pivot_longer(
        cols = -sector,
        names_to = "trimestre",
        values_to = indicator_name) |>
      dplyr::mutate(
        trimestre = as.numeric(trimestre),
        fecha = lubridate::as_date(trimestre),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) |>
      dplyr::select(
        sector, fecha, year, trimestre, dplyr::all_of(indicator_name))
  }

  to_read[[modalidad]] |>
    purrr::map(\(args) do.call(read_pib_indicator, args)) |>
    purrr::reduce(
      dplyr::left_join,
      by = c("sector", "fecha", "year", "trimestre")
    )
}
