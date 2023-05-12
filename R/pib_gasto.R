#' GDP by the expenditure approach, both real and nominal
#'
#' @param modalidad string indicating if "real" or "nominal"
#' @param acumulado logical, by default FALSE
#' @param homogenea_91 logical, by default FALSE
#'
#' @return a data frame
#' @export
get_pib_gasto <- function(
    modalidad = "nominal",
    acumulado = FALSE,
    homogenea_91 = FALSE
) {
  checkmate::assert_choice(modalidad, c("nominal", "real"))
  checkmate::assert_logical(acumulado, any.missing = FALSE, max.len = 1)
  checkmate::assert_logical(homogenea_91, any.missing = FALSE, max.len = 1)

  temp_file <- tempfile(fileext = ifelse(homogenea_91, ".xlsx", ".xls"))
  start_date <- ifelse(homogenea_91, "1991-01-01", "2007-01-01")

  url <- ifelse(
    homogenea_91,
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_gasto_retro.xlsx", # nolint
    "https://cdn.bancentral.gov.do/documents/estadisticas/sector-real/documents/pib_gasto_2007.xls"    # nolint
  )

  utils::download.file(url, temp_file, mode = "wb", quiet = TRUE)

  sheet <- dplyr::case_when(
    modalidad == "nominal" & acumulado  ~ "PIB$_Trim_Acum",
    modalidad == "nominal" & !acumulado ~ "PIB$_Trim",
    modalidad == "real"    & acumulado  ~ "PIBK_Trim_Acum",
    modalidad == "real"    & !acumulado ~ "PIBK_Trim"
  )

  read_pib_indicator <- function(skip, nmax, indicator_name) {
    suppressMessages(
      pib_gasto <- readxl::read_excel(
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
      length.out = ncol(pib_gasto) - 1
    )

    pib_gasto |>
      stats::setNames(c("partida", quarters)) |>
      dplyr::filter(!is.na(partida)) |>
      tidyr::pivot_longer(
        cols = -partida,
        names_to = "trimestre",
        values_to = indicator_name) |>
      dplyr::mutate(
        trimestre = as.numeric(trimestre),
        fecha = lubridate::as_date(trimestre),
        year = lubridate::year(fecha),
        trimestre = lubridate::quarter(fecha)
      ) |>
      dplyr::select(
        partida, fecha, year, trimestre, dplyr::all_of(indicator_name))
  }

  to_read <- list(
    nominal = list(
      list(skip = 9, nmax = 14,  indicator_name = "pib_nominal"),
      list(skip = 28, nmax = 14, indicator_name = "ponderacion")
    ),
    real = list(
      list(skip = 9, nmax = 14,  indicator_name = "indice"),
      list(skip = 28, nmax = 14, indicator_name = "crecimiento_interanual"),
      list(skip = 47, nmax = 14, indicator_name = "incidencia")
    )
  )

  to_read[[modalidad]] |>
    purrr::map(\(args) do.call(read_pib_indicator, args)) |>
    purrr::reduce(
      dplyr::left_join,
      by = c("partida", "fecha", "year", "trimestre")
    )

}
