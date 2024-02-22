#' Download the loans by sector series
#'
#' Download loans by sectors
#'
#' @param osd string with the desired societies options:
#' "consolidado", "bancos_multiples", "resto_osd".
#'
#' @export
#'
#' @return a tibble
#' @examples
#' get_prestamos_osd("consolidado")
#' get_prestamos_osd("bancos_multiples")
#' get_prestamos_osd("resto_osd")
get_prestamos_osd <- function(osd = "consolidado") {
  checkmate::assert_choice(
    osd,
    choices = c("consolidado", "bancos_multiples", "resto_osd")
  )

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/documents/",
    "serie_prestamos_por_destino_armonizados.xlsx")

  file_path <- tempfile(fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  read_sections <- function(skip, nmax, variable_name) {
    suppressMessages(
      data <- readxl::read_excel(
        path = file_path,
        sheet = 1L,
        skip = skip,
        n_max = nmax,
        col_names = FALSE
      )
    )

    months <- seq(
      as.Date("1996-01-01"),
      by = "months",
      length.out = ncol(data) - 1
    )

    data |>
      stats::setNames(c("sectores", months)) |>
      dplyr::filter(!is.na(sectores)) |>
      tidyr::pivot_longer(
        cols = -sectores,
        names_to = "mes",
        values_to = variable_name) |>
      dplyr::mutate(
        sectores = stringr::str_remove_all(sectores, " \\([:digit:]\\)|[:digit:]"),
        mes = as.numeric(mes),
        fecha = lubridate::as_date(mes)
      ) |>
      dplyr::select(
        sectores, fecha, dplyr::all_of(variable_name))

  }

  to_read <- list(
    consolidado = list(
      list(skip = 11, nmax = 16, variable_name = "mn"),
      list(skip = 29, nmax = 16, variable_name = "me"),
      list(skip = 47, nmax = 16, variable_name = "consolidado")
      ),
    bancos_multiples = list(
      list(skip = 86, nmax = 16, variable_name = "mn"),
      list(skip = 104, nmax = 16, variable_name = "me"),
      list(skip = 122, nmax = 16, variable_name = "consolidado")
      ),
    resto_osd = list(
      list(skip = 160, nmax = 16, variable_name = "mn"),
      list(skip = 178, nmax = 16, variable_name = "me"),
      list(skip = 196, nmax = 16, variable_name = "consolidado")
      )
    )

  to_read[[osd]] |>
    purrr::map(\(args) do.call(read_sections, args)) |>
    purrr::reduce(
      dplyr::left_join,
      by = c("sectores", "fecha")
    )

  }
