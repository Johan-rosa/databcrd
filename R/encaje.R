#' Download legal reserve serie
#'
#' @export
#'
#' @return a tibble
#' @examples
#' get_encaje()

get_encaje <- function() {

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/documents/",
    "encaje_bancario.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  suppressMessages(
    suppressWarnings(
      encaje <- readxl::read_excel(
        file_path,
        sheet = 1,
        skip = 12,
        col_names = c("year", "mes", "mn_oblicaciones", "mn_requerido_absoluto",
                      "mn_requerido_tasa", "mn_efectivo_absoluto",
                      "mn_efectivo_tasa", "mn_excedente_absoluto",
                      "mn_excedente_tasa", "me_oblicaciones",
                      "me_requerido_absoluto", "me_requerido_tasa",
                      "me_efectivo_absoluto", "me_efectivo_tasa",
                      "me_excedente_absoluto", "me_excedente_tasa"),
        col_types = c("numeric", "text", rep("numeric", 14))
        )
    )
  )

  encaje |>
    stats::na.omit() |> #nolint
    dplyr::mutate(
      mes = crear_mes(mes),
      fecha = lubridate::make_date(year, mes)
      ) |>
    dplyr::relocate(fecha)
}
