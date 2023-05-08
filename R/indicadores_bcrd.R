
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
      fecha = seq(as.Date("1996-01-01"), length.out = dplyr::n(), by = "month"))

  indicadores_bcrd
}
