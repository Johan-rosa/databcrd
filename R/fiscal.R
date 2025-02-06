#' Download Statement of Operations of the non-financial public sector
#'
#' @param frecuencia A character string that specifies the frequency of the
#' exchange rates to be downloaded. Valid options are "Mensual",  or "Anual".
#'
#' @export
#'
#' @return a tibble
#' @examples
#' get_fiscal()
get_fiscal <- function(
    frecuencia = "Mensual"
    ) {
  checkmate::assert_choice(frecuencia, c("Mensual", "Anual"))

  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/documents/",
    "Operaciones_Mensual.xlsx")

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  suppressMessages(
    suppressWarnings(
      data <- readxl::read_excel(
        file_path,
        sheet = 1,
        col_names = FALSE
        )
      )
    )

  vars <- data.frame(year = t(data[1,]),
                     mes = t(data[2,])) |>
    tidyr::fill(year) |>
    dplyr::mutate(
      year = stringr::str_remove(
        string = year,
        pattern = "\\*"
        ),
      date = paste(
        year, mes,
        sep = "_"
        )
      ) |>
    dplyr::pull(date)

  colnames(data) <- vars

  data <- data |>
    dplyr::filter(!is.na(data[[3]])) |>
    dplyr::select(-c(1, 2)) |>
    dplyr::slice(-c(1:2)) |>
    dplyr::bind_cols(fiscal_details) |> # nolint
    tidyr::pivot_longer(
      cols = -c(short_names, categoria, nivel,
                original_names, labels, direct_parent),
      names_to = "fecha",
      values_to = "valor") |>
    dplyr::filter(
      !stringr::str_detect(
        string = fecha,
        pattern = "Enero-"
        )
      ) |>
    dplyr::mutate(
      valor = as.numeric(valor),
      year = stringr::str_extract(
        string = fecha,
        pattern = "[:digit:]{4}"
      ) |> as.numeric(),
      fecha = lubridate::make_date(
        year = year,
        month = stringr::str_remove(
          string = fecha,
          pattern = "[:digit:]{4}_"
        ) |> crear_mes()
      )
    )

  ifelse(frecuencia == "Mensual",
         data <- data |>
           dplyr::select(-year),
         data <- data |>
           dplyr::summarise(
             valor = sum(valor),
             .by = c("original_names", "labels", "short_names", "categoria",
                     "nivel", "direct_parent", "year"))
         )

  return(data)
}
