#' Macroeconomic Expectations
#'
#' Get the data series for the monthly macroeconomic expectations of the
#' Dominican Republic.
#'
#' @param modalidad string with the desired disaggregation. options:
#' "eem", "eoe", "ecc",
#'
#' @return a tibble
#' @export
#'
#' @examples
#' get_expectativas("eem")
#' get_expectativas("eoe")
#' get_expectativas("ecc")
#'
get_expectativas <- function(modalidad = "eem") {
checkmate::assert_choice(
  modalidad,
  choices = c("eem", "eoe", "ecc")
)

expectativas_function <- switch(
  modalidad,
  "eem" = get_eem,
  "eoe" = get_eoe,
  "ecc" = get_ecc
)

expectativas_function()

}

#' Get EEM
get_eem <- function() {
  file_url <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "politica-monetaria/expectativas-macroeconomicas/",
    "documents/Historico-EEM.xlsx"
  )
  file_path <- tempfile(fileext = ".xlsx")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  header_expectativas <- c(
    "year", "mes",
    "inf_anio_actual", "inf_12m", "inf_anio_siguiente", "inf_24",
    "tc_anio_actual", "tc_12m", "tc_anio_siguiente", "tc_24",
    "pib_trim_actual", "pib_anio_actual", "pib_anio_siguiente",
    "tpm_mes_actual", "tpm_trim_actual", "tpm_anio_actual", "tpm_12m"
  )

  sheet_names <- readxl::excel_sheets(file_path)[-4]

  suppressMessages(
    purrr::map(
      sheet_names,
      \(sheet) {
        readxl::read_excel(
          file_path,
          sheet = sheet,
          skip = 9,
          col_names = header_expectativas
        ) |>
          tidyr::pivot_longer(
            !c(year, mes),
            names_to = "short_names",
            values_to = "valor"
          ) |>
          dplyr::mutate(descripcion = sheet)
      }
    )
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      fecha = lubridate::make_date(year, mes, 1),
      variable_key = stringr::str_extract(short_names, "^[a-z]+(?=_)"),
      variable = dplyr::case_when(
        stringr::str_detect(variable_key, "inf") ~ "Inflación",
        stringr::str_detect(variable_key, "tc") ~ "Varición tipo de cambio",
        stringr::str_detect(variable_key, "pib") ~ "Crecimiento del PIB",
        stringr::str_detect(variable_key, "tpm") ~ "Tasa de Política Monetaria"
      ),
      horizonte = dplyr::case_when(
        stringr::str_detect(short_names, "mes_actual$") ~ "Mes actual",
        stringr::str_detect(short_names, "trim_actual$") ~ "Trimestre actual",
        stringr::str_detect(short_names, "anio_actual$") ~ "Año actual",
        stringr::str_detect(short_names, "12m$") ~ "12 meses",
        stringr::str_detect(short_names, "anio_siguiente$") ~ "Año siguiente",
        stringr::str_detect(short_names, "24$") ~ "24 meses",
      )
    ) |>
    dplyr::select(
      fecha, year, mes, short_names, variable_key,
      variable, horizonte, expectativa = valor
    )
}

#' Get EOE
get_eoe <- function() {
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "politica-monetaria/expectativas-macroeconomicas/",
                     "documents/Historico-EOE-(Mensual).xlsx")
  file_path <- tempfile(pattern = "", fileext = ".xlsx")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  data <- suppressMessages(
    readxl::read_excel(file_path,
                             skip = 5)
    ) |>
    janitor::clean_names() |>
    dplyr::rename(year = "ano") |>
    tidyr::pivot_longer(!c(year, mes),
                        names_to = "descripcion",
                        values_to = "valor") |>
    dplyr::mutate(fecha = lubridate::make_date(
      year,
      crear_mes(mes, type = "text_to_number")
      , 1)
      )

  data

}

#' Get ECC
get_ecc <- function() {
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "politica-monetaria/expectativas-macroeconomicas/",
                     "documents/Historico-ECC.xlsx")
  file_path <- tempfile(pattern = "", fileext = ".xlsx")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  data <- suppressMessages(
    readxl::read_excel(file_path,
                             skip = 5)
    )

  names(data) <- c("descripcion", names(data)[-1])

  data <- data |>
    dplyr::filter(!is.na(descripcion) &
                    stringr::str_detect(data$descripcion,
                                        "^\\*",
                                        negate = TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(remove = any(!is.na(dplyr::c_across(-descripcion)))) |>
    dplyr::ungroup() |>
    dplyr::filter(remove) |>
    dplyr::select(-(tidyr::last_col(offset = 5):tidyr::last_col())) |>
    dplyr::bind_cols(expectativas_details) # nolint

  data <- data |>
    tidyr::pivot_longer(
      cols = -c(
        descripcion, short_names, categoria, nivel,
        original_names, labels, direct_parent
      ),
      names_to = "fecha", values_to = "valor") |>
    dplyr::group_by(direct_parent, categoria, short_names) |>
    dplyr::mutate(
      fecha = seq(
        as.Date("2013-09-01"),
        length.out = dplyr::n(),
        by = "quarter")
    ) |>
    dplyr::ungroup()

  data

}
