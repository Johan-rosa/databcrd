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

  header_expectativas <- eem_details$headers

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
      variable = factor(
        variable_key,
        eem_details$variables$levels,
        eem_details$variables$labels
      ),
      horizonte = factor(
        stringr::str_remove(short_names, "^\\w+?_"),
        eem_details$horizontes$levels,
        eem_details$horizontes$labels
      )
    ) |>
    dplyr::select(
      fecha,
      year,
      mes,
      medida = descripcion,
      short_names,
      variable_key,
      variable,
      horizonte,
      expectativa = valor
    )
}

#' Get EOE
get_eoe <- function() {
  file_url <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "politica-monetaria/expectativas-macroeconomicas/",
    "documents/Historico-EOE-(Mensual).xlsx"
  )

  file_path <- tempfile(pattern = "", fileext = ".xlsx")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  data <- readxl::read_excel(file_path, skip = 5) |>
    suppressMessages() |>
    janitor::clean_names() |>
    dplyr::rename(year = "ano") |>
    tidyr::pivot_longer(
      !c(year, mes),
      names_to = "descripcion",
      values_to = "valor"
    ) |>
    dplyr::mutate(
      fecha = lubridate::make_date(
        year,
        crear_mes(mes, type = "text_to_number"),
        1)
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
