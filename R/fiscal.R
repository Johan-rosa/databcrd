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

#' Fiscal operations as a percentage of GDP
#'
#' Downloads the annual fiscal operations table published by the Central Bank of
#' the Dominican Republic and returns it in tidy (long) format.
#'
#' The source spreadsheet reports fiscal aggregates as percentages of GDP for
#' the Central Government, the Rest of the Public Sector, and the
#' Non-Financial Public Sector. The function cleans the original layout,
#' propagates category labels and account codes, removes empty rows, derives
#' the hierarchical level of each account from its code, and reshapes the data
#' into a long format with one observation per account and year.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{codigo}{Unique account code. Duplicate codes in the source are
#'   disambiguated by appending a numeric suffix.}
#'   \item{categoria}{Institutional sector (e.g. "Central Government",
#'   "Rest of the Public Sector", or "Non-Financial Public Sector").}
#'   \item{cuenta}{Account name.}
#'   \item{level}{Hierarchical level inferred from the account code.}
#'   \item{year}{Calendar year.}
#'   \item{value}{Fiscal balance or transaction expressed as a percentage of GDP.}
#' }
#'
#' @details
#' The source workbook is downloaded directly from the Central Bank of the
#' Dominican Republic each time the function is called.
#'
#' @source
#' Central Bank of the Dominican Republic (Banco Central de la República Dominicana),
#' *Operaciones del Sector Público como porcentaje del PIB (Anual)*.
#' <https://cdn.bancentral.gov.do/documents/estadisticas/documents/Operaciones_PIB_Anual.xlsx>
#'
#' @examples
#' \dontrun{
#' fiscal <- get_fiscal_as_gdp()
#'
#' dplyr::filter(fiscal, codigo == "1")
#' }
#'
#' @export
get_fiscal_as_gdp <- function() {
  url <- "https://cdn.bancentral.gov.do/documents/estadisticas/documents/Operaciones_PIB_Anual.xlsx"
  file_path <- tempfile(fileext = ".xlsx")
  download.file(url, file_path, mode = "wb", quiet = TRUE)

  raw_data <- readxl::read_excel(file_path, sheet = 1, skip = 2, col_names = FALSE) |>
    janitor::clean_names() |>
    suppressMessages()

  ## Agrega la variable categoría
  raw_data <- dplyr::mutate(
    raw_data,
    categoria = dplyr::if_else(
      stringr::str_detect(x2, "GOBIERNO CENTRAL|RESTO DEL SECTOR PÚBLICO|SECTOR PÚBLICO NO FINANCIERO"),
      x2, NA
    )
  ) |>
    tidyr::fill(categoria) |>
    dplyr::relocate(categoria)

  # Considering annual data starting since 2000
  headers <- c(
    "categoria", "codigo", "cuenta",
    seq(from = 2000, by = 1, length.out = ncol(raw_data) - 3) # 3 is the number of fixed columns
  )

  data <- raw_data |>
    purrr::set_names(headers) |>
    dplyr::filter(!is.na(.data[["2000"]])) |>
    # Remove rows where all values of colums with YYYY format are 0
    dplyr::filter(!dplyr::if_all(dplyr::matches("\\d{4}"), ~ . == 0))


  data |>
    tidyr::fill(codigo) |>
    dplyr::mutate(
      codigo_n = dplyr::row_number() - 1,
      .by = codigo
    ) |>
    dplyr::mutate(
      cuenta    = stringr::str_remove(cuenta, "\\d+$"),
      categoria = stringr::str_remove(categoria, "\\d+$"),
      codigo = ifelse(codigo_n > 0, paste0(codigo, codigo_n), codigo),
      level = dplyr::if_else(
        stringr::str_detect(codigo, "^\\d$|[A-z]"), 1, stringr::str_length(codigo)
      ),
      categoria = stringr::str_to_title(categoria)
    ) |>
    dplyr::relocate(codigo, categoria, cuenta, level) |>
    dplyr::select(-codigo_n) |>
    tidyr::pivot_longer(
      dplyr::matches("\\d{4}"),
      names_to = "year",
      values_to = "value"
    )
}


