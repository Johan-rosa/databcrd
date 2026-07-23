#' Fiscal operations
#'
#' Downloads fiscal operations data published by the Central Bank of the
#' Dominican Republic and returns it in tidy format at either monthly or annual
#' frequency.
#'
#' The source spreadsheet reports fiscal aggregates for the Central Government,
#' the Rest of the Public Sector, and the Non-Financial Public Sector. The
#' function cleans the original workbook, propagates category labels and account
#' codes, removes empty rows, derives the hierarchical level of each account
#' from its code, and reshapes the data into a long format. Monthly observations
#' can be returned directly, or aggregated to annual totals.
#'
#' @param frecuencia Character scalar indicating the desired frequency.
#'   Must be one of:
#'   \describe{
#'     \item{"mensual"}{Return monthly observations (default).}
#'     \item{"anual"}{Return annual totals obtained by summing monthly values.}
#'   }
#'
#' @return
#' If `frecuencia = "mensual"`, a tibble with the following columns:
#' \describe{
#'   \item{codigo}{Unique account code. Duplicate codes in the source are
#'   disambiguated by appending a numeric suffix.}
#'   \item{categoria}{Institutional sector.}
#'   \item{cuenta}{Account name.}
#'   \item{level}{Hierarchical level inferred from the account code.}
#'   \item{fecha}{Observation date.}
#'   \item{year}{Calendar year.}
#'   \item{mes}{Calendar month (1--12).}
#'   \item{value}{Reported fiscal value.}
#' }
#'
#' If `frecuencia = "anual"`, a tibble with one observation per account and year
#' containing:
#' \describe{
#'   \item{codigo}{Unique account code.}
#'   \item{categoria}{Institutional sector.}
#'   \item{cuenta}{Account name.}
#'   \item{level}{Hierarchical level inferred from the account code.}
#'   \item{year}{Calendar year.}
#'   \item{value}{Annual total obtained by summing monthly values.}
#' }
#'
#' @details
#' The source workbook is downloaded directly from the Central Bank of the
#' Dominican Republic each time the function is called.
#'
#' @source
#' Central Bank of the Dominican Republic (Banco Central de la República Dominicana),
#' *Operaciones del Sector Público*.
#' <https://cdn.bancentral.gov.do/documents/estadisticas/documents/Operaciones_Mensual.xlsx>
#'
#' @examples
#' \dontrun{
#' # Monthly data
#' monthly <- get_fiscal()
#'
#' # Annual totals
#' annual <- get_fiscal("anual")
#' }
#'
#' @export
fiscal_operations <- function(
    frecuencia = c("mensual", "anual")
) {
  frecuencia <- rlang::arg_match(frecuencia)

  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/documents/",
    "Operaciones_Mensual.xlsx"
  )

  file_path <- tempfile(fileext = ".xlsx")
  download.file(url, file_path, mode = "wb", quiet = TRUE)

  raw_data <- readxl::read_excel(file_path, skip = 2, col_names = FALSE) |>
    janitor::clean_names() |>
    suppressMessages()

  ## Agrega la variable categoría
  raw_data <- dplyr::mutate(
    raw_data,
    categoria = dplyr::if_else(
      stringr::str_detect(x2, "GOBIERNO CENTRAL|RESTO DEL SECTOR PÚBLICO|SECTOR PÚBLICO NO FINANCIERO"),
      x2, NA
    ),
  ) |>
    tidyr::fill(categoria) |>
    dplyr::mutate(
      # x1 es la columnna del codigo de cuenta
      # se le agrega un prefijo dependiendo de la categoria
      # para que sea único. Ya que varias categorias pueden tener
      # las mismas cuentas. El prefijo se remueve al final
      preffix = dplyr::case_when(
        stringr::str_detect(categoria, "GOBIERNO CENTRAL") ~ "GC",
        stringr::str_detect(categoria, "RESTO DEL SECTOR") ~ "RSPNF",
        stringr::str_detect(categoria, "SECTOR P") ~ "RSPF"
      ),
      x1 = paste(preffix, x1, sep = "_")
    ) |>
    dplyr::relocate(categoria, preffix) |>
    # Se remueve la última columna porque contienen el acumulado del último año
    dplyr::select(-dplyr::last_col(), -preffix)


  fixed_columns <- c("categoria", "codigo", "cuenta")
  # Considering annual data starting since 2000
  headers <- c(
    fixed_columns,
    seq(
      from = lubridate::ymd("2000-01-01"),
      by = "month",
      length.out = ncol(raw_data) - length(fixed_columns)
    ) |> as.character()
  )

  data <- raw_data |>
    purrr::set_names(headers) |>
    dplyr::filter(!is.na(.data[["2000-01-01"]])) |>
    # Remove rows where all values of colums with YYYY format are 0
    dplyr::filter(!dplyr::if_all(dplyr::matches("^\\d{4}"), ~ . == 0)) |>
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
      names_to = "fecha",
      values_to = "value"
    ) |>
    dplyr::mutate(
      fecha = lubridate::ymd(fecha),
      year  = lubridate::year(fecha),
      mes   = lubridate::month(fecha)
    ) |>
    dplyr::relocate(year, mes, .after = fecha)

  if (frecuencia == "anual") {
    result <- data |>
      dplyr::summarise(
        value = sum(value, na.rm = TRUE),
        .by = c(codigo, categoria, cuenta, level, year)
      )

    return(result)
  }

  data
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
fiscal_operations_gdp <- function() {
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
    ),
  ) |>
    tidyr::fill(categoria) |>
    dplyr::mutate(
      # x1 es la columnna del codigo de cuenta
      # se le agrega un prefijo dependiendo de la categoria
      # para que sea único. Ya que varias categorias pueden tener
      # las mismas cuentas. El prefijo se remueve al final
      preffix = dplyr::case_when(
        stringr::str_detect(categoria, "GOBIERNO CENTRAL") ~ "GC",
        stringr::str_detect(categoria, "RESTO DEL SECTOR") ~ "RSPNF",
        stringr::str_detect(categoria, "SECTOR P") ~ "RSPF"
      ),
      x1 = paste(preffix, x1, sep = "_")
    ) |>
    dplyr::relocate(categoria, preffix) |>
    # Se remueve la última columna porque contienen el acumulado del último año
    dplyr::select(-dplyr::last_col(), -preffix)

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


