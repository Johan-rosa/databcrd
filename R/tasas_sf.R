
#' Download interest rates for savings
#'
#' Get the series of the interest rates for savings in the Dominican Republic
#' separated by time and type of saving
#'
#' @return a data frame with monthly series
#' @export
get_tasas_pasivas <- function() {
  file_names <- c(
    "Tasas 1991-2007",
    "Tasas 2008-2012",
    "Tasas 2013-2016",
    "Tasas 2017-today"
  )

  urls <- c(
   paste0(
     "https://cdn.bancentral.gov.do/documents/",
     "estadisticas/sector-monetario-y-financiero/",
     "documents/tbm_pasiva-1991-2007.xls"),

   paste0(
     "https://cdn.bancentral.gov.do/documents/",
     "estadisticas/sector-monetario-y-financiero/",
     "documents/tbm_pasivad-2008-2012.xls"),

   paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/",
    "documents/tbm_pasivad-2013-2016.xlsx"),

   paste0(
     "https://cdn.bancentral.gov.do/documents/",
     "estadisticas/sector-monetario-y-financiero/",
     "documents/tbm_pasivad.xlsx")
   )

  names(urls) <- file_names

  temp_files <- base::sapply(
    c(".xls", ".xls", ".xlsx", ".xlsx"),
    \(x) tempfile(fileext = x)
  ) |>
    stats::setNames(file_names)

  usethis::ui_info("Downloading files")
  purrr::walk2(
    urls, temp_files,
    utils::download.file, mode = "wb", quiet = TRUE
  )

  params <- list(
    list(range = "A157:M266"),
    list(range = "A14:O88"),
    list(range = "A11:O67"),
    list(skip = 10)
  ) |>
    purrr::map2(
      temp_files,
      \(x, y) append(x, list(path = y, col_names = FALSE))
    )

  usethis::ui_info("Reading downloaded files")
  suppressMessages(
    tasas_list <- purrr::map(
      seq_along(file_names),
      \(index) do.call(readxl::read_excel, params[[index]])
      ) |>
      stats::setNames(file_names)
  )

  prepare_data <- function(df, names, start_date) {
    df |>
      stats::setNames(names) |>
      dplyr::filter(stringr::str_detect(mes, "^[A-Z]")) |>
      dplyr::mutate(
        fecha = seq(as.Date(start_date), by = "month", length.out = dplyr::n()),
        year = lubridate::year(fecha),
        mes = lubridate::month(fecha),
        mes = crear_mes(mes, "number_to_text")
      ) |>
      dplyr::select(fecha, year, mes, dplyr::everything())
  }

  df_names <- list(
    c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360", "tp_m360",
      "tp_ps", "tp_pp", "tp_dep_ahorros", "tp_preferencial", "tp_general",
      "tp_interbancarios"),
    c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
      "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
      "tp_general", "tp_preferencial", "tp_interbancarios")
  )

  df_names <- list(df_names[[1]], df_names[[2]], df_names[[2]], df_names[[2]])

  df_cleaning_params <- dplyr::lst(
    df = tasas_list,
    names = df_names,
    start_date = c("2000/01/01", "2008/01/01", "2013/01/01", "2017/01/01")
  )

  usethis::ui_info("Cleaning the data")
  tasas <- purrr::pmap(
    df_cleaning_params,
    prepare_data
  ) |>
    purrr::list_rbind()

  usethis::ui_done("Data ready")
  tasas
}

#' Download interest rates for loans
#'
#' Get the series of the interest rates for loans in the dominican republic
#' separated by time and type of loan
#'
#' @return a data frame with monthly series
#' @export
get_tasa_activas <- function() {
  file_names <- c(
    "Tasas 1991-2007",
    "Tasas 2008-2012",
    "Tasas 2013-2016",
    "Tasas 2017-today"
  )

  urls <- c(
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activa-1991-2007.xls?v=1570134897519"),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activad-2008-2012.xls?v=1570198636254"),
    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activad-2013-2016.xlsx?v=1570198636254"),

    paste0(
      "https://cdn.bancentral.gov.do/documents/",
      "estadisticas/sector-monetario-y-financiero/",
      "documents/tbm_activad.xlsx?v=1570198636254")
  )

  names(urls) <- file_names

  temp_files <- base::sapply(
    c(".xls", ".xls", ".xlsx", ".xlsx"),
    \(x) tempfile(fileext = x)
  ) |>
    stats::setNames(file_names)

  usethis::ui_info("Downloading files")
  purrr::walk2(
    urls, temp_files,
    utils::download.file, mode = "wb", quiet = TRUE
  )

  params <- list(
    list(range = "A157:O266"),
    list(range = "A14:M85"),
    list(range = "A10:M66"),
    list(skip = 9)
  ) |>
    purrr::map2(
      temp_files,
      \(x, y) append(x, list(path = y, col_names = FALSE))
    )

  usethis::ui_info("Reading downloaded files")
  suppressMessages(
    tasas_list <- purrr::map(
      seq_along(file_names),
      \(index) do.call(readxl::read_excel, params[[index]])
    ) |>
      stats::setNames(file_names)
  )

  prepare_data <- function(df, names, start_date) {
    df |>
      janitor::remove_empty(which = "cols") |>
      janitor::remove_empty(which = "rows") |>
      stats::setNames(names) |>
      dplyr::filter(stringr::str_detect(mes, "^[A-Z]")) |>
      dplyr::mutate(
        fecha = seq(as.Date(start_date), by = "month", length.out = dplyr::n()),
        year = lubridate::year(fecha),
        mes = lubridate::month(fecha),
        mes = crear_mes(mes, "number_to_text")
      ) |>
      dplyr::filter(!is.na(ta_90d)) |>
      dplyr::select(fecha, year, mes, dplyr::everything())
  }

  df_names <- list(
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
      "ta_consumo", "ta_hipotecario"),
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
      "ta_consumo", "ta_hipotecario"),
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_ps", "ta_pp", "ta_preferencial", "ta_comercio",
      "ta_consumo", "ta_hipotecario"),
    c("mes", "ta_90d", "ta_180d", "ta_360d", "ta_2a", "ta_5a",
      "ta_m5a", "ta_pp", "ta_ps", "ta_comercio", "ta_consumo",
      "ta_hipotecario", "ta_preferencial", "ta_preferencial_comercio",
      "ta_preferencial_consumo", "ta_preferencial_hipotecario")
  )

  df_cleaning_params <- dplyr::lst(
    df = tasas_list,
    names = df_names,
    start_date = c("2000/01/01", "2008/01/01", "2013/01/01", "2017/01/01")
  )

  usethis::ui_info("Cleaning the data")
  tasas <- purrr::pmap(
    df_cleaning_params,
    prepare_data
  ) |>
    purrr::list_rbind()

  usethis::ui_done("Data ready")
  tasas
}
