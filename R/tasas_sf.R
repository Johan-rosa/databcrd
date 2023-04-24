get_tasas_pasivas <- function() {
  # Rates urls
  url_tasas_pasivas_91_07 <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/",
    "documents/tbm_pasiva-1991-2007.xls")

  url_tasas_pasivas_08_12 <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/",
    "documents/tbm_pasivad-2008-2012.xls")

  url_tasas_pasivas_13_16 <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/",
    "documents/tbm_pasivad-2013-2016.xlsx")

  url_tasas_pasivas_17 <- paste0(
    "https://cdn.bancentral.gov.do/documents/",
    "estadisticas/sector-monetario-y-financiero/",
    "documents/tbm_pasivad.xlsx")

  # Temporal files
  pasivas_9107_path <- tempfile(pattern = "", fileext = ".xls")
  pasivas2_0812_path <- tempfile(pattern = "", fileext = ".xls")
  pasivas3_1316_path <- tempfile(pattern = "", fileext = ".xlsx")
  pasivas4_17_path <- tempfile(pattern = "", fileext = ".xlsx")

  # Downloading the files
  download.file(
    url_tasas_pasivas_91_07, pasivas_9107_path, mode = "wb", quiet = TRUE)
  download.file(
    url_tasas_pasivas_08_12, pasivas2_0812_path, mode = "wb", quiet = TRUE)
  download.file(
    url_tasas_pasivas_13_16, pasivas3_1316_path, mode = "wb", quiet = TRUE)
  download.file(
    url_tasas_pasivas_17, pasivas4_17_path, mode = "wb", quiet = TRUE)

  # Reading the files
  pasivas_9107 <- readxl::read_excel(
    pasivas_9107_path, sheet = "Pasivas",
    col_names = FALSE, range = "B157:M266"
  )

  pasivas_0812 <- readxl::read_excel(
    pasivas2_0812_path, sheet = "Pasivas",
    col_names = FALSE, range = "A14:O88")

  pasivas_1316 <- readxl::read_excel(
    pasivas3_1316_path, sheet = "Pasivas",
    col_names = FALSE, range = "A11:O67")

  pasivas_17 <- readxl::read_excel(
    pasivas4_17_path, sheet = "Pasivas",
    col_names = FALSE, skip = 10)

  # Wrangling 1991-2007 rates --- ---
  pasivas_9107 <- pasivas_9107 |>
    stats::setNames(
      c("tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360", "tp_m360",
        "tp_ps", "tp_pp", "tp_dep_ahorros", "tp_preferencial", "tp_general",
        "tp_interbancarios")
    ) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      fecha = seq(as.Date("2000/01/01"), by = "month", length.out = dplyr::n()),
      year = lubridate::year(fecha),
      mes = lubridate::month(fecha),
      mes = crear_mes(mes, "number_to_text")
    ) |>
    dplyr::select(fecha, year, mes, dplyr::everything())

  # Wrangling 2008-2012 rates --- ---
  pasivas_0812 <- pasivas_0812 |>
    stats::setNames(
      c("mes", "tp_30d", "tp_60d", "tp_90d", "tp_180d", "tp_360d",
        "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
        "tp_general", "tp_preferencial", "tp_interbancarios")
    ) |>
    dplyr::filter(stringr::str_detect(mes, "^[A-Z]")) |>
    dplyr::mutate(
      fecha = seq(as.Date("2008/01/01"), by = "month", length.out = dplyr::n()),
      year = lubridate::year(fecha),
      mes = lubridate::month(fecha),
      mes = crear_mes(mes, "number_to_text")
    ) |>
    dplyr::select(fecha, year, mes, dplyr::everything())

  # Wrangling 2013-2016 rates --- ---

  pasivas_1316 <- pasivas_1316 |>
    setNames(
      c("mes", "tp_30d", "tp_60d", "tp_90d",  "tp_180d", "tp_360d",
        "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
        "tp_general", "tp_preferencial", "tp_interbancarios")
    ) |>
    dplyr::filter(stringr::str_detect(mes, "^[A-Z]")) |>
    dplyr::mutate(
      fecha = seq(as.Date("2013/01/01"), by = "month", length.out = dplyr::n()),
      year = lubridate::year(fecha),
      mes = lubridate::month(fecha),
      mes = crear_mes(mes, "number_to_text")
    ) |>
    dplyr::select(fecha, year, mes, everything())

  # Wrangling 2017-present rates --- ---

  pasivas_17 <- pasivas_17 |>
    setNames(
      c("mes", "tp_30d", "tp_60d", "tp_90d",  "tp_180d", "tp_360d",
        "tp_2a", "tp_5a", "tp_m5a", "tp_pp", "tp_ps", "tp_dep_ahorros",
        "tp_general", "tp_preferencial", "tp_interbancarios")
    ) |>
    dplyr::filter(stringr::str_detect(mes, "^[A-Z]")) |>
    dplyr::filter(!is.na(.data[["tp_30d"]])) |>
    dplyr::mutate(
      fecha = seq(as.Date("2017/01/01"), by = "month", length.out = dplyr::n()),
      year = lubridate::year(fecha),
      mes = lubridate::month(fecha),
      mes = crear_mes(mes, "number_to_text")
    ) |>
    dplyr::select(fecha, year, mes, everything())

  dplyr::bind_rows(
    pasivas_9107,
    pasivas_0812,
    pasivas_1316,
    pasivas_17)
}
