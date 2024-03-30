#' Average exchange rate of the operations in financial institutions
#'
#' This function returns the average exchange rates of the operations conducted
#' by the banks and exchange operators in the  Dominican Republic
#' based on the specified frequency.
#'
#' @param frecuencia A character string that specifies the frequency of the
#' exchange rates to be downloaded. Valid options are "diaria", "mensual",
#' "trimestral",  or "anual".
#'
#' @return A data frame with columns:
#'  compra: for the buying rates
#'  venta: selling rates
#'
#' @export
#'
#' @examples
#'get_tc_eif("mensual")
#'get_tc_eif("anual")
get_tc_eif <- function(frecuencia = "diaria") {
  checkmate::assert_choice(
    frecuencia,
    choices = c("diaria", "mensual", "trimestral", "anual"))

  sheet <- stringr::str_to_sentence(frecuencia)
  url <-  paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "mercado-cambiario/documents/TASA_ENTIDADES_FINANCIERAS",
    ".xls")

  path <- tempfile(pattern = "", fileext = ".xls")

  utils::download.file(url, path, mode = "wb", quiet = TRUE)

  tipo_cambio <- readxl::read_excel(path, sheet = sheet, skip = 2) |>
    janitor::clean_names() |>
    dplyr::rename(year = ano)

  if (frecuencia == "diaria") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        mes = crear_mes(mes, "text_to_number"),
        fecha = lubridate::make_date(year, mes, dia)
      ) |>
      dplyr::select(fecha, year, mes, dia, dplyr::everything())
  }

  if (frecuencia == "mensual") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        mes = crear_mes(mes, "text_to_number"),
        fecha = lubridate::make_date(year, mes, "01")
      ) |>
      dplyr::select(fecha, year, mes, dplyr::everything())
  }

  if (frecuencia == "trimestral") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        fecha = seq(
          as.Date("1992/01/01"),
          by = "quarter",
          length.out = dplyr::n()),
        trimestre = lubridate::quarter(fecha)
      ) |>
      dplyr::select(fecha, year, trimestre, dplyr::everything())
  }

  tipo_cambio
}

#' Exchange rates in the spot market of the Dominican Republic
#'
#' This function returns the average exchange rates of the operations conducted
#' by the banks and exchange operators in the  Dominican Republic
#' based on the specified frequency.
#'
#' @param frecuencia A character string that specifies the frequency of the
#' exchange rates to be downloaded. Valid options are "diaria", "mensual",
#' "trimestral",  or "anual".
#'
#' @param average_or_fp A character string that specifies if the average or the
#' value for the last day of the period is desired. valid options are "average"
#' and "fp"
#'
#' @return A data frame with columns:
#'  compra: for the buying rates
#'  venta: selling rates
#' @export
#'
#' @examples
#' get_tc_spot("mensual", "average")
#' get_tc_spot("mensual", "fp")
#' get_tc_spot("trimestral", "average")
#' get_tc_spot("trimestral", "fp")
get_tc_spot <- function(frecuencia = "mensual", average_or_fp = "average") {
  checkmate::assert_choice(
    frecuencia, choices = c("diaria", "mensual", "trimestral", "anual"))
  checkmate::assert_choice(average_or_fp, choices = c("average", "fp"))

  sheet <- dplyr::case_when(
    frecuencia == "diaria" ~ "Diaria",

    frecuencia == "mensual"    & average_or_fp == "average" ~ "PromMensual",
    frecuencia == "trimestral" & average_or_fp == "average" ~ "PromTrimestral",
    frecuencia == "anual"      & average_or_fp == "average" ~ "PromAnual",

    frecuencia == "mensual"    & average_or_fp == "fp"      ~ "FPMensual",
    frecuencia == "trimestral" & average_or_fp == "fp"      ~ "FPTrimestral",
    frecuencia == "anual"      & average_or_fp == "fp"      ~ "FPAnual"
  )

  url <- paste0(
    "https://cdn.bancentral.gov.do/documents/estadisticas/",
    "mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xlsx"
  )

  path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url, path, mode = "wb", quiet = TRUE)

  tipo_cambio <- readxl::read_excel(path, sheet = sheet, skip = 2) |>
    janitor::clean_names() |>
    dplyr::rename(year = ano)

  if (frecuencia == "diaria") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        mes = crear_mes(mes, "text_to_number"),
        fecha = lubridate::make_date(year, mes, dia)
      ) |>
      dplyr::select(fecha, year, mes, dia, dplyr::everything())
  }

  if (frecuencia == "mensual") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        mes = crear_mes(mes, "text_to_number"),
        fecha = lubridate::make_date(year, mes, "01")
      ) |>
      dplyr::select(fecha, year, mes, dplyr::everything())
  }

  if (frecuencia == "trimestral") {
    tipo_cambio <- tipo_cambio |>
      dplyr::mutate(
        fecha = seq(
          as.Date("1992/01/01"),
          by = "quarter",
          length.out = dplyr::n()),
        trimestre = lubridate::quarter(fecha)
      ) |>
      dplyr::select(fecha, year, trimestre, dplyr::everything())
  }

  tipo_cambio
}

#' Average exchange rate of the operations in financial institutions
#'
#' This function returns the average exchange rates of the operations conducted
#' by the banks and exchange operators in the  Dominican Republic
#' based on the specified frequency.
#'
#' @param frecuencia A character string that specifies the frequency of the
#' exchange rates to be downloaded. Valid options are "diaria", "mensual",
#' "trimestral",  or "anual".
#'
#' @param entidad Valid options are "eif" for financial institutions,
#' "ac" for foreign currency exchange agencies or "spot" for reference
#' exchange rate
#'
#' @param average_or_fp A character string that specifies if the average or the
#' value for the last day of the period is desired. valid options are "average"
#' and "fp" for end of period.
#'
#' @return A data frame with columns:
#'  compra: for the buying rates
#'  venta: selling rates
#'
#' @export
#'
#' @examples
#'get_tc(frecuencia = "diaria", entidad = "spot", average_or_fp = "average")
#'get_tc(frecuencia = "mensual", entidad = "eif", average_or_fp = "average")
#'get_tc(frecuencia = "trimestral", entidad = "ac", average_or_fp = "fp")

get_tc <- function(frecuencia = "diaria",
                   entidad = "spot",
                   average_or_fp = "average") {
  checkmate::assert_choice(
    frecuencia,
    choices = c("diaria", "mensual", "trimestral", "anual")
    )
  checkmate::assert_choice(
    entidad,
    choices = c("eif", "ac", "spot")
  )
  checkmate::assert_choice(
    average_or_fp,
    choices = c("average", "fp")
  )


  url <- dplyr::case_when(
    entidad == "spot" ~ paste0("https://cdn.bancentral.gov.do/documents/",
                               "estadisticas/mercado-cambiario/documents/",
                               "TASA_DOLAR_REFERENCIA_MC.xlsx"),
    entidad == "eif" ~ paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                              "mercado-cambiario/documents/",
                              "TASA_ENTIDADES_FINANCIERAS.xls"),
    entidad == "ac" ~ paste0("https://cdn.bancentral.gov.do/documents/estadisticas/",
                             "mercado-cambiario/documents/",
                             "TASAS_AGENTES_CAMBIO.xls")
    )

  path <- tempfile(
    pattern = "",
    fileext = stringr::str_extract(
      string = url,
      pattern = "\\.x.+"
      )
    )

  utils::download.file(url, path, mode = "wb", quiet = TRUE)

  tipo_cambio <- readxl::read_excel(
    path,
    sheet = "Diaria",
    skip = 2
    ) |>
    janitor::clean_names() |>
    dplyr::rename(anual = ano) |>
    dplyr::mutate(
      mes = crear_mes(mes, "text_to_number"),
      diaria = lubridate::make_date(anual, mes, dia),
      mensual = lubridate::floor_date(diaria, unit = "month"),
      trimestral = lubridate::floor_date(diaria, unit = "quarter")
    )

  if (average_or_fp == "average") {
    tipo_cambio <- dplyr::reframe(
      tipo_cambio,
      compra = mean(compra),
      venta = mean(venta),
      .by = {{ frecuencia }}
    ) |>
      dplyr::rename(fecha = 1)
  }

  if (average_or_fp == "fp") {
    fp_dates <- dplyr::reframe(
      tipo_cambio,
      diaria = max(diaria),
      .by = {{ frecuencia }}
    )

    tipo_cambio <- dplyr::left_join(x = fp_dates,
                                    y = tipo_cambio |>
                                      dplyr::select(diaria, compra, venta),
                                    by = c("diaria" = "diaria")) |>
      dplyr::rename(fecha = diaria) |>
      dplyr::select(fecha, compra, venta)
  }

  return(tipo_cambio)
}
