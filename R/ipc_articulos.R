#' Download the CPI series at item level
#'
#' Download the CPI inflacion series for the Dominican Republic by item level with details
#' about the group, subgroup, class and subclass
#'
#' @export
#'
#' @return a tibble
#'
#' @examples
#' get_ipc_articulos()
get_ipc_articulos <- function() {
  url_descarga <- paste0(
    "https://cdn.bancentral.gov.do/",
    "documents/estadisticas/precios/documents/",
    "ipc_articulos_base_2019-2020.xlsx"
  )

  file_path <- tempfile(pattern = "", fileext = ".xlsx")

  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

  sheets <- readxl::excel_sheets(file_path) |>
    stringr::str_subset("202[01234]", negate = TRUE) |>
    sort()

  new_data <- sheets |>
    purrr::map(
      \(ref_year) {
        read_ipc_articulos_sheet(ref_year, file_path)
      }
    ) |>
    purrr::list_rbind()

  dplyr::bind_rows(data_ipc_articulos_long_2010_2024, new_data) |>
    dplyr::arrange(date)
}

#' Download the CPI series in long format
#'
#' Download the CPI inflacion series for the Dominican Republic in any
#' disaggregation
#'
#' @param desagregacion string with the desired disaggregation. options:
#' "general", "grupo", "subgrupo", "clase", "subclase", "articulo"
#'
#' @export
#'
#' @return a tibble
#' @examples
#' get_ipc_long("general")
#' get_ipc_long("grupo")
get_ipc_long <- function(desagregacion = c("general", "grupo", "subgrupo", "clase", "subclase", "articulo")) {
  desagregacion <- match.arg(desagregacion)

  data <- get_ipc_articulos() |>
    dplyr::filter(agregacion == stringr::str_to_title(desagregacion))

  if (desagregacion != "articulo") {
    to_remove <- switch (
      desagregacion,
      general = c("grupo", "subgrupo", "clase", "subclase", "articulo", "ponderacion"),
      grupo = c("subgrupo", "clase", "subclase", "articulo"),
      subgrupo = c("clase", "subclase", "articulo"),
      clase = c("articulo", "subclase"),
      subclase = c("articulo"),
      stop("Wrong aggregation name")
    )

    data <- data |>
      dplyr::select(-dplyr::all_of(to_remove))
  }

  data
}

reshape_ipc_data <- function(raw_data, ref_year) {
  ipc_articulos_key <- ipc_articulos_details |>
    dplyr::select(id = posicion, nombre, agregacion, grupo, subgrupo, clase, subclase, articulo)

  dplyr::left_join(ipc_articulos_key, raw_data, by = c("nombre", "agregacion")) |>
    dplyr::select(-code) |>
    tidyr::pivot_longer(-c(id:ponderacion), values_to = "indice", names_to = "mes") |>
    dplyr::mutate(
      year = ref_year,
      mes = crear_mes(mes),
      date = lubridate::make_date(year, mes, 1)
    ) |>
    dplyr::relocate(date,year, mes, .before = ponderacion)
}

read_ipc_articulos_sheet <- function(ref_year, file_path) {
  year_data <- readxl::read_excel(
    file_path,
    sheet = ref_year,
    col_names = FALSE,
    skip = 4
  ) |>
    suppressMessages()

  number_of_cols <- ncol(year_data)
  fixed_columns <- c("grupo", "subgrupo", "clase", "subclase", "articulo", "ponderacion")
  data_names <- c(fixed_columns, crear_mes(seq_len(number_of_cols - length(fixed_columns)), "number_to_text"))

  raw_data <- year_data |>
    stats::setNames(data_names) |>
    dplyr::mutate(
      nombre = dplyr::coalesce(grupo, subgrupo, clase, subclase, articulo),
      code = stringr::str_extract(nombre, "^\\d+"),
      nombre = stringr::str_remove(nombre, "^\\d+ ")
    ) |>
    dplyr::mutate(
      agregacion = dplyr::case_when(
        stringr::str_length(code) == 2 ~ "Grupo",
        stringr::str_length(code) == 3 ~ "Subgrupo",
        stringr::str_length(code) == 4 ~ "Clase",
        stringr::str_length(code) == 5 ~ "SubClase",
        stringr::str_length(code) == 7 ~ "Articulo",
        TRUE ~ "General"
      ),
      nombre = dplyr::recode(
        nombre,
         "Suplementos alimenticios (Ensure y similares)" = "Suplementos alimenticios",
         "Celebración de cumpleaños" = "Celebración de eventos"
      )
    ) |>
    dplyr::relocate(code, agregacion, nombre) |>
    dplyr::select(-all_of(c("grupo", "subgrupo", "clase", "subclase", "articulo")))

  raw_data |>
    reshape_ipc_data(as.numeric(ref_year))
}
