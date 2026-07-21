# Utils ---------------------------------------------------------------------------------------
# Load the package
ipc_articulos_details <- readRDS("inst/ipc_articulos_details.rds") |>
  dplyr::as_tibble()

ipc_articulos_key <- ipc_articulos_details |>
  dplyr::select(id, nombre, agregacion, grupo, subgrupo, clase, subclase, articulo)

reshape_ipc_data <- function(raw_data, ref_year) {
  dplyr::bind_cols(ipc_articulos_key, raw_data) |>
    dplyr::select(-name) |>
    tidyr::pivot_longer(-c(id:ponderacion), values_to = "indice", names_to = "mes") |>
    dplyr::mutate(
      year = ref_year,
      mes = crear_mes(mes),
      date = lubridate::make_date(year, mes, 1)
    ) |>
    dplyr::relocate(date,year, mes, .before = ponderacion)
}

# Download IPC file ---------------------------------------------------------------------------

url_descarga <- paste0(
  "https://cdn.bancentral.gov.do/",
  "documents/estadisticas/precios/documents/",
  "ipc_articulos_base_2019-2020.xlsx"
)

file_path <- tempfile(pattern = "", fileext = ".xlsx")

utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)

data_columns <- readxl::read_excel(file_path, sheet = 1, skip = 3, col_names = FALSE, n_max = 1) |>
  janitor::clean_names() |>
  suppressMessages()

dates <- seq(
  from = lubridate::ymd("2020-10-01"),
  length.out = ncol(data_columns) - 6,
  by = "month"
) |>
  format("%b_%Y")

fixed_columns <- c("grupo", "subgrupo", "clase", "subclase", "articulo", "ponderacion")
headers <- c(fixed_columns, dates)

# Data 2020 y 2021 -----------------------------------------------------------------------------
data_raw <- readxl::read_excel(
  file_path, sheet = 1, skip = 3) |>
  janitor::clean_names() |>
  suppressMessages()

data_ipc_articulos_2020_2025 <- data_raw |>
  purrr::set_names(headers) |>
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
    code = ifelse(nombre == "Indice General", "Z0001", code)
  ) |>
  dplyr::relocate(id = code, agregacion, nombre) |>
  dplyr::select(-dplyr::all_of(c("grupo", "subgrupo", "clase", "subclase", "articulo"))) |>
  dplyr::left_join(
    dplyr::select(ipc_articulos_key, -c(nombre, agregacion)), by = "id"
  ) |>
  tidyr::pivot_longer(dplyr::matches("^[A-z]{3}_\\d{4}"), values_to = "indice", names_to = "mes") |>
  dplyr::mutate(
    date = lubridate::my(mes),
    year = lubridate::year(date),
    mes  = lubridate::month(date)
  ) |>
  dplyr::relocate(date, year, mes, .before = ponderacion) |>
  dplyr::filter(year <= 2026)


usethis::use_data(data_ipc_articulos_2020_2025, overwrite = TRUE)

# Data empalmada ------------------------------------------------------------------------------
names <- readxl::read_excel(
  "inst/data_ipc_articulos_wide_empalmada.xlsx", col_names = FALSE, n_max = 1, col_types = "text") |>
  suppressMessages() |>
  sapply(\(x) x) |>
  unname()

# El orden de los elementos debe ser el mismo en ambos objetos
nombres <- dplyr::tibble(nombre = names[-1]) |>
  dplyr::mutate(posicion = dplyr::row_number()) |>
  dplyr::relocate(posicion) |>
  dplyr::bind_cols(
    dplyr::select(ipc_articulos_details, id, nombre_details = nombre)
  )

if (all(nombres$nombre == nombres$nombre_details)) {
  usethis::ui_done("Los nombres concuerdan")
  nombres <- dplyr::select(nombres, posicion, id, nombre)
} else {
  usethis::ui_oops("Los nombres no concuerdan")
  nombres <- NULL
}

ipc_articulos_wide_2010_2024 <- readxl::read_excel(
  "inst/data_ipc_articulos_wide_empalmada.xlsx",
  col_names = FALSE,
  skip = 1,
  col_types = c("date", rep("numeric", length(names) - 1))
) |>
  suppressMessages() |>
  janitor::clean_names() |>
  dplyr::mutate(x1 = as.Date(x1))


ipc_articulos_long_2010_2024 <- ipc_articulos_wide_2010_2024 |>
  tidyr::pivot_longer(-x1, names_to = "id", values_to = "indice") |>
  setNames(c("date", "posicion", "indice")) |>
  dplyr::mutate(posicion = readr::parse_number(posicion) -1) |>
  dplyr::left_join(nombres, by = "posicion") |>
  dplyr::relocate(posicion, id, date, nombre, indice) |>
  dplyr::left_join(
    dplyr::select(ipc_articulos_details, -nombre),
    by = "id"
  ) |>
  dplyr::mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    mes = lubridate::month(date)
  ) |>
  dplyr::filter(year < 2025) |>
  dplyr::select(
    id,
    agregacion,
    nombre,
    grupo:articulo,
    date,
    year,
    mes,
    ponderacion = ponderacion_ipc,
    indice
  )


usethis::use_data(data_ipc_articulos_long_2010_2024, overwrite = TRUE)
