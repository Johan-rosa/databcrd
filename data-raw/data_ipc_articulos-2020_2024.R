# Utils ---------------------------------------------------------------------------------------
# Load the package
ipc_articulos_details <- readRDS("inst/ipc_articulos_details.rds")

ipc_articulos_key <- ipc_articulos_details |>
  dplyr::select(id = posicion, nombre, agregacion, grupo, subgrupo, clase, subclase, articulo)

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

# Data 2020 y 2021 -----------------------------------------------------------------------------
data_2020_2021_raw <- readxl::read_excel(
  file_path, sheet = "2020-2021", col_names = FALSE, skip = 4) |>
  suppressMessages()

ipc_2020_raw <- data_2020_2021_raw[ , 1:5] |>
  setNames(c("name", "ponderacion", "octubre", "noviembre", "diciembre"))

ipc_2021_raw <- data_2020_2021_raw[ , -c(3:5)] |>
  setNames(c("name", "ponderacion", crear_mes(1:12, "number_to_text")))

ipc_articulo_2020 <- reshape_ipc_data(ipc_2020_raw, 2020)
ipc_articulo_2021 <- reshape_ipc_data(ipc_2021_raw, 2021)

# data 2022-2024 ------------------------------------------------------------------------------

sheets_2022_2024 <- readxl::excel_sheets(file_path) |>
  stringr::str_subset("202[234]") |>
  sort()

ipc_articulo_2022_2024 <- sheets_2022_2024 |>
  purrr::map(
    \(ref_year) {
      year_data <- readxl::read_excel(file_path, sheet = ref_year, col_names = FALSE, skip = 4) |>
        suppressMessages()

      data_names <- c("name", "ponderacion", crear_mes(seq_len(ncol(year_data) - 2), "number_to_text"))

      year_data |>
        setNames(data_names) |>
        reshape_ipc_data(as.numeric(ref_year))
    }
  ) |>
  purrr::list_rbind()

data_ipc_articulos_2020_2024 <-  dplyr::bind_rows(
  ipc_articulo_2020,
  ipc_articulo_2021,
  ipc_articulo_2022_2024
)

usethis::use_data(data_ipc_articulos_2020_2024, overwrite = TRUE)

# Data empalmada ------------------------------------------------------------------------------
names <- readxl::read_excel(
  "inst/data_ipc_articulos_wide_empalmada.xlsx", col_names = FALSE, n_max = 1, col_types = "text") |>
  suppressMessages() |>
  sapply(\(x) x) |>
  unname()

ipc_articulos_wide_2010_2024 <- readxl::read_excel(
  "inst/data_ipc_articulos_wide_empalmada.xlsx",
  col_names = FALSE,
  skip = 1,
  col_types = c("date", rep("numeric", length(names) - 1))
) |>
  suppressMessages() |>
  janitor::clean_names() |>
  dplyr::mutate(x1 = as.Date(x1))


data_ipc_articulos_long_2010_2024 <- dplyr::left_join(
  ipc_articulos_details |>
    dplyr::select(
      posicion, nombre, agregacion, grupo, subgrupo, clase, subclase, articulo, ponderacion = ponderacion_ipc
    ),
  ipc_articulos_wide_2010_2024 |>
    tidyr::pivot_longer(-x1, names_to = "id", values_to = "indice") |>
    setNames(c("date", "posicion", "indice")) |>
    dplyr::mutate(posicion = readr::parse_number(posicion) -1),
  by = c("posicion")
) |>
  dplyr::mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    mes = lubridate::month(date)
  ) |>
  dplyr::filter(year < 2025) |>
  dplyr::select(
    id = posicion,
    nombre:articulo,
    date,
    year,
    mes,
    ponderacion,
    indice
  )

usethis::use_data(data_ipc_articulos_long_2010_2024, overwrite = TRUE)
