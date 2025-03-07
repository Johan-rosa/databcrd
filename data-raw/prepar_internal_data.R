load("data/data_ipc_articulos_2020_2024.rda")
load("data/data_ipc_articulos_long_2010_2024.rda")
load("data/expectativas_details.rda")
load("data/exports_details.rda")
load("data/fiscal_details.rda")
load("data/imports_details.rda")
load("data/indicadores_bcrd_details.rda")
load("data/ipc_articulos_details.rda")

usethis::use_data(
  data_ipc_articulos_2020_2024,
  data_ipc_articulos_long_2010_2024,
  ipc_articulos_details,
  expectativas_details,
  exports_details,
  fiscal_details,
  imports_details,
  indicadores_bcrd_details,
  internal = TRUE,
  overwrite = TRUE
)

