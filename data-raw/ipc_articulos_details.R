
ipc_articulos_details <- readxl::read_excel("inst/detalles_ipc_articulos-base_2020.xlsx") |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(grupo:nuevos, as.logical),
    division = dplyr::recode(division, "Inflacion" = "General")
  ) |>
  dplyr::rename(
    agregacion = division,
    is_grupo = grupo,
    is_subgrupo = subgrupo,
    is_clase = clase,
    is_subclase = subclase,
    is_articulo = articulo,
    is_subyacente = subyacente,
    is_no_subyacente = no_subyacente,
    is_transables = transables,
    is_no_transables = no_transables,
    is_bienes = bienes,
    is_servicios = servicios,
    is_alimentos = alimentos,
    is_transporte = transporte,
    is_vivienda = vivienda,
    is_resto = resto,
    is_nuevo = nuevos,
  ) |>
  dplyr::mutate(
    grupos = ifelse(is_grupo, nombres, NA),
    subgrupo = ifelse(is_subgrupo, nombres, NA),
    clase = ifelse(is_clase, nombres, NA),
    subclase = ifelse(is_subclase, nombres, NA),
    articulo = ifelse(is_articulo, nombres, NA)
  ) |>
  tidyr::fill(grupos, subgrupo, clase, subclase) |>
  dplyr::mutate(
    posicion = posicion - 1,
    subgrupo = ifelse(is_grupo, NA, subgrupo),
    clase = ifelse(is_grupo | is_subgrupo, NA, clase),
    subclase = ifelse(is_grupo | is_subgrupo | is_clase, NA, subclase),
    articulo = ifelse(is_grupo | is_subgrupo | is_clase | is_subclase, NA, articulo)
  ) |>
  dplyr::relocate(posicion) |>
  dplyr::relocate(c(subgrupo, clase, subclase, articulo), .after = grupos) |>
  dplyr::rename(
    grupo = grupos,
    nombre = nombres
  )

usethis::use_data(ipc_articulos_details, overwrite = TRUE)
