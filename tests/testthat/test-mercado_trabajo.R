describe("pea()", {
  data_pea <- pea()

  it("retorna un tibble con las columnas esperadas", {
    expect_s3_class(data_pea, "tbl_df")
    expect_named(
      data_pea,
      c("desagregacion", "fecha", "year", "trimestre",
        "pet", "pea", "ocupados", "desocupados", "inactivos")
    )
  })

  it("retorna filas para las tres desagregaciones esperadas", {
    expect_setequal(unique(data_pea$desagregacion), c("Total", "Masculino", "Femenino"))
  })

  it("no tiene valores faltantes en las columnas numericas", {
    columnas_numericas <- c("pet", "pea", "ocupados", "desocupados", "inactivos")
    expect_true(all(!is.na(data_pea[columnas_numericas])))
  })

  it("retorna `fecha` como Date y `trimestre` entre 1 y 4", {
    expect_s3_class(data_pea$fecha, "Date")
    expect_true(all(data_pea$trimestre %in% 1:4))
  })

  it("construye `fecha` de forma consistente con year y trimestre", {
    expect_equal(data_pea$year, lubridate::year(data_pea$fecha))
    expect_equal(lubridate::month(data_pea$fecha), data_pea$trimestre * 3)
    expect_true(all(lubridate::day(data_pea$fecha) == 1))
  })

  it("no tiene fechas duplicadas dentro de una misma desagregacion", {
    duplicados <- data_pea |>
      dplyr::count(desagregacion, fecha) |>
      dplyr::filter(n > 1)

    expect_equal(nrow(duplicados), 0)
  })

  it("filtra por un solo valor de filtro_desagregacion", {
    data_total <- pea("total")

    expect_true(nrow(data_total) > 0)
    expect_true(all(data_total$desagregacion == "Total"))
  })

  it("filtra por varios valores de filtro_desagregacion", {
    data_filtrado <- pea(c("masculino", "femenino"))

    expect_setequal(unique(data_filtrado$desagregacion), c("Masculino", "Femenino"))
  })

  it("el filtro no distingue mayusculas de minusculas", {
    expect_equal(pea("TOTAL"), pea("total"))
  })

  it("lanza un error cuando filtro_desagregacion trae un valor invalido", {
    expect_error(pea("invalido"), "El filtro debe contener")
  })

  it("lanza un error cuando filtro_desagregacion mezcla valores validos e invalidos", {
    expect_error(pea(c("total", "invalido")), "El filtro debe contener")
  })

})



describe("mercado_laboral_niveles()", {

  data_niveles <- mercado_laboral_niveles()

  it("retorna un tibble con las columnas esperadas", {
    expect_s3_class(data_niveles, "tbl_df")
    expect_named(data_niveles, c("desagregacion", "indicador", "fecha", "poblacion"))
  })

  it("retorna las tres desagregaciones esperadas", {
    expect_setequal(unique(data_niveles$desagregacion), c("Total", "Masculino", "Femenino"))
  })

  it("Masculino y Femenino no son copias de Total (regresion: hoja hardcodeada)", {
    # Bug original: `sheet` se hardcodeaba a "Indicadores" dentro del map(),
    # por lo que Masculino y Femenino terminaban siendo una copia exacta de
    # Total. Este test falla si ese bug regresa.
    total <- data_niveles |> dplyr::filter(desagregacion == "Total") |> dplyr::pull(poblacion)
    masculino <- data_niveles |> dplyr::filter(desagregacion == "Masculino") |> dplyr::pull(poblacion)
    femenino <- data_niveles |> dplyr::filter(desagregacion == "Femenino") |> dplyr::pull(poblacion)

    expect_false(identical(total, masculino))
    expect_false(identical(total, femenino))
    expect_false(identical(masculino, femenino))
  })

  it("retorna los 20 indicadores esperados por desagregacion", {
    conteo_por_desagregacion <- data_niveles |>
      dplyr::count(desagregacion) |>
      dplyr::pull(n)

    expect_true(all(conteo_por_desagregacion %% 20 == 0))
  })

  it("los nombres de indicador no traen referencias a notas al pie", {
    expect_false(any(stringr::str_detect(data_niveles$indicador, "\\d/")))
  })

  it("`poblacion` es numerica y sin NA", {
    expect_type(data_niveles$poblacion, "double")
    expect_true(all(!is.na(data_niveles$poblacion)))
  })

  it("filtra por un indicador especifico (regresion: filtro ignorado)", {
    # Bug original: `filtro_indicador` se pasaba como NULL fijo dentro del
    # map(), por lo que el argumento de la funcion no tenia ningun efecto.
    data_filtrada <- mercado_laboral_niveles("PEA")

    expect_true(nrow(data_filtrada) > 0)
    expect_true(all(stringr::str_detect(tolower(data_filtrada$indicador), "pea")))
    expect_lt(nrow(data_filtrada), nrow(data_niveles))
  })

  it("el filtro hace coincidencia parcial y no distingue mayusculas/minusculas", {
    data_filtrada <- mercado_laboral_niveles("desocup")

    expect_true(nrow(data_filtrada) > 0)
    expect_true(all(stringr::str_detect(tolower(data_filtrada$indicador), "desocup")))
  })

  it("el filtro acepta varios patrones a la vez", {
    data_filtrada <- mercado_laboral_niveles(c("cesantes", "nuevos"))

    expect_setequal(
      unique(tolower(data_filtrada$indicador)),
      c("cesantes", "nuevos")
    )
  })

  it("lanza un error cuando el filtro no coincide con ningun indicador", {
    expect_error(mercado_laboral_niveles("esto-no-existe"), "no devuelve ningún resultado")
  })

})

describe("mercado_laboral_tasas()", {

  data_tasas <- mercado_laboral_tasas()

  it("retorna un tibble con las columnas esperadas", {
    expect_s3_class(data_tasas, "tbl_df")
    expect_named(data_tasas, c("desagregacion", "indicador", "fecha", "tasa"))
  })

  it("retorna las tres desagregaciones esperadas", {
    expect_setequal(unique(data_tasas$desagregacion), c("Total", "Masculino", "Femenino"))
  })

  it("Masculino y Femenino no son copias de Total (regresion: hoja hardcodeada)", {
    total <- data_tasas |> dplyr::filter(desagregacion == "Total") |> dplyr::pull(tasa)
    masculino <- data_tasas |> dplyr::filter(desagregacion == "Masculino") |> dplyr::pull(tasa)
    femenino <- data_tasas |> dplyr::filter(desagregacion == "Femenino") |> dplyr::pull(tasa)

    expect_false(identical(total, masculino))
    expect_false(identical(total, femenino))
    expect_false(identical(masculino, femenino))
  })

  it("retorna los 15 indicadores esperados por desagregacion", {
    conteo_por_desagregacion <- data_tasas |>
      dplyr::count(desagregacion) |>
      dplyr::pull(n)

    expect_true(all(conteo_por_desagregacion %% 15 == 0))
  })

  it("los nombres de indicador no traen referencias a notas al pie", {
    expect_false(any(stringr::str_detect(data_tasas$indicador, "\\d/")))
  })

  it("`tasa` es numerica y sin NA", {
    expect_type(data_tasas$tasa, "double")
    expect_true(all(!is.na(data_tasas$tasa)))
  })

  it("filtra por un indicador especifico (regresion: filtro ignorado)", {
    data_filtrada <- mercado_laboral_tasas("inactividad")

    expect_true(nrow(data_filtrada) > 0)
    expect_true(all(stringr::str_detect(tolower(data_filtrada$indicador), "inactividad")))
    expect_lt(nrow(data_filtrada), nrow(data_tasas))
  })

  it("lanza un error cuando el filtro no coincide con ningun indicador", {
    expect_error(mercado_laboral_tasas("esto-no-existe"), "no devuelve ningún resultado")
  })

})

describe("mercado_laboral_niveles() y mercado_laboral_tasas() son consistentes entre si", {

  data_niveles <- mercado_laboral_niveles()
  data_tasas <- mercado_laboral_tasas()

  it("cubren el mismo rango de fechas", {
    expect_setequal(unique(data_niveles$fecha), unique(data_tasas$fecha))
  })

  it("tienen las mismas tres desagregaciones", {
    expect_setequal(unique(data_niveles$desagregacion), unique(data_tasas$desagregacion))
  })

})
