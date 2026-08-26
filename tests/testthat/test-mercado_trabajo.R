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
