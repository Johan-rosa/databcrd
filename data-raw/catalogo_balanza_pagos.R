# data-raw/catalogo_balanza_pagos.R
library(tibble)

catalogo_balanza_pagos <- tribble(
  ~code,        ~nivel, ~code_padre, ~concepto,                                                              ~naturaleza, ~cuenta,             ~nota,               ~concepto_fuente,
  # -- 1. Cuenta Corriente ------------------------------------------------
  "1",          1L,     NA_character_, "Cuenta Corriente",                                                    "Saldo",       "Cuenta Corriente", NA_character_,       "1. Cuenta Corriente",
  "1.1",        2L,     "1",           "Balanza de Bienes y Servicios",                                       "Saldo",       "Cuenta Corriente", NA_character_,       "1.1 Balanza de Bienes y Servicios",
  "1.1.1",      3L,     "1.1",         "Balanza de Bienes",                                                   "Saldo",       "Cuenta Corriente", NA_character_,       "1.1.1 Balanza de Bienes",
  "1.1.1.1",    4L,     "1.1.1",       "Exportaciones",                                                       "Credito",     "Cuenta Corriente", NA_character_,       "Exportaciones",
  "1.1.1.1.1",  5L,     "1.1.1.1",     "Nacionales",                                                          "Credito",     "Cuenta Corriente", NA_character_,       "Nacionales",
  "1.1.1.1.2",  5L,     "1.1.1.1",     "Zonas Francas",                                                       "Credito",     "Cuenta Corriente", NA_character_,       "Zonas Francas",
  "1.1.1.2",    4L,     "1.1.1",       "Importaciones",                                                       "Debito",      "Cuenta Corriente", NA_character_,       "Importaciones",
  "1.1.1.2.1",  5L,     "1.1.1.2",     "Nacionales",                                                          "Debito",      "Cuenta Corriente", NA_character_,       "Nacionales",
  "1.1.1.2.2",  5L,     "1.1.1.2",     "Zonas Francas",                                                       "Debito",      "Cuenta Corriente", NA_character_,       "Zonas Francas",
  "1.1.2",      3L,     "1.1",         "Balanza de Servicios",                                                "Saldo",       "Cuenta Corriente", NA_character_,       "1.1.2 Balanza de Servicios",
  "1.1.2.1",    4L,     "1.1.2",       "Credito",                                                             "Credito",     "Cuenta Corriente", NA_character_,       "Cr\u00e9dito",
  "1.1.2.1.1",  5L,     "1.1.2.1",     "Viajes",                                                              "Credito",     "Cuenta Corriente", NA_character_,       "Viajes",
  "1.1.2.1.2",  5L,     "1.1.2.1",     "Servicios de Manufactura sobre Insumos Fisicos Pertenecientes a Terceros", "Credito", "Cuenta Corriente", NA_character_, "Servicios de Manufactura sobre insumos F\u00edsicos Pertenecientes a terceros",
  "1.1.2.1.3",  5L,     "1.1.2.1",     "Otros",                                                               "Credito",     "Cuenta Corriente", NA_character_,       "Otros",
  "1.1.2.2",    4L,     "1.1.2",       "Debito",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "D\u00e9bito",
  "1.1.2.2.1",  5L,     "1.1.2.2",     "Fletes",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "Fletes",
  "1.1.2.2.2",  5L,     "1.1.2.2",     "Otros",                                                               "Debito",      "Cuenta Corriente", NA_character_,       "Otros",

  "1.2",        2L,     "1",           "Ingreso Primario",                                                    "Saldo",       "Cuenta Corriente", NA_character_,       "1.2 Ingreso Primario",
  "1.2.1",      3L,     "1.2",         "Remuneracion de Empleados",                                           "Saldo",       "Cuenta Corriente", NA_character_,       "Remuneraci\u00f3n de Empleados",
  "1.2.1.1",    4L,     "1.2.1",       "Credito",                                                             "Credito",     "Cuenta Corriente", NA_character_,       "Cr\u00e9dito",
  "1.2.1.2",    4L,     "1.2.1",       "Debito",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "D\u00e9bito",
  "1.2.2",      3L,     "1.2",         "Renta de la Inversion",                                               "Saldo",       "Cuenta Corriente", NA_character_,       "Renta de la Inversi\u00f3n",
  "1.2.2.1",    4L,     "1.2.2",       "Inversion Extranjera Directa",                                       "Saldo",       "Cuenta Corriente", NA_character_,       "Inversi\u00f3n Extranjera Directa",
  "1.2.2.1.1",  5L,     "1.2.2.1",     "Credito",                                                             "Credito",     "Cuenta Corriente", NA_character_,       "Cr\u00e9dito",
  "1.2.2.1.2",  5L,     "1.2.2.1",     "Debito",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "D\u00e9bito",
  "1.2.2.2",    4L,     "1.2.2",       "Inversion de Cartera",                                                "Saldo",       "Cuenta Corriente", NA_character_,       "Inversi\u00f3n de Cartera",
  "1.2.2.2.1",  5L,     "1.2.2.2",     "Credito",                                                             "Credito",     "Cuenta Corriente", NA_character_,       "Cr\u00e9dito",
  "1.2.2.2.2",  5L,     "1.2.2.2",     "Debito",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "D\u00e9bito",
  "1.2.2.3",    4L,     "1.2.2",       "Otra Inversion",                                                      "Saldo",       "Cuenta Corriente", NA_character_,       "Otra Inversi\u00f3n",
  "1.2.2.3.1",  5L,     "1.2.2.3",     "Credito",                                                             "Credito",     "Cuenta Corriente", NA_character_,       "Cr\u00e9dito",
  "1.2.2.3.2",  5L,     "1.2.2.3",     "Debito",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "D\u00e9bito",
  "1.2.2.4",    4L,     "1.2.2",       "Activos de Reservas",                                                 "Saldo",       "Cuenta Corriente", NA_character_,       "Activos de Reservas",
  "1.2.2.4.1",  5L,     "1.2.2.4",     "Credito",                                                             "Credito",     "Cuenta Corriente", NA_character_,       "Cr\u00e9dito",
  "1.2.2.4.2",  5L,     "1.2.2.4",     "Debito",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "D\u00e9bito",

  "1.3",        2L,     "1",           "Ingreso Secundario",                                                  "Saldo",       "Cuenta Corriente", NA_character_,       "1.3 Ingreso Secundario",
  "1.3.1",      3L,     "1.3",         "Credito",                                                             "Credito",     "Cuenta Corriente", NA_character_,       "Cr\u00e9dito",
  "1.3.1.1",    4L,     "1.3.1",       "Remesas Familiares",                                                  "Credito",     "Cuenta Corriente", NA_character_,       "Remesas Familiares",
  "1.3.1.2",    4L,     "1.3.1",       "Otras Transferencias",                                                "Credito",     "Cuenta Corriente", NA_character_,       "Otras Transferencias",
  "1.3.2",      3L,     "1.3",         "Debito",                                                              "Debito",      "Cuenta Corriente", NA_character_,       "D\u00e9bito",
  "1.3.2.1",    4L,     "1.3.2",       "Remesas Familiares",                                                  "Debito",      "Cuenta Corriente", NA_character_,       "Remesas Familiares",
  "1.3.2.2",    4L,     "1.3.2",       "Otras Transferencias",                                                "Debito",      "Cuenta Corriente", NA_character_,       "Otras Transferencias",

  # -- 2. Cuenta de Capital ------------------------------------------------
  "2",          1L,     NA_character_, "Cuenta de Capital",                                                   "Saldo",       "Cuenta de Capital", "1/",               "2. Cuenta de Capital 1/",

  # -- 3. Prestamo / Endeudamiento Neto ------------------------------------
  "3",          1L,     NA_character_, "Prestamo / Endeudamiento Neto",                                       "Saldo",       "Prestamo Neto",     "Formula: 3 = 1 + 2", "3. Pr\u00e9stamo /\r\n    Endeudamiento Neto (3=1+2)",

  # -- 4. Cuenta Financiera -------------------------------------------------
  "4",          1L,     NA_character_, "Cuenta Financiera",                                                   "Saldo",       "Cuenta Financiera", NA_character_,       "4. Cuenta Financiera",
  "4.1",        2L,     "4",           "Inversion Directa",                                                   "Saldo",       "Cuenta Financiera", NA_character_,       "Inversi\u00f3n Directa",
  "4.2",        2L,     "4",           "Inversion de Cartera",                                                "Saldo",       "Cuenta Financiera", NA_character_,       "Inversi\u00f3n de Cartera",
  "4.3",        2L,     "4",           "Deuda Publica y Privada Mediano y Largo Plazo (Neto)",                "Saldo",       "Cuenta Financiera", NA_character_,       "Deuda Pub. y Priv. Med. y LP (Neto)",
  "4.4",        2L,     "4",           "Deuda Publica y Privada Corto Plazo (Neto)",                          "Saldo",       "Cuenta Financiera", NA_character_,       "Deuda Pub. y Priv. Corto Plazo (Neto)",
  "4.5",        2L,     "4",           "Moneda y Depositos",                                                  "Saldo",       "Cuenta Financiera", NA_character_,       "Moneda y Dep\u00f3sitos",
  "4.6",        2L,     "4",           "Otros",                                                               "Saldo",       "Cuenta Financiera", "2/",               "Otros  2/",

  # -- 5. Saldo --------------------------------------------------------------
  "5",          1L,     NA_character_, "Saldo",                                                               "Saldo",       "Saldo",             NA_character_,       "5. Saldo",

  # -- 6. Errores y Omisiones -------------------------------------------------
  "6",          1L,     NA_character_, "Errores y Omisiones",                                                 "Saldo",       "Errores y Omisiones", NA_character_,     "6. Errores y Omisiones",

  # -- 7. Financiamiento -------------------------------------------------------
  "7",          1L,     NA_character_, "Financiamiento",                                                      "Saldo",       "Financiamiento",     NA_character_,      "7. Financiamiento",
  "7.1",        2L,     "7",           "Activos de Reservas",                                                 "Saldo",       "Financiamiento",     NA_character_,      "Activos de Reservas",
  "7.2",        2L,     "7",           "Uso del Credito y Prestamos del FMI",                                 "Saldo",       "Financiamiento",     "3/",               "Uso del cr\u00e9dito y  Pr\u00e9stamos del FMI 3/",
  "7.3",        2L,     "7",           "Transferencias (Condonacion de Deudas)",                              "Saldo",       "Financiamiento",     NA_character_,      "Transferecias (Condonaci\u00f3n de deudas)",
  "7.4",        2L,     "7",           "Otra Inversion - Pasivos",                                            "Saldo",       "Financiamiento",     "4/",               "Otra inversi\u00f3n-pasivos 4/",
)

usethis::use_data(catalogo_balanza_pagos, overwrite = TRUE)
