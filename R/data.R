#' Utility complement to handle the monetary indicators data
#'
#' @format ## `indicadores_bcrd_details`
#' A data frame with 39 rows and 6 columns:
#' \describe{
#'   \item{original_names}{Names in the excel file}
#'   \item{labels}{Names with meaning and without suffix}
#'   \item{short_names}{unique names in snake case}
#'   \item{categoria}{Category of the indicator}
#'   \item{nivel}{level}
#'   \item{direct_parent}{Direct parent}
#' }
"indicadores_bcrd_details"

#' Utility complement to handle the total imports data
#'
#' @format ## `imports_details`
#' A data frame with 64 rows and 6 columns:
#' \describe{
#'   \item{original_names}{Names in the excel file}
#'   \item{labels}{Names with meaning and without suffix}
#'   \item{short_names}{unique names in snake case}
#'   \item{categoria}{Category of the indicator}
#'   \item{nivel}{level}
#'   \item{direct_parent}{Direct parent}
#' }
"imports_details"

#' Utility complement to handle the macroeconomic expectations
#'
#' @format ## `expectativas_details`
#' A data frame with 39 rows and 6 columns:
"expectativas_details"


#' Utility complement to handle the CPI by item
#'
#' @format ## `ipc_articulos_details`
#' A data frame with 612 rows and 26 columns:
#'
#' \describe{
#'   \item{posicion}{Order in the original excel}
#'   \item{nombre}{Item name, it can be a group, class or other level}
#'   \item{agregacion}{This is the level of the given item}
#'   \item{group}{Group name}
#'   \item{subgrupo}{Subgroup name}
#'   \item{clase}{Class name}
#'   \item{subclase}{Subclass name}
#'   \item{articulo}{Item name}
#'   \item{is_grupo}{Boolean indicating if is a group}
#'   \item{is_subgrupo}{Boolean indicating if is a subgroup}
#'   \item{is_clase}{Boolean indicating if is a class}
#'   \item{is_subclase}{Boolean indicating if is a subclass}
#'   \item{is_articulo}{Boolean: is an item?}
#'   \item{is_subyacente}{Boolean: is part of the core inflation?}
#'   \item{is_no_subyacente}{Boolean: is part of the non-core inflation items?}
#'   \item{is_transables}{Boolean: is a tradable good?}
#'   \item{is_no_transables}{Boolean: is a non-tradable good?}
#'   \item{is_bienes}{Boolean: is a good?}
#'   \item{is_servicios}{Boolean: is a service?}
#'   \item{is_alimentos}{Boolean: is a food item?}
#'   \item{is_transporte}{Boolean: is a transportation item?}
#'   \item{is_vivienda}{Boolean: is housing?}
#'   \item{is_resto}{Boolean: is others?}
#'   \item{is_nuevo}{Boolean: was introduced in the most recent basket?}
#'   \item{codigo_articulo}{id of the item}
#'   \item{ponderacion_ipc}{weight of the element as proportion of the general basket}
#' }
"ipc_articulos_details"

#' CPI by item data, from October 2020 to December 2025
#'
#' @format ## `data_ipc_articulos_2020_2025`
#' A data frame with 612 rows and 26 columns:
#'
#' \describe{
#'   \item{id}{Order in the original excel}
#'   \item{nombre}{Item name, it can be a group, class or other level}
#'   \item{agregacion}{This is the level of the given item}
#'   \item{group}{Group name}
#'   \item{subgrupo}{Subgroup name}
#'   \item{clase}{Class name}
#'   \item{subclase}{Subclass name}
#'   \item{articulo}{Item name}
#'   \item{articulo}{Item name}
#'   \item{date}{Date}
#'   \item{year}{Year}
#'   \item{mes}{Month}
#'   \item{ponderacion_ipc}{Weight of the element as proportion of the general basket}
#'   \item{indice}{item index}
#' }
#' @keywords internal
"data_ipc_articulos_2020_2025"

#' CPI by item data, from 2010 to December 2024
#'
#' @format ## `data_ipc_articulos_long_2010_2024`
#' A data frame with 612 rows and 26 columns:
#'
#' \describe{
#'   \item{id}{Order in the original excel}
#'   \item{nombre}{Item name, it can be a group, class or other level}
#'   \item{agregacion}{This is the level of the given item}
#'   \item{group}{Group name}
#'   \item{subgrupo}{Subgroup name}
#'   \item{clase}{Class name}
#'   \item{subclase}{Subclass name}
#'   \item{articulo}{Item name}
#'   \item{articulo}{Item name}
#'   \item{date}{Date}
#'   \item{year}{Year}
#'   \item{mes}{Month}
#'   \item{ponderacion_ipc}{Weight of the element as proportion of the general basket}
#'   \item{indice}{item index}
#' }
#' @keywords internal
"data_ipc_articulos_long_2010_2024"

#' Metadatos para las series de indicadores de las OSD
#'
#' Tabla interna que contiene la clasificación de las series publicadas en el
#' panorama monetario de las Otras Sociedades de Depósito (OSD). Su propósito es
#' estandarizar las etiquetas originales de las publicaciones del Banco Central
#' de la República Dominicana y facilitar la construcción de consultas,
#' transformaciones y visualizaciones.
#'
#' Cada fila representa una serie o un elemento de la jerarquía de encabezados,
#' asociando la etiqueta original con un conjunto de variables descriptivas.
#'
#' @format Un tibble con las siguientes variables:
#' \describe{
#'   \item{original_label}{Etiqueta original tal como aparece en la publicación
#'   del Banco Central.}
#'   \item{variable}{Agregado económico al que pertenece la serie, por ejemplo
#'   `"Préstamos"`, `"Depósitos"` o `"Activos externos"`.}
#'   \item{sector}{Sector institucional o componente del agregado al que hace
#'   referencia la serie, por ejemplo `"Sector público"`, `"Sector privado"`,
#'   `"Sociedades financieras"` o `"Depósitos transferibles"`. Cuando no aplica,
#'   toma el valor `NA`.}
#'   \item{moneda}{Moneda en la que está expresada la serie. Los valores
#'   utilizados son `"DOP"`, `"USD"` y `"Total"`. Cuando la clasificación por
#'   moneda no aplica, toma el valor `NA`.}
#'   \item{entidad}{Nivel institucional de agregación de la serie:
#'   `"General"`, `"Bancos múltiples"` o `"Resto OSD"`.}
#' }
#'
#' @keywords internal
#' @name detalles_indicadores_osd
#' @noRd
"detalles_indicadores_osd"

#' Catalogo jerarquico de cuentas de la Balanza de Pagos
#'
#' Catalogo de referencia con la estructura de cuentas de la Balanza de
#' Pagos de la Republica Dominicana (formato MBP6), tal como la publica
#' el BCRD en el archivo "Balanza de Pagos" (hoja `BOP_TRIM`). Cada fila
#' representa una cuenta o subcuenta, con su codigo jerarquico decimal,
#' nivel de profundidad, codigo padre y naturaleza contable, para poder
#' unirse contra los datos crudos por posicion sin depender de parsear
#' el texto de cada fila con expresiones regulares.
#'
#' @details
#' El orden de las 57 filas de este catalogo coincide exactamente, fila
#' por fila, con las 57 filas de datos de la hoja `BOP_TRIM` (columna A,
#' filas 12-77, excluyendo separadores en blanco y notas al pie). Esa
#' correspondencia posicional fue verificada explicitamente contra el
#' archivo fuente y es la base de como [balanza_pagos()] une los datos
#' crudos con este catalogo: por posicion, no por texto.
#'
#' `concepto` no es una copia literal del texto del Excel; se
#' normalizo para uso consistente en el paquete:
#' \itemize{
#'   \item Se removieron acentos (`"Credito"`, no `"Crédito"`).
#'   \item Las llamadas al pie (`1/`, `2/`, `3/`, `4/`) y la formula
#'     `(3=1+2)` se movieron a la columna `nota`.
#'   \item Se expandieron abreviaturas (p. ej. `"Deuda Pub. y Priv. Med.
#'     y LP (Neto)"` -> `"Deuda Publica y Privada Mediano y Largo Plazo
#'     (Neto)"`).
#'   \item Se corrigio el typo de la fuente `"Transferecias"` ->
#'     `"Transferencias"`.
#' }
#'
#' `naturaleza` toma siempre uno de tres valores: `"Credito"`,
#' `"Debito"` o `"Saldo"`. Las filas de subtotal (que suman a sus
#' hijos, p. ej. `"Cuenta Corriente"`, `"Balanza de Bienes"`) y las
#' cuentas que el BCRD reporta ya netas sin desglose de credito/debito
#' (p. ej. los componentes de la Cuenta Financiera) quedan como
#' `"Saldo"`.
#'
#' "Exportaciones" se codifico como Credito e "Importaciones" como
#' Debito por convencion estandar de balanza de pagos; el archivo
#' fuente no las etiqueta explicitamente con esos terminos (a
#' diferencia de Balanza de Servicios, que si dice "Credito"/"Debito").
#'
#' @format Un tibble con 57 filas y 7 columnas:
#' \describe{
#'   \item{code}{`chr`. Codigo jerarquico decimal (`"1"`, `"1.1"`,
#'     `"1.1.1.1"`, ...). Respeta la numeracion original de la fuente
#'     para los niveles 1 a 3 y la extiende hacia abajo para las filas
#'     sin numeral propio (p. ej. "Exportaciones", "Credito").}
#'   \item{nivel}{`int`. Profundidad en la jerarquia, de 1 (cuentas
#'     principales: Cuenta Corriente, Cuenta de Capital, etc.) a 5
#'     (el nivel de detalle mas fino, p. ej. "Nacionales" dentro de
#'     "Exportaciones").}
#'   \item{code_padre}{`chr`. `code` de la fila padre en la jerarquia;
#'     `NA` en las cuentas de nivel 1. Permite reconstruir el arbol o
#'     hacer joins recursivos.}
#'   \item{concepto}{`chr`. Descripcion de la cuenta, normalizada (ver
#'     `@details`).}
#'   \item{naturaleza}{`chr`. `"Credito"`, `"Debito"` o `"Saldo"`.}
#'   \item{cuenta}{`chr`. Nombre de la cuenta de nivel 1 a la que
#'     pertenece la fila (p. ej. `"Cuenta Corriente"`,
#'     `"Cuenta Financiera"`, `"Financiamiento"`), util para filtrar
#'     sin tener que interpretar el codigo.}
#'   \item{nota}{`chr`. Llamada al pie (`"1/"`, `"2/"`, `"3/"`,
#'     `"4/"`) o formula (`"Formula: 3 = 1 + 2"`) asociada a la fila
#'     en la fuente; `NA` cuando no aplica.}
#' }
#'
#' @source
#' Estructura tomada de "BALANZA DE PAGOS DE LA REPUBLICA DOMINICANA -
#' RESULTADOS CONFORME AL MBP6", Departamento Internacional, BCRD
#' (hoja `BOP_TRIM` del archivo de balanza de pagos). Catalogo
#' construido y validado manualmente contra esa hoja.
#'
#' @seealso [balanza_pagos()], que une los datos trimestrales crudos
#'   contra este catalogo por posicion.
#'
#' @examples
#' # Todas las cuentas de la Cuenta Corriente
#' catalogo_balanza_pagos |>
#'   dplyr::filter(cuenta == "Cuenta Corriente")
#'
#' # Solo las filas "hoja" (sin hijos), utiles para no doble-contar al sumar
#' catalogo_balanza_pagos |>
#'   dplyr::filter(!code %in% code_padre)
"catalogo_balanza_pagos"
