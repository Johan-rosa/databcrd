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

#' Utility complement to handle the total exports data
#'
#' @format ## `exports_details`
#' A data frame with 64 rows and 6 columns:
#' \describe{
#'   \item{original_names}{Names in the excel file}
#'   \item{labels}{Names with meaning and without suffix}
#'   \item{short_names}{unique names in snake case}
#'   \item{categoria}{Category of the indicator}
#'   \item{nivel}{level}
#'   \item{direct_parent}{Direct parent}
#' }
"exports_details"

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

#' Utility complement to handle the total imports data
#'
#' @format ## `fiscal_details`
#' A data frame with 102 rows and 6 columns:
#' \describe{
#'   \item{original_names}{Names in the excel file}
#'   \item{labels}{Names with meaning and without suffix}
#'   \item{short_names}{unique names in snake case}
#'   \item{categoria}{Category of the indicator}
#'   \item{nivel}{level}
#'   \item{direct_parent}{Direct parent}
#' }
"fiscal_details"

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

#' CPI by item data, from October 2020 to December 2024
#'
#' @format ## `data_ipc_articulos_2020_2024`
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
"data_ipc_articulos_2020_2024"

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
"data_ipc_articulos_long_2010_2024"

