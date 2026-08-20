#' Cotizaciones de futuros de CME Group
#'
#' Descarga las cotizaciones vigentes de un contrato de futuros publicado
#' por CME Group (Chicago Mercantile Exchange), a partir del identificador
#' numerico del producto. Por defecto consulta los futuros de oro (Gold,
#' \code{product_id = 437}).
#'
#' @param product_id Numero entero con el identificador del producto en
#'   CME Group (por ejemplo, \code{437} para oro). Por defecto \code{437}.
#'
#' @return Un tibble con una fila por mes de vencimiento del contrato y las
#'   siguientes columnas:
#' \describe{
#'   \item{date}{Fecha (primer dia del mes de vencimiento del contrato).}
#'   \item{year}{Anio de vencimiento del contrato.}
#'   \item{month}{Mes de vencimiento del contrato.}
#'   \item{last}{Ultimo precio negociado.}
#'   \item{prior_settle}{Precio de liquidacion de la sesion anterior.}
#'   \item{open}{Precio de apertura.}
#'   \item{high}{Precio maximo de la sesion.}
#'   \item{low}{Precio minimo de la sesion.}
#'   \item{volume}{Volumen negociado.}
#' }
#'
#' @details
#' La funcion consulta el endpoint publico
#' \code{https://www.cmegroup.com/CmeWS/mvc/quotes/v2/<product_id>}, el cual
#' devuelve, entre otra informacion, un listado de cotizaciones por mes de
#' vencimiento (\code{quotes}). Cada solicitud incluye un timestamp en
#' milisegundos (\code{_t}) para evitar respuestas cacheadas.
#'
#' @source CME Group. <https://www.cmegroup.com/>.
#'
#' @examples
#' \dontrun{
#' cme_futuros()
#' cme_futuros(product_id = 437)
#' }
#'
#' @export
cme_futuros <- function(product_id = 437) {

  checkmate::assert_int(product_id, lower = 1)

  result <- tryCatch(
    httr2::request("https://www.cmegroup.com/CmeWS/mvc/quotes/v2") |>
      httr2::req_url_path_append(product_id) |>
      httr2::req_url_query(
        isProtected = "",
        `_t` = as.numeric(Sys.time()) * 1000
      ) |>
      httr2::req_headers(
        `User-Agent` = paste(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          "(KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36"
        ),
        `Accept` = "application/json"
      ) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),

    error = function(e) {
      rlang::abort(
        paste(
          "No fue posible consultar las cotizaciones de CME Group.",
          conditionMessage(e)
        ),
        parent = e
      )
    }
  )

  if (httr2::resp_status(result) >= 400) {
    rlang::abort(
      paste0(
        "La solicitud a CME Group fallo (",
        httr2::resp_status(result),
        "): ",
        httr2::resp_status_desc(result)
      )
    )
  }

  quotes <- httr2::resp_body_json(result)$quotes

  if (length(quotes) == 0) {
    rlang::abort(
      paste0("CME Group no devolvio cotizaciones para product_id = ", product_id, ".")
    )
  }

  quotes |>
    purrr::map(
      \(x) {
        tibble::tibble(
          date = lubridate::my(x$expirationMonth),
          year = lubridate::year(date),
          month = lubridate::month(date),
          last = x$last,
          prior_settle = x$priorSettle,
          open = x$open,
          high = x$high,
          low  = x$low,
          volume = x$volume
        ) |>
          dplyr::mutate(dplyr::across(-c(date, year, month), readr::parse_number)) |>
          suppressWarnings()
      }
    ) |>
    purrr::list_rbind()
}
