# china_cpi.R -----------------------------------------------------------
#
# Functions to locate and download Consumer Price Index (CPI) data from
# China's National Bureau of Statistics (NBS) data portal
# (https://data.stats.gov.cn).
#
# Typical usage:
#   china_price_index_node_id()   # (re-)discover the CPI node id, if needed
#   china_series_cpi_metadata()   # list of individual CPI indicators
#   china_cpi()                   # download the actual monthly values
# -------------------------------------------------------------------------

#' Common HTTP headers for NBS data-portal requests
#'
#' Internal helper returning the headers shared by all requests made to the
#' NBS `data.stats.gov.cn` data portal. Centralizing them avoids repeating
#' (and risking inconsistent) header sets across functions.
#'
#' @return A named list of HTTP headers.
#' @noRd
china_stats_headers <- function() {
  list(
    "accept"           = "application/json, text/plain, */*",
    "accept-language"  = "en-US,en;q=0.9,es-DO;q=0.8,es;q=0.7",
    "client"           = "pc",
    "Referer"          = "https://data.stats.gov.cn/dg/website/page.html"
  )
}

#' Perform an httr2 request with consistent error handling
#'
#' Wraps [httr2::req_perform()] in `tryCatch()` so that network failures,
#' timeouts, and HTTP error statuses raise a single, informative condition
#' (class `"china_stats_request_error"`) instead of a raw curl/httr2 error.
#' This keeps the calling functions free of repeated error-handling
#' boilerplate.
#'
#' @param req An `httr2_request` object.
#' @param action A short human-readable description of what the request is
#'   trying to do (used in the error message), e.g. `"fetch tree node"`.
#'
#' @return An `httr2_response` object on success.
#' @noRd
china_stats_perform <- function(req, action = "check the NBS data portal") {
  tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      rlang::abort(
        message = sprintf("Failed to %s: %s", action, conditionMessage(e)),
        class   = "china_stats_request_error",
        parent  = e
      )
    }
  )
}

#' Navigate the NBS data-portal folder tree
#'
#' The NBS data portal organizes its data series in a tree of "nodes"
#' (folders and leaves). This function queries one level of that tree given
#' a parent node id, and is the building block used to locate specific
#' indicators (e.g. CPI) by walking down from the root.
#'
#' @param parent_id Character. The `id` of the parent node to list children
#'   for. Use `NULL` (the default) to fetch the root of the tree.
#' @param code Integer/character. Which panel of the site to query: `1` =
#'   "Browse topics" / "advanced search" panel, `19` = "Visualized" panel
#'   (the default). During development, `code = 19` was the panel that
#'   reliably contained the Price Index section, but this is not guaranteed
#'   to remain stable if the site changes.
#'
#' @return A tibble with (when available) columns `level`, `parent_id`,
#'   `id`, `name`, and `dt` -- one row per child node. Returns a 0-row
#'   tibble if the node has no children.
#'
#' @examples
#' \dontrun{
#' root <- china_tree_node()
#' sections <- china_tree_node(root$id)
#' }
#' @export
china_tree_node <- function(parent_id = NULL, code = 19) {
  url <- "https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndexTreeAsync"

  req <- httr2::request(url) |>
    httr2::req_url_query(pid = parent_id, code = code) |>
    httr2::req_headers(!!!china_stats_headers()) |>
    httr2::req_method("GET")

  resp <- china_stats_perform(req, action = "fetch a data-portal tree node")
  data <- httr2::resp_body_json(resp)

  if (length(data$data) == 0) {
    return(dplyr::tibble())
  }

  data$data |>
    purrr::map(\(x) dplyr::as_tibble(x[!sapply(x, is.null)])) |>
    purrr::list_rbind() |>
    dplyr::select(
      dplyr::any_of(
        c("level" = "treeinfo_level", "parent_id" = "treeinfo_pid", "id" = "_id", "name", "dt")
      )
    )
}

#' Find the node id for China's CPI indicator category
#'
#' Walks the NBS data-portal tree (root -> sections -> indicators) to locate
#' the id of the "Consumer Price Indices by Category" node under "Price
#' Index". This id (`cid`) is what [china_series_cpi_metadata()] and
#' [china_cpi()] need in order to query the actual CPI series.
#'
#' During development this id was observed to be the fixed value
#' `"5353d942c68f42c789c7d8c546510ff4"`, which is what those two functions
#' use as their default. This function exists so that value can be
#' re-derived programmatically if the site ever changes it.
#'
#' @return A character scalar with the node id.
#'
#' @examples
#' \dontrun{
#' china_price_index_node_id()
#' }
#' @export
china_price_index_node_id <- function() {
  root     <- china_tree_node()
  sections <- china_tree_node(root$id)

  cpi_section <- sections[sections$name == "Price Index", ]
  if (nrow(cpi_section) == 0) {
    rlang::abort(
      "Could not find a 'Price Index' section in the data-portal tree. The site structure may have changed.",
      class = "china_stats_node_not_found"
    )
  }

  cpi_indicators <- china_tree_node(cpi_section$id)
  index <- which(stringr::str_detect(cpi_indicators$name, "^Consumer Price Indices by Category"))

  if (length(index) == 0) {
    rlang::abort(
      "Could not find a 'Consumer Price Indices by Category' indicator. The site structure may have changed.",
      class = "china_stats_node_not_found"
    )
  }

  cpi_indicators$id[index]
}

#' Fetch metadata for China's CPI series
#'
#' Retrieves the list of individual CPI indicators (id + display name)
#' available under a given data-portal node (`cid`) -- e.g. "CPI, current
#' month=same month of previous year", "CPI, current month=previous month",
#' etc. The resulting ids are needed to query actual values via
#' [china_cpi()].
#'
#' @param cid Character. The data-portal node id for the CPI-by-category
#'   indicator group. Defaults to `"5353d942c68f42c789c7d8c546510ff4"`, the
#'   value observed during development (see [china_price_index_node_id()]
#'   to re-derive it if this ever stops working).
#'
#' @return A tibble with columns `id` and `name`, one row per CPI indicator.
#'
#' @examples
#' \dontrun{
#' china_series_cpi_metadata()
#' }
#' @export
china_series_cpi_metadata <- function(cid = "5353d942c68f42c789c7d8c546510ff4") {
  req <- httr2::request("https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndicatorsByCid") |>
    httr2::req_url_query(cid = cid, dt = "", name = "") |>
    httr2::req_headers(!!!china_stats_headers())

  resp <- china_stats_perform(req, action = "fetch CPI series metadata")
  data <- httr2::resp_body_json(resp)

  if (length(data$data$list) == 0) {
    rlang::abort(
      sprintf("No CPI indicators found for cid = '%s'.", cid),
      class = "china_stats_empty_result"
    )
  }

  data$data$list |>
    purrr::map(\(x) dplyr::as_tibble(x[c("_id", "i_showname")])) |>
    purrr::list_rbind() |>
    dplyr::rename(id = "_id", name = "i_showname")
}

#' Download China's Consumer Price Index (CPI) series
#'
#' Downloads monthly CPI data (all categories available under the CPI node)
#' from the NBS data portal, for a given date range.
#'
#' @param start_period Integer or character in `"YYYYMM"` format. First
#'   month to request. Defaults to `201601`.
#' @param end_period Integer or character in `"YYYYMM"` format. Last month
#'   to request. Defaults to the current year and month.
#' @param cid Character. The data-portal node id for the CPI-by-category
#'   indicator group, passed to [china_series_cpi_metadata()]. Defaults to
#'   `"5353d942c68f42c789c7d8c546510ff4"`.
#'
#' @return A tibble in long format with columns `id`, `value`, `date`, and
#'   `name` (one row per indicator per month). Rows with a missing (`NA`)
#'   value are dropped.
#'
#' @examples
#' \dontrun{
#' china_cpi(start_period = 202001, end_period = 202312)
#' }
#' @export
china_cpi <- function(
    start_period = 201601,
    end_period   = NULL,
    cid          = "5353d942c68f42c789c7d8c546510ff4"
) {
  start_period <- as.character(start_period)
  if (!grepl("^[0-9]{6}$", start_period)) {
    rlang::abort("`start_period` must be in 'YYYYMM' format, e.g. 201601.")
  }

  if (is.null(end_period)) {
    end_period <- format(Sys.Date(), "%Y%m")
  } else {
    end_period <- as.character(end_period)
    if (!grepl("^[0-9]{6}$", end_period)) {
      rlang::abort("`end_period` must be in 'YYYYMM' format, e.g. 202312.")
    }
  }

  if (start_period > end_period) {
    rlang::abort("`start_period` must be earlier than or equal to `end_period`.")
  }

  metadata <- china_series_cpi_metadata(cid = cid)

  body <- list(
    cid          = cid,
    indicatorIds = metadata$id,
    daCatalogId  = "",
    das          = list(list(text = "全国", value = "000000000000")),
    showType     = "1",
    dts          = as.list(paste0(start_period, "MM-", end_period, "MM")),
    rootId       = "0cff94832c7f4cbe9ca57b7c0ef09704"
  )

  req <- httr2::request("https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/stream/esData") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      accept            = "*/*",
      `accept-language` = "en-US,en;q=0.9,es-DO;q=0.8,es;q=0.7",
      `content-type`    = "application/json",
      referer           = "https://data.stats.gov.cn/dg/website/page.html"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE)

  resp <- china_stats_perform(req, action = "download CPI data")
  data <- httr2::resp_body_json(resp)

  if (length(data$data) == 0) {
    rlang::abort(
      "The request succeeded but returned no data. Check the date range and cid.",
      class = "china_stats_empty_result"
    )
  }

  data$data |>
    purrr::map(\(x) {
      purrr::map(x$values, \(v) dplyr::as_tibble(v[c("_id", "value")])) |>
        purrr::list_rbind() |>
        dplyr::mutate(date = lubridate::my(x$name)) |>
        dplyr::rename(id = "_id")
    }) |>
    purrr::list_rbind() |>
    dplyr::left_join(metadata, by = "id") |>
    dplyr::mutate(
      value = as.numeric(value),
      name  = stringr::str_squish(name)
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::relocate(id, name, date, value)
}
