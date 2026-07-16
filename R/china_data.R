
#' Esta función es para obtener el id global del catalogo de indicadores.
#' Es necesario para request subsiguientes. Al momento del desarrollo el id es
#' "fc982599aa684be7969d7b90b1bd0e84", pero creo que podría cambiar.
#'
#' Selectionar el elemento `_id`. Ese es el ID.

china_tree_node <- function(parent_id = NULL, code = 1) {
  url <- "https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndexTreeAsync"
  page_tree <- httr2::request(url) |>
    httr2::req_url_query(pid = parent_id, code = code) |>
    httr2::req_headers(
      "accept" = "application/json, text/plain, */*",
      "client" = "pc",
      Referer = "https://data.stats.gov.cn/dg/website/page.html",
      dt = "2025-2026"
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  data <- page_tree |> httr2::resp_body_json()
  data$data |>
    purrr::map(\(x) dplyr::as_tibble(x[!sapply(x, is.null)])) |>
    purrr::list_rbind() |>
    dplyr::select(
      dplyr::any_of(
        c("level" = "treeinfo_level", "parent_id" = "treeinfo_pid", "id" = "_id", "name", "dt")))
}

china_series_list <- function(indicator_id) {
  url <- "https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndicatorsByCid"

  resp <- httr2::request(url) |>
    httr2::req_url_query(
      "cid" = indicator_id
    ) |>
    httr2::req_headers(
      "accept" = "application/json, text/plain, */*",
      "client" = "pc",
      Referer = "https://data.stats.gov.cn/dg/website/page.html"
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  data <- resp |> httr2::resp_body_json()

  data$data$list |>
    purrr::map(\(x) {
      dplyr::as_tibble(x[!sapply(x, is.null)])
    }) |>
    purrr::list_rbind() |>
    dplyr::select(dplyr::any_of(
      c("id" = "_id", "parent_id" = "catalogid", "name" = "i_showname")
    ))
}

china_indicator_data <- function(series_id, parent_id, root_id) {
  url <- "https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/stream/esData"

  body <- list(
    cid = parent_id,
    indicatorIds = as.list(series_id),
    daCatalogId = "",
    das = list(
      list(
        text = "全国",
        value = "000000000000"
      )
    ),
    showType = "1",
    dts = "",
    rootId = root_id
  )

  resp <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      Accept = "*/*",
      Referer = "https://data.stats.gov.cn/dg/website/page.html"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_perform()

  data <- resp |> httr2::resp_body_json()

  data$data |>
    purrr::map(
      \(data) {
        code <- data$code
        name <- data$name
        data$values |>
          purrr::map(\(x) dplyr::as_tibble(x[c("_id", "value")])) |>
          purrr::list_rbind() |>
          dplyr::mutate(period = name, month_code = code)
      }
    ) |>
    purrr::list_rbind() |>
    dplyr::rename("id" = "_id")
}


china_inflation <- function() {
  root <- china_tree_node()
  sections <- china_tree_node(root$id)
  indicadores <- china_tree_node(sections$id[1])

  series <- china_tree_node(indicadores$id[1]) |>
    dplyr::filter(stringr::str_detect(name, "^Consumer Price Indices by Category"))

    purrr::map(
      seq_len(nrow(series)),
      \(serie_index) {
        serie <- series[serie_index, ]
        series_details <- china_series_list(serie$id)
        series_details$id |>
          china_indicator_data(parent_id = serie$id, root_id = root$id) |>
          dplyr::left_join(series_details, by = "id") |>
          dplyr::mutate(date = lubridate::my(period)) |>
          dplyr::select(id, name, date, value)
      },
      .progress = TRUE
    ) |>
      purrr::list_rbind()
}


# Another approach
#TODO: Add httr2 :: namespace

china_series_cpi_metadata <- function() {
  resp <- httr2::request("https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndicatorsByCid") |>
    httr2::req_url_query(
      cid = "5353d942c68f42c789c7d8c546510ff4",
      dt = "",
      name = ""
    ) |>
    httr2::req_headers(
      accept = "application/json, text/plain, */*",
      `accept-language` = "en-US,en;q=0.9,es-DO;q=0.8,es;q=0.7",
      client = "pc",
      Referer = "https://data.stats.gov.cn/dg/website/page.html"
    ) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp)

  data$data$list |>
    purrr::map(
      \(x) dplyr::as_tibble(x[c("_id", "i_showname")])
    ) |>
    purrr::list_rbind() |>
    dplyr::rename("id" = "_id", "name" = "i_showname")
}

china_cpi <- function(
    start_period = 201601,
    end_period   = NULL
) {

  if (is.null(end_period)) {
    end_period <- format(Sys.Date(), "%Y%m")
  }

  metadata <- china_series_cpi_metadata()

  body <- list(
    cid          = "5353d942c68f42c789c7d8c546510ff4",
    indicatorIds = metadata$id,
    daCatalogId = "",
    das = list(
      list(text = "全国", value = "000000000000")
    ),
    showType = "1",
    dts = as.list(paste0(start_period, "MM-", end_period, "MM")),
    rootId = "0cff94832c7f4cbe9ca57b7c0ef09704"
  )


  resp <- httr2::request("https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/stream/esData") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      accept          = "*/*",
      `accept-language` = "en-US,en;q=0.9,es-DO;q=0.8,es;q=0.7",
      `content-type`  = "application/json",
      referer         = "https://data.stats.gov.cn/dg/website/page.html"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_perform()

  data <- resp |>
    httr2::resp_body_json()

  data$data |>
    purrr::map(
      \(x) {
        purrr::map(x$values, \(x) dplyr::as_tibble(x[c("_id", "value")])) |>
          purrr::list_rbind() |>
          dplyr::mutate(date = lubridate::my(x$name)) |>
          dplyr::rename("id" = "_id")
      }
    ) |>
    purrr::list_rbind() |>
    dplyr::left_join(metadata, by = "id") |>
    dplyr::mutate(
      value = as.numeric(value),
      name  = stringr::str_squish(name)
    ) |>
    dplyr::filter(!is.na(value))
}
