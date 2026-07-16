
# Navigate the folder tree of the web application.
# Code refere to the page panel: use 1 for "Browse topics" and "advance search", 19 for the Visualized
# With multiple fetch you can navigate an check if the ids changed.
china_tree_node <- function(parent_id = NULL, code = 19) {
  url <- "https://data.stats.gov.cn/dg/website/publicrelease/en/web/external/new/queryIndexTreeAsync"
  page_tree <- httr2::request(url) |>
    httr2::req_url_query(pid = parent_id, code = code) |>
    httr2::req_headers(
      "accept" = "application/json, text/plain, */*",
      "client" = "pc",
      Referer = "https://data.stats.gov.cn/dg/website/page.html"
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

# This function serach for the ID of the CPI indicators. During development it was
# 5353d942c68f42c789c7d8c546510ff4 but is not certain that it is an static value.
# This is a way to get it using the API.
china_price_index_node_id <- function() {
  root     <- china_tree_node()
  sections <- china_tree_node(root$id)
  cpi_section   <- sections[sections$name == "Price Index", ]
  cpi_indicators <- china_tree_node(cpi_section$id)
  index <- which(stringr::str_detect(cpi_indicators$name, "^Consumer Price Indices by Category"))
  cpi_indicators$id[index]
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
