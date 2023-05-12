
#' Download GROSS FIXED CAPITAL FORMATION BY SECTOR AND TYPE OF GOOD
#'
#' Get the series of the gross fixed capital formation by sector and
#' type of good of the Dominican Republic.
#'
#' @return a data frame with monthly series
#' @export
#'
#' @examples
#' get_fbkf()

get_fbkf <- function() {
  file_url <- paste0("https://cdn.bancentral.gov.do/documents/",
                     "estadisticas/sector-real/",
                     "documents/fbkf.xlsx?v=1634051828540")
  file_path <- tempfile(pattern = "", fileext = "")
  utils::download.file(file_url, file_path, mode = "wb", quiet = TRUE)

  skip_lines <- c(6, 20, 34)
  data <- suppressMessages(
    purrr::map(skip_lines,
               \(.x) readxl::read_excel(file_path,
                                        sheet = 1L,
                                        skip = .x,
                                        n_max = 8,
                                        col_names = TRUE)[, -1])
  )

  wrangling <- function(x, value) {
    df <- data.frame(year = 2006 + 1:ncol(x),
                     total = t(x[1, ]),
                     privado = t(x[3, ]),
                     publico = t(x[4, ]),
                     construccion = t(x[6, ]),
                     maquinaria_y_equipos = t(x[7, ])) |>
      tidyr::pivot_longer(!year,
                          names_to = "categoria",
                          values_to = value)

    return(df)
  }

  data <- purrr::map2(.x = data,
                        .y = c("monto", "variacion_interanual", "proporcion"),
                        .f = wrangling)

  fbkf <- dplyr::left_join(x = data[[1]],
                           y = data[[2]],
                           by = c("year", "categoria")) |>
    dplyr::left_join(y = data[[3]],
                     by = c("year", "categoria"))

  return(fbkf)

}
