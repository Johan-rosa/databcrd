
#' Function to read files metadata
#'
#' @param topic specific topic to query
#'
#' @return a list
read_details <- function(topic = NULL) {
  details <- yaml::read_yaml(system.file("details.yaml", package = "databcrd"))

  if (!is.null(topic)) {
    checkmate::check_choice(topic, names(details))
    return(details[[topic]])
  }

  details
}

eem_details <- read_details("eem")
