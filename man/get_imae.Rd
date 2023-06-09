% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imae.R
\name{get_imae}
\alias{get_imae}
\title{Monthly Economic Activity Indicator}
\usage{
get_imae(variaciones = TRUE)
}
\arguments{
\item{variaciones}{Boolean indicating if variations should be included or
index only}
}
\value{
a tibble
}
\description{
Get the data series for the monthly Economic Activity Indicator of the
Dominican Republic. It includes seasonal and non seasonal adjusted series,
as well as monthly and year over year variations.
}
\examples{
get_imae(variaciones = FALSE)
}
