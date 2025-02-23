% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embi.R
\name{get_embi}
\alias{get_embi}
\title{Download EMBI}
\usage{
get_embi(periodicidad = "mensual")
}
\arguments{
\item{periodicidad}{string with the desired periodicity options:
"diario", "semanal", "mensual", "trimestral", "anual".}
}
\value{
a tibble
}
\description{
Download EMBI of different countries
}
\examples{
get_embi("diario")
get_embi("semanal")
get_embi("mensual")
get_embi("trimestral")
get_embi("anual")
}
