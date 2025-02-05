% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_treatment_group.R
\name{get_treatment_group}
\alias{get_treatment_group}
\title{get treatment group}
\usage{
get_treatment_group(studies = NULL, db_path)
}
\arguments{
\item{ind}{Mandatory, character or vector \cr
Studies number}
}
\value{
list
}
\description{
get treatment group
}
\examples{
\dontrun{
get_treatment_group(studies = '12345678', db_path = 'path/to/sqllite/database.db')
}
}
