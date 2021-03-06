% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draft_data.R
\name{draft_data}
\alias{draft_data}
\title{Extract NFL Draft Data w/ Images}
\usage{
draft_data(start_year = 1936, end_year = 2021)
}
\arguments{
\item{start_year}{Year. Start year of data gather (e.g. 2020). Data is  available starting from the year 1936.  If no start_year or end_year is specific, data from 1936-current is pulled.}

\item{end_year}{Year. End year of data gather (e.g. 2022).}
}
\value{
A data frame with descriptive statistics. If you are only interested in certain columns
you can add these columns.
}
\description{
Extract NFL Draft Data w/ Images
}
\examples{
\dontrun{
draft_data(
start_year = 1936,
end_year = 2021
)
}
}
