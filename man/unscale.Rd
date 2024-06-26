% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_functions.R
\name{unscale}
\alias{unscale}
\title{Unscale data}
\usage{
unscale(x)
}
\arguments{
\item{x}{dataset that may have been scaled}
}
\value{
data with any previous scaling reversed
}
\description{
Given a set of data, x, that has previously been passed through the scale function,
reverses centering and scaling as needed
}
\examples{
x = c(-1, -1.5, -2, 4, 3, 5)
x = scale(x, center = TRUE, scale = TRUE)
unscale(x)
}
