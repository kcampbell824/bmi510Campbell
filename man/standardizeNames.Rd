% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_functions.R
\name{standardizeNames}
\alias{standardizeNames}
\title{Standardize column names}
\usage{
standardizeNames(data)
}
\arguments{
\item{data}{tibble of data with column names to standardize}
}
\value{
data with variable names standardized to small_camel format
}
\description{
Given a tibble, data, standardizes all column names to be in small_camel format
}
\examples{
test_tibble = tibble::tibble(x = seq(1, 10, 1), y = x * 2)
names(test_tibble) = c("First 10 Numbers", "$$Squared$$$values")
test_tibble = standardizeNames(test_tibble)
}
