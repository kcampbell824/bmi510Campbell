% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_functions.R
\name{pcApprox}
\alias{pcApprox}
\title{PCA Approximation of original data}
\usage{
pcApprox(x, npc)
}
\arguments{
\item{x}{original dataset}

\item{npc}{number of principal components to base the approximation of x on}
}
\value{
PCA approximation of x (rescaled and centered to match original dataset)
}
\description{
Given a set of data, x, returns an approximation of the original data based on npc principal components
}
\examples{
data = iris[1:4]
pcApprox(data, npc = 2)
}
