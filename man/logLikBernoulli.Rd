% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_functions.R
\name{logLikBernoulli}
\alias{logLikBernoulli}
\title{Bernoulli Maximum Log Likelihood Estimate}
\usage{
logLikBernoulli(data)
}
\arguments{
\item{data}{Binary vector of observed data}
}
\value{
Maximum log-likelihood estimate of parameter p
}
\description{
Given a binary vector, identifies the parameter p that maximizes Bernoulli log-likelihood
}
\examples{
logLikBernoulli(c(1,0,0,0,1,1,1,1))
}
