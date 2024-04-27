#' Bernoulli Maximum Log Likelihood Estimate
#'
#' Given a binary vector, identifies the parameter p that maximizes Bernoulli log-likelihood
#'
#' @param data Binary vector of observed data
#' @return Maximum log-likelihood estimate of parameter p
#' @examples
#' logLikBernoulli(c(1,0,0,0,1,1,1,1))
#' @export
logLikBernoulli <- function(data){
  n = length(data)
  n_s = sum(data)
  p_s = seq(0, 1, 0.001)
  
  max_like = 0
  mle_p = -1
  
  # Test possible estimates of p in range [0, 1] in steps of 0.001
  # Track maximum logLikelihood observed & associated estimate p
  for(i in seq(1, length(p_s))){
    p = p_s[i]
    logLik = (n_s*log(p)) + ((n - n_s) * log(1 - p))
    if(i == 1){
      mle_p = p
      max_like = logLik
    }
    else if(logLik > max_like){
      max_like = logLik
      mle_p = p
    }
  }
  mle_p
}

#' Unscale data
#'
#' Given a set of data, x, that has previously been passed through the scale function,
#' reverses centering and scaling as needed
#'
#' @param x dataset that may have been scaled
#' @return data with any previous scaling reversed
#' @examples
#' x2 = c(-1, -1.5, -2, 4, 3, 5)
#' x = scale(x, center = TRUE, scale = TRUE)
#' unscale(x)
#' @export
unscale <- function(x){
  
  # Check attributes of input x to check for centering/scaling
  scaled = attr(x, which = "scaled:scale")
  centered = attr(x, which = "scaled:center")
  if(!is.null(scaled)){
    x = x*scaled
  }
  if(!is.null(centered)){
    x = x + centered
  }
  x
}

