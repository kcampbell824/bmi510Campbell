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