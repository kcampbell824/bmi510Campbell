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

#' Plot survival curve of time to event data
#'
#' Given a set of events, status, with 1 corresponding to the event and 0 corresponding to a censored patient,
#' and the corresponding event times, time, plots a survival curve
#'
#' @param status set of events (0/1 valued)
#' @param time time of each event
#' @return ggplot object containing the survival curve
#' @examples
#' events = c(1,1,0,0,1,1,1,0,1,0)
#' year = c(2,3,4,5,7,9,9,10,12,15)
#' survCurv(events, year)
#' @export
survCurv <- function(status,time){
  data = data.frame(status = status, time = time)
  # sort data frame by ascending time
  data = data[order(data$time), ]
  
  grouped_events = data |> 
    dplyr::group_by(time) |> 
    dplyr::summarize(n.events = sum(status), n.censored = sum(status == 0), n.tot = dplyr::n()) |> 
    dplyr::ungroup()
  
  # product limit approach
  total = length(status)
  n.at.risk = c(total)
  survival = c((n.at.risk[1] - grouped_events[1, "n.events"])/n.at.risk[1])
  for(i in seq(2, nrow(grouped_events), 1)){
    n.at.risk[i] = total - sum(grouped_events[1:i-1, "n.tot"])
    survival[i] = survival[i-1] * ((n.at.risk[i] - grouped_events[i, "n.events"])/n.at.risk[i])
  }
  grouped_events = grouped_events |> 
    dplyr::mutate(n.at.risk = n.at.risk) |> 
    dplyr::mutate(survival = unname(unlist(survival)))
  
  ggplot2::ggplot(grouped_events, ggplot2::aes(time, survival)) +
    ggplot2::geom_step()
  
}

#' Unscale data
#'
#' Given a set of data, x, that has previously been passed through the scale function,
#' reverses centering and scaling as needed
#'
#' @param x dataset that may have been scaled
#' @return data with any previous scaling reversed
#' @examples
#' x = c(-1, -1.5, -2, 4, 3, 5)
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

#' PCA Approximation of original data
#'
#' Given a set of data, x, returns an approximation of the original data based on npc principal components
#'
#' @param x original dataset
#' @param npc number of principal components to base the approximation of x on
#' @return PCA approximation of x (rescaled and centered to match original dataset)
#' @examples
#' data = iris[1:4]
#' pcApprox(data, npc = 2)
#' @export
pcApprox = function(x, npc){
  if(npc > dim(x)[2]){
    stop("Number of principal components cannot be greater than the number of columns in x!")
  }
  
  all_pca = prcomp(x, center = TRUE, scale = TRUE, rank = npc)
  means = all_pca$center
  scale = all_pca$scale
  
  scores = all_pca$x
  loadings = all_pca$rotation
  approx = scores %*% t(loadings)
  (approx * scale) + means
}

#' Standardize column names
#'
#' Given a tibble, data, standardizes all column names to be in small_camel format
#'
#' @param data tibble of data with column names to standardize
#' @return data with variable names standardized to small_camel format
#' @examples
#' test_tibble = tibble::tibble(x = seq(1, 10, 1), y = x * 2)
#' names(test_tibble) = c("First 10 Numbers", "$$Squared$$$values")
#' test_tibble = standardizeNames(test_tibble)
#' @export
standardizeNames = function(data){
  data |> dplyr::rename_with(janitor::make_clean_names, case = "small_camel")
}

#' Find minimum sample size
#'
#' Given 1 sample, x1, find the minimum sample size to determine if the mean of x1 differs from 
#' 0 with 80% power with alpha = 0.05 
#' 
#' Given 2 samples, x1 and x2, finds the minimum sample sizes to test if the means differ
#'
#' @param x1 one sample of data (required)
#' @param x2 second sample of data (option)
#' @return minimum sample size for one-sample or two-sample t-test
#' @examples
#' # One-sample t-test example
#' x1 = c(0, -1, -3, -6, 1, 2, -5)
#' minimumN(x1)
#' 
#' # Two-sample t-test example
#' x2 = c(10, 11, -1, 12, 0, 3, 15)
#' minimumN(x1, x2)
#' @export
minimumN <- function(x1,x2=NULL){
  if(is.null(x2)){
    d = mean(x1)/sd(x1)
    min.n = pwr::pwr.t.test(d=d, sig.level = 0.05, power = 0.8, type = "one.sample")$n
  }
  else{
    sd1 = sd(x1)
    sd2 = sd(x2)
    x1_size = length(x1)
    x2_size = length(x2)
    pooled_sd = (sd1 * x1_size + sd2 *x2_size)/(x1_size + x2_size - 2)
    d = (mean(x1)-mean(x2))/pooled_sd
    min.n = pwr::pwr.t.test(d=d, sig.level = 0.05, power = 0.8, type = "two.sample")$n
  }
  ceiling(min.n)
}

#' Download RedCap report data
#'
#' Given a redCap URL and report ID, downloads the report data using the users' 
#' RedCap API token specified in .Renviron under the key redcapTokenName
#'
#' @param redcapTokenName key in .Renviron file to find redCap API token
#' @param redcapUrl URL to pull redCap data from
#' @param redcapReportId ID of redCap report to pull data from
#' @return tibble of data contained at the redCap URL under the specified report ID
#' @examples
#' url <- "https://redcap.test.edu/api/"
#' reportId <- '12345'
#' tokenName <- 'mySecretAPItoken'
#' data = downloadRedcapReport(tokenName,url,reportId)
#' @export
downloadRedcapReport = function(redcapTokenName,redcapUrl,redcapReportId){
  
  # Pull API key from .Renviron (should be in project directory)
  api.token = Sys.getenv(c(redcapTokenName))
  if(api.token == ""){
    stop("Make sure you place your RedCap API token in the .Renviron file using the correct key identifier!")
  }
  
  # Adapted from provided example code
  formData <- list("token"=api.token,
                   content='report',
                   format='csv',
                   report_id=redcapReportId,
                   csvDelimiter='',
                   rawOrLabel='raw',
                   rawOrLabelHeaders='raw',
                   exportCheckboxLabel='false',
                   returnFormat='csv'
  )
  response <- httr::POST(redcapUrl, body = formData, encode = "form")
  result <- httr::content(response, show_col_types = FALSE)
  result
}