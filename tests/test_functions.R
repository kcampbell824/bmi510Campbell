# Test script for package functions
# This mimics a new user installing the package and attempting to use functions
# Also includes accuracy/functionality tests for each function

# Load package from git
devtools::install_git("https://github.com/kcampbell824/bmi510Campbell", branch = "dev")

# Bernoulli log likelihood test
true_p = 0.62
x = rbinom(100, 1, true_p)
obs_p = sum(x)/length(x)
log_lik1 = bmi510Campbell::logLikBernoulli(x) == obs_p # Expect TRUE

# Compare to using dbinom for grid search
p_s = seq(0, 1, 0.001)
mle_p = 0
max_like = -1

for(i in seq(1, length(p_s))){
  logLik = sum(dbinom(x, 1, p_s[i], log = TRUE))
  if(i == 1){
    mle_p = p_s[i]
    max_like = logLik
  }
  else if(logLik > max_like){
    max_like = logLik
    mle_p = p_s[i]
  }
}
log_lik2 = bmi510Campbell::logLikBernoulli(x) == mle_p # Expect TRUE
if(log_lik1 && log_lik2){
  print("Bernoulli Log Likelihood Tests: Pass")
} else{
  print("Bernoulli Log Likelihood Tests: Failed")
}

# Unscaling
x = c(-1, -1.5, -2, 4, 3, 5)
x1 = scale(x, center = TRUE, scale = FALSE)
x2 = scale(x, center = FALSE, scale = TRUE)
x3 = scale(x)

unscale1 = all(bmi510Campbell::unscale(x1) == x) # Expect TRUE
unscale2 = all(bmi510Campbell::unscale(x2) == x) # Expect TRUE
unscale3 = all(bmi510Campbell::unscale(x3) == x) # Expect TRUE
if(unscale1 && unscale2 && unscale3){
  print("Unscale Tests: Pass")
} else{
  print("Unscale Tests: Failed")
}

# Standardize Names
test_tibble = tibble::tibble(x = seq(1, 10, 1), y = x * 2)
names(test_tibble) = c("First 10 Numbers", "$$Squared$$$values")
test_tibble = bmi510Campbell::standardizeNames(test_tibble)
if(all(names(test_tibble) == c("first10Numbers", "squaredValues"))){
  print("Standardize Names Tests: Pass")
} else{
  print("Standardize Names Tests: Failed")
}

# Download RedCap report
url <- "https://redcap.emory.edu/api/"
reportId <- '46524'
tokenName <- 'redCapAPIkey'
data = bmi510Campbell::downloadRedcapReport(tokenName,url,reportId)
download1 = all(dim(data) == c(91,7)) # Expect TRUE
download2 = all(colnames(data) == c("record_id","code","visit_number","muscle","mv","arm","condition")) # Expect TRUE

if(download1 && download2){
  print("RedCap Download Tests: Pass")
} else{
  print("RedCap Download Tests: Failed")
}
