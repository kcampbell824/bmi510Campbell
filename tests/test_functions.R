# Test script for package functions
# This mimics a new user installing the package and attempting to use functions
# Also includes accuracy/functionality tests for each function

# Load package from git
devtools::install_git("https://github.com/kcampbell824/bmi510Campbell", branch = "dev")

# Bernoulli log likelihood test
true_p = 0.62
x = rbinom(100, 1, true_p)
obs_p = sum(x)/length(x)
print(logLikBernoulli(x) == obs_p) # Expect TRUE

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
print(logLikBernoulli(x)) == mle_p # Expect TRUE

# Unscaling
x = c(-1, -1.5, -2, 4, 3, 5)
x1 = scale(x, center = TRUE, scale = FALSE)
x2 = scale(x, center = FALSE, scale = TRUE)
x3 = scale(x)

print(all(unscale(x1) == x)) # Expect TRUE
print(all(unscale(x2) == x)) # Expect TRUE
print(all(unscale(x3) == x)) # Expect TRUE

# Download RedCap report
url <- "https://redcap.emory.edu/api/"
reportId <- '46524'
tokenName <- 'redCapAPIkey'
data = downloadRedcapReport(tokenName,url,reportId)
print(all(dim(data) == c(91,7))) # Expect TRUE
print(all(colnames(data) == c("record_id","code","visit_number","muscle","mv","arm","condition"))) # Expect TRUE