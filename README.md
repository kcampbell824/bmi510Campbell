# Biostatistics Final Project
Kiersten Campbell
Spring 2024

### Installation Guide
The contents of this directory are structured as an R package, which can easily be installed locally with the following command:

`devtools::install_git("https://github.com/kcampbell824/bmi510Campbell")`

### Contents
This package contains several useful functions for biostatistical analyses. The list of available functions, with short descriptions, are listed below, but the standard `?functionName` command can be used to access the manual page for each function:

- logLikeBernoulli(data) : estimates the maximum log-likelihood estimate to fit a set of binary data
- survCurv(status, time) : plots a survival curve based on time-to-event data
- unscale(x) : reverses any centering and/or scaling
- pcApprox(x, npc) : returns a PCA-approximation of the original data based on npc principal components
- standardizeNames(data) : standardizes all column names in a tibble to camel case
- minimumN(x1, x2=NULL) : finds the minimum sample size for desired effect size with 80% power and alpha = 0.5
- downloadRedcapReport(redcapTokenName, redcapURL, redcapReportID) : using user's RedCap API token, downloads a report at designated redcap URL

### Examples/testing
Example usage of each function and corresponding accuracy tests can be found in `tests/test_functions.R` 
