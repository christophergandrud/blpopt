
# blpopt

<!-- badges: start -->
[![R-CMD-check](https://github.com/christophergandrud/blpopt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christophergandrud/blpopt/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Estimate the best linear projection of the Conditional Average Treatment Effect (CATE) for approximately optimal targeting

## Installation

You can install the development version from [GitHub](https://github.com/) with: 

``` r
# install.packages("devtools")
devtools::install_github("christophergandrud/blpopt")
```

Note: the package is in a private GitHub repo. You need to 
add your auth token (if you have access) to download using devtools.

## Example

```r
## ----setup--------------------------------------------------------------------
library(blpopt)
library(grf)
library(ggplot2)

## ---- data-generation---------------------------------------------------------
n <- 2000
p <- 20
X <- matrix(rnorm(n * p), n, p)

# CATE varies along one dim only.
tau_ex <- function(x){1 / (1 + exp(-x))} 
TAU <- tau_ex(X[,3])

# Propensity  and Outcome vary along 2 and 5 dimensions only.
W <- rbinom(n, 1, 1 / (1 + exp(-X[, 1] - X[, 2]))) 
Y <- pmax(X[, 2] + X[, 3], 0) + rowMeans(X[, 4:6]) / 2 + W * TAU + rnorm(n)

## ---- estimate----------------------------------------------------------------
cf <- causal_forest(X, Y, W)
blp <- cate_blp(cf, X[,3])

## ---- summary-----------------------------------------------------------------
summary(blp)

## ---- predictions-------------------------------------------------------------
predicted <- predict(blp)

## ---- visually-compare--------------------------------------------------------
ggplot(predicted, aes(x = A, y = predicted)) +
  xlim(c(min(predicted$A), max(predicted$A))) +
  geom_function(fun = tau_ex,col="red")  + 
  geom_point(size = 1, alpha = 0.4) +
  theme_minimal()
```

## See also

- Semenova, V. and Chernozhukov, V., 2021. Debiased machine learning of 
conditional average treatment effects and other causal functions. 
The Econometrics Journal, 24(2), pp.264-289.

- [**drlearner**](https://github.com/christophergandrud/drlearner) computes CATE 
estimates needed for best linaer projections in the format this package expects
using the doubly robust learner method from Kennedy (2022)

- [`grf::best_linear_projection`](https://grf-labs.github.io/grf/reference/best_linear_projection.html) gives
best linear projections from causal forests. This function produces equivalent results for causal forests.
**blpopt** aims to provide BLPs for a wider range of models, such as doubly robust learners.
