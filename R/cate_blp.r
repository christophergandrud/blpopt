#' Estimates the best linear projection of the CATE with respect to a feature
#' matrix A using cross-fitted DR/AIPW-scores.
#'
#' @param cf *causal_forest* object output from the `causal_forest` function
#' from the **grf** package.
#' @param A Matrix of features with the same number of rows as `cf.predictions`
#' @importFrom stats lm
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovHC
#' @export cate_blp

cate_blp <- function(cf, A) {
    stopifnot("cf must be causal_forest" = inherits(cf, "causal_forest"))
    stopifnot("A must be a matrix" = inherits(A, "matrix"))

  # These are the observed outcomes/treatments stored in the GRF object
  Y <- cf$Y.orig
  W <- cf$W.orig

  # Step 1: Compute Scores Gamma_i
  # Note, that we currently rely on the GRF implementation for cross-fitted
  # nuisance function estimates with regression forests.

  # These are the regression forest estimates of E[Y | X = x] and E[W | X = x]..
  Y.hat <- cf$Y.hat
  W.hat <- cf$W.hat
  # and the localized predictions of the causal forest E[Y_1 - Y_0 | X = x]
  tau.hat <- cf$predictions

  stopifnot("A must have the same number of rows as cf.predictions" =
                length(A) == length(tau.hat))

  # Using the relationships
  # E[Y | X = x] = mu_0(X) + e(X) * tau(X) and
  # mu_1(X) = mu_0(X) + tau(X) we can compute the GRF implied
  # estimates of mu_0(X) and mu_1(X) as

  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat

  # DML residual
  w.res <- W - W.hat

  # DR correction weights
  weights <- w.res / ((W.hat) * (1 - W.hat))
  y.res <- Y - (Y.hat + tau.hat * w.res)

  # DR Scores
  gamma.hat <- y.res * weights + tau.hat

  # Projection of DR scores onto Feature Mat A.
  blp <- lm(gamma.hat ~ A)
  # HC3-SE t-tests
  res <- coeftest(blp, vcov. = vcovHC(blp, type = "HC3"))

  print("CATE-BLP Coefficient Estimates")
  print(res[, 1])
  print("HC3 Standard Errors")
  print(res[, 2])

  return(res)
}
