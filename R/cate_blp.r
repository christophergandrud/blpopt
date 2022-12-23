#' Estimates the best linear projection of the conditional average treatment
#' effect (CATE-BLP) with respect to a feature
#' vector A using cross-fitted DR/AIPW-scores.
#'
#' @param cf *causal_forest* object output from the `causal_forest` function
#' from the **grf** package.
#' @param A matrix of features with the same number of rows as
#' `cf.predictions`
#'
#' @importFrom stats lm
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovHC
#' @export cate_blp

cate_blp <- function(cf, A) {
  m <- extract_trained_model_elements(cf)

  stopifnot(
    "A must have the same number of rows as cf.predictions" =
      nrow(A) == length(m$tau.hat)
  )

  # Using the relationships
  # E[Y | X = x] = mu_0(X) + e(X) * tau(X) and
  # mu_1(X) = mu_0(X) + tau(X) we can compute the GRF implied
  # estimates of mu_0(X) and mu_1(X) as

  # E[Y | X, W = 0]
  mu.hat.0 <- m$Y.hat - m$W.hat * m$tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- m$Y.hat + (1 - m$W.hat) * m$tau.hat

  # DML residual
  w.res <- m$W - m$W.hat

  # DR correction weights
  weights <- w.res / ((m$W.hat) * (1 - m$W.hat))
  y.res <- m$Y - (m$Y.hat + m$tau.hat * w.res)

  # DR Scores
  gamma.hat <- y.res * weights + m$tau.hat

  # Projection of DR scores onto Feature Mat A.
  blp <- lm(gamma.hat ~ A)
  # HC3-SE t-tests
  res <- coeftest(blp, vcov. = vcovHC(blp, type = "HC3"))

  # Predictions
  preds <- cbind(1, A) %*% res[, 1]
  plt.df <- data.frame(A, predicted = preds)

  out <- list(res = res, predictions = plt.df)
  class(out) <- "cateblp"

  return(out)
}
