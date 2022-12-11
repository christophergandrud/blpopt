#' Helper function to extract components of trained model objects
#' needed for the CATE-BLP
#'
#' @param model a trained model object from a supported model type
#'
#' @return a list with observed outcomes (`Y``), treatments (`W``),
#' estimates of E[Y | X = x] (`Y.hat`) and E[W | X = x] (`W.hat`),
#' and the localized predictions of the causal forest E[Y_1 - Y_0 | X = x]
#' (`tau.hat`)

extract_trained_model_elements <- function(model){
    m <- list(Y = NULL, W = NULL, Y.hat= NULL, W.hat= NULL, tau.hat= NULL)
    if (inherits(model, "causal_forest")) {
        m$Y <- cf$Y.orig
        m$W <- cf$W.orig
        m$Y.hat <- cf$Y.hat
        m$W.hat <- cf$W.hat
        m$tau.hat <- cf$predictions
    }
    else
        stop("No supported trained model object supplied", call. = FALSE )

    return(m)
}
