#' Helper function to extract components of trained model objects
#' needed for the CATE-BLP
#' Create a list with observed outcomes (`Y`), treatments (`W`),
#' estimates of E\[Y | X = x\] (`Y.hat`) and E\[W | X = x\] (`W.hat`),
#' and the localized predictions of the causal forest E\[Y_1 - Y_0 | X = x\]
#' (`tau.hat`)
#'
#' @param model a trained model object from a supported model type

extract_trained_model_elements <- function(model){
    m <- list(Y = NULL, W = NULL, Y.hat = NULL, W.hat = NULL, tau.hat = NULL)
    if (inherits(model, "causal_forest")) {
        m$Y <- model$Y.orig
        m$W <- model$W.orig
        m$Y.hat <- model$Y.hat
        m$W.hat <- model$W.hat
        m$tau.hat <- model$predictions
    }
    else if (inherits(model, "drlearner_blp")) {
        m <- model
    }
    else
        stop("No supported trained model object supplied", call. = FALSE)

    return(m)
}
