#' Predict method for cateblp objects
#'
#' @param obj cateblp object from `cate_blp`
#' @param ... other
#' @export
predict <- function(obj, ...) {
    UseMethod("predict", obj)
}

#' @export
predict.cateblp <- function(obj, ...) {
    obj$predictions
}

#' Summary method for cateblp objects
#'
#' @param obj cateblp object from `cate_blp`
#' @param ... other
#' @export
summary <- function(obj, ...) {
    UseMethod("summary", obj)
}

#' @export
summary.cateblp <- function(obj, ...) {
    obj$res
}
