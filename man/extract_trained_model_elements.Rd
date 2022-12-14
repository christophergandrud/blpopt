% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.r
\name{extract_trained_model_elements}
\alias{extract_trained_model_elements}
\title{Helper function to extract components of trained model objects
needed for the CATE-BLP
Create a list with observed outcomes (\code{Y}), treatments (\code{W}),
estimates of E[Y | X = x] (\code{Y.hat}) and E[W | X = x] (\code{W.hat}),
and the localized predictions of the causal forest E[Y_1 - Y_0 | X = x]
(\code{tau.hat})}
\usage{
extract_trained_model_elements(model)
}
\arguments{
\item{model}{a trained model object from a supported model type}
}
\description{
Helper function to extract components of trained model objects
needed for the CATE-BLP
Create a list with observed outcomes (\code{Y}), treatments (\code{W}),
estimates of E[Y | X = x] (\code{Y.hat}) and E[W | X = x] (\code{W.hat}),
and the localized predictions of the causal forest E[Y_1 - Y_0 | X = x]
(\code{tau.hat})
}
