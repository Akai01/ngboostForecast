#' Probabilistic forecasting using Python's NGBoost library.
#'
#' The goal of ngboostForecast is to provide a probabilistic forecasting
#' interface for Python's \href{https://stanfordmlgroup.github.io}{NGBoost} library
#' @rdname ngboostForecast
#' @docType package
#' @name ngboost
NULL


ngboost <- NULL
sklearn <- NULL
scores <- NULL

#' @export
.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  ngboost <<- reticulate::import("ngboost",delay_load = TRUE)
  sklearn <<- reticulate::import("sklearn",delay_load = TRUE)
  scores <<- reticulate::import("ngboost.scores",delay_load = TRUE)
}

