#' Scikit-Learn interface
#' @param module scikit-learn module name, default is 'tree'.
#' @param class scikit-learn's module class, default is 'DecisionTreeRegressor'
#' @param ... Other arguments passed to model class
#' @author Resul Akay
#' @examples
#' \dontrun{
#'
#' sklearner(module = "tree", class = "DecisionTreeRegressor",
#' criterion="friedman_mse", min_samples_split=2)
#'
#' }
#'
#' @export
sklearner <- function(module = "tree", class = "DecisionTreeRegressor",
                      ...){
  sklearn[[module]][[class]](...)
}

#' NGBoost distributions
#' @param dist NGBoost distributions. One of the following:
#' \itemize{
#' \item Bernoulli
#' \item k_categorical
#' \item StudentT
#' \item Poisson
#' \item Laplace
#' \item Cauchy
#' \item Exponential
#' \item LogNormal
#' \item MultivariateNormal
#' \item Normal
#' }
#' @import reticulate
#' @param k Used only with k_categorical and MultivariateNormal
#' @returns An NGBoost Distribution object
#' @export
Dist <- function(dist = c("Normal", "Bernoulli", "k_categorical", "StudentT",
                          "Laplace", "Cauchy", "Exponential", "LogNormal",
                          "MultivariateNormal", "Poisson"), k){
  dist <- match.arg(dist)
  if(dist %in% c("MultivariateNormal", "k_categorical")){
    if(dist == "k_categorical"){
      out <- ngboost[["distns"]][[dist]](K = k)
    } else{
      out <- ngboost[["distns"]][[dist]](k = k)
    }
  } else {
    out <- ngboost[["distns"]][[dist]]
  }
  out
}

#' Select a rule to compare probabilistic predictions to the observed data.
#' @description 
#' Select a rule to compare probabilistic predictions to the observed data.
#' A score from ngboost.scores, e.g. LogScore.
#' @param score A string. can be one of the following:
#' \itemize{
#'
#'  \item LogScore : Generic class for the log scoring rule.
#'  \item CRPS : Generic class for the continuous ranked probability scoring rule.
#'  \item CRPScore : Generic class for the continuous ranked probability scoring rule.
#'  \item MLE : Generic class for the log scoring rule.
#'  }
#' @author Resul Akay
#' @return A score class from ngboost.scores
#' @export
Scores <- function(score = c("LogScore", "CRPS", "CRPScore", "MLE")){
  score <- match.arg(score)
  out <- scores[[score]]
  return(out)
}

#' Is conda installed?
#' @description 
#' Only for internal usage. 
#' @return Logical, TRUE if conda is installed.
#' @export
#' @author Resul Akay
is_exists_conda <- function() {
  clist <- tryCatch({
    reticulate::conda_list()
  }, error = function(e){
    data.frame()
  }
  )
  return(length(clist) > 0)
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Road Casualties in Great Britain 1969-84
#'
#' @description
#' The Seatbelts dataset from the datasets package.
#' @source
#' Harvey, A.C. (1989). Forecasting, Structural Time Series Models and the
#'  Kalman Filter. Cambridge University Press, pp. 519–523.
#'
#'
#' Durbin, J. and Koopman, S. J. (2001). Time Series Analysis by State Space
#' Methods. Oxford University Press.
#'
#'
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/UKDriverDeaths.html}
#'
#' @references
#' Harvey, A. C. and Durbin, J. (1986). The effects of seat belt legislation on
#' British road casualties: A case study in structural time series modelling.
#' Journal of the Royal Statistical Society series A, 149, 187–227.
#'
"seatbelts"



#' Assignment pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%<>\%}} for details.
#'
#' @name %<>%
#' @rdname assignment_pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
NULL

#' forecast package autoplot function
#'
#' See \code{\link[forecast]{autoplot}} for details.
#'
#' @name autoplot
#' @rdname autoplot
#' @keywords internal
#' @export
#' @importFrom forecast autoplot
#' @usage autoplot(object,...)
#' @return A ggplot object
#' @seealso \code{\link[forecast]{autoplot}}
NULL

#' forecast package autolayer function
#'
#' See \code{\link[forecast]{autolayer}} for details.
#'
#' @name autolayer
#' @rdname autolayer
#' @keywords internal
#' @export
#' @importFrom forecast autolayer
#' @return A ggplot layer
#' @usage autolayer(object,...)
#' @seealso \code{\link[forecast]{autolayer}}
NULL

#' magrittr pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @return Nothing
#'
#' @usage lhs \%>\% rhs
NULL


`%notin%` <- Negate(`%in%`)

#' @importFrom stats na.omit
lag_maker <- function(y, max_lag) {
  if ("ts" %notin% class(y)) {
    stop("y must be a 'ts' object")
  }
  max_lag1 <- round(max_lag)
  if (max_lag1 != max_lag) {
    message(paste("'max_lag' should not be a fractional number.", 
                  "'max_lag' rounde to",  max_lag1, sep = " "))
  }
  y <- c(y)
  out <- matrix(nrow = length(y), ncol = max_lag)
  for (i in seq_len(max_lag)) {
    out[, i] <- dplyr::lag(y, i)
  }
  out <- as.matrix(na.omit(out))
  colnames(out) <- paste0("y_lag", seq_len(max_lag))
  return(out)
}

#' @importFrom stats frequency time ts
prepare_data <- function(y,
                         max_lag = 5,
                         xreg = NULL,
                         seasonal = TRUE,
                         K =  frequency(y) / 2 - 1){

  if ("ts" %notin% class(y)) {
    stop("y must be a univariate time series")
  }
  
  length_y <- length(y)
  freq <- stats::frequency(y)
  
  if (length_y < freq) {
    stop("Not enough data to fit a model")
  }
  if (max_lag <= 0) {
    warning("max_lag increased to 1. max_lag must be max_lag >= 1")
    max_lag <- 1
  }
  if (c(length_y - freq - round(freq / 4)) < max_lag) {
    warning(paste("Input data is too short. Reducing max_lags to ",
                  round(length_y - freq - round(freq / 4))))
    max_lag <- round(length_y - freq - round(freq / 4))
  }
  if (max_lag != round(max_lag)) {
    max_lag <- round(max_lag)
    message(paste("max_lag must be an integer, max_lag rounded to", max_lag))
  }
  if (!is.null(xreg)) {
    if ("matrix" %notin% class(xreg)) {
      xreg <- as.matrix(xreg)
    }
  }
  constant_y <- forecast::is.constant(forecast::na.interp(y))
  if (constant_y) {
    warning("Constant data, setting max_lag = 1")
    max_lag = 1
  }

  if (!is.null(xreg))
  {
    ncolxreg <- ncol(xreg)
  }

  modified_y <-
    ts(y[-seq_len(max_lag)], start = time(y)[max_lag + 1], frequency = freq)
  
  if (seasonal == TRUE | freq > 1)
  {
    if (K == freq / 2) {
      ncolx <- max_lag + K * 2 - 1
    } else {
      ncolx <- max_lag + K * 2
    }
  }
  if (seasonal == FALSE | freq == 1)
  {
    ncolx <- max_lag
  }

  x <- matrix(0, nrow = c(length_y - max_lag), ncol = ncolx)

  x[, seq_len(max_lag)] <- lag_maker(y, max_lag)
  
  fourier_s <- NULL
  
  if (seasonal == TRUE & freq > 1)
  {
    fourier_s <- forecast::fourier(modified_y, K = K)
    x[, (max_lag + 1):ncolx] <- fourier_s
    colnames(x) <- c(paste0("lag", 1:max_lag), colnames(fourier_s))
  }

  if (seasonal == FALSE | freq == 1)
  {
    colnames(x) <- c(paste0("lag", 1:max_lag))
  }

  if (!is.null(xreg)) {
    col_xreg <- ncol(xreg)
    name_xreg <- colnames(xreg)
    xreg <- xreg[-seq_len(max_lag), ]
    if (col_xreg == 1) {
      xreg <- as.matrix(matrix(xreg, ncol = 1))
      colnames(xreg)[1] <- name_xreg[1]
      rm(name_xreg, col_xreg)
    }
    x <- cbind(x, xreg)
  }
  return(list("x" = x,
              "y" = modified_y,
              "frequency" = freq,
              "fourier_s" = fourier_s))
}
