#' @title NGBoost forecasting class
#' @description
#' The main forecasting class.
#'
#' @examples
#' \dontrun{
#' 
#' library(ngboostForecast)
#' 
#' model <- NGBforecast$new(Dist = Dist("Normal"),
#'                          Base = sklearner(module = "linear_model",
#'                          class = "Ridge"),
#'                          Score = Scores("LogScore"),
#'                          natural_gradient = TRUE,
#'                          n_estimators = 200,
#'                          learning_rate =  0.1,
#'                          minibatch_frac = 1,
#'                          col_sample = 1,
#'                          verbose = TRUE,
#'                          verbose_eval = 100,
#'                          tol = 1e-5)
#' model$fit(y = AirPassengers, seasonal = TRUE, max_lag = 12, xreg = NULL,
#' early_stopping_rounds = 10L)
#' fc <- model$forecast(h = 12, level = c(90, 80), xreg = NULL)
#' 
#' autoplot(fc)
#'}
#'
#' @references 
#' 
#' Duan, T et. al. (2019), NGBoost: Natural Gradient Boosting for Probabilistic 
#' Prediction. 
#' @returns An NGBforecast class
#' @author Resul Akay
#' 
#' @importFrom R6 R6Class
#' @export
NGBforecast <- R6::R6Class(
  classname = "NGBforecast",
  public = list(
    #' @description Initialize an NGBforecast model.
    #' @param Dist Assumed distributional form of \code{Y|X=x}. An output of 
    #' \code{\link{Dist}} function, e.g. \code{Dist('Normal')}
    #' 
    #' @param Score Rule to compare probabilistic predictions to 
    #' the observed data. A score from \code{\link{Scores}} function, e.g. 
    #' \code{Scores(score = "LogScore")}.
    #' @param Base Base learner. An output of \code{\link{sklearner}} function,
    #' e.g. \code{sklearner(module = "tree", class = "DecisionTreeRegressor", ...)}
    #' @param natural_gradient Logical flag indicating whether the natural 
    #' gradient should be used
    #' @param n_estimators The number of boosting iterations to fit
    #' @param learning_rate The learning rate
    #' @param minibatch_frac The percent subsample of rows to use in each 
    #' boosting iteration
    #' @param col_sample The percent subsample of columns to use in each 
    #' boosting iteration
    #' @param verbose Flag indicating whether output should be printed 
    #' during fitting. If TRUE it will print logs.
    #' @param verbose_eval Increment (in boosting iterations) at which 
    #' output should be printed
    #' @param tol Numerical tolerance to be used in optimization
    #' @param random_state Seed for reproducibility.
    #' @return An NGBforecast object that can be fit.
    initialize = function(Dist = NULL,
                          Score = NULL,
                          Base = NULL,
                          natural_gradient = TRUE,
                          n_estimators = as.integer(500),
                          learning_rate = 0.01,
                          minibatch_frac = 1.0,
                          col_sample = 1.0,
                          verbose = TRUE,
                          verbose_eval = as.integer(100),
                          tol = 0.0001,
                          random_state = NULL){
      private$Dist <- Dist
      private$Base <- Base
      private$Score <- Score
      private$natural_gradient <- natural_gradient
      private$n_estimators <- as.integer(n_estimators)
      private$learning_rate <- learning_rate
      private$minibatch_frac <- minibatch_frac
      private$col_sample <- col_sample
      private$verbose <- verbose
      private$verbose_eval <- as.integer(verbose_eval)
      private$tol <- tol
      private$random_state <- random_state

      if(!ngboostForecast::is_exists_conda()) {
        stop(
          paste(
            "\nConda is not available!",
            "\nPlease use",
            "'reticulate::install_miniconda(update = TRUE, force = TRUE)'",
            "and then restart R.'"
          )
        )
      }

      private$model <- ngboost$NGBRegressor(
        Dist = private$Dist,
        Score = private$Score,
        Base = private$Base,
        natural_gradient = private$natural_gradient,
        n_estimators = private$n_estimators,
        learning_rate = private$learning_rate,
        minibatch_frac = private$minibatch_frac,
        col_sample = private$col_sample,
        verbose = private$verbose,
        verbose_eval = private$verbose_eval,
        tol = private$tol,
        random_state = private$random_state
      )
      return(self)
    },
    #' @description Fit the initialized model.
    #' @param y A time series (ts) object
    #' @param max_lag Maximum number of lags
    #' @param xreg Optional. A numerical matrix of external regressors,
    #' which must have the same number of rows as y. 
    #' @param test_size The length of validation set. 
    #' If it is NULL, then, it is automatically specified.
    #' @param seasonal Boolean. If \code{seasonal = TRUE} the fourier terms 
    #' will be used for modeling seasonality.
    #' @param K Maximum order(s) of Fourier terms, used only if
    #' \code{seasonal = TRUE}.
    #' @param train_loss_monitor A custom score or set of scores to track on the
    #' training set during training. Defaults to the score defined in the NGBoost
    #' constructor. Please do not modify unless you know what you are doing.
    #' @param val_loss_monitor A custom score or set of scores to track on the
    #'  validation set during training. Defaults to the score defined in the
    #'  NGBoost  constructor. Please do not modify unless you know what you are
    #'  doing.
    #' @param early_stopping_rounds The number of consecutive boosting
    #'  iterations during which the loss has to increase before the algorithm
    #'  stops early.
    #' @return NULL
    #'
    #' @importFrom forecast is.constant na.interp fourier
    #'
    fit = function(y,
                   max_lag = 5,
                   xreg = NULL,
                   test_size = NULL,
                   seasonal = TRUE,
                   K =  frequency(y) / 2 - 1,
                   train_loss_monitor=NULL,
                   val_loss_monitor=NULL,
                   early_stopping_rounds=NULL){

      prepared_d <- prepare_data(y = y,
                                 max_lag = max_lag,
                                 xreg = xreg,
                                 seasonal = seasonal,
                                 K = K)

      x = prepared_d$x
      modified_y = prepared_d$y
      freq <- prepared_d$frequency
      fourier_s <- prepared_d$fourier_s

      if(is.numeric(test_size)){
        trainlen <- length(modified_y) - test_size
        X = x[seq_len(trainlen), ]
        Y = as.numeric(modified_y[seq_len(trainlen)])
        X_val <- x[-seq_len(trainlen), ]
        Y_val <- as.numeric(modified_y[-seq_len(trainlen)])

      }else{
        X = x
        Y = as.numeric(modified_y)
        X_val <- NULL
        Y_val <- NULL
      }

      private$feature_names <- colnames(X)
      model = private$model
      model$fit(X = X,
                Y = Y,
                X_val = X_val,
                Y_val = Y_val,
                train_loss_monitor = train_loss_monitor,
                val_loss_monitor = val_loss_monitor,
                early_stopping_rounds = early_stopping_rounds)
      method = paste0("NGBforecast", " with ",
                      gsub("[()]", "", paste0(private$Base)),
                      "(",max_lag,", ", K, ")")
      
      fitted_int <- ts(c(rep(NA, max_lag), c(model$predict(X))), 
                       start = start(y), frequency = frequency(y))
      
      private$y =  y
      private$y_modified = modified_y
      private$x = x
      private$model = model
      private$fitted = fitted_int
      private$max_lag = max_lag
      private$seasonal = seasonal
      private$method = method

      if (seasonal == TRUE & freq > 1)
      {
        private$fourier_s <- fourier_s
        private$K <- K
      }
      private$xreg_fit <- NULL
      if (!is.null(xreg)) {
        private$xreg_fit <- xreg
      }
      return(invisible(NULL))
    },
    #' @description Forecast the fitted model
    #' @param h Forecast horizon
    #' @param xreg A numerical vector or matrix of external regressors
    #' @param level Confidence level for prediction intervals
    #' @param data_frame Bool. If TRUE, forecast will be returned as a
    #' data.frame object, if FALSE it will return a forecast class. If TRUE, 
    #' \code{\link{autoplot}} will function.
    #' @importFrom dplyr select starts_with
    forecast = function(h = 6,
                        xreg = NULL,
                        level = c(80, 95),
                        data_frame = FALSE){
      level2 <- level/100
      if (!is.null(private$xreg_fit)) {
        ncolxreg <- ncol(private$xreg_fit)
      }

      if (is.null(xreg)) {
        if (!is.null(private$xreg_fit)) {
          stop("No regressors provided")
        }
      }

      if (!is.null(xreg)) {
        if (is.null(private$xreg_fit)) {
          stop("No regressors provided to fitted model")
        }

        if (ncol(xreg) != ncolxreg) {
          stop("Number of regressors does not match to fitted model")
        }

        h <- nrow(xreg)
        newxreg1 <- xreg
      }

      if (is.null(h)) {
        h <- ifelse(frequency(private$y) > 1, 2 * frequency(private$y), 10)
      }

      if (is.null(xreg)) {
        newxreg1 <- NULL
      }

      fc_x <- private$forecast_loop(xreg = newxreg1, h = h, level = level2)
      x <- fc_x$x
      y <- fc_x$y
      prediction_iter <- as.data.frame(fc_x$prediction_iter)

      colnames(prediction_iter) <- unlist(
        sapply(level, function(x){
        upper <- paste0("upper_", x)
        lower <- paste0("lower_", x)
        return(c(upper, lower))
      }, simplify = FALSE)
      )

      lower <- dplyr::select(prediction_iter, dplyr::starts_with("lower"))
      lower <- ts(lower, start = start(y), frequency = frequency(y))

      upper <- dplyr::select(prediction_iter, dplyr::starts_with("upper"))
      upper <- ts(upper, start(y), frequency = frequency(y))

      output <- list(
        x = private$y,
        mean = y,
        lower = lower,
        upper = upper,
        fitted = private$fitted,
        level = level,
        newxreg = x,
        method = private$method,
        model = list(private$model)
      )
      class(output) <- c("forecast", "NGBforecast")

      if(data_frame){
        output <- as.data.frame(output)
      }

      return(output)
    },

    #' @description Return the feature importance for all parameters in the
    #' distribution (the higher, the more important the feature).
    #' @return A data frame
    #'
    feature_importances = function(){
      model = private$model
      out <- model$feature_importances_
      if(is.null(out)){
        return("The model does not have feature importances")
      }
      feature_names <- private$feature_names
      out <- data.frame("features" = c(feature_names), "importance" = c(out))
      private$feature_importance_data <- out
      return(out)
    },
    #' @description Plot feature importance
    #' @return A ggplot object
    #' 
    plot_feature_importance = function() {
      feature_importance_data <- private$feature_importance_data

      if (is.null(feature_importance_data)) {
        feature_importance_data <- self$feature_importances()
        private$feature_importance_data <- feature_importance_data
      }

      if ("The model does not have feature importances" %in% feature_importance_data) {
        return(feature_importance_data)
      }

      ggplot2::ggplot(data = feature_importance_data,
                      ggplot2::aes(y = .data$features, x = .data$importance)) +
        ggplot2::geom_col()
    },
    
    #' @description Get parameters for this estimator.
    #' @param deep bool, default = TRUE
    #' If True, will return the parameters for this estimator and
    #' contained subobjects that are estimators.
    #' @return A named list of parameters.
    get_params = function(deep = TRUE){
      model = private$model
      model$get_params(deep = deep)
    }
  ),
  private = list(Dist = NULL,
                 Score = NULL,
                 Base = NULL,
                 natural_gradient = NULL,
                 n_estimators = NULL,
                 learning_rate = NULL,
                 minibatch_frac = NULL,
                 col_sample = NULL,
                 verbose = NULL,
                 verbose_eval = NULL,
                 tol = NULL,
                 random_state = NULL,
                 model = NULL,
                 feature_names = NULL,
                 feature_importance_data = NULL,
                 y =  NULL,
                 y_modified = NULL,
                 x = NULL,
                 fitted = NULL,
                 max_lag = NULL,
                 seasonal = NULL,
                 method = NULL,
                 xreg_fit = NULL,
                 K = NULL,
                 fourier_s = NULL,

                 pred_func = function(i, x, y, newxreg, freq, fourier_h, level) {
                   newxreg_in <- newxreg[i,]
                   new_data <- c(y[length(y)], x[nrow(x), 1:(private$max_lag - 1)])
                   if (private$max_lag == 1) {
                     new_data = new_data[-1]
                   }
                   if (private$seasonal == TRUE & freq > 1)
                   {
                     new_data <- c(new_data, fourier_h[i, ])
                   }
                   if (!is.null(newxreg_in)) {
                     new_data <- c(new_data, newxreg_in)
                   }
                   new_data <- matrix(new_data, nrow = 1)
                   colnames(new_data) <- colnames(x)
                   model <- private$model
                   distribution <- model$pred_dist(new_data)

                   pred <- distribution$dist$mean()

                   pi <- unlist(sapply(level, function(x) {
                     unlist(distribution$dist$interval(x))
                   }, simplify = FALSE))

                   return(list("x" = rbind(x, new_data),
                               "y" = c(y, pred),
                               "pi" = pi))
                 },
                 forecast_loop = function(xreg, h, level) {
                   x <- private$x
                   y <- private$y_modified
                   freq <- stats::frequency(private$y_modified)
                   if (private$seasonal == TRUE & freq > 1)
                   {
                     fourier_h <- forecast::fourier(private$y_modified,
                                                    K = private$K, h = h)
                   }

                   prediction_iter <- matrix(ncol = length(level)*2, nrow = h)

                   for (i in 1:h) {
                     fc_x <- private$pred_func(
                       i,
                       x = x,
                       y = y,
                       newxreg = xreg,
                       freq = freq,
                       fourier_h = fourier_h,
                       level = level
                     )
                     x <- fc_x$x
                     y <- fc_x$y
                     prediction_iter[i,] <- fc_x$pi
                   }
                   y <- ts(y[-(1:length(private$y_modified))],
                           frequency = freq,
                           start = max(time(private$y)) + 1 / freq)
                   x <- x[-(1:nrow(private$x)),]

                   return(list("x" = x,
                               "y" = y,
                               "prediction_iter" = prediction_iter))
                 }
                 )
)
