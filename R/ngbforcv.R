#' @title NGBoost forecasting model selection class
#' @description
#' It is a wrapper for the sklearn GridSearchCV with TimeSeriesSplit.
#'
#' @examples
#' \dontrun{
#'
#' library(ngboostForecast)
#'
#' dists <- list(Dist("Normal"))
#'
#' base_learners <- list(sklearner(module = "tree", class = "DecisionTreeRegressor",
#'                                 max_depth = 1),
#'                       sklearner(module = "tree", class = "DecisionTreeRegressor",
#'                                 max_depth = 2),
#'                       sklearner(module = "tree", class = "DecisionTreeRegressor",
#'                                 max_depth = 3),
#'                       sklearner(module = "tree", class = "DecisionTreeRegressor",
#'                                 max_depth = 4),
#'                       sklearner(module = "tree", class = "DecisionTreeRegressor",
#'                                 max_depth = 5),
#'                       sklearner(module = "tree", class = "DecisionTreeRegressor",
#'                                 max_depth = 6),
#'                       sklearner(module = "tree", class = "DecisionTreeRegressor",
#'                                 max_depth = 7))
#'
#' scores <-  list(Scores("LogScore"))
#'
#' model <- NGBforecastCV$new(Dist = dists,
#'                            Base = base_learners,
#'                            Score = scores,
#'                            natural_gradient = TRUE,
#'                            n_estimators = list(10, 100),
#'                            learning_rate = list(0.1, 0.2),
#'                            minibatch_frac = list(0.1, 1),
#'                            col_sample = list(0.3),
#'                            verbose = FALSE,
#'                            verbose_eval = 100,
#'                            tol = 1e-5)
#'
#' params <- model$tune(y = AirPassengers,
#' seasonal = TRUE,
#' max_lag = 12,
#' xreg = NULL,
#' early_stopping_rounds = NULL,
#' n_splits = 4L)
#'
#' params
#'
#'}
#' @references
#' \url{https://stanfordmlgroup.github.io/ngboost/2-tuning.html}
#'
#' @author Resul Akay
#' @importFrom R6 R6Class
#' @export
NGBforecastCV <- R6::R6Class(
  classname = "NGBforecastCV",
  public = list(
    #' @description Initialize an NGBforecastCV model.
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
    #' @return An NGBforecastCV object that can be fit.
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

      if(!inherits(Dist, "list")){
        stop(
          "Please profide a list of Dist object with at least one spesification"
          )
      }
      if(!inherits(Score, "list")){
        stop(
          "Please profide a list of Score object with at least one spesification"
        )
      }
      if(!inherits(Base, "list")){
        stop(
          "Please profide a list of Base base learnesr with at least one spesification"
        )
      }
      if(!inherits(n_estimators, "list")){
        stop(
          "Please profide a list of n_estimators object with at least one spesification"
        )
      }
      if(!inherits(learning_rate, "list")){
        stop(
          "Please profide a list of learning_rate object with at least one spesification"
        )
      }
      if(!inherits(minibatch_frac, "list")){
        stop(
          "Please profide a list of minibatch_frac object with at least one spesification"
        )
      }
      if(!inherits(col_sample, "list")){
        stop(
          "Please profide a list of col_sample object with at least one spesification"
        )
      }

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
        natural_gradient = private$natural_gradient,
        verbose = private$verbose,
        verbose_eval = private$verbose_eval,
        tol = private$tol,
        random_state = private$random_state
      )
      return(self)
    },
    #' @description Tune ngboosForecast.
    #' 
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
    #' @param n_splits Number of splits. Must be at least 2.
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
    #'  
    #'  
    #' @return A named list of best parameters.
    #'
    #' @importFrom forecast is.constant na.interp fourier
    tune = function(y,
                    max_lag = 5,
                    xreg = NULL,
                    seasonal = TRUE,
                    K =  frequency(y) / 2 - 1,
                    n_splits = NULL,
                    train_loss_monitor=NULL,
                    val_loss_monitor=NULL,
                    early_stopping_rounds=NULL){

      prepared_d <- prepare_data(y = y,
                                 max_lag = max_lag,
                                 xreg = xreg,
                                 seasonal = seasonal,
                                 K = K)

      X = prepared_d$x
      Y = prepared_d$y

      private$feature_names <- colnames(X)
      model = private$model

      param_grid = list("n_estimators" = private$n_estimators,
                        "learning_rate" = private$learning_rate,
                        "minibatch_frac" = private$minibatch_frac,
                        "col_sample" = private$col_sample,
                        "Base" = private$Base,
                        "Dist" = private$Dist,
                        "Score" = private$Score)

      tscv = sklearn[["model_selection"]]$TimeSeriesSplit(
        n_splits = as.integer(n_splits)
        )

      grdsrch<- sklearn[["model_selection"]]$GridSearchCV(
        model, param_grid=param_grid, cv = tscv
        )
      result <- grdsrch$fit(X,
                            Y,
                            train_loss_monitor = train_loss_monitor,
                            val_loss_monitor = val_loss_monitor,
                            early_stopping_rounds = early_stopping_rounds)

      out <- list(
        "ngboost_best_params" = result$best_params_,
        "ngb_forecast_params" = list(
          "seasonal" = seasonal,
          "max_lag" = max_lag,
          "K" = K
        )
      )

      return(out)
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
                 feature_names = NULL)
)
