if(require(testthat)){
  test_that("tests for some arguments in NGBforecastCV", {
    
    if_not_ngboost_exist_skip()
    
    dists <- list(Dist("Normal"))
    base_learners <- list(sklearner(module = "tree", class = "DecisionTreeRegressor",
                                    max_depth = 6),
                          sklearner(module = "tree", class = "DecisionTreeRegressor",
                                    max_depth = 7))
    scores <-  list(Scores("LogScore"))
    
    model <- NGBforecastCV$new(Dist = dists,
                               Base = base_learners,
                               Score = scores,
                               natural_gradient = TRUE,
                               n_estimators = list(10, 12),
                               learning_rate = list(0.1),
                               minibatch_frac = list(0.1),
                               col_sample = list(0.3),
                               verbose = FALSE,
                               verbose_eval = 100,
                               tol = 1e-5)
    
    params <- model$tune(y = AirPassengers,
                         seasonal = TRUE,
                         max_lag = 12,
                         xreg = NULL,
                         early_stopping_rounds = NULL,
                         n_splits = 4L)
    
    out <- class(params)
    
    expect_equal(out, "list")
    
  })
  
}