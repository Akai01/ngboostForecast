library(testthat)
library(ngboostForecast)

if_not_ngboost_exist_skip <- function() {
  clist <- FALSE
  clist <- reticulate::py_module_available("ngboost")

  if (!clist) {
    testthat::skip("ngboost not available for testing")
  }
}

test_check("ngboostForecast")
