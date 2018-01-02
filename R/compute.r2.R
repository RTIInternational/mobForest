#' Predictive accuracy estimates across trees for linear or poisson regression
#' 
#' pseudo R-square (R2) computation - proportion of total variance in response
#' variable explained by the tree model. The function takes observed and
#' predicted responses as input arguments and computes pseudo-R2 to determine
#' how well the tree model fits the given data.
#' 
#' @param response A vector of actual response of outcome variable.
#' @param predictions A vector of predictions for the same outcome variable
#' @return Predictive accuracy estimates ranging between 0 and 1. 
#' 
#' @examples
#' # This example explains 90% of the variance
#' response <- matrix(c(rep(0, 100), rep(10, 100)))
#' predictions <-
#'   matrix(nrow = 200, ncol = 3,
#'          data = c(rep(1, 100), rep(8, 100), rep(1, 100), rep(8, 100),
#'                   rep(1, 100), rep(8, 100)))
#' compute.r2(response, predictions)
#' @export
compute.r2 <- function(response, predictions) {
  mean_predictions <- apply(predictions, 1, mean, na.rm = T)
  sse <- sum( (response[, 1] - mean_predictions) ** 2, na.rm = T)
  ssto <- sum( (response[, 1] - colMeans(response, na.rm = T)) ** 2, na.rm = T)
  R2 <- 1 - (sse / ssto)
  if (R2 < 0) {
    R2 <- 0
  }
  return(R2)
}
