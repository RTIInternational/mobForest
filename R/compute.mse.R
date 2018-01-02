#' Predictive accuracy estimates (MSE) across trees for linear or poisson
#' regression model.
#'
#'
#' @param response A vector of actual response of outcome variable.
#' @param predictions A vector of predicted response for the same outcome
#' variable.
#' @return MSE estimates
#' 
#' @examples
#' # The MSE should be 2.5. Off by 2 half the time, off by 1 the other half
#' response <- matrix(c(rep(0,100), rep(10,100)))
#' predictions <-
#'     matrix(nrow=20, ncol = 3,
#'            data = c(rep(1,100), rep(8,100), rep(1,100), rep(8,100),
#'                     rep(1,100), rep(8,100)))
#' compute.mse(response, predictions)
#' 
#' @export
compute.mse <- function(response, predictions) {
  mean.predictions <- apply(predictions, 1, mean, na.rm = T)
  sse <- sum( (response[, 1] - mean.predictions) ** 2, na.rm = T)
  n <- length(which(!is.na( (response[, 1] - colMeans(response, na.rm = T)))))
  return( sse / n )
}
