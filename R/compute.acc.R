#' Predictive accuracy estimates across trees for logistic regression model
#' 
#' Compute predictive accuracy of response variable with binary outcome. 
#' The function takes observed and predicted binary responses as input and
#' computes proportion of observations classified in same group.
#' @param response A vector of binary outcome.
#' @param predictions A matrix of predicted probabilities (logit model) for 
#' out-of-bag observations for each tree.
#' @param prob_cutoff The threshold for predicting 1's & 0's.
#' @return Predictive accuracy estimate ranging between 0 and 1.
#' @examples
#' response <- as.data.frame( c(rep(0, 10000), rep(1, 10000)))
#' predictions <-
#'   matrix(nrow = 20000, ncol = 3,
#'          data = c(rep(.1, 15000), rep(.8, 5000), rep(.1, 15000),
#'                   rep(.8, 5000), rep(.1, 15000), rep(.8, 5000)))
#' compute.acc(response, predictions, prob_cutoff = .5)
#' @export
compute.acc <- function(response, predictions, prob_cutoff = .5) {
  pred_class <- rep(0, length(response[, 1]))
  mean_pred <- apply(predictions, 1, mean, na.rm = T)
  pred_class[which(mean_pred > prob_cutoff)] <- 1
  acc <- mean(pred_class == response[, 1])
  return(acc)
}
