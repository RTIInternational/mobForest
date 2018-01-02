#' Contingency table: Predicted vs. Observed Outcomes
#' 
#' This function takes predicted probabilities (for out of bag cases) obtained
#' through logistic regression-based tree models and converts them into binary
#' classes (based on specified probability threshold). The predicted
#' classifications are then compared to actual binary response.
#' 
#' @param response A vector of binary classes of out-of-cases for a given tree.
#' @param predicted A vector of predicted probabilities of out-of-cases using
#' same tree.
#' @param prob_thresh Probability threshold for classification (default = .5).
#' 
#' @examples
#' # We should get 15 predictions correct and miss 5
#' response <- matrix(c(rep(0,10), rep(1,10)))
#' predicted <- c(rep(.1,15), rep(.8,5))
#' logistic.acc(response, predicted, .5)	
#' 
#' @export
logistic.acc <- function(response, predicted, prob_thresh = .5) {
  pred_class <-
    rep(levels(as.factor(response[, 1]))[1], length(predicted))
  pred_class[which(predicted > prob_thresh)] <-
    levels(as.factor(response[, 1]))[2]
  c_table <- table(Data_Class = response, Predicted_Class = pred_class)
  c_table
}
