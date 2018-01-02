#' Predictive Accuracy Report
#'
#'
#' @param x An object of class 'predictive.acc' returned by
#' \code{"\link[=predictive.acc]{predictive.acc()}"} function
#' @param \dots Additional arguments to print method
#' 
#' @examples
#' \dontrun{
#' library(mlbench)
#' set.seed(1111)
#' # Random Forest analysis of model based recursive partitioning load data
#' data("BostonHousing", package = "mlbench")
#' BostonHousing <- BostonHousing[1:90, c("rad", "tax", "crim", "medv", "lstat")]
#' 
#' # Recursive partitioning based on linear regression model medv ~ lstat with 3
#' # trees.  1 core/processor used. 
#' rfout <- mobforest.analysis(as.formula(medv ~ lstat), c("rad", "tax", "crim"),
#'     mobforest_controls = mobforest.control(ntree = 3, mtry = 2, replace = T,
#'         alpha = 0.05, bonferroni = T, minsplit = 25), data = BostonHousing,
#'     processors = 1, model = linearModel, seed = 1111)
#' # prints predictive accuracy output
#' pacc <- predictive.acc(rfout)
#' }
#' 
#' @importFrom stats quantile
#' @export
print.estimates <- function(x, ...){
  cat("MobForest Predictive Accuracy Report:\n\n")
  cat("Tree Level Summary \n\n")
  if (x$fam != "binomial") {
    cat("Pseudo R2 Report \n")
  }
  if (x$fam == "binomial"){
    cat("Proportion of subjects correctly classified\n")
  }

  cat("Data Used\tMin.\tQ1\tMedian\tMean\tQ3\tMax.\n")
  oob_r2_qs <- as.numeric(quantile(x$oob.R2,
                                 probs = c(0, 0.25, 0.5, 0.75, 1)))

  cat("OOB (Training) ", round(oob_r2_qs[1:3], 3),
      round(mean(x$oob.R2), 3),
      round(oob_r2_qs[4:5], 3), sep = "\t", "\n\n")

  gen_r2_qs <- as.numeric(quantile(x$General.R2,
                                 probs = c(0, 0.25, 0.5, 0.75, 1)))
  cat("All (Training)", round(gen_r2_qs[1:3], 3),
      round(mean(x$General.R2), 3),
      round(gen_r2_qs[4:5], 3), sep = "\t", "\n\n")

  if (x@fam != "binomial") {
    cat("MSE Report \n")
    cat("Data Used\tMin.\tQ1\tMedian\tMean\tQ3\tMax.\n")
    oob_mse_qs <- as.numeric(
      quantile(x$oob.mse, probs = c(0, 0.25, 0.5, 0.75, 1)))
    cat("OOB (Training)", round(oob_mse_qs[1:3], 3),
        round(mean(x$oob.mse), 3),
        round(oob_mse_qs[4:5], 3), sep = "\t", "\n\n")

    gen_mse_qs <-
      as.numeric(quantile(x$General.mse, probs = c(0, 0.25, 0.5, 0.75, 1)))
    cat("All (Training)", round(gen_mse_qs[1:3], 3),
        round(mean(x$General.mse), 3),
        round(gen_mse_qs[4:5], 3), sep = "\t", "\n\n")
  }

  cat("Forest Level Summary \n\n")
  if (x$fam == "binomial") {
    cat("OOB (Training) Data: \n\n")
    logistic.acc(x$train_response, x$oob_pred_mean, x$prob_cutoff)
    cat("\n\n All (Training) Data: \n\n")
    logistic.acc(x$train_response, x$gen_pred_mean, x$prob_cutoff)
    cat("\n\n")
    if (!is.null(x$new_data_overall_r2)) {
      cat("Validation Data: \n\n")
      logistic.acc(x$new_response, x$new_pred_mean, x$prob_cutoff)
      cat("\n\n")
    }
  } else {
    cat("Data Used\tPseudo R2\tMSE\n")

    cat("OOB (Training)",
        formatC(c(x$oob_overall_r2, x$oob_overall_mse), format = "f",
                digits = 7, drop0trailing = F), sep = "\t", "\n\n")
    cat("All (Training)",
        formatC(c(x$general_overall_r2, x$general_overall_mse), format = "f",
                digits = 7, drop0trailing = F), sep = "\t", "\n\n")

    if (!is.null(x$new_data_overall_r2)) {
      if (x$fam != "binomial") {
        cat(
          "Validation",
          formatC(c(x$new_data_overall_r2, x$new_data_overall_mse),
                  format = "f", digits = 7, drop0trailing = F),
          sep = "\t", "\n\n")
      }
    }
  }
}
