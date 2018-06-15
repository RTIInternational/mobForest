#' Predictions and predictive accuracy estimates
#'
#' This function takes predictions and predictive accuracy estimates as input
#' arguments and creates objects of class
#' \code{\linkS4class{prediction.output}}.
#'
#' @param pred_mean Mean predictions across trees.
#' @param pred_sd Standard deviation predictions across trees.
#' @param residual Residuals (predicted outcome - observed outcome).
#' @param R2_or_acc R2 or accuracy (for binomial) across trees
#' @param mse MSE across trees
#' @param overall_r2_or_acc Overall R2 or accuracy (for binomial)
#' @param pred_type Out-of-bag data or test data or learning data.
#' @return An object of class \code{"\link[=prediction.output]{prediction.output()}"}.
#' @seealso \code{\linkS4class{prediction.output}},
#' \code{\link{mobforest.analysis}}
#'
#' @importFrom methods new
#' @export
prediction.output <-
  function(pred_mean = numeric(), pred_sd = numeric(), residual = numeric(),
           R2_or_acc = numeric(), mse = numeric(), overall_r2_or_acc = numeric(),
           pred_type = character()) {
  pred_mat <- matrix(0, nrow = length(pred_mean), ncol = 3)
  pred_mat[, 1:3] <- c(pred_mean, pred_sd, residual)
  colnames(pred_mat) <- c("pred_mean", "pred_stdev", "residual")
  rval <- new("prediction.output", pred_mat = pred_mat, R2_or_acc = R2_or_acc,
              mse = mse, overall_r2_or_acc = overall_r2_or_acc,
              pred_type = pred_type)
  return(rval)
  }