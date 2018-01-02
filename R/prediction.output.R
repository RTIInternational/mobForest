#' Predictions and predictive accuracy estimates
#'
#' This function takes predictions and predictive accuracy estimates as input
#' arguments and creates objects of class
#' \code{\linkS4class{prediction.output}}.
#'
#' @param pred_mean Mean predictions across trees.
#' @param pred_sd Standard deviation predictions across trees.
#' @param residual Residuals (predicted outcome - observed outcome).
#' @param R2 Predictive accuracy across trees
#' @param mse MSE across trees
#' @param overall_r2 Overall R2
#' @param pred_type Out-of-bag data or test data or learning data.
#' @return An object of class \code{"\link[=prediction.output]{prediction.output()}"}.
#' @seealso \code{\linkS4class{prediction.output}},
#' \code{\link{mobforest.analysis}}
#'
#' @importFrom methods new
#' @export
prediction.output <-
  function(pred_mean = numeric(), pred_sd = numeric(), residual = numeric(),
           R2 = numeric(), mse = numeric(), overall_r2 = numeric(),
           pred_type = character()) {
  pred_mat <- matrix(0, nrow = length(pred_mean), ncol = 3)
  pred_mat[, 1:3] <- c(pred_mean, pred_sd, residual)
  colnames(pred_mat) <- c("pred_mean", "pred_stdev", "residual")
  rval <- new("prediction.output", pred_mat = pred_mat, R2 = R2, mse = mse,
              overall_r2 = overall_r2, pred_type = pred_type)
  return(rval)
}
