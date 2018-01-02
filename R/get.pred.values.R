#' Get predictions summarized across trees for out-of-bag cases or all cases 
#' for cases from new test data
#' 
#' @param rf An object of class \code{mobforest.output}.
#' @param OOB a logical determining whether to return predictions from the
#' out-of-bag sample or the learning sample (not suggested).
#' @param newdata a logical determining whether to return predictions from
#' test data. If newdata = TRUE, then OOB argument is ignored.
#' @return matrix with three columns: 1) Mean Predictions across trees, 2)
#' Standard deviation of predictions across trees, and 3) Residual (mean
#' predicted - observed). The third column is applicable only when linear
#' regression is considered as the node model.
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
#'     mobforest_controls = mobforest.control(ntree = 3, mtry = 2, replace = TRUE,
#'     alpha = 0.05, bonferroni = TRUE, minsplit = 25), data = BostonHousing,
#'     processors = 1, model = linearModel, seed = 1111)
#'     
#' # Obtain out-of-bag predicted values
#' OOB_pred_mat <- get.pred.values(rfout, OOB = TRUE)
#' OOB_pred = OOB_pred_mat[, 1]
#' }
#' 
#' @export
get.pred.values <- function(rf, OOB = T, newdata = F){
  rval <- c()
  if (nrow(rf@new_data_predictions@pred_mat) == 0 && newdata == TRUE) {
    cat("Predicted values were only computed on original data. Please set",
        "newdata = FALSE and run the get.pred.values() again. Or you can",
        "re-run the mobforest.analysis() with 'new_test_data' parameter not",
        "missing and later use getPredictedValues() to get predicted values",
        "on the new test data.")
    stop()
  }
  if (newdata == F) {
    if (OOB == T) {
      rval <- (rf@oob_predictions)@pred_mat
    } else {
      rval <- (rf@general_predictions@pred_mat)
    }
  } else {
    rval <- (rf@new_data_predictions)@pred_mat
  }
  return(rval)
}
