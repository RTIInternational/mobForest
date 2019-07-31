#' Produces two plots: a) histogram of residuals, b) predicted Vs residuals.
#' This feature is applicable only when linear regression is considered as the
#' node model.
#'
#' Residuals are computed as difference between the predicted values of
#' outcome (summarized across all trees) and observed values of outcome. The
#' residual plots are typical when the fitted values are obtained through
#' linear regression but not when logistic or Poisson regression is considered
#' as a node model. Therefore, the residual plots are produced only when
#' linear regression is considered. For logistic or Poisson models, a message
#' is printed saying "Residual Plot not produced when logistic of Poisson
#' regression is considered as the node model".
#'
#' @param object An object of class 'mobforest.output'
#' @param breaks Integer for number of breaks in histogram
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
#' # get predictive performance estimates and produce a performance plot
#' residualPlot(rfout)
#' }
#' 
#' @importFrom graphics par plot hist
#' @export
residual.plot <- function(object, breaks = 50){
  rf <- object
  if (rf@family == "binomial" | rf@family == "poisson") {
    cat("Residual Plot not produced when logistic of Poisson regression",
        "is considered as the node model\n")
  } else {
    par(mfrow = c(2, 1))
    plot(rf@oob_predictions@pred_mat[, 1], rf@oob_predictions@pred_mat[, 3],
         xlab = "Out-of-bag predcitions", ylab = "Out-of-bag residuals")
    hist(rf@oob_predictions@pred_mat[, 3],
         main = "Out-of-bag residuals histogram",
         xlab = "Out-of-bag residuals",
         breaks = breaks)
  }
}
