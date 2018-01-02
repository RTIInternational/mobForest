#' Variable importance scores computed through random forest analysis
#' 
#' @param rf An object of class \code{\linkS4class{mobforest.output}}
#' returned by \link[=mobforest.analysis]{mobforest.analysis()}
#' @references Leo Breiman (2001). Random Forests. \emph{Machine Learning},
#' 45(1), 5-32.\cr
#' @rdname get.varimp-methods
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
#'         alpha = 0.05, bonferroni = TRUE, minsplit = 25), data = BostonHousing,
#'     processors = 1, model = linearModel, seed = 1111)
#' # Returns a vector of variable importance scores
#' get.varimp(rfout)
#' }
#' 
#' @export
get.varimp <- function(rf) {
  var_imp_scores <- apply( (rf@varimp_object)@varimp_matrix, 1, mean, na.rm = T)
  return(sort(var_imp_scores, decreasing = T))
}
