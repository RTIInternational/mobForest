#' A plot with variable importance score on X-axis and variable name on
#' Y-axis.
#'
#'
#' @param object An object of class \code{\linkS4class{mobforest.output}}
#' returned by \link[=mobforest.analysis]{mobforest.analysis()}
#' @seealso \link[=get.varimp]{get.varimp}
#' @references Leo Breiman (2001). Random Forests. \emph{Machine Learning},
#' 45(1), 5-32.\cr
#' @rdname varimplot-methods
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
#'  varimplot(rfout)
#' }
#' 
#' @export
varimplot <- function(object) {
  rf <- object
  var_imp_scores <- apply( (rf@varimp_object@varimp_matrix), 1, mean, na.rm = T)
  par(mfrow = c(1, 2))
  lattice::dotplot(
    sort(var_imp_scores),
    xlab = "Variable Importance in the data",
    panel = function(x, y){
      lattice::panel.dotplot(x, y, col = "darkblue", pch = 16, cex = 1.1,
                             main = "Variance Importance Plot")
      lattice::panel.abline(v = abs(min(var_imp_scores)), col = "red",
                            lty = "longdash", lwd = 2)
      lattice::panel.abline(v = 0, col = "blue")
      })
}
