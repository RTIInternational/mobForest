#' Predictions from tree model
#'
#' This method computes predicted outcome for each observation in the data
#' frame using the tree model supplied as an input argument.
#'
#' @param j the observation
#' @param df A data frame containing the variables in the model.
#' @param tree An object of class mob inheriting from
#' \code{\linkS4class{BinaryTree}}
#' @return A vector of predicted outcome
#' 
#' @examples
#' library(mlbench)
#' set.seed(1111)
#' # Random Forest analysis of model based recursive partitioning load data
#' data("BostonHousing", package = "mlbench")
#' data <- BostonHousing[1:90, c("rad", "tax", "crim", "medv", "lstat")]
#' fmBH <- mob.rf.tree(main_model = "medv ~ lstat", 
#'                      partition_vars = c("rad", "tax", "crim"), mtry = 2,
#'                      control = mob_control(), data = data, 
#'                      model = linearModel)
#' tree.predictions(j = 1, df = data, tree = fmBH@tree)
#'
#' @export
tree.predictions <- function(j, df, tree) {
  while (tree$terminal == FALSE) {
    newvar <- tree$psplit$variableName
    if (class(tree$psplit) == "nominalSplit") {
      left_split_vars <-
        levels(tree$psplit$splitpoint)[as.logical(tree$psplit$splitpoint)]
      if (!is.na(match(as.character(df[j, newvar]), left_split_vars))){
        tree <- tree$left
        } else {
          tree <- tree$right
          }
      } else {
        if (df[j, newvar] <= tree$psplit$splitpoint) {
          tree <- tree$left
          } else {
            tree <- tree$right
          }
      }
    }
  return(as.numeric(tree$model$predict_response(df[j, ])))
  }
