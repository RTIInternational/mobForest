#' Class \code{"mobforest.control"} of mobForest model
#'
#' Control parameters for random forest
#'
#'
#' @name mobforest.control-class
#' @docType class
#' @section Objects from the Class: Objects can be created by
#' \code{\link{mobforest.control}}.
#' @keywords classes
#' @examples
#'
#' # showClass("mobforest.control") The following code creates following forest
#' # controls: 400 trees to be constructed, sampling with replacement, a node
#' # contains at least 200 observations
#' mobforest_controls = mobforest.control(ntree = 400, mtry = 4,
#'     replace = TRUE, minsplit = 200)
#'
#' @export
setClass("mobforest.control",
         representation(ntree = "numeric", mtry = "numeric",
                        replace = "logical", fraction = "numeric",
                        mob_control = "list"),
         prototype = list(ntree = 300, mtry = 0,
                          replace = FALSE, fraction = 0.632))

#' Class \code{"varimp.output"} of mobforest model
#'
#' Variable importance
#'
#'
#' @name varimp.output-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the
#' form \code{\link{varimp.output}}. %% ~~ describe objects here ~~
#' @seealso \code{\linkS4class{varimp.output}}
#' @references Strobl, C., Malley, J. and Tutz, G. (2009) An introduction to
#' recursive partitioning: rationale, application, and characteristics of
#' classification and regression trees, bagging, and random forests,
#' \emph{Psychol Methods}, 14, 323-348.\cr
#' @export
setClass("varimp.output",
         representation(varimp_matrix = "matrix"),
         prototype = list(varimp_matrix = matrix(0, 0, 0)))


#' Class \code{"prediction.output"} of mobForest model
#'
#' The object of this class stores predictions and predictive accuracy
#' estimates.
#'
#'
#' @name prediction.output-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the
#' form \code{\link{prediction.output}}.
#'
#' @seealso \code{\link{prediction.output}}, \code{\link{predictive.acc}}
#' @keywords classes
#' @export
setClass("prediction.output",
         representation(pred_mat = "matrix",
                        R2_or_acc = "numeric",
                        mse = "numeric",
                        overall_r2_or_acc = "numeric",
                        pred_type = "character"),
         prototype = list(pred_mat = matrix(0, 0, 0),
                          R2_or_acc = numeric(),
                          mse = numeric(),
                          overall_r2_or_acc = numeric(),
                          pred_type = character()))


#' Class \code{"mobforest.output"} of mobforest model
#'
#' Random Forest output for model based recursive partitioning
#'
#'
#' @name mobforest.output-class
#' @aliases mobforest.output-class
#' get.pred.values, mobfores.output, logical, logical-method
#' mobforest.output-method
#' predictive.acc, mobforest.output, logical, logical-method
#' mobforest.output-method, mobforest.output-method
#' mobforest.output-method
#' @docType class
#' @section Objects from the Class: Objects can be created by
#' \code{\link{mobforest.output}}.
#' @seealso \code{\linkS4class{prediction.output}},
#' \code{\linkS4class{varimp.output}}
#' @keywords classes
#' @rdname mobforest.output-class
#' @examples
#'
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
#' }
#'
#' @exportClass mobforest.output
setClass("mobforest.output",
         representation(oob_predictions = "prediction.output",
                        general_predictions = "prediction.output",
                        new_data_predictions = "prediction.output",
                        varimp_object = "varimp.output",
                        model_used = "character",
                        family = "character",
                        train_response = "data.frame",
                        new_response = "data.frame"
                        ))
