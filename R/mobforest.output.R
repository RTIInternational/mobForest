#' Model-based random forest object
#'
#' Random Forest Output object that stores all the results including
#' predictions, variable importance matrix, model, family of error
#' distributions, and observed responses.
#'
#' @param oob_predictions Predictions on out-of-bag data.
#' @param general_predictions Predictions on learning data.
#' @param new_data_predictions Predictions on new test data.
#' @param varimp_object The variable importance object.
#' @param model_used The model used.
#' @param family A description of the error distribution and link function to be
#' used in the model.
#' @param train_response Response outcome of training data.
#' @param new_response Response outcome of test data.
#' @seealso \code{\linkS4class{prediction.output}},
#' \code{\linkS4class{varimp.output}}
#'
#' @export
mobforest.output <-
  function(oob_predictions, general_predictions, new_data_predictions,
           varimp_object, model_used, family, train_response,
           new_response = data.frame(matrix(0, 0, 0))) {
    rval <- new("mobforest.output", oob_predictions = oob_predictions,
                general_predictions = general_predictions,
                new_data_predictions = new_data_predictions,
                varimp_object = varimp_object, model_used = model_used,
                family = family, train_response = train_response,
                new_response = new_response)
    return(rval)
  }



#' @export
#' @rdname mobforest.output-class
#' @importFrom methods show
#' @aliases show,mobforest.output-method
#' @param object object of class \code{\linkS4class{mobforest.output}}
setMethod("show", "mobforest.output", function(object) {
  rf <- object
  cat("\tRandom Forest of Model Based Recursive Partitioning Trees\n\n")
  cat("Number of trees:", ncol( (rf@varimp_object)@varimp_matrix), "\n\n")
  cat("Model Used:", rf@model_used, "\n\n")
})
