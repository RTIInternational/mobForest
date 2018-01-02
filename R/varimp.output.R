#' Variable importance matrix containing the decrease in predictive accuracy
#' after permuting the variables across all trees
#'
#' Values of variable 'm' in the oob cases are randomly permuted and R2
#' obtained through variable-m-permuted oob data is subtracted from R2
#' obtained on untouched oob data. The average of this number over all the
#' trees in the forest is the raw importance score for variable m.
#'
#' @param varimp_matrix a matrix containing decrease in predictive accuracy for
#' all variables for each tree
#' @return An object of class \code{\linkS4class{varimp.output}}.
#' @references Strobl, C., Malley, J. and Tutz, G. (2009) An introduction to
#' recursive partitioning: rationale, application, and characteristics of
#' classification and regression trees, bagging, and random forests,
#' \emph{Psychol Methods}, 14, 323-348.\cr
#'
#' @importFrom methods new
#' @export
varimp.output <- function(varimp_matrix) {
  rval <- new("varimp.output", varimp_matrix = varimp_matrix)
  return(rval)
}
