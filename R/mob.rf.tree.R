#' Model based recursive partitioning - randomized subset of partition variables
#' considered during each split. 
#' 
#' The \link[=mob]{mob} function in party package is modified so that a random
#' subset of predictor variables are considered during each split. mtry
#' represents the number of predictor variables to be considered during each
#' split.
#' @param main_model A model in character format
#' @param partition_vars A vector of partition variables
#' @param mtry A Random subset of partition variables to be considered at each
#' node of decision tree
#' @param weights An optional vector of weights, as described in
#' \link[=mob]{mob}
#' @param data A data frame containing the variables in the model.
#' @param na.action A function which indicates what should happen when the
#' data contain NAs, as described in \link[=mob]{mob}
#' @param model A model of class \code{\linkS4class{StatModel}}
#' @param control A list with control parameters as returned by
#' \link[=mob_control]{mob_control}
#' @param \dots Additional arguments passed to the fit call for the model.
#' @return An object of class mob inheriting from
#' \code{\linkS4class{BinaryTree}}. Every node of the tree is additionally
#' associated with a fitted model.
#' @references Achim Zeileis, Torsten Hothorn, and Kurt Hornik (2008).
#' Model-Based Recursive Partitioning. \emph{Journal of Computational and
#' Graphical Statistics}, 17(2), 492-514.\cr
#' @importFrom modeltools ParseFormula glinearModel dpp dimension fit na.omit
#' @importFrom party reweight
#' @importFrom stats formula as.formula
#' @importFrom methods new
#' @export
mob.rf.tree <- function(main_model, partition_vars, mtry, weights,
                        data = list(), na.action = na.omit,
                        model = glinearModel, control = mob_control(), ...) {
  base_fm <- formula(paste(main_model, paste(sample(partition_vars)[1:mtry],
                                            collapse = " + "), sep = " | "))
  if (inherits(base_fm, "formula")) {
    mobpp <- function(formula, data, model) {
      ff <- attr(modeltools::ParseFormula(formula), "formula")
      ff$input[[3]] <- ff$input[[2]]
      ff$input[[2]] <- ff$response[[2]]
      dpp(model, as.formula(ff$input),
          other = list(part = as.formula(ff$blocks)),
          data = data, na.action = na.action)
    }
    base_fm <- mobpp(base_fm, data, model)
  }
  if (missing(weights)) {
    weights <- rep(1, dimension(base_fm, "part")[1])
  }
  fm <- fit(model, base_fm, ...)
  where <- integer(length(weights))

  # Main Function
  mob_fit <- function(obj, mf, weights, control) {
    obj <- reweight(obj, weights)
    if (inherits(obj, "try-error")) {
      node <-
        list(node_id = NULL, weights = weights,
             criterion = list(statistic = 0, criterion = 0, maxcriterion = 0),
             terminal = T, psplit = NULL, ssplits = NULL,
             prediction = 0, left = NULL, right = NULL,
             sumweights = as.double(sum(weights)), model = obj)
      class(node) <- "TerminalNodeModel"
      node$node_id <- as.integer(nodeid)
      where[weights > 0] <<- as.integer(nodeid)
      nodeid <<- nodeid + 1
      return(node)
    }
    thisnode <- mob_fit_setupnode(obj, mf, weights, control)
    thisnode$node_id <- as.integer(nodeid)
    where[weights > 0] <<- as.integer(nodeid)
    nodeid <<- nodeid + 1
    thisnode$model <- obj
    if (!thisnode$terminal) {
      childweights <- mob_fit_childweights(thisnode, mf, weights)
      if (any(sapply(childweights, sum) == 0)) {
        thisnode$terminal <- TRUE
        class(thisnode) <- "TerminalModelNode"
        return(thisnode)
      }
      mf <-
        formula(paste(main_model, paste(sample(partition_vars)[1:mtry],
                                        collapse =  " + "), sep = " | "))
      mf <- mobpp(mf, data, model)
      thisnode$left <- mob_fit(obj, mf, weights = childweights$left, control)
      thisnode$right <- mob_fit(obj, mf, weights = childweights$right, control)
    }
    return(thisnode)
  }
  nodeid <- 1
  tr <- mob_fit(fm, base_fm, weights = weights, control = control)
  y <- base_fm@get ("response")
  yy <- new("VariableFrame", nrow(y), ncol(y))
  yy@variables <- base_fm@get ("response")
  rval <- new("mob", tree = tr, responses = yy, data = base_fm,
              where = where)
  return(rval)
}
