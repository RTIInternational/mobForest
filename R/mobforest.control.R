#' Control parameters for random forest
#'
#' Various parameters that control the forest growing.
#'
#' This function is used to set up forest controls. The mob_control (from
#' party 'package') object is used to set up control parameters for single
#' tree model.
#' For most parameters, please see: \link[=mob_control]{mob_control()}
#'
#' @param ntree Number of trees to be constructed in forest (default = 300).
#' @param mtry Number of input variables randomly sampled as candidates at
#' each node.
#' @param replace logical. replace = TRUE (default) performs
#' bootstrapping. replace = FALSE performs sampling without replacement.
#' @param fraction Number of observations to draw without replacement (only
#' relevant if replace = FALSE).
#' @param alpha A node is considered for splitting if the p value for any
#' partitioning variable in that node falls below alpha (default 0.05).
#' Please see \link[=mob_control]{mob_control()}.
#' @param bonferroni logical. Should p values be Bonferroni corrected?
#' (default TRUE). Please see \link[=mob_control]{mob_control()}.
#' @param minsplit An integer. The minimum number of observations in a node
#' (default 20). Please see \link[=mob_control]{mob_control()}. 
#' @param trim A numeric, as defined in \link[=mob_control]{mob_control()}.
#' @param objfun A function, as defined in \link[=mob_control]{mob_control()}.
#' @param breakties A logical, as defined in \link[=mob_control]{mob_control()}.
#' @param parm A numeric or vector, as defined in
#' \link[=mob_control]{mob_control()}.
#' @param verbose A logical, as defined in \link[=mob_control]{mob_control()}.
#' @return An object of class \code{\linkS4class{mobforest.control}}.
#' @references Achim Zeileis, Torsten Hothorn, and Kurt Hornik (2008).
#' Model-Based Recursive Partitioning. \emph{Journal of Computational and
#' Graphical Statistics}, 17(2), 492-514.\cr
#' @examples
#' # create forest controls before starting random forest analysis
#' mobforest_control = mobforest.control(ntree = 400, mtry = 4, replace = TRUE,
#'     minsplit = 200)
#'
#' @importFrom stats deviance
#' @importFrom methods new
#' @export
mobforest.control <-
  function(ntree = 300, mtry = 0, replace = FALSE, fraction = 0.632,
           alpha = 1, bonferroni = FALSE, minsplit = 20, trim = 0.1,
           objfun = deviance, breakties = FALSE, parm = NULL, verbose = FALSE) {
  controls <-
    mob_control(alpha = alpha, bonferroni = bonferroni, minsplit = minsplit,
                trim = trim, objfun = objfun, breakties = breakties,
                parm = parm, verbose = verbose)
  class(controls) <- "list"
  rval <- new("mobforest.control", ntree = ntree, mtry = mtry,
              replace = replace, fraction = fraction, mob_control = controls)
  return(rval)
  }
