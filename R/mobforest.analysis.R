#' Model-based random forest analysis
#'
#' Main function that takes all the necessary arguments to start model-based
#' random forest analysis.
#'
#' \code{mobforest.analysis} is the main function that takes all the input
#' parameters - model, partition variables, and forest control parameters -
#' and starts the model-based random forest analysis. \code{mobforest.analysis}
#' calls \code{bootstrap} function which constructs decision trees, computes
#' out-of-bag (OOB) predictions, OOB predictive accuracy and perturbation in
#' OOB predictive accuracy through permutation. \code{bootstrap} constructs
#' trees on multiple cores/processors simultaneously through parallel
#' computation. Later, the \code{get.mf.object} function wraps the
#' analysis output into \code{mobforest.output} object.\cr
#'
#' Predictive accuracy estimates are computed using pseudo-R2 metric, defined
#' as the proportion of total variation in outcome variable explained by a
#' tree model on out-of-bag cases. R2 ranges from 0 to 1. R2 of zero suggests
#' worst tree model (in terms of predicting outcome) and R2 of 1 suggests
#' perfect tree model.\cr
#' @param formula An object of class formula specifying the model. This should
#' be of type y ~ x_1 + ... + x_k, where the variables x_1, x_2, ..., x_k are
#' predictor variables and y represents an outcome variable. This model is
#' referred to as the node model
#' @param partition_vars A character vector specifying the partition
#' variables
#' @param data An input dataset that is used for constructing trees in random
#' forest.
#' @param mobforest_controls An object of class
#' \code{"\linkS4class{mobforest.control}"} returned by
#' \link[=mobforest.control]{mobforest.control()}, that contains parameters
#' controlling the construction of random forest.
#' @param new_test_data A data frame representing test data for validating
#' random forest model. This data is not used in in tree building process.
#' @param processors A number of processors/cores on your computer that should
#' be used for parallel computation.
#' @param model A model of class \code{"\link[=StatModel-class]{StatModel}"}
#' used for fitting observations in current node. This parameter allows
#' fitting a linear model or generalized linear model with formula y ~ x_1 +
#' ... + x_k. The Parameter "linearModel" fits linear model. The parameter
#' "glinearModel" fits Poisson or logistic regression model depending upon the
#' specification of parameter "family" (explained next). If "family" is
#' specified as binomial() then logistic regression is performed. If the
#' "family" is specified as poisson() then Poisson regression is performed.
#' @param family A description of error distribution and link function to be
#' used in the model. This parameter needs to be specified if generalized
#' linear model is considered. The parameter "binomial()" is to be specified
#' when logistic regression is considered and "poisson()" when Poisson
#' regression is considered as the node model. The values allowed for this
#' parameter are binomial() and poisson().
#' @param prob_cutoff In case of logistic regression as a node model, the
#' predicted probabilities for OOB cases are converted into classes (yes/no,
#' high/low, etc as specified) based on this probability cutoff. If logistic
#' regression is not considered as node model, the prob_cutoff = NULL. By
#' default it is 0.5 when parameter not specified (and logistic regression
#' considered).
#' @param seed Since this function uses parallel processes,
#' to replicate results, set the cluster
#' \code{"\link[=clusterSetRNGStream]{clusterSetRNGStream()}"} seed.
#' @return An object of class \code{\linkS4class{mobforest.output}}.
#' @seealso \link[=mobforest.control]{mobforest.control()},
#' \code{\link{mobforest.output-class}}
#' @references Achim Zeileis, Torsten Hothorn, and Kurt Hornik (2008).
#' Model-Based Recursive Partitioning. \emph{Journal of Computational and
#' Graphical Statistics}, 17(2), 492-514.\cr
#'
#' Hothorn, T., Hornik, K. and Zeileis, A. (2006) Unbiased recursive
#' partitioning: A conditional inference framework, \emph{J Compute Graph
#' Stat}, 15, 651-674.\cr
#'
#' Strobl, C., Malley, J. and Tutz, G. (2009) An introduction to recursive
#' partitioning: rationale, application, and characteristics of classification
#' and regression trees, bagging, and random forests, \emph{Psychol Methods},
#' 14, 323-348.\cr
#' 
#' @examples
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
#' \dontrun{
#' rfout  
#' }
#'
#' @importFrom parallel makeCluster clusterEvalQ clusterExport clusterApply stopCluster clusterSetRNGStream
#' @importFrom party mob_control
#' @importFrom modeltools linearModel
#' @importFrom stats as.formula
#' @export
mobforest.analysis <-
  function(formula, partition_vars, data,
           mobforest_controls = mobforest.control(),
           new_test_data = as.data.frame(matrix(0, 0, 0)), processors = 1,
           model = linearModel, family = NULL, prob_cutoff = .5,
           seed = sample(1:10000000, 1)) {
    # From library(party)
    mod <- string.formula(formula)
    B <- mobforest_controls@ntree
    mtry <- mobforest_controls@mtry
    if (mtry == 0) {
      mtry <- round(length(partition_vars) / 3) # Is this B? And not 3?
    }
    fraction <- mobforest_controls@fraction # Why .632?
    if (mobforest_controls@replace == TRUE) {
      fraction <- 1
    }
    # From Library(parallel)
    cl <- makeCluster(getOption("cl.cores", processors))
    clusterEvalQ(cl, {
      library(party)
      library(mobForest)
      })
    clusterSetRNGStream(cl, iseed = seed)
    clusterExport(cl, c("mob.rf.tree", "tree.predictions", "compute.r2",
                        "compute.acc", "compute.mse"))
    c_out <-
      clusterApply(cl, 1:B, bootstrap, data = data, main_model = mod,
                   partition_vars = partition_vars, mtry = mtry,
                   new_test_data = new_test_data,
                   mobforest_controls = mobforest_controls,
                   fraction = fraction, model = model, family = family,
                   prob_cutoff = prob_cutoff)
    stopCluster(cl)
    obs.outcome <-
      ModelEnvFormula(as.formula(paste(mod, partition_vars, sep = " | ")),
                      data = data) @get ("response")

    mf_object <- NULL
    if (model@name == "generalized linear regression model") {
      if (family$family %in% c("binomial", "poisson")) {
        mf_object <-
          get.mf.object.glm(c_out, main_model = mod,
                        partition_vars = partition_vars, data = data,
                        new_test_data = new_test_data, ntree = B,
                        family = family, prob_cutoff = prob_cutoff)
      }
    }
    if (model@name == "linear regression model") {
      mf_object <-
        get.mf.object.lm(c_out, main_model = mod,
                              partition_vars = partition_vars, data = data,
                              new_test_data = new_test_data, ntree = B,
                              family = family)
    }
    return(mf_object)
  }
