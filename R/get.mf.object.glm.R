#' Fit a general linear model to a mobForest model
#' 
#' This method computes predicted outcome for each observation in the data
#' frame using the tree model supplied as an input argument.
#' 
#' @param object A bootstrap model object created by
#' \link[=bootstrap]{bootstrap()}
#' @param data A data frame containing the variables in the model.
#' @param main_model A model in character format.
#' @param partition_vars A vector of partition variables.
#' @param new_test_data A data frame representing test data for validating
#' random forest model. This data is not used in the tree building process
#' @param ntree Number of trees to be constructed in forest (default = 300). 
#' @param fam A description of error distribution and link function to be
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
#' @return An object of class \code{\linkS4class{mobforest.output}}.
#' @seealso \link[=mobforest.control]{mobforest.control()},
#' \code{\link{mobforest.output-class}}
#' @importFrom modeltools ModelEnvFormula
#' @importFrom stats as.formula
#' 
#' @export
get.mf.object.glm <- function(object, main_model, partition_vars, data,
                                   new_test_data, ntree, fam,
                                   prob_cutoff = .5) {
  ### Remove Global Variable Warnings ###
  sd = ""; 
  c_out <- object
  var_imp <- matrix(0, nrow = length(partition_vars), ncol = ntree)
  rownames(var_imp) <- partition_vars
  oob_acc <- c()
  general_acc <- c()
  gen_predictions <-
    matrix(NA, ncol = ntree, nrow = length(c_out[[1]]$pred))
  oob_predictions <-
    matrix(NA, ncol = ntree, nrow = length(c_out[[1]]$pred))

  for (i in 1:ntree) {
    oob_acc[i] <- c_out[[i]]$oob_acc
    general_acc[i] <- c_out[[i]]$gen_acc
    gen_predictions[, i] <- c_out[[i]]$pred
    oob_predictions[c_out[[i]]$oob_inds, i] <-
      c_out[[i]]$pred[c_out[[i]]$oob_inds]
    var_imp[, i] <- c_out[[i]]$raw_var_imp
  }

  obs_outcome <-
    ModelEnvFormula(as.formula(main_model), data = data) @get ("response")

  oob_res <- rep(NA, nrow(obs_outcome))
  oob_pred <-
    prediction.output(pred_mean = apply(oob_predictions, 1, mean, na.rm = T),
                      pred_sd = apply(oob_predictions, 1, sd, na.rm = T),
                      residual = oob_res, R2 = oob_acc,
                      overall_r2 = compute.acc(obs_outcome, oob_predictions,
                                             prob_cutoff), pred_type = "OOB")
  gen_res <- rep(NA, nrow(obs_outcome))
  general_pred <-
    prediction.output(pred_mean = apply(gen_predictions, 1, mean, na.rm = T),
                      pred_sd = apply(gen_predictions, 1, sd, na.rm = T),
                      residual = gen_res, R2 = general_acc,
                      overall_r2 =
                        compute.acc(obs_outcome, gen_predictions,
                                    prob_cutoff), pred_type = "General")
  new_data_pred <- prediction.output()
  new_data <- new_test_data
  new_data_obs <- data.frame(matrix(0, 0, 0))

  if (nrow(new_data) > 0) {
    pred_new_data <- lapply(1:ntree, function(x) c_out[[x]]$pred_new)
    new_data_predictions <- matrix(unlist(pred_new_data), ncol = ntree)
    new_data_obs <-
      ModelEnvFormula(
        as.formula(paste(main_model, paste(partition_vars, collapse = " + "),
                         sep = " | ")), data = new_data) @get ("response")
    new_data_acc <- unlist(lapply(1:ntree, function(x) c_out[[x]]$newdat_acc))
    new_data_res <- rep(NA, nrow(new_data_obs))
    new_data_pred <-
      prediction.output(
        pred_mean = apply(new_data_predictions, 1, mean, na.rm = T),
        pred_sd = apply(new_data_predictions, 1, sd, na.rm = T),
        residual = new_data_res, R2 = new_data_acc,
        overall_r2 = compute.acc(new_data_obs, new_data_predictions,
                               prob_cutoff), pred_type = "Newdata")
  }
  var_imp_obj <- varimp.output(var_imp)
  mfout <-
    mobforest.output(
      oob_pred, general_pred, new_data_pred, var_imp_obj,
      paste(main_model, paste(partition_vars, collapse = " + "), sep = " | "),
      fam = fam, train_response = obs_outcome,
      new_response = new_data_obs)
  return(mfout)
}
