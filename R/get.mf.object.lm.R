#' Fit a linear model to a mobForest model
#' 
#' This method computes predicted outcome for each observation in the data
#' frame using the tree model supplied as an input argument.
#' 
#' @param object A bootstrap model object created by \link[=bootstrap]{bootstrap()}
#' @param data A data frame containing the variables in the model.
#' @param main_model A model in character format.
#' @param partition_vars A vector of partition variables.
#' @param new_test_data A data frame representing test data for validating
#' random forest model. This data is not used in in tree building process.
#' @param ntree Number of trees to be constructed in forest (default = 300)
#' @param family A description of error distribution and link function to be used
#' in the model. This parameter needs to be specified if generalized linear
#' model is considered. The parameter "binomial()" is to be specified
#' when logistic regression is considered and "poisson()" when Poisson
#' regression is considered as the node model. The values allowed for this
#' parameter are binomial() and poisson().
#' @return An object of class \code{\linkS4class{mobforest.output}}.
#' @seealso \link[=mobforest.control]{mobforest.control()},
#' \code{\link{mobforest.output-class}}
#' @importFrom modeltools ModelEnvFormula
#' @importFrom stats as.formula sd
#' 
#' @export
get.mf.object.lm <- function(object, main_model, partition_vars,
                                  data, new_test_data, ntree, family) {
  c_out <- object
  var_imp <- matrix(0, nrow = length(partition_vars), ncol = ntree)
  rownames(var_imp) <- partition_vars
  oob_R2 <- c()
  oob_mse <- c()
  general_R2 <- c()
  general_mse <- c()
  gen_predictions <-
    matrix(NA, ncol = ntree, nrow = length(c_out[[1]]$pred))
  oob_predictions <-
    matrix(NA, ncol = ntree, nrow = length(c_out[[1]]$pred))

  for (i in 1:ntree) {
    oob_R2[i] <- c_out[[i]]$oob_R2
    oob_mse[i] <- c_out[[i]]$mse_oob
    general_R2[i] <- c_out[[i]]$gen_R2
    general_mse[i] <- c_out[[i]]$mse_gen
    gen_predictions[, i] <- c_out[[i]]$pred
    oob_predictions[c_out[[i]][[1]], i] <-
      c_out[[i]]$pred[c_out[[i]]$oob_inds]
    var_imp[, i] <- c_out[[i]]$raw_var_imp
  }

  obs_outcome <-
    ModelEnvFormula(as.formula(main_model), data = data) @get ("response")
  oob_res <- obs_outcome[, 1] - apply(oob_predictions, 1, mean, na.rm = T)
  oob_pred <-
    prediction.output(apply(oob_predictions, 1, mean, na.rm = T),
                      apply(oob_predictions, 1, sd, na.rm = T),
                      oob_res, oob_R2, oob_mse,
                      compute.r2(obs_outcome, oob_predictions), "OOB")
  gen_res <- obs_outcome[, 1] - apply(gen_predictions, 1, mean, na.rm = T)
  general_pred <-
    prediction.output(apply(gen_predictions, 1, mean, na.rm = T),
                      apply(gen_predictions, 1, sd, na.rm = T),
                      gen_res, general_R2, general_mse,
                      compute.r2(obs_outcome, gen_predictions), "General")

  new_data_pred <- prediction.output()
  newdata <- new_test_data
  new_data_obs <- data.frame(matrix(0, 0, 0))
  if (nrow(newdata) > 0) {
    pred_newdata <- lapply(1:ntree, function(x) c_out[[x]]$pred_new)
    newdata_predictions <- matrix(unlist(pred_newdata), ncol = ntree)
    new_data_obs <-
      ModelEnvFormula(
        as.formula(paste(main_model,
                         paste(partition_vars, collapse = " + "), sep = " | ")),
        data = newdata) @get ("response")
    new_data_R2 <- unlist(lapply(1:ntree, function(x) c_out[[x]]$new_data_R2))
    new_data_res <-
      new_data_obs[, 1] - apply(newdata_predictions, 1, mean, na.rm = T)
    new_data_pred <-
      prediction.output(
        pred_mean = apply(newdata_predictions, 1, mean, na.rm = T),
        pred_sd = apply(newdata_predictions, 1, sd, na.rm = T),
        residual = new_data_res, R2_or_acc = new_data_R2,
        overall_r2_or_acc = compute.r2(new_data_obs, newdata_predictions),
        pred_type = "Newdata")
  }
  var_imp_obj <- varimp.output(var_imp)
  mfout <-
    mobforest.output(
      oob_pred, general_pred, new_data_pred, var_imp_obj,
      paste(main_model, paste(partition_vars, collapse = " + "), sep = " | "),
      family = "", train_response = obs_outcome, new_response = new_data_obs)
  return(mfout)
}
