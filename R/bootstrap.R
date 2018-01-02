#'
#' This method computes predicted outcome for each observation in the data
#' frame using the tree model supplied as an input argument.
#' @param i the tree
#' @param data A data frame containing the variables in the model.
#' @param main_model A model in character format
#' @param partition_vars A vector of partition variables
#' @param mtry A Random subset of partition variables to be considered at each
#' @param new_test_data A data frame representing test data for validating
#' random forest model. This data is not used in in tree building process.
#' @param mobforest_controls An object of class
#' \code{"\linkS4class{mobforest.control}"} returned by
#' \link[=mobforest.control]{mobforest.control()}, that contains parameters
#' controlling the construction of random forest.
#' @param fraction number of observations to draw without replacement (only
#' relevant if replace = FALSE)
#' @param replace TRUE or FALSE. replace = TRUE (default) performs
#' bootstrapping. replace = FALSE performs sampling without replacement.
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
#' @return A list model performance metrics including R2/accuracy, predictions, 
#' MSE, and variable importance
#' 
#' @examples
#' \dontrun{
#' formula <- as.formula(medv ~ lstat)
#' # load data
#' data("BostonHousing", package = "mlbench")
#' mobforest_controls <- 
#'   mobforest.control(ntree = 1, mtry = 2, replace = TRUE,
#'                     alpha = 0.05, bonferroni = TRUE, minsplit = 25)
#' 
#' out <- bootstrap(i, data = BostonHousing, main_model = string.formula(formula),
#'                  partition_vars = partition_vars <- c("rad", "crim", "tax"),
#'                  mtry = 2, new_test_data = as.data.frame(matrix(0,0,0)),
#'                  mobforest_controls = mobforest_controls@mob_control, fraction = 1,
#'                  replace = TRUE, model = linearModel, family = "", prob_cutoff = .5)
#' out
#' }
#' @importFrom modeltools ModelEnvFormula
#' @importFrom stats as.formula
#' @export
bootstrap <- function(i, data, main_model, partition_vars, mtry, new_test_data,
                      mobforest_controls, fraction, replace, model, family,
                      prob_cutoff = .5) {
  # Grab a Fraction of the Data
  data_sub_inds <-
    sample(nrow(data), replace = replace)[1:round(fraction * nrow(data))]
  data_sub <- data[data_sub_inds, ]

  # Call mob_RF_tree based on model@name
  fmBH <- NULL
  if (model@name == "linear regression model") {
    fmBH <-
      mob.rf.tree(main_model = main_model, partition_vars = partition_vars,
                  mtry = mtry, control = mobforest_controls, data = data_sub,
                  model = model)
  }
  if (model@name == "generalized linear regression model") {
    fmBH <-
      mob.rf.tree(main_model = main_model, partition_vars = partition_vars,
                  mtry = mtry, control = mobforest_controls, data = data_sub,
                  model = model, family = family)
  }
  # Find the out-of-box rows. This is used when calculate variable importance
  oob_inds <- setdiff(1:nrow(data), data_sub_inds)
  oob_sub <- data[oob_inds, ]
  # Generate Predictions. YOU CANNOT USE PREDICT() HERE. It does not work.
  pred <- sapply(1:nrow(data), tree.predictions, df = data, tree = fmBH@tree)
  obs_outcome <-
    ModelEnvFormula(as.formula(main_model), data = data) @get ("response")

  ret <- NULL
  # if !binomial, or if log + poisson:
  if ( (is.null(fmBH@tree$model$family)) ||
       (fmBH@tree$model$family$link == "log" &&
        fmBH@tree$model$family$family == "poisson")) {
    # Calculate R^2 and MSE for all observations
    gen_rsq <- compute.r2(obs_outcome, matrix(pred, ncol = 1))
    mse_gen <- sum( (obs_outcome - pred) ** 2, na.rm = T) / nrow(data)
    # Calculate the OOB R^2 and MSE
    oob_rsq <- compute.r2(matrix(obs_outcome[oob_inds, ], ncol = 1),
                         matrix(pred[oob_inds], ncol = 1))
    mse_oob <- compute.mse(matrix(obs_outcome[oob_inds, ], ncol = 1),
                          matrix(pred[oob_inds], ncol = 1))
    # Calculate the Variable Importance.
    # Randomize the values for partitionVar X. How does it change predictions?
    oob_mse_perm <- rep(0, length(partition_vars))
    for (p in 1:length(partition_vars)) {
      oob_perm <- oob_sub
      oob_perm[, partition_vars[p]] <- sample(oob_sub[, partition_vars[p]])
      oob_pred_perm <- sapply(1:nrow(oob_perm), tree.predictions,
                              df = oob_perm, tree = fmBH@tree)
      oob_mse_perm[p] <- compute.mse(matrix(obs_outcome[oob_inds, ], ncol = 1),
                                    matrix(oob_pred_perm, ncol = 1))
    }
    # If there is testing data, calculate tree predictions for it
    pred_new <- c()
    newdat_rsq <- c()
    if (nrow(new_test_data) > 0) {
      pred_new <- sapply(1:nrow(new_test_data), tree.predictions,
                         df = new_test_data, tree = fmBH@tree)
      obs_newdat <-
        ModelEnvFormula(
          as.formula(paste(main_model, partition_vars, sep = " | ")),
          data = new_test_data) @get ("response")
      newdat_rsq <-
        compute.r2(obs_newdat, matrix(pred_new, ncol = 1))
    }
    # Cleanup the output.
    ret <- list(oob_inds, oob_rsq, pred, (oob_mse_perm - mse_oob),
                mse_oob, gen_rsq, mse_gen, pred_new, newdat_rsq)
    names(ret) <- c("oob_inds", "oob_R2", "pred", "raw_var_imp",
                    "mse_oob", "gen_R2", "mse_gen", "pred_new", "newdat_R2")
  }

  # If model is a glm.
  if (model@name == "generalized linear regression model") {
    if (fmBH@tree$model$family$link == "logit") {
      levels(obs_outcome[, 1]) <- list("0" = levels(obs_outcome[, 1])[1],
                                       "1" = levels(obs_outcome[, 1])[2])
      pred_class <- rep(0, length(pred))
      pred_class[which(pred > prob_cutoff)] <- 1
      gen_acc <- length(which(pred_class == obs_outcome[, 1]))
      oob_acc <- length(which(pred_class[oob_inds] == obs_outcome[oob_inds, 1]))
      oob_acc_perm <- rep(0, length(partition_vars))
      for (p in 1:length(partition_vars)) {
        oob_perm <- oob_sub
        oob_perm[, partition_vars[p]] <- sample(oob_sub[, partition_vars[p]])
        oob_pred_perm <- sapply(1:nrow(oob_perm), tree.predictions,
                               df = oob_perm, tree = fmBH@tree)
        pred_class_perm <- rep(0, length(oob_pred_perm))
        pred_class_perm[which(oob_pred_perm > prob_cutoff)] <- 1
        oob_acc_perm[p] <-
          length(which(pred_class_perm == obs_outcome[oob_inds, 1]))
      }
      # If there is testing data, calculate tree predictions for it
      pred_new <- c()
      new_data_acc <- c()
      if (nrow(new_test_data) > 0) {
        pred_new <- sapply(1:nrow(new_test_data), tree.predictions,
                          df = new_test_data, tree = fmBH@tree)
        pred_new_class <- rep(0, length(pred_new))
        pred_new_class[which(pred > prob_cutoff)] <- 1
        obs_newdat <-
          ModelEnvFormula(
            as.formula(paste(main_model, partition_vars, sep = " | ")),
            data = new_test_data) @get ("response")
        levels(obs_newdat[, 1]) <- list("0" = levels(obs_newdat[, 1])[1],
                                        "1" = levels(obs_newdat[, 1])[2])
        new_data_acc <-
          length(which(pred_new_class == obs_newdat[, 1])) / nrow(new_test_data)
      }
      ret <- list(oob_inds, (oob_acc / length(oob_inds)), pred,
                  (oob_acc - oob_acc_perm) / length(oob_inds),
                  (gen_acc / nrow(data)), pred_new, new_data_acc)
      names(ret) <- c("oob_inds", "oob_acc", "pred", "raw_var_imp", "gen_acc",
                      "pred_new", "new_data_acc")
    }
  }
  return(ret)
}
