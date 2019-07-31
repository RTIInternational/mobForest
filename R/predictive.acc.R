#' Predictive performance across all trees
#'
#'
#' @param object An object of class \code{mobforest.output}
#' @param newdata A logical value specifying if the performance needs to be
#' summarized on test data supplied as \code{new_test_data} argument to
#' \code{mobforest.analysis} function.
#' @param prob_cutoff Predicted probabilities converted into classes (Yes/No,
#' 1/0) based on this probability threshold. Only used for producing predicted
#' Vs actual classes table.
#' @param plot A logical value specifying if the use wishes to view
#' performance plots
#' @return A list with performance parameters 
#' \item{oob_r2 }{A vector of predictive accuracy
#' estimates (ranging between 0 and 1) measured on Out-of-bag cases for each
#' tree}
#' \item{oob_mse }{A vector of MSE for Out-of-bag data for each tree.
#' Valid only if the outcome is continuous.}
#' \item{oob_overall_r2 }{ Overall
#' predictive accuracy measured by combining Out-of-bag predictions across
#' trees.}
#' \item{oob_overall_mse }{ Overall MSE measured by combining
#' Out-of-bag predictions across trees.}
#' \item{general_r2 }{ A vector of
#' predictive accuracy (ranging between 0 and 1) measured on complete
#' learning data for each tree }
#' \item{general_mse }{ A vector of MSE
#' measured on complete learning data for each tree. Valid only if the outcome
#' is continuous. }
#' \item{general_overall_r2 }{ Overall predictive accuracy
#' measured by combining predictions across trees. }
#' \item{general_overall_mse }{ Overall MSE measured by combining predictions
#' across trees. Valid only if the outcome is continuous.}
#' \item{model_used }{ The node model and partition variables used for analysis}
#' \item{family }{ Error distribution assumptions of the model}
#' @examples
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
#'     mobforest_controls = mobforest.control(ntree = 3, mtry = 2, replace = T,
#'         alpha = 0.05, bonferroni = T, minsplit = 25), data = BostonHousing,
#'     processors = 1, model = linearModel, seed = 1111)
#'
#' # get predictive performance estimates and produce a performance plot
#' pacc <- predictive.acc(rfout)
#' }
#'
#' @importFrom graphics par hist box axis
#' @export
predictive.acc <-
  function(object="mfOutput", newdata=F, prob_cutoff=NULL, plot=T) {
    rf <- object
    rval <- list()
    vec_r2 <- NULL
    vec_mse <- NULL
    # Create the list output. 11 outputs if newdata == F
    if (newdata == FALSE) {
      ss <- nrow( (rf@general_predictions)@pred_mat)
      rval <- list( (rf@oob_predictions)@R2,
                   (rf@oob_predictions)@mse,
                   (rf@oob_predictions)@overall_r2,
                   sum( (rf@oob_predictions)@pred_mat[, 3] ** 2,
                        na.rm = T) / ss,
                   (rf@general_predictions)@R2,
                   (rf@general_predictions)@mse,
                   (rf@general_predictions)@overall_r2,
                   sum( (rf@general_predictions)@pred_mat[, 3] ** 2,
                       na.rm = T) / ss,
                   rf@model_used,
                   rf@family,
                   prob_cutoff)
      names(rval) <-
        c("oob_r2", "oob_mse", "oob_overall_r2", "oob_overall_mse",
          "general_r2", "general_mse", "general_overall_r2",
          "general_overall_mse", "model_used", "family", "prob_cutoff")
      vec_r2 <-
        c(rval$oob_overall_r2, min(rval$oob_r2), max(rval$oob_r2))
      vec_mse <-
        c(rval$oob_overall_mse, min(rval$oob_mse), max(rval$oob_mse))
      } else {
        # 14 outputs if newdata is not F
        ss1 <- nrow( (rf@general_predictions)@pred_mat)
        ss2 <- nrow( (rf@new_data_predictions)@pred_mat)
        rval <- list( (rf@oob_predictions)@R2,
                      (rf@oob_predictions)@mse,
                      (rf@oob_predictions)@overall_r2,
                      sum( (rf@oob_predictions)@pred_mat[, 3] ** 2,
                           na.rm = T) / ss1,
                      (rf@general_predictions)@R2,
                      (rf@general_predictions)@mse,
                      (rf@general_predictions)@overall_r2,
                      sum( (rf@general_predictions)@pred_mat[, 3] ** 2,
                           na.rm = T) / ss1,
                      rf@model_used,
                      rf@family,
                      (rf@new_data_predictions)@R2,
                      (rf@new_data_predictions)@overall_r2,
                      sum( (rf@new_data_predictions)@pred_mat[, 3] ** 2,
                           na.rm = T) / ss2,
                      prob_cutoff)
        names(rval) <-
          c("oob_r2", "oob_mse", "oob_overall_r2", "oob_overall_mse",
            "general_r2", "general_mse", "general_overall_r2",
            "general_overall_mse", "model_used", "family", "Newdata.R2",
            "new_data_overall_r2", "new_data_overall_mse", "prob_cutoff")
        vec_r2 <- c(rval$oob_overall_r2, min(rval$oob_r2), max(rval$oob_r2),
                    rval$new_data_overall_r2)
        vec_mse <- c(rval$oob_overall_mse, min(rval$oob_mse),
                     max(rval$oob_mse), rval$new_data_overall_mse)
        }
    # if rf@ram is binomial, add additional output
    newd <- which(regexpr("Newdata", names(rval)) > 0)
    if (rf@family == "binomial") {
      rval$train_response <- rf@train_response
      rval$new_response <- rf@new_response
      rval$oob_pred_mean <- rf@oob_predictions@pred_mat[, 1]
      rval$gen_pred_mean <- rf@general_predictions@pred_mat[, 1]
      if (length(newd) > 0) {
        rval$new_pred_mean <- rf@new_data_predictions@pred_mat[, 1]
      }
    }
    # The rest is generating the four plots. Only if T, of course.
    if (plot == TRUE) {
      xlab <- ""
      r <- 2
      if (rf@family == "binomial") {
        r <- r - 1
        xlab <- "Proportion of subjects correctly classified"
        } else {
          xlab <- expression(R ** 2)
          }
      xlim1 <- ifelse(min(vec_r2) < 0, 0, min(vec_r2))
      xlim2 <- ifelse(max(vec_r2) > 1, 1, max(vec_r2))
      if (length(newd) == 0) {
        par(mfrow = c(r, 2))
      }
      if (length(newd) > 0) {
        par(mfrow = c( (r + 1), 2))
      }
      hist(rval$oob_r2, main = "OOB performance (Tree Level)", xlab = xlab,
           xlim = c(xlim1, xlim2))
      box()
      plot(c(rval$oob_overall_r2, rval$oob_overall_r2), c(1, 2), axes = F,
           type = "l", col = "red", lwd = 2,
           main = "OOB performance (Forest Level)", xlab = xlab,
           ylab = "", xlim = c(xlim1, xlim2))
      axis(1)
      box()
      if (rf@family != "binomial"){
        hist(rval$oob_mse, xlab = "MSE", main = "OOB performance (Tree Level)",
             xlim = c(min(vec_mse), max(vec_mse)))
        box()
        plot(c(rval$oob_overall_mse, rval$oob_overall_mse), c(1, 2), axes = F,
             type = "l", col = "red", lwd = 2, xlab = "MSE", ylab = "",
             main = "OOB performance (Forest Level)",
             xlim = c(min(vec_mse), max(vec_mse)))
        axis(1)
        box()
      }
      if (length(newd) > 0) {
        if (length(rval$new_data_overall_r2) != 0) {
          plot(c(rval$new_data_overall_r2, rval$new_data_overall_r2), c(1, 2),
               axes = F, type = "l", col = "red", lwd = 2, xlab = xlab,
               ylab = "", xlim = c(xlim1, xlim2),
               main = "Validation Performance")
          axis(1)
          box()
          if (rf@family != "binomial") {
            plot(c(rval$new_data_overall_mse, rval$new_data_overall_mse),
                 c(1, 2), axes = F, type = "l", col = "red", lwd = 2,
                 xlab = "MSE", ylab = "", main = "Validation Performance",
                 xlim = c(min(vec_mse), max(vec_mse)))
            axis(1)
            box()
          }
        } else {
          cat("No Performance Plot for new test data because 'new_test_data'",
              "argument was not supplied to mobforest.analysis() function \n")
        }
      }
      }
    class(rval) <- "predictive.acc.estimates"
    return(rval)
  }
