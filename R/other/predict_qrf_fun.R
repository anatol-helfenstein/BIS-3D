# Code from Laura Poggio ISRIC from public release of updated Soil Grids paper
# DOI: https://doi.org/10.5194/soil-2020-65
# https://git.wur.nl/isric/soilgrids/soilgrids/-/blob/master/models/ranger/predict_qrf_fun.R

# Modified ranger func. so that it can compute the mean for quantile regression
# forests (QRF) as well (as opposed to just the median i.e. 50th quantile)

# We will modify the tweaked function so that it also works for OOB predictions
# where num.trees != random.node.values.oob (see line 101)

predict.ranger.tree <- function(object, data = NULL, predict.all = FALSE,
                                num.trees = object$num.trees,
                                type = "response", se.method = "infjack",
                                quantiles = c(0.1, 0.5, 0.9), 
                                seed = NULL, num.threads = 1,
                                verbose = TRUE, ...) {
  forest <- object$forest
  if (is.null(forest)) {
    stop("Error: No saved forest in ranger object. Please set write.forest to TRUE when calling ranger.")
  }
  if (object$importance.mode %in% c("impurity_corrected", "impurity_unbiased")) {
    warning("Forest was grown with 'impurity_corrected' variable importance. For prediction it is advised to grow another forest without this importance setting.")
  }
  
  if (type == "quantiles") {
    ## Quantile prediction
    if (object$treetype != "Regression") {
      stop("Error: Quantile prediction implemented only for regression outcomes.")
    }
    if (is.null(object$random.node.values)) {
      stop("Error: Set quantreg=TRUE in ranger(...) for quantile prediction.")
    }
    
    if (is.null(data)) {
      ## OOB prediction
      if (is.null(object$random.node.values.oob)) {
        stop("Error: Set keep.inbag=TRUE in ranger(...) for out-of-bag quantile prediction or provide new data in predict(...).")
      }
      node.values <- object$random.node.values.oob
    } else {
      ## New data prediction
      terminal.nodes <- predict(object, data, type = "terminalNodes")$predictions + 1
      node.values <- 0 * terminal.nodes
      for (tree in 1:num.trees) {
        node.values[, tree] <- object$random.node.values[terminal.nodes[, tree], tree]
      }
    }
    
    ## Prepare results
    result <- list(num.samples = nrow(node.values),
                   treetype = object$treetype,
                   num.independent.variables = object$num.independent.variables,
                   num.trees = num.trees)
    class(result) <- "ranger.prediction"
    
    ## Compute quantiles of distribution
    result$predictions <- t(apply(node.values, 1, quantile, quantiles, na.rm=TRUE))
    if (nrow(result$predictions) != result$num.samples) {
      ## Fix result for single quantile
      result$predictions <- t(result$predictions)
    }
    colnames(result$predictions) <- paste("quantile=", quantiles)
    result
    
  } else if (type == "treepred") {
    
    ## Single tree predictions
    if (object$treetype != "Regression") {
      stop("Error: Quantile prediction implemented only for regression outcomes.")
    }
    if (is.null(object$random.node.values)) {
      stop("Error: Set quantreg=TRUE in ranger(...) for quantile prediction.")
    }
    
    if (is.null(data)) {
      ## OOB prediction
      if (is.null(object$random.node.values.oob)) {
        stop("Error: Set keep.inbag=TRUE in ranger(...) for out-of-bag quantile prediction or provide new data in predict(...).")
      }
      node.values <- object$random.node.values.oob
    } else {
      ## New data prediction
      terminal.nodes <- predict(object, data, type = "terminalNodes")$predictions + 1
      node.values <- 0 * terminal.nodes
      for (tree in 1:num.trees) {
        node.values[, tree] <- object$random.node.values[terminal.nodes[, tree], tree]
      }
    }
    
    ## Prepare results
    result <- list(num.samples = nrow(node.values),
                   treetype = object$treetype,
                   num.independent.variables = object$num.independent.variables,
                   num.trees = num.trees)
    class(result) <- "ranger.prediction"
    
    ## assign single tree predictions
    result$predictions <- node.values
    
    # MODIFY FOR WHEN num.trees != random.node.values.oob
    # this will always be the case for predicting OOB samples because only trees
    # are used for predicting samples that were OOB...
    if (is.null(data)) {
      colnames(result$predictions) <- paste0("tree_", 1:ncol(object$random.node.values.oob))
      result
    } else {
      colnames(result$predictions) <- paste0("tree_", 1:num.trees)
      result
    }
  } else {
    ## Non-quantile prediction
    if (is.null(data)) {
      stop("Error: Argument 'data' is required for non-quantile prediction.") 
    }
    predict(forest, data, predict.all, num.trees, type, se.method, seed, num.threads, verbose, object$inbag.counts, ...)
  }
}
