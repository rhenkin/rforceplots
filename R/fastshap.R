#' Additive force plots for fastshap explanation objects
#'
#' @param shaps An \code{explain} object created with [fastshap::explain()]
#' @param features Data used for model training or new data that was explained
#' @param baseValue Optional baseline for predictions (e.g. mean of all
#' predictions)
#' @param ... Additional parameters for [rforceplots::AdditiveForcePlot()] or
#'  [rforceplots::AdditiveForceArrayPlot()]
#' @export
#' @return An object of class \code{htmlwidget}
#' @method ForcePlot explain
#'
#' @examples
#' if (interactive() & require("fastshap")) {
#'    data(mtcars)
#'    mtcars.ppr <- ppr(mpg ~ ., data = mtcars, terms = 1)
#'    # Compute approximate Shapley values using 10 Monte Carlo simulations
#'    set.seed(101)  # for reproducibility
#'    shap <- explain(fit, X = subset(mtcars, select = -mpg), nsim = 10,
#'                pred_wrapper = predict)
#'    # Visualize first explanation
#'    preds <- predict(mtcars.ppr, newdata = mtcars)
#'    ForcePlot(shap[1,], mtcars[1,], mean(preds))
#' }
ForcePlot.explain <-
  function(shaps, features, baseValue = NULL, ...) {

  if (is.null(baseValue)) {
    baseline <- attr(shaps, "baseline")
    if (is.null(baseline)) {
      stop(paste("baseline not found in `shaps` object. Run `explain` with",
                 "`exact = TRUE` or provide baseValue (e.g. mean(preds))."))
    } else {
      baseValue <- baseline[[1]]
    }
  }

  featureNames <- colnames(shaps)

  if ((nrow(shaps)) == 1) {
    # Single additive force plot
    if (!is.vector(features)) {
      if (nrow(features) > 1) {
        stop("For force plots for one observation",
              "`features` must be a vector or contain one row.")
      }
    }
    if (is.data.frame(features)) features <- unlist(features)

    # shaps should be a tibble so need to use drop = TRUE to get single value
    features_forceplot <-
      lapply(featureNames, function(x)
        list(value = features[[x]], effect = shaps[1, x, drop = TRUE]))

    AdditiveForcePlot(baseValue, features_forceplot, featureNames, ...)

  } else {
    # Array additive force plot
    if (is.vector(features) & (nrow(shaps) > 1)) {
      stop("Incompatible dimensions for `shaps` and `features`")
    }
    if (nrow(shaps) != nrow(features)) {
      stop("Number of rows in shaps and features are different.",
           "Use the original values used for fitting the model.")
    }
    # Compute similarity index
    dmat <- stats::dist(as.matrix(shaps))
    order <- seriation::get_order(seriation::seriate(dmat, method = "OLO"))
    sim_index <- match(seq_len(nrow(shaps)), order)

    # Iterate through samples to compute explanations
    explanations <- lapply(seq_len(nrow(shaps)), function(row_index) {
      values <- features[row_index, ]
      effects <- shaps[row_index, ]
      features <-
        lapply(featureNames,
               function(x) list(value = values[[x]], effect = effects[[x]]))
      names(features) <- seq_along(featureNames)
      list(outValue = baseValue + sum(effects),
           simIndex = sim_index[[row_index]],
           features = features
      )
    })

    AdditiveForceArrayPlot(baseValue, explanations, featureNames, ...)

  }
}
