#' Additive force plots for shapr explanation objects
#'
#' @param explanation A \code{shapr} object created with [shapr::explain()]
#' @param i Index for single object explanation
#' @param ... Additional parameters for [rforceplots::AdditiveForcePlot()] or
#'  [rforceplots::AdditiveForceArrayPlot()]
#'
#' @return An object of class \code{htmlwidget}
#' @method ForcePlot shapr
#' @export
#'
#' @examples
#' if (require("shapr")) {
#'  data(mtcars)
#'  x_train <- mtcars[1:5,]
#'  x_test <- mtcars[6:10, ]
#'  model <- lm(mpg ~ disp + wt + drat, data = x_train)
#'  explainer <- shapr(x_train, model)
#'  p <- mean(x_train$mpg)
#'  explanation <- explain(x_test, explainer, approach = "empirical",
#'  prediction_zero = p, n_samples = 1e2)
#'  ForcePlot(explanation)
#' }
ForcePlot.shapr <- function(explanation, i = NULL, ...) {
  shaps <- explanation$dt
  data <- explanation$x_test
  outValues <- explanation$p

  baseValue <- shaps[1, "none"]
  featureNames <- as.list(colnames(data))
  names(featureNames) <- seq_along(featureNames)

  if (is.null(i)) {
    # Compute similarity index
    dmat <- stats::dist(shaps)
    order <- seriation::get_order(seriation::seriate(dmat, method = "OLO"))
    sim_index <- match(seq_len(nrow(shaps)), order)

    # Iterate through samples to compute explanations
    explanations <- lapply(seq_len(nrow(shaps)), function(row_index) {
      values <- data[row_index, ]
      effects <- shaps[row_index, ]
      features <-
        lapply(featureNames,
               function(x) list(value = values[[x]], effect = effects[[x]]))
      names(features) <- seq_along(featureNames)
      list(outValue = outValues[[row_index]],
           simIndex = sim_index[[row_index]],
           features = features
      )
    })
    AdditiveForceArrayPlot(baseValue, explanations, featureNames, ...)
  } else {
    # If i is defined, display only a single force plot
    if (i > nrow(data)) stop("Invalid sample index in original data")
    if (i > nrow(shaps)) stop("Invalid sample index in explanations")
    values <- data[i, ]
    effects <- shaps[i, ]
    features <-
      lapply(featureNames,
             function(x) list(value = values[[x]], effect = effects[[x]]))
    AdditiveForcePlot(baseValue, features, featureNames, ...)
  }
}
