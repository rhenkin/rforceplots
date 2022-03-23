#' Display additive force plots for shapr explanation objecs
#'
#' @param explanation A \code{shapr} object, created with the [shapr::explain()]
#' method
#' @param i Index for single object explanation
#' @param ... Additional parameters to [rforceplots::AdditiveForcePlot()] or
#'  [rforceplots::AdditiveForceArrayPlot()]
#'
#' @return An object of class \code{htmlwidget}
#' @export
#'
ForcePlot.shapr <- function(explanation, i = NULL, ...) {
  shaps <- explanation$dt
  data <- explanation$x_test
  outValues <- explanation$p

  baseValue <- shaps[1, "none"]
  featureNames <- as.list(colnames(data))
  names(featureNames) <- seq_along(featureNames)

  # Compute similarity index
  dmat <- dist(shaps)
  order <- seriation::get_order(seriation::seriate(dmat, method = "OLO"))
  sim_index <- match(seq_len(nrow(shaps)), order)

  if (is.null(i)) {
    # Iterate through samples to compute explanations
    explanations <- lapply(seq_len(nrow(shaps)), function(row_index) {
      values <- data[row_index,]
      effects <- shaps[row_index,]
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
    values <- data[i,]
    effects <- shaps[i,]
    features <-
      lapply(featureNames,
             function(x) list(value = values[[x]], effect = effects[[x]]))
    AdditiveForcePlot(baseValue, features, featureNames, ...)
  }
}
