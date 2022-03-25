#' Additive force plots for individual_variable_effect objects
#'
#' @param ive An \code{individual_variable_effect} object created with
#'  [shapper::individual_variable_effect()]
#' @param baseValue Base value for prediction (e.g. mean of all predictions)
#' @param id Optional object identifier if the \code{ive} object contains
#' multiple ids
#' @param outName Output name. Optional for plotting but required for multiclass
#' models (values in \code{_ylevel_} variable)
#' @param ... Additional parameters for [rforceplots::AdditiveForcePlot()] or
#'  [rforceplots::AdditiveForceArrayPlot()]
#'
#' @return An object of class \code{htmlwidget}
#'
#' @method ForcePlot individual_variable_effect
#' @export
#'
ForcePlot.individual_variable_effect <-
  function(ive, baseValue, id = NULL, outName = NULL, ...) {

  featureNames <- unique(ive$`_vname_`)

  # Retrieve training data from the ive object
  sample_data <- ive[, featureNames, drop = TRUE]
  data <- sample_data[!duplicated(sample_data), ]

  # Extract shap related values
  shap_l <-
    ive[,
        c("_id_", "_ylevel_", "_yhat_", "_ymean_", "_vname_", "_attribution_")]
  # Reshape data frame and rename columns to use featureNames
  shap_w <- stats::setNames(
    stats::reshape(
      shap_l,
      direction = "wide",
      idvar = c("_id_", "_ylevel_", "_yhat_", "_ymean_"),
      timevar = "_vname_"
    ),
    c("_id_", "_ylevel_", "_yhat_", "_ymean_", featureNames)
  )

  # outName is required if the ive object is multiclass
  # multiclass is defined by the _ylevel_ column not being empty
  outName_req <- FALSE
  if (all(shap_w[["_ylevel_"]] != "")) {
    if (is.null(outName)) {
      stop("The `outName` argument is required for multiclass predictions")
    } else {
      if (!outName %in% unique(shap_w[["_ylevel_"]])) {
        stop("`outName` not found in _ylevel_")
      }
    }
    outName_req <- TRUE
  }
  number_of_ids <- length(unique(ive[["_id_"]]))
  if ((number_of_ids == 1) | (!is.null(id))) {
    # Single additive force plot
    if (!is.null(id)) {
      if (!id %in% ive[["_id_"]]) {
        stop("Selected `id` not found")
      }
    } else {
      id <- unique(ive[["_id_"]])
    }
    # outName was already validated, so use to extract single class prediction
    # if provided
    if (outName_req) {
      shaps <- shap_w[
        with(shap_w, `_id_` == id & `_ylevel_` == outName), , drop = TRUE]
    } else {
      shaps <-
        shap_w[with(shap_w, `_id_` == id), , drop = TRUE]
    }

    baseValue <- shaps["_ymean_"]

    features <- lapply(featureNames, function(featName) {
      list(effect = shaps[[featName]],
           value = data[id, featName])
    })

    AdditiveForcePlot(
      baseValue = baseValue,
      features = features,
      featureNames = featureNames,
      outName = outName,
      ...
    )

  }
  else {
    # Array additive force plot
    if (!is.null(outName)) {
      sample_shap <-
        shap_w[with(shap_w, `_ylevel_` == outName), , drop = TRUE]
    } else sample_shap <- shap_w
    sample_shap <- sample_shap[!duplicated(sample_shap), ]
    shaps <- sample_shap[, featureNames]

    #Get predicted values
    outValues <- shap_w[["_yhat_"]]

    # Compute shap values similarity
    dmat <- dist(shaps)
    order <- seriation::get_order(seriation::seriate(dmat, method = "OLO"))
    sim_index <- match(seq_len(nrow(shaps)), order)

    # Create list of explanations for each sample
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

    AdditiveForceArrayPlot(baseValue, explanations, featureNames, outName, ...)

  }

}
