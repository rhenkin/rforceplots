#' Display simple feature plot
#'
#' This function uses the SimpleListVisualizer component from the shapjs package
#'
#' @param features A named list of features with effects
#' @param featureNames Optional named list matching indices with names
#' @param plot_cmap Optional color map to pass to component
#' @param width Optional widget width
#' @param height Optional widget height
#'
#' @import htmlwidgets
#'
#' @export
SimpleListPlot <-
  function(features,
           featureNames = NULL,
           plot_cmap = NULL,
           width = NULL,
           height = NULL) {

    if (is.null(featureNames)) {
      #featureNames <- as.list(names(features))
      featureNames <- sapply(features, function(x) x$name)
    }

    features  <- jsonlite::toJSON(features, auto_unbox = T)

    component <-
      reactR::component(
        "SimpleListVisualizer",
        list(features = features,
             featureNames = featureNames,
             plot_cmap = plot_cmap)
      )

    # create widget
    htmlwidgets::createWidget(
      name = 'ForcePlots',
      reactR::reactMarkup(component),
      width = width,
      height = height,
      package = 'rforceplots',
      elementId = NULL
    )
  }


#' Single additive force plot
#'
#' Create a force plot widget for a single explanation
#'
#' @param baseValue Baseline for predictions
#' @param features List of features effects and values. This should be a list of
#' lists where each sublist has names effect and value. See example.
#' @param featureNames Optional named list of feature names
#' @param outNames Optional list of the target variable names
#' @param link Optional link function to use (identity is default)
#' @param plot_cmap Optional color map to pass to component
#' @param width Optional widget width
#' @param height Optional optional widget height
#'
#' @import htmlwidgets
#'
#' @export
AdditiveForcePlot <-
  function(baseValue,
           features,
           featureNames = NULL,
           outNames = NULL,
           link = c("identity", "logit"),
           plot_cmap = NULL,
           width = NULL,
           height = NULL) {

  link <- match.arg(link)

  if (is.vector(outNames)) outNames <- as.list(outNames)

  if (is.null(featureNames))
      featureNames <- seq_len(length(features))

  features  <- jsonlite::toJSON(features, auto_unbox = T)

  component <-
      reactR::component(
        "AdditiveForceVisualizer",
        list(baseValue = baseValue,
             outNames = outNames,
             link = link,
             features = features,
             featureNames = featureNames,
             plot_cmap = plot_cmap)
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'ForcePlots',
    reactR::reactMarkup(component),
    width = width,
    height = height,
    package = 'rforceplots',
    elementId = NULL
  )
  }

#' Additive force plots for array of explanations
#'
#' Create a force plot widget for multiple explanations
#'
#' @param baseValue Baseline for predictions
#' @param explanations List of explanations containing outValue (predicted
#' value), simIndex (similarity index) and features list of effects and values.
#' @param featureNames Optional named list of feature names
#' @param outNames Optional list of the target variable names
#' @param link Optional link function to use (identity is default)
#' @param plot_cmap Optional color map to pass to component
#' @param width Optional widget width
#' @param height Optional optional widget height
#'
#' @import htmlwidgets
#'
#' @export
AdditiveForceArrayPlot<-
  function(baseValue,
           explanations,
           featureNames,
           outNames = "",
           link = c("identity", "logit"),
           plot_cmap = NULL,
           width = NULL,
           height = NULL) {

    link <- match.arg(link)

    explanations  <- jsonlite::toJSON(explanations, auto_unbox = T)

    component <-
      reactR::component(
        "AdditiveForceArrayVisualizer",
        list(baseValue = baseValue,
             outNames = outNames,
             link = link,
             explanations = explanations,
             featureNames = featureNames,
             plot_cmap = plot_cmap)
      )

    # create widget
    htmlwidgets::createWidget(
      name = 'ForcePlots',
      reactR::reactMarkup(component),
      width = width,
      height = height,
      package = 'rforceplots',
      elementId = NULL
    )
  }

#' Called by HTMLWidgets to produce the widget's root element.
#' @noRd
widget_html.ForcePlots <- function(id, style, class, ...) {
  htmltools::tagList(
    # Necessary for RStudio viewer version < 1.2
    reactR::html_dependency_corejs(),
    reactR::html_dependency_react(),
    reactR::html_dependency_reacttools(),
    htmltools::tags$div(id = id, class = class, style = style)
  )
}

#' Shiny bindings for force plots
#'
#' Output and render functions for using shapjs within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a force plot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name forceplots-shiny
#'
#' @export
forcePlotOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'ForcePlots', width, height, package = 'rforceplots')
}

#' @rdname forceplots-shiny
#' @export
renderForcePlot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, forcePlotOutput, env, quoted = TRUE)
}
