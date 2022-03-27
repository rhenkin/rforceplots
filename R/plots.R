#' ForcePlots for compatible objects
#'
#' Generic method for \code{shapper}, \code{shapr} and \code{fastshap} objects.
#' See [rforceplots::ForcePlot.individual_variable_effect],
#' [rforceplots::ForcePlot.shapr] and [rforceplots::ForcePlot.explain] for
#' specific documentation.
#'
#' @param ... Parameters for the specific functions
#'
#' @return An object of class \code{htmlwidget}
#' @export
#'
ForcePlot <- function(...) {
  UseMethod("ForcePlot")
}

#' Display simple feature effect plot
#'
#' This function uses the SimpleListVisualizer component from the shapjs package
#'
#' @param features A named list of features with \code{effect} and optional
#' \code{name}
#' @param featureNames Optional named list or vector of feature names
#' @param plot_cmap Optional color map to pass to component or list of two valid
#' web color names or hex codes. Valid color maps are: RdBu, GnPR, CyPU, PkYg,
#' DrDb, LpLb, YlDp and OrId.
#' @param width Optional widget width
#' @param height Optional widget height
#'
#' @import htmlwidgets
#'
#' @export
#'
#' @examples
#' feature_1 <- list(name = "A", effect = 0.5)
#' feature_2 <- list(name = "B", effect = -1)
#' features <- list(feature_1, feature_2)
#' SimpleListPlot(features)
SimpleListPlot <-
  function(features,
           featureNames = NULL,
           plot_cmap = NULL,
           width = NULL,
           height = NULL) {

    if (is.null(featureNames)) {
      if (all(sapply(features, function(x) "name" %in% names(x))))
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
      name = "ForcePlots",
      reactR::reactMarkup(component),
      width = width,
      height = height,
      package = "rforceplots",
      elementId = NULL
    )
  }


#' Single additive force plot
#'
#' Create a force plot widget for a single explanation
#'
#' The feature naming is index-based. The \code{featureNames} parameter can be
#' a named list or simply a vector of names in order of appearance in the
#' code{features} list, as in the example below.
#'
#' @param baseValue Baseline for predictions
#' @param features List of features effects and values. This should be a list of
#' lists, where each sublist has \code{effect} and \code{value}. See example.
#' @param featureNames Optional named list or vector of feature names
#' @param outNames Optional target variable name
#' @param link Optional link function to use: identity (default) or logit
#' @param plot_cmap Optional color map to pass to component or list of two valid
#' web color names or hex codes. Valid color maps are: RdBu, GnPR, CyPU, PkYg,
#' DrDb, LpLb, YlDp and OrId.
#' @param width Optional widget width
#' @param height Optional widget height
#'
#' @import htmlwidgets
#'
#' @export
#'
#' @examples
#' feature1 <- list(effect = 0.5, value = 1)
#' feature2 <- list(effect = -0.5, value = 2)
#' features <- list(feature1, feature2)
#' featureNames <- c("Feature 1", "Feature 2")
#' AdditiveForcePlot(0, features, featureNames, plot_cmap = c("gold","red"))
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

  outValue <- baseValue + sum(sapply(features, function(x) x[["effect"]]))

  features  <- jsonlite::toJSON(features, auto_unbox = T)

  component <-
      reactR::component(
        "AdditiveForceVisualizer",
        list(baseValue = baseValue,
             outNames = outNames,
             outValue = outValue,
             link = link,
             features = features,
             featureNames = featureNames,
             plot_cmap = plot_cmap,
             labelMargin = 20)
  )

  # create widget
  htmlwidgets::createWidget(
    name = "ForcePlots",
    reactR::reactMarkup(component),
    width = width,
    height = height,
    package = "rforceplots",
    elementId = NULL
  )
  }

#' Additive force plots for array of explanations
#'
#' Create a force plot widget for multiple explanations
#'
#' @details The \code{explanations} parameter should be a list containing named
#' sublists. Each sublist has the explanation for a sample with \code{outValue}
#' as the predicted model value, \code{simIndex} as the precomputed similarity
#' index and another named list called \code{features}, containing \code{effect}
#'  and \code{value}. See example for details.
#'
#' The feature naming is index-based. The \code{featureNames} parameter can be
#' a named list or simply a vector of names in order of appearance in the
#' code{features} list, as in the example below.
#'
#' @param baseValue Baseline for predictions
#' @param explanations Named list of explanations containing outValue, simIndex
#' and named list of features effects and values.
#' @param featureNames Optional named list or vector of feature names
#' @param outNames Optional target variable name
#' @param link Optional link function to use: identity (default) or logit
#' @param plot_cmap Optional color map to pass to component or list of two valid
#' web color names or hex codes. Valid color maps are: RdBu, GnPR, CyPU, PkYg,
#' DrDb, LpLb, YlDp and OrId.
#' @param width Optional widget width
#' @param height Optional widget height
#'
#' @import htmlwidgets
#'
#' @export
#'
#' @examples
#' featureNames <- c("A", "B")
#' outValue_1 <- 0.5
#' simIndex_1 <- 2
#' features_1 <- list(
#'                  list(effect = 0.5, value = 1),
#'                  list(effect = -0.5, value = 2)
#'                )
#' explanation_1 <-
#'    list(outValue = outValue_1, simIndex = simIndex_1, features = features_1)
#' outValue_2 <- 0.9
#' simIndex_2 <- 1
#' features_2 <- list(
#'                  list(effect = -0.5, value = 0.5),
#'                  list(effect = 0.5, value = 3)
#'                )
#' explanation_2 <-
#'    list(outValue = outValue_2, simIndex = simIndex_2, features = features_2)
#' explanations <- list(explanation_1, explanation_2)
#' AdditiveForceArrayPlot(0, explanations, featureNames)
AdditiveForceArrayPlot <-
  function(baseValue,
           explanations,
           featureNames,
           outNames = "",
           link = c("identity", "logit"),
           plot_cmap = NULL,
           width = NULL,
           height = NULL) {

    link <- match.arg(link)

    if (is.vector(outNames)) outNames <- as.list(outNames)

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
      name = "ForcePlots",
      reactR::reactMarkup(component),
      width = width,
      height = height,
      package = "rforceplots",
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
#' @name rforceplots-shiny
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#' library(shiny)
#' library(rforceplots)
#'
#' ui <- fluidPage(
#'   titlePanel("reactR HTMLWidget Example"),
#'   forcePlotOutput('widgetOutput')
#' )
#'
#' server <- function(input, output, session) {
#'   output$widgetOutput <- renderForcePlot(
#'     AdditiveForcePlot(
#'       baseValue = 0.0,
#'       outNames = c("color rating"),
#'       features = list(
#'         list(value = 1.0, effect = 1.0),
#'         list(value = 0, effect = -0.5)
#'       ),
#'       featureNames = list("ABC", "DEF")
#'     )
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#' }
forcePlotOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "ForcePlots", width, height, package = "rforceplots")
}

#' @rdname rforceplots-shiny
#' @export
renderForcePlot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr) # force quoted
  htmlwidgets::shinyRenderWidget(expr, forcePlotOutput, env, quoted = TRUE)
}
