#' <Add Title>
#'
#' <Add Description>
#'
#' @param features
#' @param featureNames
#' @param plot_cmap
#' @param width
#' @param height
#'
#' @import htmlwidgets
#'
#' @export
SimpleListVisualizer <-
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
      package = 'forceplots',
      elementId = NULL
    )
  }


#' Single additive force plot
#'
#' Create a force plot widget for a single explanation
#'
#' @param baseValue
#' @param outNames
#' @param features
#' @param link
#' @param plot_cmap
#' @param width
#' @param height
#'
#' @import htmlwidgets
#'
#' @export
AdditiveForceVisualizer <-
  function(baseValue,
           outNames,
           features,
           featureNames,
           link = c("identity", "logit"),
           plot_cmap = NULL,
           width = NULL,
           height = NULL) {

  link <- match.arg(link)

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
    package = 'forceplots',
    elementId = NULL
  )
  }

#' Single additive force plot
#'
#' Create a force plot widget for a single explanation
#'
#' @param baseValue
#' @param outNames
#' @param explanations
#' @param featureNames
#' @param link
#' @param plot_cmap
#' @param width
#' @param height
#'
#' @import htmlwidgets
#'
#' @export
AdditiveForceArrayVisualizer <-
  function(baseValue,
           outNames,
           explanations,
           featureNames,
           link = c("identity", "logit"),
           plot_cmap = NULL,
           width = NULL,
           height = NULL) {

    link <- match.arg(link)

    features  <- jsonlite::toJSON(features, auto_unbox = T)

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
      package = 'forceplots',
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

#' Shiny bindings for shapjs
#'
#' Output and render functions for using shapjs within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a shapjs
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name forceplots-shiny
#'
#' @export
forcePlotOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'ForcePlots', width, height, package = 'forceplots')
}

#' @rdname shapjs-shiny
#' @export
renderForcePlot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, forcePlotOutput, env, quoted = TRUE)
}
