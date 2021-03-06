% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{rforceplots-shiny}
\alias{rforceplots-shiny}
\alias{forcePlotOutput}
\alias{renderForcePlot}
\title{Shiny bindings for force plots}
\usage{
forcePlotOutput(outputId, width = "100\%", height = "400px")

renderForcePlot(expr, env = parent.frame(), quoted = FALSE)
}
\arguments{
\item{outputId}{output variable to read from}

\item{width, height}{Must be a valid CSS unit (like \code{'100\%'},
\code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
string and have \code{'px'} appended.}

\item{expr}{An expression that generates a force plot}

\item{env}{The environment in which to evaluate \code{expr}.}

\item{quoted}{Is \code{expr} a quoted expression (with \code{quote()})? This
is useful if you want to save an expression in a variable.}
}
\description{
Output and render functions for using shapjs within Shiny
applications and interactive Rmd documents.
}
\examples{
\donttest{
if (interactive()) {
library(shiny)
library(rforceplots)

ui <- fluidPage(
  titlePanel("reactR HTMLWidget Example"),
  forcePlotOutput('widgetOutput')
)

server <- function(input, output, session) {
  output$widgetOutput <- renderForcePlot(
    AdditiveForcePlot(
      baseValue = 0.0,
      outNames = c("color rating"),
      features = list(
        list(value = 1.0, effect = 1.0),
        list(value = 0, effect = -0.5)
      ),
      featureNames = list("ABC", "DEF")
    )
  )
}

shinyApp(ui, server)
}
}
}
