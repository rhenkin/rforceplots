library(shiny)
library(forceplots)

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
