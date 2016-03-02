shinyUI(fluidPage(
  tags$style(type="text/css", "#example.recalculating { opacity: 1.0; }"),
  tags$style(type="text/css", "#helpText{color: red; }"),
  withMathJax(),

  column(8, offset = 2,

    h3("Example of AdaBoost using Shiny"),

    actionButton("run", label = "Run"),
    actionButton("stop", label = "Stop"),
    actionButton("reset", label = "Reset"),

    fluidRow(
      plotOutput('example')
    )
  )
))

