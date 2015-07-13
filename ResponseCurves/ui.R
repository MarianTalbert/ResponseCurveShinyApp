shinyUI(fluidPage(
titlePanel("Response curve explorer"),
fluidRow(
  column(4,
  wellPanel(
  plotOutput("map", click = "plot_click"))),
  column(6,
  wellPanel(plotOutput("curves")) )
  )

))