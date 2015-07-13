shinyUI(basicPage(
  plotOutput("map", click = "plot_click"),
  plotOutput("curves")
))