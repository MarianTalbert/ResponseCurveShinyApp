shinyUI(fluidPage(
titlePanel("Response curve explorer"),
# Insert the right number of plot output objects into the web page
fluidRow(
  column(4,
  wellPanel(
  plotOutput("map1", click = "plot_click"))),
  column(6,
  wellPanel(plotOutput("curves1")) )
  ),
conditionalPanel(length(modelLst)>1,
 fluidRow(
  column(4,
  wellPanel(
  plotOutput("map2", click = "plot_click"))),
  column(6,
  wellPanel(plotOutput("curves2")) )
  )
 ),
 conditionalPanel(length(modelLst)>2,
 fluidRow(
  column(4,
  wellPanel(
  plotOutput("map3", click = "plot_click"))),
  column(6,
  wellPanel(plotOutput("curves3")) )
  )
 ),
 conditionalPanel(length(modelLst)>3,
 fluidRow(
  column(4,
  wellPanel(
  plotOutput("map4", click = "plot_click"))),
  column(6,
  wellPanel(plotOutput("curves4")) )
  )
 )   
))