 library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  headerPanel("Climate Primer"),
  h3("Park Information",align="left"),
  p("Please either select from the available shape files or upload the desired file.",align="left"),
  fluidRow(
   
    column(3,
      selectInput("Dataset", label = h3("Default Shapefiles"), 
        choices = list("NPS Shape" = "NpsShapes", 
                       "State Boundaries" = "StateBounds",
                       "County Boundaries" = "CountyBounds"), selected = "NpsShapes")),
   
		
    column(3,
      selectInput("Attribute", "Attribute:","Loading...") 
        ),
    column(3,
      selectInput("AttributeValue", label = h3("Select the Attribute Value"), 
        choices = list("NPS Shape" = "NpsShapes", 
                       "State Boundaries" = "StateBounds",
                       "County Boundaries" = "CountyBounds"), selected = "NpsShapes")),                                      
    column(3,
      fileInput("NpsShapes", label = h3("Please point to the shapefile .shp"))),
      
   
   column(3, 
      textInput("ParkName", label = h3("Park Name for Graphics"), 
        value = "Enter text ...")),
   
   column(3,
   radioButtons("Clip", label = h3("Clip to Polygon"),
        choices = list("Yes" = 1, 
                       "No (uses the bounding box)" = 2)
                  )
           )       
                       
  ),
  
  fluidRow(
       
    column(8, 
      sliderInput("Baseline", label = h3("Baseline Years"),
        min = 1895, max = as.numeric(as.character(years(Sys.time()))), value =c(1895,2010))
      )
  ),
  
  
    
  fluidRow(
    column(3,
      radioButtons("PlotUnits", label = h3("Plot Units"),
        choices = list("Metric (C/mm per month)" = "c(\"C\",\"mm\")", 
                       "US units (F/ inches per month)" = "c(\"F\",\"In\")")
                  ) 
    ),  
    column(3, 
      checkboxGroupInput("RCP", 
        label = h3("RCPs for Plotting"), 
        choices = list("RCP 2.6" = 1, 
           "RCP 4.5" = 2,
           "RCP 6.0" = 3,
           "RCP 8.5" = 4
           ),
        selected = 1)
    ),    
    column(3,
      submitButton("Submit"))          
  ),
  mainPanel(
   plotOutput("plotTry")
  )

))