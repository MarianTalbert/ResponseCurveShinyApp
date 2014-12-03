 library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Climate Primer"),
  h3("Park Information"),
  
  fluidRow( 
    column(3,
      fileInput("file", label = h3("Please point to the shapefile .shp"))),
    
   column(3, 
      textInput("text", label = h3("Park Name for Graphics"), 
        value = "Enter text ...")),
   
   column(3,
   radioButtons("radio2", label = h3("Clip to Polygon"),
        choices = list("Yes" = 1, 
                       "No (uses the bounding box)" = 2)
                  )
           )       
                       
  ),
  
  fluidRow(
       
    column(8, 
      sliderInput("slider1", label = h3("Baseline Years"),
        min = 1895, max = as.numeric(as.character(years(Sys.time()))), value =c(1895,2010))
      )
  ),
  
  
    
  fluidRow(
    column(3,
      radioButtons("radio", label = h3("Plot Units"),
        choices = list("Metric (C/mm per month)" = 1, 
                       "US units (F/ inches per month)" = 2)
                  ) 
    ),  
    column(3, 
      checkboxGroupInput("checkGroup", 
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
  )

))