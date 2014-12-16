 library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Climate Primer"),
    #========= Main Panel================#
    mainPanel(
        img(src="NCCSClogo.jpg",height=250,width=250),
        img(src="Temp_1950_to_2100_EmissionsSD.png",height=650,width=650)
        #plotOutput("plotTry",width=600,height=600)
    ),
    
    #========== Sidebar Options =========#
    sidebarPanel(position="right",
        h1("Park Information"),
        helpText("Please either select from the available", 
              "shapefiles or upload the desired file."),
        
        selectInput("Dataset", choices=names(ShapeList),label=h4("Available Shapefiles")),
        
        fileInput("InputFile", label = h4("Please point to the .zip containing the shapefile")),
        
        selectInput("Attribute", label=h4("Select Attribute"),"Loading..."), 
         
        selectInput("AttributeValue", label = h4("Select the Attribute Value"), 
          "Loading..."),                                      
     
        textInput("ParkName", label = h4("Park Name for Graphics"), 
          value = "Enter text ..."),
  
        radioButtons("Clip", label = h4("Clip to Polygon"),
          choices = list("Yes" = 1, 
                         "No (uses the bounding box)" = 2)),
  
        h1("Plot Information"),
        sliderInput("Baseline", label = h4("Baseline Years"),
          min = 1895, max = as.numeric(as.character(format(Sys.time(),"%Y"))), value =c(1895,2010),format="#",width="100%"),
			
        radioButtons("PlotUnits", label = h4("Plot Units"),
          choices = list("Metric (C/mm per month)" = "c(\"C\",\"mm\")", 
                         "US units (F/ inches per month)" = "c(\"F\",\"In\")")
                    ), 
                    
        checkboxGroupInput("RCP", 
          label = h3("RCPs for Plotting"), 
          choices = list("RCP 2.6" = 1, 
             "RCP 4.5" = 2,
             "RCP 6.0" = 3,
             "RCP 8.5" = 4
             ),
          selected = 1),
     
        actionButton("action", label = "Create Graphics")
    
    )

))