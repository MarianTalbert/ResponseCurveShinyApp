 library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Climate Primer",

 tabPanel("Data Specifications",

        #========= Main Panel================#
        mainPanel(
           
            #img(src="Temp_1950_to_2100_EmissionsSD.png",height=650,width=650),
            
            leafletMap("map", 700, 550, initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                    initialTileLayerAttribution = HTML('&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'),
                    options = list(center = c(37.45, -93.85),
          zoom = 4,
          maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)))),
            
             actionButton("DisplayShape", label = "Display Shape on Map"),
           
           img(src="NCCSClogo.jpg",height=250,width=250)      
            #plotOutput("plotTry",width=600,height=600)
      ),
   
       
        #========== Sidebar Options =========#
        sidebarPanel(position="right",
            h1("Study Area Information"),
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
    ),
    tabPanel("Mapped Output",
    	mainPanel(
                 h2("Available Color Scales for Mapped Output"),
                img(src="MapDefaultColors.png",height="70%",width="100%")
            ),
      sidebarPanel(position="right",
            h1("Map Specification"),
            #helpText("Please either select from the available", 
             #     "shapefiles or upload the desired file.")
            selectInput("MappedData", label = h3("Dataset to use"), 
                choices = list("PRISM" = 1, "Maurer" = 2,
                       "1/8th degree BSCD projections" = 3,"NEX"=4), selected = 1),
             selectInput("MappedVar", label = h4("Variable"), 
                choices = list("Avg. Monthly Temperature" = 1, "Minimum Monthly Temperature" = 2,
                       "Maximum Monthly Temperature" = 3,"Precipitation"=4), selected = 1),
              selectInput("MappedSubset", label = h4("Seasonal Subset"), 
                choices = list("FullYear"=1,"Jan" = 2, "Feb" = 3,
                       "March" = 4,"April"=5), selected = 1),
              checkboxGroupInput("MapRCP", 
                  label = h3("RCPs for Plotting"), 
                  choices = list("RCP 2.6" = 1, 
                     "RCP 4.5" = 2,
                     "RCP 6.0" = 3,
                     "RCP 8.5" = 4
                     ),
                  selected = 1),
              sliderInput("BaselineTime", label = h4("Baseline Years"),
              min = 1895, max = as.numeric(as.character(format(Sys.time(),"%Y"))), value =c(1895,2010),format="#",width="100%"),         
              
              sliderInput("FutureTime1", label = h4("Future Period 1"),
              min = 2015, max = 2100, value =c(2030,2060),format="#",width="100%"),
              
              sliderInput("FutureTime2", label = h4("Future Period 2"),
              min = 2015, max = 2100, value =c(2070,2100),format="#",width="100%")         
                       
                                                  
             )     
                        
         ),  
  tabPanel("About",
    	mainPanel(
                #img(src="NCCSClogo.jpg",height=250,width=250),
                includeHTML("include.html"),
                 img(src="NCCSClogo.jpg",height=250,width=250)  
            )
         )   
))