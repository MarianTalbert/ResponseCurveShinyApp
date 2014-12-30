 library(shiny)
LatLst<-as.list(seq(from=29,to=50))
LonLst<-as.list(seq(from=-125,to=-67))
names(LatLst)<-seq(from=29,to=50) 
names(LonLst)<-seq(from=-125,to=-67) 
# Define UI for application that draws a histogram
shinyUI(navbarPage("Climate Primer",
#===============================================
# ==========  Data Specification Tab ==========#
 tabPanel("Data Specifications",

        #========= Main Panel================#
        mainPanel(
 h4("Latitude"),
 div(class="row",
  div(class="span1", selectInput("LatStart",choices=LatLst, 
    selected = 29,label="")),
  div(class="span2", selectInput("LatSDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
    selected = 1,label="")),
  div(class="span.5",h4("to")),
   div(class="span1", selectInput("LatEnd",choices=LatLst, 
    selected = 50,label="")),
  div(class="span2", selectInput("LaEtDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
    selected = 1,label=""))  
),

h4("Longitude"),
 div(class="row",
  div(class="span1", selectInput("LonStart",choices=LonLst, 
    selected =-125,label="")),
  div(class="span2", selectInput("LonSDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
    selected = 1,label="")),
  div(class="span.5",h4("to")),
   div(class="span1", selectInput("LonEnd",choices=LonLst, 
    selected = -67,label="")),
  div(class="span2", selectInput("LoEtDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
    selected = 1,label=""))  
),
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
                  "shapefiles, upload the desired file or specify a bounding box."),
 
            selectInput("Dataset", choices=names(ShapeList),label=h4("Available Shapefiles")),
            
            fileInput("InputFile", label = h4("Please point to the .zip containing the shapefile")),
            
            selectInput("Attribute", label=h5("Select Attribute"),"Loading..."), 
             
            selectInput("AttributeValue", label = h5("Select the Attribute Value"), 
              "Loading..."),                                      
         
            textInput("ParkName", label = h3("Park Name for Graphics"), 
              value = "Enter text ..."),
      
            radioButtons("Clip", label = h3("Clip to Polygon"),
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
#===============================================
# ==========  Mapped Output Tab ==========#    
    tabPanel("Mapped Output",
    	mainPanel(
                 h2("Available Color Scales for Mapped Output"),
                img(src="MapDefaultColors.png",height="70%",width="80%"),
                 img(src="NCCSClogo.jpg",height=250,width=250) 
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
                  label = h4("RCPs for Plotting"), 
                  choices = list("RCP 2.6" = 1, 
                     "RCP 4.5" = 2,
                     "RCP 6.0" = 3,
                     "RCP 8.5" = 4
                     ),
                  selected = 1),
              sliderInput("BaselineTime", label = h4("Baseline Years"),
              min = 1895, max = as.numeric(as.character(format(Sys.time(),"%Y"))), value =c(1895,2010),format="#",width="100%"),         
              
              sliderInput("FutureTime1", label = h4("Future Period 1"),
              min = 2015, max = 2100, value =c(2030,2060),format="#",width="50%"),
              
              sliderInput("FutureTime2", label = h4("Future Period 2"),
              min = 2015, max = 2100, value =c(2070,2100),format="#",width="50%"),
              
              selectInput("ColorScale", label = h4("Color Scale"), 
                choices = list("Default"=0,"yellow to red" = 1, "teal to blue" = 2,
                       "red to blue" = 3,"brown to blue"=4,"green to red"=4,"brown to purple"=4), selected = 0),         
                       
              checkboxGroupInput("AddBoundaries", 
                  label = h4("Add boundaries"), 
                  choices = list("Shape Boundary" = 1, 
                     "State Boundary" = 2
                     ),
                  selected = 1) 
                                                      
             )     
                        
         ),
#===============================================
# ==========  About Tab ==========#           
  tabPanel("About",
    	mainPanel(
                #img(src="NCCSClogo.jpg",height=250,width=250),
                includeHTML("include.html"),
                 img(src="NCCSClogo.jpg",height=250,width=250)  
            )
         )   
))