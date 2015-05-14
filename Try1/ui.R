 library(shiny)
LatLst<-as.list(seq(from=29,to=50))
LonLst<-as.list(seq(from=-125,to=-67))
names(LatLst)<-seq(from=29,to=50) 
names(LonLst)<-seq(from=-125,to=-67) 
# Define UI for application that draws a histogram
shinyUI(navbarPage("Climate Primer",
#===============================================
# ==========  Study Area Specification Tab ==========#
 tabPanel("Specify Study Area",

        #========= Main Panel================#
    mainPanel(
       wellPanel(              
           h2("Specify a bounding box for the study"),       
          
           div(class="row",
            div(class="span1",h5("Latitude")),
            div(class="span1", selectInput("LatStart",choices=LatLst, 
              selected = 29,label="")),
            div(class="span2", selectInput("LatSDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
              selected = 1,label="")),
            div(class="span.5",h5("to")),
             div(class="span1", selectInput("LatEnd",choices=LatLst, 
              selected = 50,label="")),
            div(class="span2", selectInput("LaEtDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
              selected = 1,label=""))  
          ),
          
         
           div(class="row",
           div(class="span1",h5("Longitude")),
            div(class="span1", selectInput("LonStart",choices=LonLst, 
              selected =-125,label="")),
            div(class="span2", selectInput("LonSDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
              selected = 1,label="")),
            div(class="span.5",h5("to")),
             div(class="span1", selectInput("LonEnd",choices=LonLst, 
              selected = -67,label="")),
            div(class="span2", selectInput("LoEtDec",choices=list(".0625" = .0625, ".1875" = .1875, ".3125" = .3125), 
              selected = 1,label=""))  
          )
        ),
      
        wellPanel(
                h2("or upload a shapefile"),
                helpText("Please either select from the available", 
                      "shapefiles, upload the desired file or specify a bounding box."),
                 div(class="row",
                    div(class="span3", 
                    selectInput("Dataset", choices=names(ShapeList),label=h4("Available Shapefiles"))),
                
                    div(class="span3",
                    fileInput("InputFile", label = h5("Please point to the .zip containing the shapefile")))),
                
                 div(class="row",
                    div(class="span3", 
                selectInput("Attribute", label=h5("Select Attribute"),"Loading...")), 
                     div(class="span3",
                selectInput("AttributeValue", label = h5("Select the Attribute Value"), 
                  "Loading...")))
                #img(src="Temp_1950_to_2100_EmissionsSD.png",height=650,width=650),
        ),       
       
        actionButton("DisplayShape", label = "Display study area on map"),  
        leafletMap("map", 700, 550, initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            initialTileLayerAttribution = 
            HTML('&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'),
            options = list(center = c(37.45, -93.85),
              zoom = 4, maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)))),
         img(src="NCCSClogo.jpg",height=250,width=250)      
              
    )
   
        
    ),
    
   #========================================
   #  Projected Trends  
 tabPanel("Projected Trends",
     sidebarPanel(
                                                
         h2("Settings that Apply for all Graphics"),
            textInput("ParkName", label = h4("Study area name for graphics"), 
              value = "Enter text ..."),
			
            radioButtons("PlotUnits", label = h4("Plot Units"),
              choices = list("US units (F/ inches per month)" = "c(\"F\",\"In\")",
                              "Metric (C/mm per month)" = "c(\"C\",\"mm\")")
                        ),
              radioButtons("Var", 
                    label = h3("Variable"), 
                    choices = list("Max Temp" = 1, 
                 "Min Temp" = 2,
                 "Avg Temp" = 3,
                 "Precip" = 4
                  ),
              selected = 1),
              
              checkboxGroupInput("RibbonRCP", 
                  label = h4("RCPs for Plotting"), 
                  choices = list("RCP 2.6" = "RCP 2.6", 
                     "RCP 4.5" = "RCP 4.5",
                     "RCP 6.0" = "RCP 6.0",
                     "RCP 8.5" = "RCP 8.5"
                     ),
                  selected = c("RCP 2.6","RCP 4.5","RCP 6.0","RCP 8.5")),
                
               radioButtons("ObsRibbon", 
                        label = h4("Add Observational Data"), 
                        choices = list("Maurer" = "Maurer", 
                           "PRISM" = "Prism",
                           "TopoWx"="TopoWx"
                           ),
                        selected = "Maurer")
        
        ),
      mainPanel(
        wellPanel(              
              h2("Future Projection Ribbon Plots"),
                radioButtons("RibbonOrLine", 
                        label = h4(""), 
                        choices = list("Ribbon" = "Ribbon", 
                           "Line" = "Line",
                           "Seasonal Boxplot"="SeasonalBox"
                           ),
                        selected = "Ribbon"),    
                plotOutput("Emissions")
               
                 
        )
      )
    ),
#====================================================
#======= Historic Trends    
tabPanel("Historic Trends",
    h2("Historic Trend Plots"),
    div(class="row",
                    div(class="span5",              
              checkboxInput("Trend", label = h6("Add Linear Trend"),
                               value = TRUE),
              checkboxInput("MovAvg", label = h6("Add Moving Average"),
                               value = TRUE),
               radioButtons("ObsHist", 
                        label = h4("Observational Data"), 
                        choices = list("Maurer" = "Maurer", 
                           "PRISM" = "Prism"
                           ),
                        selected = "Maurer")),                                  
              div(class="span8",plotOutput("HistoricTrends"))   
           ),
       h2("Anomaly Plots"),
       div(class="row",    
          div(class="span5",       
          
          sliderInput("Baseline", label = h4("Baseline Years"),
                  min = 1895, max = 2010, value =c(1895,1980),format="#",width="100%")),
          div(class="span8",plotOutput("AnomalyPlot"))   
           ), 
      h2("Monthly Anomaly Plots"),
       div(class="row",    
          div(class="span5",       
          
          sliderInput("Baseline", label = h4("Baseline Years"),
                  min = 1895, max = 2010, value =c(1895,1980),format="#",width="100%")),
          div(class="span8",plotOutput("ImagePlot"))   
           )       
            
),    
#===============================================
# ==========  Mapped Output Tab ==========#    
tabPanel("Generate Maps",
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