## TODO: Fix reactive subsetting -- using dropdown bar should filter by group 
# questions? email andrea.julca@gmail.com
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# breakdown by county -- you have lat, lon, county_ori
# year captured: frequency by species by county over time
# Subset shapefile to counties you want, then just use that subset -- sf + rgdal libs 

library(shiny)
library(readxl)
library(data.table)
library(sf)
library(rgdal)
library(leaflet)
library(dplyr)


dt  <- read_excel("data/MasterDataSheet.xlsx")

tab <- as.data.table(dt)
tab[,Max_Temp := as.numeric(Max_Temp)]
tab[,Min_Temp := as.numeric(Min_Temp)]
tab[,YearEndCDa:=as.numeric(YearEndCDa)]


yrs  <- tab[, unique(YearEndCDa)]
grps <- tab[, unique(group_)]
spcs <- tab[, unique(gen_sp)]

grp1 <- as.data.frame(tab[,
            .(
              count = .N, 
              Latitude = mean(Latitude, na.rm = T), 
              Longitude = mean(Longitude, na.rm = T), 
              maxT = mean(Max_Temp, na.rm = T), 
              minT = mean(Min_Temp, na.rm = T)
            ), 
            by = list(
              group_, 
              county_ori,
              YearEndCDa
            )]
)

grp2 <- as.data.frame(tab[,
            .(
              count = .N, 
              Latitude = mean(Latitude, na.rm = T), 
              Longitude = mean(Longitude, na.rm = T), 
              maxT = mean(Max_Temp, na.rm = T), 
              minT = mean(Min_Temp, na.rm = T)
            ), 
            by = list(
              gen_sp, 
              county_ori,
              YearEndCDa
            )]
)

grp3 <- as.data.frame(tab[,
                          .(
                            count = .N, 
                            Latitude = mean(Latitude, na.rm = T), 
                            Longitude = mean(Longitude, na.rm = T), 
                            maxT = mean(Max_Temp, na.rm = T), 
                            minT = mean(Min_Temp, na.rm = T)
                          ), 
                          by = list(
                            YearEndCDa,
                            gen_sp 
                            )]
)

shp <- "data/cb_2016_us_county_5m"
counties <- st_read(shp, stringsAsFactors = FALSE)
cnty <- counties %>% 
  filter(STATEFP == '17') %>%
    filter(NAME %in% c('Cook','DuPage', 'Lake'))


#plot(counties_il_three$geometry)

mx <- as.numeric(dt$Max_Temp[!is.na(as.numeric(dt$Max_Temp))])
mn <- as.numeric(dt$Min_Temp[!is.na(as.numeric(dt$Min_Temp))])


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Invasive Pets Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("mxbins",
                     "Number of bins:",
#                     min = min(mx),
#                     max = max(mx),
#                    value = mean(mx)),
                      0,
                      30,
                      30),
         sliderInput("mnbins",
                     "Number of bins:",
#                     min = min(mn),
#                     max = max(mn),
#                     value = mean(mn))
                      0,
                      30,
                      30),
        selectInput("group",
                    label = NULL,
                    choices = grps
                    ),
        selectInput("species",
                    label = NULL,
                    choices = spcs
        ),
        selectInput("years",
                    label=NULL,
                    choices=yrs)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("mxPlot"),
         plotOutput("mnPlot"),
         leafletOutput("cntymap"),
         leafletOutput("cntymap_sp"),
         leafletOutput("cntymap_yr")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mnPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- mn
      bins <- seq(min(x), max(x), length.out = input$mnbins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'blue', border = 'white',
           xlab = "Minimum Temperature Tolerance",
          main = "Histogram of Minimum Temperature Tolerance")
   })
   
   
   output$mxPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     x    <- mx
     bins <- seq(min(x), max(x), length.out = input$mxbins + 1)
     
     # draw the histogram with the specified number of bins
     hist(x, breaks = bins, col = 'red', border = 'white',
          xlab = "Maximum Temperature Tolerance",
          main = "Histogram of Maximum Temperature Tolerance")
   })
#Reactive codes for leaflet
   filteredData <- reactive({
     print(grp1[grp1$group_ == input$group[1],]);
     grp1[grp1$group_ == input$group[1],]
   })
   
  filteredData_sp <- reactive({
  print(grp2[grp2$gen_sp == input$species[1],]);
     grp2[grp2$gen_sp == input$species[1],]
   })
  
  filteredData_yr<-reactive({
    print(grp3[grp3$YearEndCDa==input$years[1],]);
    grp3[grp3$YearEndCDa==input$years[1],]
  })

#outmap for taxon   
   output$cntymap <-renderLeaflet({
     leaflet(cnty)%>%
     addProviderTiles('Esri.WorldImagery', group='Esri')%>% 
    addProviderTiles('Stamen.TonerLite', group='Stamen')%>% 
       addLayersControl(baseGroups=c('Esri','Stamen'))%>%
      addProviderTiles(providers$Stamen.TonerLite,
      options = providerTileOptions(noWrap = TRUE)
     ) %>% 
       addPolygons()%>%
     addCircles(
       data = filteredData(), 
#     data = grp1[grp1$group_ == "Fishes",],
       radius = ~count, 
       lat = ~Latitude, 
       lng = ~Longitude,
        col='red'
      )
   }) 

#output map for species...something is wrong with the coordinate system
   output$cntymap_sp <-renderLeaflet({
    leaflet(cnty)%>%
     addProviderTiles('Esri.WorldImagery', group='Esri')%>% 
    addProviderTiles('Stamen.TonerLite', group='Stamen')%>% 
       addLayersControl(baseGroups=c('Esri','Stamen'))%>%
      addProviderTiles(providers$Stamen.TonerLite,
    options = providerTileOptions(noWrap = TRUE)
     ) %>% 
       addPolygons()%>%
    addCircles(
       data = filteredData_sp(), 
#     data = grp1[grp2$gen_sp == "Fishes",],
      radius = ~count, 
     lat = ~Latitude, 
       lng = ~Longitude,
       col='yellow'
      )
  }) 

   
 #output map for year      
   output$cntymap_yr <-renderLeaflet({
     leaflet(cnty)%>%
       addProviderTiles('Esri.WorldImagery', group='Esri')%>% 
       addProviderTiles('Stamen.TonerLite', group='Stamen')%>% 
       addLayersControl(baseGroups=c('Esri','Stamen'))%>%
       addProviderTiles(providers$Stamen.TonerLite,
                        options = providerTileOptions(noWrap = TRUE)
       ) %>% 
       addPolygons()%>%
       addCircles(
         data = filteredData_yr(), 
         #     data = grp1[grp2$gen_sp == "Fishes",],
         radius = ~count, 
         lat = ~Latitude, 
         lng = ~Longitude,
         col='maroon'
       )
   }) 
   
observe({
#     userGrp <- filteredData();
     leafletProxy("cntymap", data = cnty) %>% 
      clearShapes() %>%
           addCircles(
          data = grp1[grp1$group_ == "Fishes",],
           radius = ~count, 
           lat = ~Latitude, 
          lng = ~Longitude
       )
 })   


observe({
  #     userGrp <- filteredData();
  leafletProxy("cntymap_sp", data = cnty) %>% 
    clearShapes() %>%
    addCircles(
#             data = userGrp, 
#          data = filteredData(), 
      data = grp2[grp2$gen_sp == "Fishes",],
      radius = ~count, 
      lat = ~Latitude, 
      lng = ~Longitude
    )
  
})   

observe({
  leafletProxy("cntymap_yr", data = cnty) %>% 
    clearShapes() %>%
    addCircles(
      data = grp1[grp1$YearEndCDa == "1873",],
      radius = ~count, 
      lat = ~Latitude, 
      lng = ~Longitude
    )
})
}

# Run the application 
shinyApp(ui = ui, server = server)

