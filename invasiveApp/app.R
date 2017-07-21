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
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("mxPlot"),
         plotOutput("mnPlot"),
         leafletOutput("cntymap")
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
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$mxPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     x    <- mx
     bins <- seq(min(x), max(x), length.out = input$mxbins + 1)
     
     # draw the histogram with the specified number of bins
     hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })

   filteredData <- reactive({
     print(grp1[grp1$group_ == input$groups[1],]);
     grp1[grp1$group_ == input$groups[1],]
   })
   
   output$cntymap <-renderLeaflet({
     leaflet(cnty)%>%
     addProviderTiles('Esri.WorldImagery')%>% 
#     addProviderTiles(providers$Stamen.TonerLite,
#      options = providerTileOptions(noWrap = TRUE)
#     ) %>% 
     addCircles(
#       data = filteredData(), 
       data = grp1[grp1$group_ == "Fishes",],
       radius = ~count, 
       lat = ~Latitude, 
       lng = ~Longitude
      )
   }) 
   
   observe({
#     userGrp <- filteredData();
     
     leafletProxy("cntymap", data = cnty) %>% 
       clearShapes() %>%
          addCircles(
#             data = userGrp, 
            data = filteredData(), 
            radius = ~count, 
             lat = ~Latitude, 
             lng = ~Longitude
           )
     
   })   
}

# Run the application 
shinyApp(ui = ui, server = server)

