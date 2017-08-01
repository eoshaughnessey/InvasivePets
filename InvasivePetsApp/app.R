#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(data.table)
library(sf)
library(rgdal)
library(dplyr)
library(datasets)
library(ggplot2)


dt <- read.csv("~/InvasivePets/MasterDataSheet_temperature.csv")

tab <- as.data.table(dt)
tab[,Max_Temp := as.numeric(Max_Temp)]
tab[,Min_Temp := as.numeric(Min_Temp)]
tab[,YearEndCDa:=as.numeric(YearEndCDa)]


huc_pop <- read.csv("~/InvasivePets/huc_populations.csv")
hp <- as.data.table(huc_pop)
hp[,Population := as.numeric(Population)] 


hg <- read.csv("~/InvasivePets/huc_gps_table.csv")
hg <- as.data.table(hg)

grp1 <- as.data.frame(hp[,
                          .(
                            count = .N
                            ),
                         by = list (
                           HUC12_1,
                           Population
                         )]
                      )

#grp2 <- as.data.frame(hg[,
#                         .(
#                           count=.N,
#                           Latitude= Latitude, na.rm=T,
#                           Longitude= Longitude, na.rm=T
#                         ),
#                         by = list(
#                           common,
#                           HUC12_1
#                           )]
#                      )

hg_count <- setDT(hg)[ , .(
  count=.N) ,
  by = .(common,HUC12_1)]
count_pop <- merge(hp,hg_count,by="HUC12_1")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Where to survey?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("variable", "First variable:",
                    list( "HUC" = "Population")),
        selectInput("variable2", "Second variable:",
                    list ("Number of Individuals Caught" = "count_pop$count"))
        
      ), 
      # Show a plot of the generated distribution
      mainPanel(
        h3(textOutput("caption")),
        plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  text <- reactive({
    paste(input$variable, 'versus', input$variable2)
    })
  
  # Return as text the selected variables
  output$caption <- renderText({
    text()
  })
  
  # Generate a plot of the requested variables
  output$plot <- renderPlot({
    p <- ggplot(data, aes_string(x=input$variable, y=input$variable2)) #+ 
#      geom_point()
    print(p)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

