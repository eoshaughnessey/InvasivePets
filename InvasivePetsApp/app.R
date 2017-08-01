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
cn<-tab[, unique(common)]

huc_pop <- read.csv("~/InvasivePets/huc_populations.csv")

hp <- as.data.table(huc_pop)

hp[,Population := as.numeric(Population)]

grp1 <- as.data.frame(hp[,
                          .(
                            count = .N
                            ),
                         by = list (
                           HUC12_1,
                           Population
                         )]
                      )

grp2 <- as.data.frame(dt[,
                         .(
                           count=.N,
                           Latitude= Latitude, na.rm=T,
                           Longitude= Longitude, na.rm=T
                         ),
                         by =
                           common 
                         ])

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Where to survey?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("variable", "First variable:",
                    list( "HUC" = "HUC12_1")),
        selectInput("variable2", "Second variable:",
                    list ("Population size" = "Population"))
        
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
    p <- ggplot(data, aes_string(x=input$variable, y=input$variable2)) + geom_point()
    print(p)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

