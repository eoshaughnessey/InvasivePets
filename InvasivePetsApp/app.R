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


dt <- read.csv("~/InvasivePets/MasterDataSheet_temperature.csv",
               stringsAsFactors = FALSE)

tab <- as.data.table(dt)
tab[,Max_Temp := as.numeric(Max_Temp)]
tab[,Min_Temp := as.numeric(Min_Temp)]
tab[,YearEndCDa:=as.numeric(YearEndCDa)]


huc_pop <- read.csv("~/InvasivePets/huc_populations.csv",
                    stringsAsFactors = FALSE)
hp <- as.data.table(huc_pop)
hp[,Population := as.numeric(Population)] 


hg <- read.csv("~/InvasivePets/huc_gps_table.csv")
hg <- as.data.table(hg)


#grp1 <- as.data.frame(hp[,
#                          .(
#                            count = .N
#                           ),
#                         by = list (
#                           HUC12_1,
#                           Population
#                         )]
#                     )

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
count_pop <-merge(hp,hg_count,by="HUC12_1")
count_pop[156,4] <- "Piraoatinga"
count_pop$common<-gsub('\\s+', '_', count_pop$common)
#cn<-count_pop[, unique(common)]
#cn <- parse(text=gsub(' ', '_', cn))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Where to survey?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         conditionalPanel(
            condition="input.conditionedPanels==1",
        selectInput( "variable", "First variable:",
                  list( "Population" = "count_pop$Population")),
        selectInput("variable2", "Second variable:",
                   list ("Number of Individuals Caught" = "count_pop$count"))
      )
      ,
      conditionalPanel(
        condition="input.conditionedPanels==2",
        selectInput("Variable", "Population size:",
                    list("Population size" = "count_pop$Population")),
        selectInput("Variable2", "Species:",
                    choices=count_pop[, unique(common)])
      )
      ), 
      # Show a plot of the generated distribution
      mainPanel(
#        h3(textOutput("caption")),
#        plotOutput("plot"),
        tabsetPanel(
          #create tabs so you don't have to scroll down
          tabPanel("Overall", 
                   plotOutput("plot"), 
                   h3(textOutput("caption")),
                   value=1
                   ),
          tabPanel("Species", 
                   plotOutput("plot2"),
                   h3(textOutput("caption2")),
                   value=2
                   ),
          id="conditionedPanels"
        )
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    text1 <- reactive({
    paste('Population size versus All Occurences')
  })
    text2<-reactive({
      paste('Population size versus Specie Occurence(s)')
    })
  
  # Return as text the selected variables
  output$caption <- renderText({
    text1()
  })
  
  output$caption2 <- renderText({
    text2()
  })
  
  
  # Generate a plot of the requested variables
  output$plot <- renderPlot({
    p <- ggplot(count_pop, aes_string(x=input$variable, y=input$variable2, 
                                      col="common")) + 
      geom_point()
    print(p)
   })
  
#theres something wrong with this code  
  output$plot2 <- renderPlot({
    p2<-ggplot(count_pop, aes_string(x=input$Variable, y=input$Variable2))+
      geom_point()
    print(p2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

