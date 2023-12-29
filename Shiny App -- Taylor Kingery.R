library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(rvest)
library(tidyr)
library(maps)
library(leaflet)
library(stringr)
library(tidyverse)
library(leafpop)
library(httr)
library(jsonlite)
library(XML)
library(gpx)
library(sf)

#Shiny App

load("CleanData.Rdata")

ui <- navbarPage("An Analysis of Southwestern Border Crossing Danger and Causes",
                 tabPanel("Crossing The Border", 
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              selectInput(inputId = "Death" , label = "Death according to:", choices = c("Age Group", "Gender", "Sector", "Type of Death"), selected = NULL, multiple = FALSE, selectize = TRUE),
                              
                              
                              
                              radioButtons(inputId = "FillType", label = "View by:", choices = c("Stacked Totals", "Proportion of Total"), selected = NULL)
                              
                            ),
                            mainPanel(
                              plotOutput("DeathBar")
                              
                            )
                            
                          )     
                 ),
                 tabPanel("Temperature Map",
                          fluidRow(
                            column(6,
                                   leafletOutput("LeafMap")
                            ),
                            column(6,
                                   plotOutput("TempGraph")
                            ),
                            pickerInput(
                              inputId = "Sector", 
                              label = "Average Seasonal Temperature for:", 
                              choices = c("San Diego", "Yuma/El Centro", "Tucson", "El Paso","Big Bend","Del Rio","Laredo","Rio Grande Valley", "Juarez City"),
                              selected = "San Diego",
                              options = pickerOptions(
                                actionsBox = TRUE, 
                                size = 10,
                                selectedTextFormat = "count > 3"
                              ), 
                              multiple = TRUE
                            )
                          )
                 )
)

server <- function(input, output) {
  
  DeathChoice<- reactive({
    var1 <- switch(input$Death, 
                   "Age Group" = "DeathAge", 
                   "Gender" = "DeathGender",
                   "Sector" = "DeathSector",
                   "Type of Death" = "DeathType",
                   NULL)
    get(var1)
    
  })
  
  FillType <- reactive({
    input$FillType
  })
  
  
  output$DeathBar <- renderPlot({
    
    DeathPlot <- ggplot(data = DeathChoice(), aes(x = Year, y = Count, fill = get(input$Death)))+theme_minimal()+scale_fill_discrete(name = "Death if:")
    
    if (input$FillType == "Stacked Totals"){
      DeathPlot + geom_col(position = "stack")
    } else{
      DeathPlot + geom_col(position = "fill")
    }
    
  })
  
  tempNorm <- as.data.frame(tempoNormals1)
  
  output$LeafMap <- renderLeaflet({
    
    leaflet(data = tempNorm) %>% 
      addTiles()%>% 
      addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, popup = ~paste(Sector))%>%
      addCircles(data = BorderCheckFinal, lng = ~as.numeric(Longitude), lat = ~as.numeric(Latitude), color = "magenta", popup = BorderCheckFinal$`Checkpoint Name`)%>%
      clearBounds()
  })
  
  SectorSelect <- reactive({
    input$Sector
  })
  
  SeasonOrder <- c("MAM.TMAX.NORMAL","JJA.TMAX.NORMAL","SON.TMAX.NORMAL","DJF.TMAX.NORMAL")
  
  output$TempGraph <- renderPlot({
    req(SectorSelect())
    ggplot(subset(tempNorm, Sector %in% SectorSelect()), aes(Season, Average_Temperature, fill = Sector)) + geom_point()+geom_line(aes(color = Sector, group = Sector))+scale_x_discrete(limits = SeasonOrder, labels = c("Spring", "Summer","Fall", "Winter"))+scale_y_continuous(breaks = c(50,75,90), labels = c("50", "75", "90"), name = "Average Temperature") +theme_minimal()+ylim(40, 110)+theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
