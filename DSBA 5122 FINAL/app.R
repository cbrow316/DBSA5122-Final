#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(DT)


data <- read.csv('Data/energy.csv')
countryNamesFull <- unique(data$Country)
countryNames <- countryNamesFull[-1]
energySourcesFull <- unique(data$Energy_type)
energySources <- energySourcesFull[-1]
choices <- list("GDP", "Population")


# Define UI for application that draws a histogram
ui <- fluidPage(
  mainPanel(
    tabsetPanel(type = 'tabs',
                tabPanel("Country Summary",
                         sidebarLayout(
                           sidebarPanel(
                             h3("Summary Settings"),
                             
                             selectizeInput("countrySelect", "Search a Country", countryNames, selected = NULL, multiple = FALSE,
                                            options = list(maxOptions = 5)),
                             
                             sliderInput("years", "Time Frame:",
                                         min = 1980, max = 2020,
                                         value = c(1980,2020)),
                             
                             checkboxInput("worldAvg",
                               label = "Show Global Average?",
                               value = TRUE),
                             
                             
                             
                           ),#sidebar Panel
                           mainPanel(
                             plotOutput("pop"),
                             plotOutput("gdp")
                             
                           )#mainPanel
                         ),#Sidebar Layout
                  
                ),#tab Panel Country Summary
                
                tabPanel("Energy Economy",
                         sidebarLayout(
                           sidebarPanel(width = 4,
                             h3("Summary Settings"),
                             
                             selectizeInput("countrySelectEE", "Search a Country", countryNames, selected = NULL, multiple = FALSE,
                                            options = list(maxOptions = 5)),
                             
                             sliderInput("yearsEE", "Time Frame:",
                                         min = 1980, max = 2020,
                                         value = c(1980,2020)),
                             hr(),
                             
                             h3("Energy Economy Settings:"),
                             
                             selectInput("energyType", label = "Select an Energy Source", choices = energySources),
                             
                             checkboxInput("energyProduction",
                                           label = 'Show Total Energy Production?',
                                           value = TRUE),
                             
                             checkboxInput("energyConsumption",
                                         label = 'Show Total Energy Consumption?',
                                         value = TRUE),
                             hr(),
                             
                             h3("Polution Settings"),
                             
                             selectInput("pollutionType", label = "Select an Energy Source", choices = energySources),
                             
                             checkboxInput("totalPollution",
                                           label = 'Show Total CO2 Emissions?',
                                           value = TRUE),
                             
                             hr(),
                             h3("Energy In-Efficiency Settings"),
                             
                             radioButtons("EI_Type",
                                          "How to calculate Energy Intensity",
                                          choices = choices
                             ),
                             
                             checkboxInput("worldEI",
                                           label = 'Show Global Energy Inefficiency?',
                                           value = TRUE)

                           ),#sidebar Panel
                           mainPanel(width = 8,
                             fluidRow(
                               splitLayout(cellWidths = c("50%", "50%"), plotOutput("energyEconomy"), plotOutput("pollution"))
                             ),
                             fluidRow(plotOutput("EI"))
                             
                           )#mainPanel
                         ),#Sidebar Layout
                ),#Energy Economy
                
                tabPanel("Country Comparisons",
                         sidebarLayout(
                           sidebarPanel(
                             h3("Background (Black) Country Settings"),
                             
                             selectizeInput("firstCountrySelectCC1", "Search a Country", countryNamesFull, selected = NULL, multiple = FALSE,
                                            options = list(maxOptions = 5)),
                             
                             selectInput("energyTypeCC1", label = "Select an Energy Source", choices = energySourcesFull),
                             
                             hr(),
                             h3("Foreground (Blue) Country Settings"),
                             
                             selectizeInput("secondCountrySelectCC2", "Search a Country", countryNamesFull, selected = NULL, multiple = FALSE,
                                            options = list(maxOptions = 5)),
                             
                             selectInput("energyTypeCC2", label = "Select an Energy Source", choices = energySourcesFull),
                             
                             hr(),
                             h3("Plot Settings"),
                             
                             sliderInput("yearsCC", "Time Frame:",
                                         min = 1980, max = 2020,
                                         value = c(1980,2020)),
                             
                             
                             
                           ),#sidebar Panel
                           mainPanel(
                             plotOutput("CCE"),
                             plotOutput("CCP")
                             
                           )#mainPanel
                         ),#Sidebar Layout
                ),#Country Comparisons,
                
                tabPanel("Raw Data",
                         DT::dataTableOutput("dataTable")
                )
      
    )#Tabset panel
  )#Mainpanel 1
)#Fluidpage


server <- function(input, output) {
  
  output$pop <- renderPlot({
    data_country <- filter(data, Country == input$countrySelect)
    data_world <- filter(data, Country == 'World')
    ggplot(NULL)+
      geom_bar(data = data_country, aes(x = Year, y = Population), stat = 'identity') + 
      {if(input$worldAvg) geom_line(data = data_world, aes(x = Year, y = Population/230), stat = 'identity', col = "Cyan", size = 2)} +
      xlim(input$years) +
      labs(x="Years",y="Population",
           title="Country Population each Year")
  })#Output$pop
  
  output$gdp <- renderPlot({
    data_country <- filter(data, Country == input$countrySelect)
    data_world <- filter(data, Country == 'World')
    ggplot(NULL)+
      geom_bar(data = data_country, aes(x = Year, y = GDP), stat = 'identity') + 
      {if(input$worldAvg) geom_line(data = data_world, aes(x = Year, y = GDP/230), stat = 'identity', col = "Cyan", size = 2)} +
      xlim(input$years) +
      labs(x="Years",y="GDP",
           title="Country GDP each Year")
  })#Output$pop
  
  output$energyEconomy <- renderPlot({
    energy_type <- filter(data, Country == input$countrySelectEE & Energy_type == input$energyType)
    energy_full <- filter(data, Country == input$countrySelectEE & Energy_type == 'all_energy_types')
    ggplot(NULL)+
      geom_area(data = energy_type, aes(x = Year, y = Energy_production), stat = 'identity', fill = 'Cyan', position = 'stack') + 
      {if(input$energyProduction) geom_line(data = energy_full, aes(x = Year, y = Energy_production), stat = 'identity', color = "Green", size = 1)} +
      {if(input$energyConsumption) geom_line(data = energy_full, aes(x = Year, y = Energy_consumption), stat = 'identity', color = "Red", size = 1)} +
      xlim(input$yearsEE) +
      labs(x="Years",y="Energy Produced",
           title="Amount of Energy Produced each Year")
  })#Output$energyEconomy
  
  output$pollution <- renderPlot({
    pollution_type <- filter(data, Country == input$countrySelectEE & Energy_type == input$pollutionType)
    pollution_full <- filter(data, Country == input$countrySelectEE & Energy_type == 'all_energy_types')
    ggplot(NULL)+
      geom_area(data = pollution_type, aes(x = Year, y = CO2_emission), stat = 'identity', fill = 'Cyan', position = 'stack') + 
      {if(input$totalPollution) geom_line(data = pollution_full, aes(x = Year, y = CO2_emission), stat = 'identity', color = "Red", size = 1)} +
      xlim(input$yearsEE) +
      labs(x="Years",y="CO2 Emissions",
           title="Amount of Energy Produced each Year")
  })#Output$pollution
  
  output$EI <- renderPlot({
    energy_typeEI <- filter(data, Country == input$countrySelectEE & Energy_type == 'all_energy_types')
    world_typeEI <- filter(data, Country == 'World')
    
    if(input$EI_Type == 'Population'){
      ggplot(NULL) +
        geom_area(data = energy_typeEI, aes(x = Year, y = Energy_intensity_per_capita), stat = 'identity', fill = 'Cyan', position = 'stack') + 
        {if(input$worldEI) geom_line(data = world_typeEI, aes(x = Year, y = Energy_intensity_per_capita), stat = 'identity', color = "Red", size = 1)} +
        xlim(input$yearsEE) +
        labs(x="Years",y="Energy Intensity",
             title="Energy Intensity per Capita each Year")
    }else{
      ggplot(NULL) +
        geom_area(data = energy_typeEI, aes(x = Year, y = Energy_intensity_by_GDP), stat = 'identity', fill = 'Cyan', position = 'stack') + 
        {if(input$worldEI) geom_line(data = world_typeEI, aes(x = Year, y = Energy_intensity_by_GDP), stat = 'identity', color = "Red", size = 1)} +
        xlim(input$yearsEE) +
        labs(x="Years",y="Energy Intensity",
             title="Energy Intensity each Year by GDP")
    }
    
    
  })
  
  output$CCE <- renderPlot({
    C1E <- filter(data, Country == input$firstCountrySelectCC1 & Energy_type == input$energyTypeCC1)
    C2E <- filter(data, Country == input$secondCountrySelectCC2 & Energy_type == input$energyTypeCC2)
    
    ggplot(NULL) +
      geom_area(data = C1E, aes(x = Year, y = Energy_production), stat = 'identity', fill = 'Black', position = 'stack', alpha = .75) +
      geom_area(data = C2E, aes(x = Year, y = Energy_production), stat = 'identity', fill = 'Cyan', position = 'stack', alpha = .5) +
      xlim(input$yearsCC) +
      labs(x="Years",y="Energy Produced",
           title="Amount of Energy Produced each Year")
  })
  
  output$CCP <- renderPlot({
    C1P <- filter(data, Country == input$firstCountrySelectCC1 & Energy_type == input$energyTypeCC1)
    C2P <- filter(data, Country == input$secondCountrySelectCC2 & Energy_type == input$energyTypeCC2)
    
    ggplot(NULL) +
      geom_area(data = C1P, aes(x = Year, y = CO2_emission), stat = 'identity', fill = 'Black', position = 'stack', alpha = .75) +
      geom_area(data = C2P, aes(x = Year, y = CO2_emission), stat = 'identity', fill = 'Cyan', position = 'stack', alpha = .5) +
      xlim(input$yearsCC) +
      labs(x="Years",y="CO2 Emissions",
           title="Amount of CO2 Produced each Year")
  })
  
  output$dataTable = DT::renderDataTable({
    data
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
