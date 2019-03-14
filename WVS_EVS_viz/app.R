# Load packages & data

library(shiny)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(survey)
library(rvest)
library(plyr)
library(broom)
library(knitr)
library(RCurl)

url <- getURL("https://raw.githubusercontent.com/Jklein29/internationalLeft/master/WVS_viz_vars.csv")
W_EVS <- read.csv(text = url)

# Define UI

ui <- fluidPage(
  titlePanel("Economic Ideology and Racial Resentment"),
  sidebarLayout(position = "right", 
    sidebarPanel(
      selectInput("country", label = "Country", 
                  choices = levels(W_EVS$country), selected = "United States")),
    mainPanel(
      plotOutput("scatterplot"))))

# Define server

server <- function(input, output) {
  
  filtered <- reactive({
    W_EVS %>% filter(country == input$country)
  })
  
  output$scatterplot <- renderPlot({
    filtered() %>% 
      ggplot(aes(x = ec_ideo, y = rac_ideo)) + geom_point(color = "darkblue") + 
      geom_smooth(method = "lm", mapping = aes(weight = S017), color = "red") + 
      xlab("Economic Ideology (0 = left, 1 = right)") + ylab("Racial Resentment (0 = low, 1 = high)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
