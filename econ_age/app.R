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
library(stargazer)
library(reshape2)

url <- getURL("https://raw.githubusercontent.com/jordan-klein/internationalLeft/master/W_EVS_clean.csv")
W_EVS <- read.csv(text = url)

filter(W_EVS, !is.na(ec_ideo) & !is.na(X003)) %>% 
  mutate(country = droplevels(country)) %>% 
  mutate(S020 = factor(S020, levels = c(as.character(sort(unique(S020)))))) -> W_EVS

# Define UI

ui <- fluidPage(
  titlePanel("Age and Economic Ideology"),
  sidebarLayout(position = "right", 
                sidebarPanel(
                  selectInput("country", label = "Country", 
                              choices = levels(W_EVS$country), selected = "United States")),
                mainPanel(
                  plotOutput("scatterplot"))))

# Define server

server <- function(input, output) {
  
  filtered <- reactive({
    filter(W_EVS) %>% 
      filter(country == input$country)
  })
  
  output$scatterplot <- renderPlot({
    filtered() %>% 
      ggplot(aes(x = X003, y = ec_ideo, colour = S020)) + 
      geom_smooth(method = "glm", aes(weight = S017)) + 
      labs(caption = "Linear regression plots stratified by survey wave", colour = "Survey wave (year)") + 
      scale_x_continuous("Age (years)", breaks = c(0, 20, 40, 60, 80, 100, 120)) + 
      scale_y_continuous("Economic Ideology (left to right)", breaks = c(0, .25, .5, .75, 1)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(20, 80), ylim = c(0, 1)) 
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
