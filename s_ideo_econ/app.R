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

url <- getURL("https://raw.githubusercontent.com/jordan-klein/internationalLeft/master/W_EVS_clean.csv")
W_EVS <- read.csv(text = url)

filter(W_EVS, !is.na(self_ideo) & !is.na(ec_ideo)) %>% 
  mutate(country_wave = droplevels(country_wave)) -> W_EVS

# Define UI

ui <- fluidPage(
  titlePanel("Economic Ideology and Self left-right Placement"),
  sidebarLayout(position = "right", 
                sidebarPanel(
                  selectInput("country_wave", label = "Country & survey year", 
                              choices = levels(W_EVS$country_wave), selected = "United States 2011")),
                mainPanel(
                  plotOutput("scatterplot"))))

# Define server

server <- function(input, output) {
  
  filtered <- reactive({
    filter(W_EVS) %>% 
      filter(country_wave == input$country_wave)
  })
  
  output$scatterplot <- renderPlot({
    filtered() %>% 
      ggplot(aes(x = ec_ideo, y = self_ideo, colour = generation)) + 
      geom_smooth(method = "glm", aes(weight = S017)) + 
      labs(caption = "Linear regression plots stratified by generation") + 
      scale_x_continuous("Economic Ideology (left to right)", breaks = c(0, .25, .5, .75, 1)) + 
      scale_y_continuous("Self ideological placement", breaks = c(1, 4, 7, 10)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(0, 1), ylim = c(1, 10))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
