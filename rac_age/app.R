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

filter(W_EVS, !is.na(RacRes_n) & !is.na(X003) & !is.na(edu)) %>% 
  mutate(country = droplevels(country)) %>% 
  mutate(S020 = factor(S020, levels = c(as.character(sort(unique(S020)))))) -> W_EVS

# Define UI

ui <- navbarPage("Model selection", 
                 tabPanel("Not adjusted for education", 
                          titlePanel("Age and Racial Resentment"), 
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          selectInput("country1", label = "Country", 
                                                      choices = levels(W_EVS$country), 
                                                      selected = "United States")), 
                                        mainPanel(plotOutput("n_edu_plot")))), 
                 tabPanel("Adjusted for education", 
                          titlePanel("Age and Racial Resentment"),
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          selectInput("country2", label = "Country", 
                                                      choices = levels(W_EVS$country), 
                                                      selected = "United States")), 
                                        mainPanel(plotOutput("edu_plot")))))

# Define server

server <- function(input, output) {
  
  filter1 <- reactive({
    filter(W_EVS) %>% 
      filter(country == input$country1)
  })
  
  output$n_edu_plot <- renderPlot({
    filter1() %>% 
      ggplot(aes(x = X003, y = RacRes_n, colour = S020)) + 
      geom_smooth(method = "glm", aes(weight = S017)) + 
      labs(caption = "Linear regression plots stratified by survey wave", colour = "Survey wave (year)") + 
      scale_x_continuous("Age (years)", breaks = c(0, 20, 40, 60, 80, 100, 120)) + 
      scale_y_continuous("Racial Resentment (low to high)", breaks = c(0, .25, .5, .75, 1)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(20, 80), ylim = c(0, 1)) 
  })
  
  filter2 <- reactive({
    filter(W_EVS) %>% 
      filter(country == input$country2)
  })
  
  output$edu_plot <- renderPlot({
    filter2() %>% 
      ggplot(aes(x = resid(lm(X003 ~ edu, weights = S017)), 
                 y = resid(lm(RacRes_n ~ edu, weights = S017)), colour = S020)) + 
      geom_smooth(method = "glm") + 
      labs(caption = "Partial multiple linear regression plots adjusted for education, stratified by survey wave", 
           colour = "Survey wave (year)") + 
      scale_x_continuous("Age (young to old, residuals)", breaks = c(-45, -30, -15, 0, 15, 30, 45)) + 
      scale_y_continuous("Racial Resentment (low to high, residuals)", breaks = c(-.5, -.25, 0, .25, .5)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(-30, 30), ylim = c(-.5, .5)) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
