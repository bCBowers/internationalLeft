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
library(devtools)
library(ggiraph)
library(ggiraphExtra)
library(MASS)
library(car)

url <- getURL("https://raw.githubusercontent.com/Jklein29/internationalLeft/master/WVS_viz_vars.csv")
W_EVS <- read.csv(text = url)

# Create racial resentment factor variable

W_EVS <- W_EVS %>% mutate(RacRes = case_when(rac_ideo == 0 ~ "Very low", 
                                             rac_ideo > 0 & rac_ideo <= .25 ~ "Low", 
                                             rac_ideo > .25 & rac_ideo <= .5 ~ "Moderate", 
                                             rac_ideo > .5 & rac_ideo <= .75 ~ "High", 
                                             rac_ideo > .75 ~ "Very high")) %>% 
  mutate(RacRes = factor(RacRes, levels = c("Very low", "Low", "Moderate", "High", "Very high")))

W_EVS <- W_EVS %>% mutate(RacRes_n = case_when(RacRes == "Very low" ~ 0, 
                                               RacRes == "Low" ~ .25, RacRes == "Moderate" ~ .5, 
                                               RacRes == "High" ~ .75, RacRes == "Very high" ~ 1))

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
    filter(W_EVS, !is.na(RacRes_n) & !is.na(ec_ideo) & !is.na(edu) & !is.na(relig)) %>% 
      filter(country == input$country)
  })
  
  output$scatterplot <- renderPlot({
    filtered() %>% 
      ggplot(aes(x = resid(lm(ec_ideo ~ edu + relig, weights = S017)), 
                 y = resid(lm(RacRes_n ~ edu + relig, weights = S017)))) + 
      geom_smooth(method = "glm", color = "darkblue") + 
      labs(caption = "Partial multiple linear regression plot adjusted for education and religion") + 
      scale_x_continuous("Economic Ideology (left to right)", breaks = c(-.4, -.2, 0, .2, .4)) + 
      scale_y_continuous("Racial Resentment (low to high)", breaks = c(-.4, -.2, 0, .2, .4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(-.4, .4), ylim = c(-.4, .4))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
