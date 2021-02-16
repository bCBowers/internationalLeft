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

# Create probability data

country_list <- W_EVS %>% dlply('country')

lapply(country_list, function(x) {
  polr(RacRes ~ ec_ideo, data = x, weights = S017, Hess = T) %>% 
    predict(x, type = "probs") %>% 
    cbind(dplyr::select(x, country, ec_ideo), .) %>% 
    reshape2::melt(id.vars = c("country", "ec_ideo"), 
                   variable.name = "Resentment_level", value.name = "Probability")
}) %>% 
  do.call(rbind.data.frame, .) -> W_EVS_probs

# Define UI

ui_2 <- fluidPage(
  titlePanel("Economic Ideology and Racial Resentment"),
  sidebarLayout(position = "right", 
                sidebarPanel(
                  selectInput("country", label = "Country", 
                              choices = levels(W_EVS_probs$country), selected = "United States")),
                mainPanel(
                  plotOutput("scatterplot"))))

# Define server

server_2 <- function(input, output) {
  
  filtered <- reactive({
    W_EVS_probs %>% filter(country == input$country)
  })
  
  output$scatterplot <- renderPlot({
    filtered() %>% 
      ggplot(aes(x = ec_ideo, y = Probability, colour = Resentment_level)) + geom_line() + 
      labs(caption = "Ordinal logistic regression plots") + 
      scale_x_continuous("Economic Ideology (left to right)", breaks = c(0, .25, .5, .75, 1)) + 
      scale_y_continuous("Probability", breaks = c(0, .25, .5, .75)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(0, 1), ylim = c(0, .75))
    
  })
}

# Run the application 
shinyApp(ui = ui_2, server = server_2)
