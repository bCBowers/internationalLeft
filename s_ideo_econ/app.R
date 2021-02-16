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

filter(W_EVS, !is.na(self_ideo) & !is.na(ec_ideo) & !is.na(edu) & !is.na(generation)) %>% 
  mutate(country_wave = droplevels(country_wave)) -> W_EVS

cw_list <- W_EVS %>% dlply('country_wave')

lapply(cw_list, function(x) {
  table(x$generation) < 50
}) %>% 
  do.call(rbind, .) %>% 
  melt() -> gen_index

left_join(W_EVS, gen_index, by = c("country_wave" = "Var1", "generation" = "Var2")) %>% 
  filter(value == F) -> W_EVS

# Define UI

ui <- navbarPage("Model selection", 
                 tabPanel("Not adjusted for education", 
                          titlePanel("Economic Ideology and Ideological Self-Placement"), 
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          selectInput("country_wave1", label = "Country & survey year", 
                                                      choices = levels(W_EVS$country_wave), 
                                                      selected = "United States 2011")), 
                                        mainPanel(plotOutput("n_edu_plot")))), 
                 tabPanel("Adjusted for education", 
                          titlePanel("Economic Ideology and Ideological Self-Placement"),
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          selectInput("country_wave2", label = "Country & survey year", 
                                                      choices = levels(W_EVS$country_wave), 
                                                      selected = "United States 2011")), 
                                        mainPanel(plotOutput("edu_plot")))))

# Define server

server <- function(input, output) {
  
  filter1 <- reactive({
    filter(W_EVS) %>% 
      filter(country_wave == input$country_wave1)
  })
  
  output$n_edu_plot <- renderPlot({
    filter1() %>% 
      ggplot(aes(x = ec_ideo, y = self_ideo, colour = generation)) + 
      geom_smooth(method = "glm", aes(weight = S017)) + 
      labs(caption = "Linear regression plots stratified by generation", colour = "Generation (years born)") + 
      scale_x_continuous("Economic Ideology (left to right)", breaks = c(0, .2, .4, .6, .8, 1)) + 
      scale_y_continuous("Ideological self-placement (left to right)", breaks = c(1, 4, 7, 10)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(.2, .8), ylim = c(1, 10)) + 
      scale_color_discrete(breaks = c("Greatest", "Silent", "Boomers", "Gen X", "Millennials"), 
                           labels = c("Greatest (pre-1928)", "Silent (1928-1945)", "Boomers (1946-1964)", 
                                      "Gen X (1965-1980)", "Millennials (1981-1996)"))
  })
  
  filter2 <- reactive({
    filter(W_EVS) %>% 
      filter(country_wave == input$country_wave2)
  })
  
  output$edu_plot <- renderPlot({
    filter2() %>% 
      ggplot(aes(x = resid(lm(ec_ideo ~ edu, weights = S017)), 
                 y = resid(lm(self_ideo ~ edu, weights = S017)), colour = generation)) + 
      geom_smooth(method = "glm") + 
      labs(caption = "Partial multiple linear regression plots adjusted for education, stratified by generation", 
           colour = "Generation (years born)") + 
      scale_x_continuous("Economic Ideology (left to right, residuals)", breaks = c(-.5, -.3, -.1, .1, .3, .5)) + 
      scale_y_continuous("Ideological self-placement (left to right, residuals)", breaks = c(-4.5, -1.5, 1.5, 4.5)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(-.3, .3), ylim = c(-4.5, 4.5)) + 
      scale_color_discrete(breaks = c("Greatest", "Silent", "Boomers", "Gen X", "Millennials"), 
                           labels = c("Greatest (pre-1928)", "Silent (1928-1945)", "Boomers (1946-1964)", 
                                      "Gen X (1965-1980)", "Millennials (1981-1996)"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
