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
library(reshape2)

url_og <- getURL("https://raw.githubusercontent.com/jordan-klein/internationalLeft/master/WVS_viz_vars.csv")
W_EVS_og <- read.csv(text = url_og)

W_EVS_og <- W_EVS_og %>% mutate(RacRes = case_when(rac_ideo == 0 ~ "Very low", 
                                                   rac_ideo > 0 & rac_ideo <= .25 ~ "Low", 
                                                   rac_ideo > .25 & rac_ideo <= .5 ~ "Moderate", 
                                                   rac_ideo > .5 & rac_ideo <= .75 ~ "High", 
                                                   rac_ideo > .75 ~ "Very high")) %>% 
  mutate(RacRes = factor(RacRes, levels = c("Very low", "Low", "Moderate", "High", "Very high")))

W_EVS_og <- W_EVS_og %>% mutate(RacRes_n = case_when(RacRes == "Very low" ~ 0, 
                                                     RacRes == "Low" ~ .25, RacRes == "Moderate" ~ .5, 
                                                     RacRes == "High" ~ .75, RacRes == "Very high" ~ 1))

W_EVS_og <- W_EVS_og %>% filter(!is.na(ec_ideo) & !is.na(RacRes_n) & !is.na(edu) & !is.na(relig))

url <- getURL("https://raw.githubusercontent.com/jordan-klein/internationalLeft/master/W_EVS_clean.csv")
W_EVS <- read.csv(text = url)

filter(W_EVS, !is.na(ec_ideo) & !is.na(RacRes_n) & !is.na(edu) & !is.na(relig) & !is.na(generation)) %>% 
  mutate(country_wave = droplevels(country_wave)) -> W_EVS

cw_list <- W_EVS %>% dlply('country_wave')

lapply(cw_list, function(x) {
  table(x$generation) < 50
}) %>% 
  do.call(rbind, .) %>% 
  melt() -> gen_index

left_join(W_EVS, gen_index, by = c("country_wave" = "Var1", "generation" = "Var2")) %>% 
  filter(value == F) -> W_EVS

# Create racial resentment factor variable

ui <- navbarPage("Model selection", 
                 tabPanel("Most recent wave since 2008", 
                          titlePanel("Economic Ideology and Racial Resentment"), 
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          selectInput("og_country", label = "Country", 
                                                      choices = levels(W_EVS_og$country), 
                                                      selected = "United States")), 
                                        mainPanel(plotOutput("og_plot")))), 
                 tabPanel("All waves, stratified by generation", 
                          titlePanel("Economic Ideology and Racial Resentment"),
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          selectInput("country_wave", label = "Country & survey year", 
                                                      choices = levels(W_EVS$country_wave), 
                                                      selected = "United States 2011")), 
                                        mainPanel(plotOutput("strat_plot")))))

# Define server

server <- function(input, output) {
  
  og_filter <- reactive({
    filter(W_EVS_og) %>% 
      filter(country == input$og_country)
      })
  
  output$og_plot <- renderPlot({
    og_filter() %>% 
      ggplot(aes(x = resid(lm(ec_ideo ~ edu + relig, weights = S017)), 
                 y = resid(lm(RacRes_n ~ edu + relig, weights = S017)))) + 
      geom_smooth(method = "glm", color = "darkblue") + 
      labs(caption = "Partial multiple linear regression plots adjusted for education and religion") + 
      scale_x_continuous("Economic Ideology (left to right, residuals)", breaks = c(-.4, -.2, 0, .2, .4)) + 
      scale_y_continuous("Racial Resentment (low to high, residuals)", breaks = c(-.4, -.2, 0, .2, .4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(-.4, .4), ylim = c(-.4, .4))
  })
    
    strat_filter <- reactive({
      filter(W_EVS) %>% 
        filter(country_wave == input$country_wave)
    })
    
    output$strat_plot <- renderPlot({
      strat_filter() %>% 
        ggplot(aes(x = resid(lm(ec_ideo ~ edu + relig, weights = S017)), 
                   y = resid(lm(RacRes_n ~ edu + relig, weights = S017)), colour = generation)) + 
        geom_smooth(method = "glm") + 
        labs(caption = "Partial multiple linear regression plots adjusted for education and religion, stratified by generation", 
             colour = "Generation (years born)") + 
        scale_x_continuous("Economic Ideology (left to right, residuals)", breaks = c(-.4, -.2, 0, .2, .4)) + 
        scale_y_continuous("Racial Resentment (low to high, residuals)", breaks = c(-.4, -.2, 0, .2, .4)) + 
        theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
        coord_cartesian(xlim = c(-.4, .4), ylim = c(-.4, .4)) + 
        scale_color_discrete(breaks = c("Greatest", "Silent", "Boomers", "Gen X", "Millennials"), 
                             labels = c("Greatest (pre-1928)", "Silent (1928-1945)", "Boomers (1946-1964)", 
                                        "Gen X (1965-1980)", "Millennials (1981-1996)"))
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
