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

filter(W_EVS, !is.na(self_ideo) & !is.na(rac_ideo) & !is.na(generation)) %>% 
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

ui <- fluidPage(
  titlePanel("Racial Resentment and Ideological Self-Placement"),
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
      ggplot(aes(x = rac_ideo, y = self_ideo, colour = generation)) + 
      geom_smooth(method = "glm", aes(weight = S017)) + 
      labs(caption = "Linear regression plots stratified by generation", colour = "Generation (years born)") + 
      scale_x_continuous("Racial resentment (low to high)", breaks = c(0, .25, .5, .75, 1)) + 
      scale_y_continuous("Ideological self-placement (left to right)", breaks = c(1, 4, 7, 10)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(0, 1), ylim = c(1, 10)) + 
      scale_color_discrete(breaks = c("Greatest", "Silent", "Boomers", "Gen X", "Millennials"), 
                           labels = c("Greatest (pre-1928)", "Silent (1928-1945)", "Boomers (1946-1964)", 
                                      "Gen X (1965-1980)", "Millennials (1981-1996)"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
