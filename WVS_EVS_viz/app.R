#### Analysis code

# Load packages

library(tidyverse)
library(ggrepel)
library(survey)
library(rvest)
library(plyr)
library(broom)
library(knitr)
library(stargazer)

# Import data

Raw <- read.csv(unz("WVS+EVS_combined.csv.zip", "WVS+EVS_combined.csv"))

W_EVS <- Raw

# Clean data

W_EVS <- W_EVS %>%  mutate(A124_02 = replace(A124_02, A124_02 == 0, 2), 
                           A124_06 = replace(A124_06, A124_06 == 0, 2), 
                           C002 = replace(C002, C002 == 2, 4)) %>% 
  mutate(C002 = replace(C002, C002 == 3, 2)) %>% 
  mutate(C002 = replace(C002, C002 == 4, 3))

W_EVS[, c("E035", "E036", "E037", "E039", "A124_02", "A124_06", "C002", "X025", "F028")] <- 
  select(W_EVS, E035:E037, E039, A124_02, A124_06, C002, X025, F028) %>% 
  sapply(function(x) {
    replace(x, x < 1, NA)
  })

# Create indices

W_EVS <- W_EVS %>% mutate(ec_ideo = (((E035 + 8)/9) * ((abs(E036 - 11) + 8)/9) * 
                                       ((abs(E037 - 11) + 8)/9) * ((abs(E039 - 11) + 8)/9))^(1/4) - 1) %>%
  mutate(rac_ideo = ((abs(A124_02 - 2) + 1) * (abs(A124_06 - 2) + 1) * 
                       ((abs(C002 - 4) + 1)/2)^2)^(1/4) - 1)

W_EVS <- W_EVS %>% mutate(edu = X025)
W_EVS <- W_EVS %>% mutate(relig = F028)

# Split into list by country

country_list <- W_EVS %>% dlply('country')

# Fix name of northern ireland

levels(W_EVS$country)[levels(W_EVS$country) == "North Ireland"] <- "Northern Ireland"

#### Visualization

# Load packages

library(shiny)
library(ggplot2)

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
