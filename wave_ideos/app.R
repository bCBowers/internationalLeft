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
library(gridExtra)
library(shinyBS)

url <- getURL("https://raw.githubusercontent.com/jordan-klein/internationalLeft/master/W_EVS_clean.csv")
W_EVS <- read.csv(text = url)

cw_list <- W_EVS %>% dlply('country_wave')

# Create coefficient matrix

lapply(cw_list, function(x) {
  filter(x, !is.na(self_ideo) & !is.na(ec_ideo))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(self_ideo ~ ec_ideo, data = x, weights = S017)
    return(data.frame(s_id_econ = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> s_id_econ

lapply(cw_list, function(x) {
  filter(x, !is.na(self_ideo) & !is.na(RacRes_n))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(self_ideo ~ RacRes_n, data = x, weights = S017)
    return(data.frame(s_id_rac = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> s_id_rac

id_ec_rac <- inner_join(s_id_econ, s_id_rac)

combined <- sort(union(levels(W_EVS$country_wave), levels(id_ec_rac$country_wave)))

left_join(mutate(id_ec_rac, country_wave = factor(country_wave, levels = combined)), 
          mutate(unique(dplyr::select(W_EVS, country_wave, country, S020)), 
                 country_wave = factor(country_wave, levels = combined))) -> id_ec_rac

## Create separate variables for each wave

id_list <- id_ec_rac %>% dlply("country")

lapply(id_list, function(x) {
  return(data.frame(wave_no = seq(1:nrow(x)), country_wave = x[1]))
}) %>% 
  do.call(rbind.data.frame, .) %>% 
  left_join(id_ec_rac, .) -> id_ec_rac

select(id_ec_rac, s_id_econ, wave_no, country) %>% 
  spread(wave_no, s_id_econ) -> econ_waves 

names(econ_waves) <- c("country", "id_ec_1", "id_ec_2", "id_ec_3", 
                       "id_ec_4", "id_ec_5", "id_ec_6")

select(id_ec_rac, s_id_rac, wave_no, country) %>% 
  spread(wave_no, s_id_rac) -> rac_waves 

names(rac_waves) <- c("country", "id_rac_1", "id_rac_2", "id_rac_3", 
                      "id_rac_4", "id_rac_5", "id_rac_6")

left_join(id_ec_rac, econ_waves) %>% 
  left_join(rac_waves) -> id_viz

# UI

ui <- fluidPage(
  titlePanel("Effect of Economic Ideology vs Racial Resentment on Ideological Self-Placement"), 
  sidebarLayout(position = "right", 
                sidebarPanel(
                  checkboxGroupInput("checkGroup", label = "Country", 
                                     choices = levels(id_viz$country), selected = "United States")),
                mainPanel(plotOutput("plot", hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce")), 
                          uiOutput("hover_info"))))


# Server

server <- function(input, output) {
  
  plot_data <- reactive({
    filter(id_viz) %>% 
      filter(country == input$checkGroup)
  })
  
  output$plot <- renderPlot({
    plot_data() %>% 
    ggplot(aes(x = s_id_econ, y = s_id_rac, colour = country, labels = country_wave)) + 
      geom_point(aes(x = id_ec_1, y = id_rac_1)) + 
      geom_point(aes(x = id_ec_2, y = id_rac_2)) + 
      geom_point(aes(x = id_ec_3, y = id_rac_3)) +
      geom_point(aes(x = id_ec_4, y = id_rac_4)) +
      geom_point(aes(x = id_ec_5, y = id_rac_5)) +
      geom_point(aes(x = id_ec_6, y = id_rac_6)) +
      geom_segment(aes(x = id_ec_1, y = id_rac_1, 
                       xend = id_ec_2, yend = id_rac_2), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_2, y = id_rac_2, 
                       xend = id_ec_3, yend = id_rac_3), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_3, y = id_rac_3, 
                       xend = id_ec_4, yend = id_rac_4), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_4, y = id_rac_4, 
                       xend = id_ec_5, yend = id_rac_5), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_5, y = id_rac_5, 
                       xend = id_ec_6, yend = id_rac_6), 
                   arrow = arrow(length = unit(.1, "inches"))) +
      labs(caption = "Scatterplot with trends over time", 
           colour = "Country") + 
      scale_x_continuous("Effect of economic ideology on ideological self-placement", breaks = c(-4, -2, 0, 2, 4, 6, 8)) + 
      scale_y_continuous("Effect of racial resentment on ideological self-placement", breaks = c(-2, -1, 0, 1, 2, 3, 4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(-2, 8), ylim = c(-2, 3))
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(plot_data(), hover, threshold = 5, maxpoints = 1)[, 1]
    
    req(nrow(point) != 0)
    
    paste(point)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



server <- function(input, output) {
  
  plot_data <- reactive({
    filter(id_viz) %>% 
      filter(country == input$checkGroup)
  })
  
  output$plot <- renderPlot({
    plot_data() %>% 
      ggplot(aes(x = s_id_econ, y = s_id_rac, colour = country, labels = country_wave)) + 
      geom_point(aes(x = id_ec_1, y = id_rac_1)) + 
      geom_point(aes(x = id_ec_2, y = id_rac_2)) + 
      geom_point(aes(x = id_ec_3, y = id_rac_3)) +
      geom_point(aes(x = id_ec_4, y = id_rac_4)) +
      geom_point(aes(x = id_ec_5, y = id_rac_5)) +
      geom_point(aes(x = id_ec_6, y = id_rac_6)) +
      geom_segment(aes(x = id_ec_1, y = id_rac_1, 
                       xend = id_ec_2, yend = id_rac_2), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_2, y = id_rac_2, 
                       xend = id_ec_3, yend = id_rac_3), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_3, y = id_rac_3, 
                       xend = id_ec_4, yend = id_rac_4), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_4, y = id_rac_4, 
                       xend = id_ec_5, yend = id_rac_5), 
                   arrow = arrow(length = unit(.1, "inches"))) + 
      geom_segment(aes(x = id_ec_5, y = id_rac_5, 
                       xend = id_ec_6, yend = id_rac_6), 
                   arrow = arrow(length = unit(.1, "inches"))) +
      labs(caption = "Scatterplot with trends over time", 
           colour = "Country") + 
      scale_x_continuous("Effect of economic ideology on ideological self-placement", breaks = c(-4, -2, 0, 2, 4, 6, 8)) + 
      scale_y_continuous("Effect of racial resentment on ideological self-placement", breaks = c(-2, -1, 0, 1, 2, 3, 4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
      coord_cartesian(xlim = c(-2, 8), ylim = c(-2, 3))
  })
  
  displayed_text <- reactive({
    req(input$plot_hover)
    hover <- input$plot_hover
    dist <- sqrt((hover$x - plot_data()$s_id_econ)^2 + (hover$y - plot_data()$s_id_rac)^2)
    
    if(min(dist) < 0.3) {
      as.character(plot_data()$country_wave[which.min(dist)])
    } else {
      NULL
    }
  })
  
  output$hover_info <- renderPrint({
    req(displayed_text())
    
    displayed_text()
  })
}
