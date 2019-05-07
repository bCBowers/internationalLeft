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
library(ggiraph)
library(colorspace)
library(MASS)
library(lubridate)

url <- getURL("https://raw.githubusercontent.com/jordan-klein/internationalLeft/master/W_EVS_clean.csv")
W_EVS <- read.csv(text = url)

W_EVS <- W_EVS %>% mutate(RacRes = factor(RacRes, levels = c("Very low", "Low", "Moderate", "High", "Very high")))

cw_list <- W_EVS %>% dlply('country_wave')

# Create coefficient matrix

lapply(cw_list, function(x) {
  filter(x, !is.na(RacRes) & !is.na(ec_ideo) & !is.na(edu) & !is.na(relig))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- polr(RacRes ~ ec_ideo + edu + relig, data = x, weights = S017, Hess = T)
    return(data.frame(rac_ec_adj = model$coefficients[1]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> rac_ec_adj

combined_adj <- sort(union(levels(W_EVS$country_wave), levels(rac_ec_adj$country_wave)))

left_join(mutate(rac_ec_adj, country_wave = factor(country_wave, levels = combined_adj)), 
          mutate(unique(dplyr::select(W_EVS, country_wave, country, S020)), 
                 country_wave = factor(country_wave, levels = combined_adj))) -> rac_ec_adj

rac_ec_adj %>% mutate(S020 = as.Date(ISOdate(S020, 1, 1))) %>% 
  mutate(country = droplevels(country)) -> rac_ec_adj

# UI

ui <- fluidPage(titlePanel("Association between Economic Ideology and Racial Resentment Level Over Time"), 
                sidebarLayout(position = "right", 
                              sidebarPanel(
                                checkboxGroupInput("checkGroup_adj", label = "Country", 
                                                   choices = levels(rac_ec_adj$country), 
                                                   selected = c("Australia", "Germany", "Great Britain", 
                                                                "Poland", "South Korea", "United States")), 
                                width = 2),
                              mainPanel(ggiraphOutput("plot_adj"), width = 10)))

# Server

server <- function(input, output) {
  
  plot_data_adj <- reactive({
    dat_adj <- filter(rac_ec_adj)
    if (is.null(dat_adj))
      return(NULL)
    if (!is.null(input$checkGroup_adj))
      dat_adj <- dat_adj[dat_adj$country %in% input$checkGroup_adj, ]
    
    return(dat_adj)
  })
  
  output$plot_adj <- renderggiraph({
    plot_data_adj() %>% 
      ggplot(aes(x = S020, y = rac_ec_adj, color = country)) +
      geom_point_interactive(aes(tooltip = country_wave), size = 2) + 
      geom_path(arrow = arrow(length = unit(.1, "inches")), size = 1) + 
      labs(caption = "Ordinal logistic regression coefficients, higher coefficients indicate stronger positive associations") +
      scale_y_continuous("Economic ideology-racial resentment association", 
                         breaks = c(-2.5, -2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, 2.5)) +
      scale_x_date("Year", date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("1990-01-01", "2013-01-01"))) + 
      theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 13), 
            legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
            legend.key.size = unit(.15, "inches"), axis.text.x = element_text(size = 7.9), 
            axis.text.y = element_text(size = 7), 
            panel.grid.major.x = element_line(color = "black", size = .25), 
            plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
      coord_cartesian(xlim = c(as.Date(c("1991-01-01", "2012-01-01"))), ylim = c(-2.5, 2.5)) + 
      scale_color_manual(values = c(rainbow_hcl(length(unique(rac_ec_adj$country)))), 
                         name = "Country", drop = F) -> gg_plot_adj
    
    girafe(code = print(gg_plot_adj), width_svg = 15)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
