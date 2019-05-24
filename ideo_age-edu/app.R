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

W_EVS <- W_EVS %>% mutate(edu_flip = 9 - edu)
W_EVS <- W_EVS %>% mutate(ideo_flip = 11 - self_ideo)

cw_list <- W_EVS %>% dlply('country_wave')

## Create coefficient matrices

lapply(cw_list, function(x) {
  filter(x, !is.na(ideo_flip) & !is.na(edu) & !is.na(X003))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(ideo_flip ~ edu + X003, data = x, weights = S017)
    return(data.frame(ideo_edu_adj = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> ideo_edu_adj

combined_edu_adj <- sort(union(levels(W_EVS$country_wave), levels(ideo_edu_adj$country_wave)))

left_join(mutate(ideo_edu_adj, country_wave = factor(country_wave, levels = combined_edu_adj)), 
          mutate(unique(dplyr::select(W_EVS, country_wave, country, S020)), 
                 country_wave = factor(country_wave, levels = combined_edu_adj))) -> ideo_edu_adj

ideo_edu_adj %>% mutate(S020 = as.Date(ISOdate(S020, 1, 1))) %>% 
  mutate(country = droplevels(country)) -> ideo_edu_adj

### UI

ui <- fluidPage(titlePanel("Association between Education and Ideological Self-Placement Over Time"), 
                sidebarLayout(position = "right", 
                              sidebarPanel(
                                checkboxGroupInput("checkGroup_edu", label = "Country", 
                                                   choices = levels(ideo_edu_adj$country), 
                                                   selected = c("United States", "Germany", "Great Britain", 
                                                                "France", "Canada", "Australia")), 
                                width = 2),
                              mainPanel(ggiraphOutput("plot_edu"), width = 10)))


### Server

server <- function(input, output) {
  
  plot_data_edu <- reactive({
    dat_edu <- filter(ideo_edu_adj)
    if (is.null(dat_edu))
      return(NULL)
    if (!is.null(input$checkGroup_edu))
      dat_edu <- dat_edu[dat_edu$country %in% input$checkGroup_edu, ]
    
    return(dat_edu)
  })
  
  output$plot_edu <- renderggiraph({
    plot_data_edu() %>% 
      ggplot(aes(x = S020, y = ideo_edu_adj, color = country)) + 
      geom_point_interactive(aes(tooltip = country_wave), size = 2) + 
      geom_path(arrow = arrow(length = unit(.1, "inches")), size = 1) + 
      geom_smooth(inherit.aes = F, aes(x = S020, y = ideo_edu_adj), data = ideo_edu_adj, method = "lm") +
      labs(caption = "Regression coefficients, controled for age \nHigher coefficients indicate stronger association between educational attainment and self-identification as left-wing") +
      scale_y_continuous("Education-Ideology Association", breaks = c(-.3, -.2, -.1, 0, .1, .2, .3)) +
      scale_x_date("Year", date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("1989-01-01", "2013-01-01"))) + 
      theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 13), 
            legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
            legend.key.size = unit(.15, "inches"), axis.text.x = element_text(size = 7.9), 
            axis.text.y = element_text(size = 7), 
            panel.grid.major.x = element_line(color = "black", size = .25), 
            plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
      coord_cartesian(xlim = c(as.Date(c("1990-01-01", "2012-01-01"))), ylim = c(-.3, .3)) +
      scale_color_manual(values = c(rainbow_hcl(length(unique(ideo_edu_adj$country)))), 
                         name = "Country", drop = F) -> gg_plot_edu
    
    girafe(code = print(gg_plot_edu), width_svg = 15)
  })
  
}

# Run App

shinyApp(ui = ui, server = server)
