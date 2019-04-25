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

lapply(cw_list, function(x) {
  filter(x, !is.na(self_ideo) & !is.na(ec_ideo) & !is.na(edu))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(self_ideo ~ ec_ideo + edu, data = x, weights = S017)
    return(data.frame(id_econ_edu = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> id_econ_edu

lapply(cw_list, function(x) {
  filter(x, !is.na(self_ideo) & !is.na(RacRes_n) & !is.na(edu))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(self_ideo ~ RacRes_n + edu, data = x, weights = S017)
    return(data.frame(id_rac_edu = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> id_rac_edu

id_ec_rac <- inner_join(s_id_econ, s_id_rac)

id_ec_rac_edu <- inner_join(id_econ_edu, id_rac_edu)

combined <- sort(union(levels(W_EVS$country_wave), levels(id_ec_rac$country_wave)))

combined_edu <- sort(union(levels(W_EVS$country_wave), levels(id_ec_rac_edu$country_wave)))

left_join(mutate(id_ec_rac, country_wave = factor(country_wave, levels = combined)), 
          mutate(unique(dplyr::select(W_EVS, country_wave, country, S020)), 
                 country_wave = factor(country_wave, levels = combined))) -> id_ec_rac

left_join(mutate(id_ec_rac_edu, country_wave = factor(country_wave, levels = combined_edu)), 
          mutate(unique(dplyr::select(W_EVS, country_wave, country, S020)), 
                 country_wave = factor(country_wave, levels = combined_edu))) -> id_ec_rac_edu

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

select(id_ec_rac, country_wave, wave_no, country) %>% 
  spread(wave_no, country_wave) -> c_waves

names(c_waves) <- c("country", "wav_1", "wav_2", "wav_3", "wav_4", "wav_5", "wav_6")

left_join(id_ec_rac, econ_waves) %>% 
  left_join(rac_waves) %>% 
  left_join(c_waves) -> id_viz

id_viz <- id_viz %>% mutate(country = droplevels(country))

# edu

id_list_edu <- id_ec_rac_edu %>% dlply("country")

lapply(id_list_edu, function(x) {
  return(data.frame(wave_no = seq(1:nrow(x)), country_wave = x[1]))
}) %>% 
  do.call(rbind.data.frame, .) %>% 
  left_join(id_ec_rac_edu, .) -> id_ec_rac_edu

select(id_ec_rac_edu, id_econ_edu, wave_no, country) %>% 
  spread(wave_no, id_econ_edu) -> econ_waves_edu 

names(econ_waves_edu) <- c("country", "id_ec_1", "id_ec_2", "id_ec_3", 
                           "id_ec_4", "id_ec_5", "id_ec_6")

select(id_ec_rac_edu, id_rac_edu, wave_no, country) %>% 
  spread(wave_no, id_rac_edu) -> rac_waves_edu 

names(rac_waves_edu) <- c("country", "id_rac_1", "id_rac_2", "id_rac_3", 
                          "id_rac_4", "id_rac_5", "id_rac_6")

select(id_ec_rac_edu, country_wave, wave_no, country) %>% 
  spread(wave_no, country_wave) -> c_waves_edu

names(c_waves_edu) <- c("country", "wav_1", "wav_2", "wav_3", "wav_4", "wav_5", "wav_6")

left_join(id_ec_rac_edu, econ_waves_edu) %>% 
  left_join(rac_waves_edu) %>% 
  left_join(c_waves_edu) -> id_viz_edu

id_viz_edu <- id_viz_edu %>% mutate(country = droplevels(country))

# UI

ui <- navbarPage("Model selection", 
                 tabPanel("Not adjusted for education", 
                          titlePanel("Effect of Economic Ideology vs. Racial Resentment on Ideological Self-Placement"), 
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          checkboxGroupInput("checkGroup", label = "Country", 
                                                             choices = levels(id_viz$country), 
                                                             selected = levels(id_viz$country)), width = 2),
                                        mainPanel(ggiraphOutput("plot"), width = 10))), 
                 tabPanel("Adjusted for education", 
                          titlePanel("Effect of Economic Ideology vs. Racial Resentment on Ideological Self-Placement"), 
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          checkboxGroupInput("checkGroup_edu", label = "Country", 
                                                             choices = levels(id_viz_edu$country), 
                                                             selected = levels(id_viz_edu$country)), width = 2),
                                        mainPanel(ggiraphOutput("plot_edu"), width = 10))))

# Server

server <- function(input, output) {
  
  plot_data <- reactive({
    dat <- filter(id_viz)
    if (is.null(dat))
      return(NULL)
    if (!is.null(input$checkGroup))
      dat <- dat[dat$country %in% input$checkGroup, ]
    
    return(dat)
  })
  
  output$plot <- renderggiraph({
    plot_data() %>% 
      ggplot(aes(x = s_id_econ, y = s_id_rac, color = country)) + 
      geom_point_interactive(aes(x = id_ec_1, y = id_rac_1, tooltip = wav_1)) + 
      geom_point_interactive(aes(x = id_ec_2, y = id_rac_2, tooltip = wav_2)) + 
      geom_point_interactive(aes(x = id_ec_3, y = id_rac_3, tooltip = wav_3)) +
      geom_point_interactive(aes(x = id_ec_4, y = id_rac_4, tooltip = wav_4)) +
      geom_point_interactive(aes(x = id_ec_5, y = id_rac_5, tooltip = wav_5)) +
      geom_point_interactive(aes(x = id_ec_6, y = id_rac_6, tooltip = wav_6)) +
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
      scale_color_manual(values = c(rainbow_hcl(length(unique(id_viz$country)))), 
                         name = "Country", drop = F) +
      labs(caption = "Scatterplot with trends over time") +
      scale_x_continuous("Effect of economic ideology on self-placement", breaks = c(-4, -2, 0, 2, 4, 6, 8)) + 
      scale_y_continuous("Effect of racial resentment on self-placement", breaks = c(-2, -1, 0, 1, 2, 3, 4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), 
            legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
            legend.key.size = unit(.1, "inches")) + 
      coord_cartesian(xlim = c(-2, 8), ylim = c(-2, 3)) -> gg_plot    
    
    girafe(code = print(gg_plot), width_svg = 14, height_svg = 7)
  })
  
  plot_data_edu <- reactive({
    dat_edu <- filter(id_viz_edu)
    if (is.null(dat_edu))
      return(NULL)
    if (!is.null(input$checkGroup_edu))
      dat_edu <- dat_edu[dat_edu$country %in% input$checkGroup_edu, ]
    
    return(dat_edu)
  })
  
  output$plot_edu <- renderggiraph({
    plot_data_edu() %>% 
      ggplot(aes(x = s_id_econ, y = s_id_rac, color = country)) + 
      geom_point_interactive(aes(x = id_ec_1, y = id_rac_1, tooltip = wav_1)) + 
      geom_point_interactive(aes(x = id_ec_2, y = id_rac_2, tooltip = wav_2)) + 
      geom_point_interactive(aes(x = id_ec_3, y = id_rac_3, tooltip = wav_3)) +
      geom_point_interactive(aes(x = id_ec_4, y = id_rac_4, tooltip = wav_4)) +
      geom_point_interactive(aes(x = id_ec_5, y = id_rac_5, tooltip = wav_5)) +
      geom_point_interactive(aes(x = id_ec_6, y = id_rac_6, tooltip = wav_6)) +
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
      scale_color_manual(values = c(rainbow_hcl(length(unique(id_viz_edu$country)))), 
                         name = "Country", drop = F) +
      labs(caption = "Scatterplot adjusted for education, with trends over time") +
      scale_x_continuous("Effect of economic ideology on self-placement", breaks = c(-4, -2, 0, 2, 4, 6, 8)) + 
      scale_y_continuous("Effect of racial resentment on self-placement", breaks = c(-2, -1, 0, 1, 2, 3, 4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), 
            legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
            legend.key.size = unit(.1, "inches")) + 
      coord_cartesian(xlim = c(-2, 8), ylim = c(-2, 3)) -> gg_plot_edu    
    
    girafe(code = print(gg_plot_edu), width_svg = 14, height_svg = 7)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
