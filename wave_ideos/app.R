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
library(wCorr)

url <- getURL("https://raw.githubusercontent.com/jordan-klein/internationalLeft/master/W_EVS_clean.csv")
W_EVS <- read.csv(text = url)

cw_list <- W_EVS %>% dlply('country_wave')

## Create coefficient matrices

# self-ideo

lapply(cw_list, function(x) {
  filter(x, !is.na(self_ideo) & !is.na(ec_ideo))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    corr <- weightedCorr(x$ec_ideo, x$self_ideo, "Pearson", x$S017)
    return(data.frame(s_id_econ = corr))
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
    corr <- weightedCorr(x$RacRes_n, x$self_ideo, "Pearson", x$S017)
    return(data.frame(s_id_rac = corr))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> s_id_rac

id_ec_rac <- inner_join(s_id_econ, s_id_rac)

combined <- sort(union(levels(W_EVS$country_wave), levels(id_ec_rac$country_wave)))

left_join(mutate(id_ec_rac, country_wave = factor(country_wave, levels = combined)), 
          mutate(unique(dplyr::select(W_EVS, country_wave, country, S020)), 
                 country_wave = factor(country_wave, levels = combined))) -> id_ec_rac

# vote ideo

lapply(cw_list, function(x) {
  filter(x, !is.na(vote_ideo) & !is.na(ec_ideo))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    corr <- weightedCorr(x$ec_ideo, x$vote_ideo, "Pearson", x$S017)
    return(data.frame(v_id_econ = corr))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> v_id_econ

lapply(cw_list, function(x) {
  filter(x, !is.na(vote_ideo) & !is.na(RacRes_n))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    corr <- weightedCorr(x$RacRes_n, x$vote_ideo, "Pearson", x$S017)
    return(data.frame(v_id_rac = corr))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(country_wave = row.names(.), .) -> v_id_rac

vote_ec_rac <- inner_join(v_id_econ, v_id_rac)

combined_v <- sort(union(levels(W_EVS$country_wave), levels(vote_ec_rac$country_wave)))

left_join(mutate(vote_ec_rac, country_wave = factor(country_wave, levels = combined_v)), 
          mutate(unique(dplyr::select(W_EVS, country_wave, country, S020)), 
                 country_wave = factor(country_wave, levels = combined_v))) -> vote_ec_rac

id_viz_vote <- vote_ec_rac %>% mutate(country = droplevels(country))

## Create separate variables for each wave- self ideo

id_list <- id_ec_rac %>% dlply("country")

lapply(id_list, function(x) {
  return(data.frame(wave_no = seq(1:nrow(x)), country_wave = x[1]))
}) %>% 
  do.call(rbind.data.frame, .) %>% 
  left_join(id_ec_rac, .) -> id_ec_rac

dplyr::select(id_ec_rac, s_id_econ, wave_no, country) %>% 
  spread(wave_no, s_id_econ) -> econ_waves 

names(econ_waves) <- c("country", "id_ec_1", "id_ec_2", "id_ec_3", 
                       "id_ec_4", "id_ec_5", "id_ec_6")

dplyr::select(id_ec_rac, s_id_rac, wave_no, country) %>% 
  spread(wave_no, s_id_rac) -> rac_waves 

names(rac_waves) <- c("country", "id_rac_1", "id_rac_2", "id_rac_3", 
                      "id_rac_4", "id_rac_5", "id_rac_6")

dplyr::select(id_ec_rac, country_wave, wave_no, country) %>% 
  spread(wave_no, country_wave) -> c_waves

names(c_waves) <- c("country", "wav_1", "wav_2", "wav_3", "wav_4", "wav_5", "wav_6")

left_join(id_ec_rac, econ_waves) %>% 
  left_join(rac_waves) %>% 
  left_join(c_waves) -> id_viz

id_viz <- id_viz %>% mutate(country = droplevels(country))

## App

# UI

ui <- navbarPage("Ideology", 
                 tabPanel("Self-Placement", 
                          titlePanel("Correlation of Economic Ideology vs. Racial Resentment with Ideological Self-Placement"), 
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          checkboxGroupInput("checkGroup", label = "Country", 
                                                             choices = levels(id_viz$country), 
                                                             selected = c("Poland", "Czech Republic", 
                                                                          "United States", "Sweden", 
                                                                          "Mexico", "Germany")), 
                                          width = 2),
                                        mainPanel(ggiraphOutput("plot"), width = 10))), 
                 tabPanel("Voting", 
                          titlePanel("Correlation of Economic Ideology vs. Racial Resentment with Voting Ideology"), 
                          sidebarLayout(position = "right", 
                                        sidebarPanel(
                                          checkboxGroupInput("checkGroup_vote", label = "Country", 
                                                             choices = levels(id_viz_vote$country), 
                                                             selected = c("Poland", "Czech Republic", 
                                                                          "Sweden", "Germany")), 
                                          width = 2),
                                        mainPanel(ggiraphOutput("plot_vote"), width = 10))))


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
      scale_x_continuous("Correlation of economic ideology with self-placement", 
                         breaks = c(-.1, 0, .1, .2, .3, .4, .5, .6)) + 
      scale_y_continuous("Correlation of racial resentment with self-placement", breaks = c(-.1, 0, .1, .2, .3, .4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), 
            legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
            legend.key.size = unit(.1, "inches")) + 
      coord_cartesian(xlim = c(-.15, .65), ylim = c(-.15, .4)) -> gg_plot    
    
    girafe(code = print(gg_plot), width_svg = 14, height_svg = 7)
  })
  
  vote_data <- reactive({
    dat_v <- filter(id_viz_vote)
    if (is.null(dat_v))
      return(NULL)
    if (!is.null(input$checkGroup_vote))
      dat_v <- dat_v[dat_v$country %in% input$checkGroup_vote, ]
    
    return(dat_v)
  })
  
  output$plot_vote <- renderggiraph({
    vote_data() %>% 
      ggplot(aes(x = v_id_econ, y = v_id_rac, color = country)) + 
      geom_point_interactive(aes(tooltip = country_wave), size = 3) + 
      scale_color_manual(values = c(rainbow_hcl(length(unique(id_viz_vote$country)))), 
                         name = "Country", drop = F) +
      labs(caption = "Scatterplot") +
      scale_x_continuous("Correlation of economic ideology with voting intention", 
                         breaks = c(-.1, 0, .1, .2, .3, .4, .5, .6)) + 
      scale_y_continuous("Correlation of racial resentment with voting intention", breaks = c(-.1, 0, .1, .2, .3, .4)) + 
      theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15), 
            legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
            legend.key.size = unit(.1, "inches")) + 
      coord_cartesian(xlim = c(-.15, .65), ylim = c(-.15, .4)) -> gg_plot_v    
    
    girafe(code = print(gg_plot_v), width_svg = 14, height_svg = 7)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
