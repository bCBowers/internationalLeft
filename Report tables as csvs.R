### Import Data

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

W_EVS <- read.csv("W_EVS_clean.csv")

cw_list <- W_EVS %>% dlply('country_wave')

## S_ideo, race res, edu

lapply(cw_list, function(x) {
  filter(x, !is.na(self_ideo) & !is.na(RacRes_n) & !is.na(edu))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(self_ideo ~ RacRes_n + edu, data = x, weights = S017)
    return(data.frame(Coefficients = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(Country_wave = row.names(.), .) %>% 
  .[order(.$Coefficients, decreasing = T), ] -> s_id_rac

write.csv(s_id_rac, "s_id_rac.csv", row.names = F)

## S_ideo, econ ideo, edu

lapply(cw_list, function(x) {
  filter(x, !is.na(self_ideo) & !is.na(ec_ideo) & !is.na(edu))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(self_ideo ~ ec_ideo + edu, data = x, weights = S017)
    return(data.frame(Coefficients = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(Country_wave = row.names(.), .) %>% 
  .[order(.$Coefficients, decreasing = T), ] -> s_id_econ

write.csv(s_id_econ, "s_id_econ.csv", row.names = F)

## Race res, age, edu

lapply(cw_list, function(x) {
  filter(x, !is.na(RacRes_n) & !is.na(X003) & !is.na(edu))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(RacRes_n ~ X003 + edu, data = x, weights = S017)
    return(data.frame(Coefficients = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(Country_wave = row.names(.), .) %>% 
  .[order(.$Coefficients, decreasing = T), ] -> age_rac

write.csv(age_rac, "age_rac.csv", row.names = F)

## Econ ideo, age, edu

lapply(cw_list, function(x) {
  filter(x, !is.na(ec_ideo) & !is.na(X003) & !is.na(edu))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- lm(ec_ideo ~ X003 + edu, data = x, weights = S017)
    return(data.frame(Coefficients = model$coefficients[2]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(Country_wave = row.names(.), .) %>% 
  .[order(.$Coefficients, decreasing = T), ] -> age_econ

write.csv(age_econ, "age_econ.csv", row.names = F)
