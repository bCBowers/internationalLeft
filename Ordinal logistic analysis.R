### Import data

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

url <- getURL("https://raw.githubusercontent.com/Jklein29/internationalLeft/master/WVS_viz_vars.csv")
W_EVS <- read.csv(text = url)

# Create racial resentment factor variable

W_EVS <- W_EVS %>% mutate(RacRes = case_when(rac_ideo == 0 ~ "Very low", 
                                             rac_ideo > 0 & rac_ideo <= .25 ~ "Low", 
                                             rac_ideo > .25 & rac_ideo <= .5 ~ "Moderate", 
                                             rac_ideo > .5 & rac_ideo <= .75 ~ "High", 
                                             rac_ideo > .75 ~ "Very high")) %>% 
  mutate(RacRes = factor(RacRes, levels = c("Very low", "Low", "Moderate", "High", "Very high")))

# Ordinal logistic regression

library(MASS)

country_list <- W_EVS %>% dlply('country')

lapply(country_list, function(x) {
  models <- polr(RacRes ~ ec_ideo, data = x, weights = S017, Hess = T)
  ctable <- coef(summary(models))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(ctable)
})

lapply(country_list, function(x) {
  models <- polr(RacRes ~ ec_ideo + edu + relig, data = x, weights = S017, Hess = T)
  ctable <- coef(summary(models))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  return(ctable)
})

