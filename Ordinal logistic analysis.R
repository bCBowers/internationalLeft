#### Load packages & import data ####

# Load packages

library(tidyverse)
library(MASS)
library(plyr)

# Import data

W_EVS <- read.csv("W_EVS_clean.csv")

#### Data manipulation ####

# Fix racial resentment factor variable levels

W_EVS <- W_EVS %>% mutate(RacRes = factor(RacRes, levels = c("Very low", "Low", "Moderate", "High", "Very high")))

# Split by country & survey wave

cw_list <- W_EVS %>% dlply('country_wave')

#### Data Analysis ####

# Create coefficient matrix

lapply(cw_list, function(x) {
  filter(x, !is.na(RacRes) & !is.na(ec_ideo) & !is.na(edu) & !is.na(relig))
}) %>% 
  .[sapply(., function(x) {
    dim(x)[1] > 0
  })] %>% 
  lapply(function(x) {
    model <- polr(RacRes ~ ec_ideo + edu + relig, data = x, weights = S017, Hess = T)
    return(data.frame(Coefficient = model$coefficients[1]))
  }) %>% 
  do.call(rbind.data.frame, .) %>% 
  data.frame(Country_survey_year = row.names(.), .) -> c_matr

# Sort by coefficient

c_matr <- c_matr[order(c_matr$Coefficients, decreasing = T), ]

#### Export as csv ####

write.csv(c_matr, "OL_coefficients.csv", row.names = F)
