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

# Create reference category

sapply(country_list, function(x) {
  model <- lm(rac_ideo ~ ec_ideo, data = x, weights = S017)
  out <- tidy(model)
  kable(out)
  return(model$coefficients[2])
}) -> coeffs
median(coeffs)

W_EVS <- W_EVS %>% within(country <- relevel(country, ref = "Great Britain"))

## Modeling

# Individual country

lapply(country_list, function(x) {
  model <- lm(rac_ideo ~ ec_ideo, data = x, weights = S017)
  out <- tidy(model)
  kable(out)
})

lapply(country_list, function(x) {
  model <- lm(rac_ideo ~ ec_ideo + edu, data = x, weights = S017)
  out <- tidy(model)
  kable(out)
})

lapply(country_list, function(x) {
  model <- lm(rac_ideo ~ ec_ideo + relig, data = x, weights = S017)
  out <- tidy(model)
  kable(out)
})

lapply(country_list, function(x) {
  model <- lm(rac_ideo ~ ec_ideo + edu + relig, data = x, weights = S017)
  out <- tidy(model)
  kable(out)
})

# Full model with dummy variables

full_model <- lm(rac_ideo ~ ec_ideo*country, data = filter(W_EVS, !is.na(edu) & !is.na(relig)), weights = S017)

summary(full_model)


full_model_edu <- lm(rac_ideo ~ ec_ideo*country + edu*country, data = filter(W_EVS, !is.na(edu) & !is.na(relig)), 
                     weights = S017)

summary(full_model_edu) 

anova(full_model, full_model_edu)


full_model_rel <- lm(rac_ideo ~ ec_ideo*country + relig*country, data = filter(W_EVS, !is.na(edu) & !is.na(relig)), 
                     weights = S017)

summary(full_model_rel) 

anova(full_model, full_model_rel)

multi_model <- lm(rac_ideo ~ ec_ideo*country + edu*country + relig*country, 
                  data = filter(W_EVS, !is.na(edu) & !is.na(relig)), weights = S017)

summary(multi_model)

anova(full_model_edu, multi_model)
anova(full_model_rel, multi_model)

# Export model

stargazer(multi_model, title = "Racial resentment as a function of economic ideology", style = "demography", 
          notes = "Adjusted for educational attainment and frequency of religious services attendance")
