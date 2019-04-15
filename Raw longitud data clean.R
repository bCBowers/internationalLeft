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

Raw <- read.csv(unz("W_EVS.csv.zip", "W_EVS.csv"))

W_EVS <- Raw

## Code countries

# Import country codes 

wvs_git <- read_html("https://github.com/xmarquez/WorldValuesSurvey/blob/master/Country%20codes%20for%20WVS%20wave%206.csv")

countryCodes <- wvs_git %>% 
  html_nodes(".js-csv-data") %>%
  html_table(header = TRUE)
countryCodes <- countryCodes[[1]]

countryCodes <- countryCodes %>% select(-c(1))

# Replace country codes with countries

W_EVS <- W_EVS %>% left_join(countryCodes, by = c("S003" = "V2"))

W_EVS <- W_EVS %>% mutate(country = replace(country, country == "Czech Rep.", "Czech Republic"))
W_EVS <- W_EVS %>% mutate(country = replace(country, country == "North Ireland", "Northern Ireland"))

# Import OECD countries

wiki <- read_html("https://en.wikipedia.org/wiki/OECD")

OECD <- wiki %>% 
  html_nodes("table.wikitable:nth-child(71)") %>%
  html_table(header = TRUE)
OECD <- OECD[[1]]

# Filter OECD countries

W_EVS <- W_EVS %>% filter(country %in% OECD$Country | country == "Great Britain" | country == "Northern Ireland")

# Clean data

W_EVS <- W_EVS %>%  mutate(A124_02 = replace(A124_02, A124_02 == 0, 2), 
                           A124_06 = replace(A124_06, A124_06 == 0, 2), 
                           C002 = replace(C002, C002 == 2, 4)) %>% 
  mutate(C002 = replace(C002, C002 == 3, 2)) %>% 
  mutate(C002 = replace(C002, C002 == 4, 3))

W_EVS[, c("E035", "E036", "E037", "E039", "A124_02", "A124_06", "C002", "X025", "F028", 
          "X001", "X002", "X003", "E033", "E179_01", "E181_01")] <- 
  select(W_EVS, E035:E037, E039, A124_02, A124_06, C002, X025, F028, X001:X003, E033, E179_01, E181_01) %>% 
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

# Create racial resentment factor variable

W_EVS <- W_EVS %>% mutate(RacRes = case_when(rac_ideo == 0 ~ "Very low", 
                                             rac_ideo > 0 & rac_ideo <= .25 ~ "Low", 
                                             rac_ideo > .25 & rac_ideo <= .5 ~ "Moderate", 
                                             rac_ideo > .5 & rac_ideo <= .75 ~ "High", 
                                             rac_ideo > .75 ~ "Very high")) %>% 
  mutate(RacRes = factor(RacRes, levels = c("Very low", "Low", "Moderate", "High", "Very high")))

W_EVS <- W_EVS %>% mutate(RacRes_n = case_when(RacRes == "Very low" ~ 0, 
                                               RacRes == "Low" ~ .25, RacRes == "Moderate" ~ .5, 
                                               RacRes == "High" ~ .75, RacRes == "Very high" ~ 1))

# ideological vars

W_EVS <- W_EVS %>% mutate(self_ideo = E033)
W_EVS <- W_EVS %>% mutate(vote_ideo = case_when(!is.na(E179_01) ~ E179_01, 
                                                is.na(E179_01) ~ E181_01))

# generation

W_EVS <- W_EVS %>% mutate(generation = case_when(X002 < 1928 ~ "Greatest", 
                                                 X002 >= 1928 & X002 < 1946 ~ "Silent", 
                                                 X002 >= 1946 & X002 < 1965 ~ "Boomers", 
                                                 X002 >= 1965 & X002 < 1981 ~ "Gen X", 
                                                 X002 >= 1981 & X002 < 1997 ~ "Millennials", 
                                                 X002 >= 1997 ~ "Gen Z"))

# Creation of country-wave variable

W_EVS <- W_EVS %>% mutate(country_wave = paste(country, S020))

# Export W_EVS clean

write.csv(W_EVS, "W_EVS_clean.csv")

