# Load packages

library(tidyverse)
library(ggrepel)
library(survey)
library(rvest)
library(plyr)
library(broom)
library(knitr)

# Import data

WVS <- read.csv('WV6_Data_r+e_vars.csv')

# Clean data

WVS <- WVS %>% filter(V96 > 0 & V97 > 0 & V98 > 0 & V99 > 0 & V37 > 0 & V39 > 0 & V46 > 0) %>%
  select(-X)

WVS <- WVS %>% add_column(ID = c(1:dim(WVS)[1]), .before = 'V2')

# Import country codes 


wvs_git <- read_html("https://github.com/xmarquez/WorldValuesSurvey/blob/master/Country%20codes%20for%20WVS%20wave%206.csv")

countryCodes <- wvs_git %>% 
  html_nodes(".js-csv-data") %>%
  html_table(header = TRUE)
countryCodes <- countryCodes[[1]]

countryCodes <- countryCodes %>% select(-c(1))

# Replace country codes with countries

WVS <- WVS %>% left_join(countryCodes)

WVS <- WVS %>% select(-V2)

# Create indices of economic ideology & racial resentment

WVS <- WVS %>% mutate(ec_ideo = (((V96 + 8)/9) * ((abs(V97 - 11) + 8)/9) * 
                                   ((V98 + 8)/9) * ((abs(V99 - 11) + 8)/9))^(1/4) - 1) %>%
  mutate(rac_ideo = ((abs(V37 - 2) + 1) * (abs(V39 - 2) + 1) * 
                       ((abs(V46 - 4) + 1)/2)^2)^(1/4) - 1)

# Import OECD countries

wiki <- read_html("https://en.wikipedia.org/wiki/OECD")

OECD <- wiki %>% 
  html_nodes("table.wikitable:nth-child(70)") %>%
  html_table(header = TRUE)
OECD <- OECD[[1]]

# Filter OECD countries

WVS_OECD <- WVS %>% filter(country %in% OECD$Country)

# Split into list by country

country_list <- WVS_OECD %>% dlply('country')

# Generate models

lapply(country_list, function(x) {
  model <- lm(rac_ideo ~ ec_ideo, data = x, weights = V258)
  out <- tidy(model)
  kable(out)
})
