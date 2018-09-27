library(ggplot2)
library(ggrepel)
library(dplyr)
library(lattice)  
library(nlme)
library(survey)

# General image formatting
opts_string = {theme(axis.text.x=element_text(size=rel(2),angle=0),
                     axis.text.y=element_text(size=rel(2)),
                     legend.text=element_text(size=rel(2)),
                     title=element_text(size=rel(2)),
                     panel.background=element_blank(),
                     panel.border=element_rect(color='black',fill=NA),
                     panel.grid.major=element_line(colour='grey20',linetype='dotted'),
                     panel.grid.minor=element_line(color='grey',linetype='dotted'))}

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

#################### All Countries
# Build survey design element
N = 3000000000
n = nrow(WVS)
des = svydesign(ids = ~ID, 
                weights= ~V258, 
                fpc = rep(N, n),
                data = WVS
)

# Aggregate data by Country and include continent for viz purposes
countryRac <- svyby(~rac_ideo, by=~country, design = des, FUN=svymean)
countryEc <- svyby(~ec_ideo, by=~country, design = des, FUN=svymean)
colnames(countryRac) <- c('Country', 'Racism', 'SE')
colnames(countryEc) <- c('Country', 'Economics', 'SE')
aggData <- merge(countryRac, countryEc, by='Country')

continent <- read.csv('Continent.csv')
aggData <- merge(aggData, continent, by='Country')

# Build scatter plot of indices
ggplot(aggData, aes(x=Racism, y=Economics, color=Continent)) + geom_point() +
  xlim(0,.7) + ylim(.3,.7) + ggtitle("Mean Racial and Economic Attitudes by Country") +
  geom_label_repel(aes(label = Country),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + xlab('Racial Index') +ylab('Economic Index') +
  theme_classic() + opts_string
ggsave('AllMeanIndices.png',units=c('cm'),width=50,height=50)

############################ OECD Countries
# Import OECD countries
wiki <- read_html("https://en.wikipedia.org/wiki/OECD")
OECD <- wiki %>% 
  html_nodes("table.wikitable:nth-child(70)") %>%
  html_table(header = TRUE)
OECD <- OECD[[1]]

# Filter OECD countries
agg_OECD <- aggData %>% filter(Country %in% OECD$Country)

# Build scatter plot of indices
ggplot(agg_OECD, aes(x=Racism, y=Economics, color=Continent)) + geom_point() +
  xlim(0,.6) + ylim(.35,.65) + ggtitle("Mean Racial and Economic Attitudes by OECD Country") +
  geom_label_repel(aes(label = Country),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + xlab('Racial Index') +ylab('Economic Index') +
  theme_classic() + opts_string
ggsave('OECDMeandIndices.png',units=c('cm'),width=50,height=50)


########################## Build a series of scatter plots for countries of interest
interestCountries <- c('United States', 'Spain', 'Sweden', 'Netherlands', 'South Korea', 'Japan',
                       'Poland', 'Slovenia')
selectCountries <- filter(WVS, country %in% interestCountries)

selectCountries$country = factor(selectCountries$country, levels=c('United States', 'Spain', 'Sweden', 'Netherlands',
                                                                   'Japan','Poland', 'Slovenia', 'South Korea'))

ggplot(data=selectCountries) + aes(x=jitter(rac_ideo, factor=5), y=ec_ideo, weight=V258) + geom_point() + 
  geom_smooth() + facet_wrap(~country, nrow=2) + xlab('Racial Index') +ylab('Economic Index') + 
  ggtitle("Distribution of Racial and Economic Attitudes for Interesting Countries") + opts_string
ggsave('interestingDistributions.png',units=c('cm'),width=50,height=50)
