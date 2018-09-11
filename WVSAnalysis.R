library(ggplot2)
library(ggrepel)
library(dplyr)

# General image formatting
opts_string = {theme(axis.text.x=element_text(size=rel(2),angle=0),
                     axis.text.y=element_text(size=rel(2)),
                     legend.text=element_text(size=rel(2)),
                     title=element_text(size=rel(2)),
                     panel.background=element_blank(),
                     panel.border=element_rect(color='black',fill=NA),
                     panel.grid.major=element_line(colour='grey20',linetype='dotted'),
                     panel.grid.minor=element_line(color='grey',linetype='dotted'))}

# Read in input files
WVSresults <- read.csv('WVS_data.csv')
countryCodes <- read.csv('CountryCodes.csv')
continent <- read.csv('Continent.csv')

# Replace country codes with countries and apply weighting to indices
WVSresults <- merge(countryCodes, WVSresults, by.x='Code', by.y='V2')
WVSresults <- select(WVSresults, -c(Code))

WVSresults$weightedRac <- WVSresults$rac * WVSresults$V258
WVSresults$weightedEc <- WVSresults$ec * WVSresults$V258

countryRac <-aggregate(WVSresults$weightedRac, by=list(Country=WVSresults$Country), FUN=sum)
countryEc <-aggregate(WVSresults$weightedEc, by=list(Country=WVSresults$Country), FUN=sum)
countryWeights <-aggregate(WVSresults$V258, by=list(Country=WVSresults$Country), FUN=length) # length of weights rather than sum because sum would cancel out the weighting

colnames(countryRac) <- c('Country', 'weightedRac')
colnames(countryEc) <- c('Country', 'weightedEc')
colnames(countryWeights) <- c('Country', 'weights')

aggData <- merge(countryRac, countryEc, by='Country')
aggData <- merge(aggData, countryWeights, by='Country')
aggData$weightedRac <- aggData$weightedRac / aggData$weights
aggData$weightedEc <- aggData$weightedEc / aggData$weights

# Add in continent information for visualization purposes
aggData <- merge(aggData, continent, by='Country')

# Build scatter plot of indices
ggplot(aggData, aes(x=weightedRac, y=weightedEc, color=Continent)) + geom_point() +
  xlim(.1,.75) + ylim(.3,.6) +
  geom_label_repel(aes(label = Country),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') + xlab('Racial Index') +ylab('Economic Index') +
  theme_classic() + opts_string
ggsave('internationalLeft.png',units=c('cm'),width=50,height=50)