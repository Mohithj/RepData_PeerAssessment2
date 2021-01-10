## Downloading the file and reading it into the object data.

if (file.exists("StormData.csv.bz2") == FALSE){
  url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url = url, destfile = "StormData.csv.bz2", method = "auto")
}
data <- read.csv("StormData.csv.bz2")

## Checking out the different events.

levels(factor(data$EVTYPE))

## Some rows contain lowercase strings and therefore converting everything to upper in order to save some discrepancies later in the analysis.
## There are many entries with variations in he name, so converting everything to a common term for the same reason as above.
## Converting just the EVTYPE WILDFIRES because it is the most prominent one.
## Combining both injuries and fatalities into a single variable assuming that both affects the population health.

data$EVTYPE <- toupper(data$EVTYPE)
data$EVTYPE <- gsub(".*WILD.*","WILDFIRES",data$EVTYPE, ignore.case = TRUE)
data$HEALTH <- data$INJURIES + data$FATALITIES

## The property damage cost is labeled with different characters to denote million, billion, etc.
## Checking out the different labels.

levels(factor(data$PROPDMGEXP))

## Replacing the labels with its numeric equivalent and also replacing some special characters so It would not hinder the analysis.

data$PROPDMGEXP <- gsub("^1", 10, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^2", 100, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^3", 1000, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^4", 10000, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^5", 1e+105, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^6", 1e+106, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^7", 1e+107, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^8", 1e+108, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^B", 1e+109, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^H", 100, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^K", 1000, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^M", 1e+106, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^$", 1, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^\\-", 1, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^\\?", 1, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^\\+", 1, data$PROPDMGEXP, ignore.case = TRUE)
data$PROPDMGEXP <- gsub("^0", 1, data$PROPDMGEXP, ignore.case = TRUE)

## Same with the crop damage.

levels(factor(data$CROPDMGEXP))

data$CROPDMGEXP <- gsub("^2", 100, data$CROPDMGEXP, ignore.case = TRUE)
data$CROPDMGEXP <- gsub("^B", 1e+109, data$CROPDMGEXP, ignore.case = TRUE)
data$CROPDMGEXP <- gsub("^K", 1000, data$CROPDMGEXP, ignore.case = TRUE)
data$CROPDMGEXP <- gsub("^M", 1e+106, data$CROPDMGEXP, ignore.case = TRUE)
data$CROPDMGEXP <- gsub("^0", 1, data$CROPDMGEXP, ignore.case = TRUE)
data$CROPDMGEXP <- gsub("^$", 1, data$CROPDMGEXP, ignore.case = TRUE)
data$CROPDMGEXP <- gsub("\\?", 1, data$CROPDMGEXP, ignore.case = TRUE)

## Converting the string into numeric and multiplying it to get the actual damage estimate.

data$PROPDMGEXP <- as.numeric(data$PROPDMGEXP)
data$CROPDMGEXP <- as.numeric(data$CROPDMGEXP)

data$PROPDMG <- data$PROPDMG * data$PROPDMGEXP
data$CROPDMG <- data$CROPDMG * data$CROPDMGEXP

## Assuming that both property damage and crop damage are factors affecting the economy. So, it is combined into a single variable.

data$DAMAGE <- data$PROPDMG + data$CROPDMG

## The total injuries/fatalities is calculated for each event type by using tapply().
## Only those event types are selected where the total injuries/fatalities exceed 100 as there are many event types.

htot <- sort(tapply(data$HEALTH, factor(data$EVTYPE), sum, na.rm = T), 
             decreasing = TRUE)
htot <- htot[htot > 100]

## The average injuries/fatalities is calculated for each event type and only some event types are selected for the same reason as above.

havg <- sort(tapply(data$HEALTH, factor(data$EVTYPE), mean, na.rm = T), 
             decreasing = TRUE)
havg <- havg[havg > 5]

## A dataframe is created for plotting a bar plot.

dfhtot <- data.frame(Event = names(htot), Total.Injury.Fatality = htot,
                     row.names = NULL)
dfhavg <- data.frame(Event = names(havg), Avg.Injury.Fatality = havg, 
                     row.names = NULL)

library(ggplot2)

phtot <- ggplot(data = dfhtot, aes(Total.Injury.Fatality,
          reorder(Event, Total.Injury.Fatality))) + geom_col() + 
  ggtitle("Total Injuries/Fatalities across US \n from 1950 to 2011") + 
        xlab("Events") + ylab("Total Injuries/Fatalities") +
              coord_cartesian(xlim = c(0,9000))

phavg <- ggplot(data = dfhavg, aes(reorder(Event, Avg.Injury.Fatality),
          Avg.Injury.Fatality)) + geom_col() + 
      ggtitle("Average Injuries/Fatalities across US \n from 1950 to 2011") + 
          xlab("Events") + ylab("Average Injuries/Fatalities") + coord_flip()

## The total damage and average damage is calculated for each event type by using tapply().
## Selecting only those events exceeding a particular threshold as there are many events to sift through.

dtot <- sort(tapply(data$DAMAGE, factor(data$EVTYPE), sum, na.rm = T), 
             decreasing = TRUE)
highdtot <- dtot[dtot > 1e+109]
davg <- sort(tapply(data$DAMAGE, factor(data$EVTYPE), mean, na.rm = T), 
             decreasing = TRUE)
highdavg <- davg[davg > 1e+107]

## Creating a dataframe for barplot.

highdtotdf <- data.frame(Event = names(highdtot), Total.Damage = highdtot, 
                         row.names = NULL)

highdavgdf <- data.frame(Event = names(highdavg), Avg.Damage = highdavg, 
                         row.names = NULL)

library(ggplot2)

highdtotp <- ggplot(data = highdtotdf, aes(reorder(Event, Total.Damage),
              Total.Damage)) + geom_col() + 
  ggtitle("Total Damages across US \n from 1950 to 2011") + 
  xlab("Events") + ylab("Total Damages (In USD)") + coord_flip()

highdavgp <- ggplot(data = highdavgdf, aes(reorder(Event, Avg.Damage),
              Avg.Damage)) + geom_col() + 
      ggtitle("Average Damage across US \n from 1950 to 2011") + 
  xlab("Events") + ylab("Average Damage (In USD)") + coord_flip()

