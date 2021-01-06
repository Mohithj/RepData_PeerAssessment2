## Downloading the file and reading it into the object data.

if (file.exists("StormData.csv") == FALSE){
  url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url = url, destfile = "StormData.csv.bz2", method = "auto")
}
data <- read.csv("StormData.csv.bz2")


## Some observations contains lower case strings and therefore converting everything to upper in order to save some disperencies later in the analysis.
## Combining both injuries and fatalities into a single variable assuming that both affects the health population.

data$EVTYPE <- toupper(data$EVTYPE)
data$HEALTH <- data$INJURIES + data$FATALITIES

## The total injuries/fatalities is calculated for each event type by using tapply().
## Only those event types are selected where the total injuries/fatalities exceed 100 as there are many event types.

htot <- sort(tapply(data$HEALTH, factor(data$EVTYPE), sum, na.rm = T), decreasing = TRUE)
htot <- htot[htot > 100]

## The average injuries/fatalities is calculated for each event type and only some event types are selected for the same reason as above.

havg <- sort(tapply(data$HEALTH, factor(data$EVTYPE), mean, na.rm = T), decreasing = TRUE)
havg <- havg[havg > 5]

## A dataframe is created for plotting a bar plot.

dfhavg <- data.frame(Event = names(havg), Avg.Injury.Fatality = havg, row.names = NULL)

library(ggplot2)
phavg <- ggplot(data = dfhavg, aes(reorder(Event, Avg.Injury.Fatality),Avg.Injury.Fatality)) + geom_col() + coord_flip()