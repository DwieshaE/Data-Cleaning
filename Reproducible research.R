Read in data
storm1 <- read.csv("C:/Users/Eric/Desktop/Reproducible research/stormData.csv.bz2", header = TRUE, sep = ",")

str(storm1)


length(unique(storm$EVTYPE))

# translate all letters to lowercase
event_types <- tolower(storm$EVTYPE)
# replace all punct. characters with a space
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
length(unique(event_types))

storm$EVTYPE <- event_types

library(plyr)
casualties <- ddply(storm, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused most death and injury
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 10)

Top 10 events that caused largest number of deaths are:

fatal_events[, c("EVTYPE", "fatalities")]

Top 10 events that caused largest number of deaths are:

injury_events[, c("EVTYPE", "injuries")]

Greatest economic consequences of weather events
We need to look at the property damage and crop damage data reportings. From the data, the property damage is represented with two fields, a number PROPDMG in dollars and the exponent PROPDMGEXP. Similarly, the crop damage is represented using two fields, CROPDMG and CROPDMGEXP. The first step in the analysis is to calculate the property and crop damage for each event.

exp_transform <- function(e) {
    # h -> hundred, k -> thousand, m -> million, b -> billion
    if (e %in% c('h', 'H'))
        return(2)
    else if (e %in% c('k', 'K'))
        return(3)
    else if (e %in% c('m', 'M'))
        return(6)
    else if (e %in% c('b', 'B'))
        return(9)
    else if (!is.na(as.numeric(e))) # if a digit
        return(as.numeric(e))
    else if (e %in% c('', '-', '?', '+'))
        return(0)
    else {
        stop("Invalid exponent value.")
    }
}

prop_dmg_exp <- sapply(storm$PROPDMGEXP, FUN=exp_transform)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(storm$CROPDMGEXP, FUN=exp_transform)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_exp)
# Compute the economic loss by event type
library(plyr)
econ_loss <- ddply(storm, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)
=[
Top 10 events that caused most property damage (in dollars) are as follows

prop_dmg_events[, c("EVTYPE", "prop_dmg")]

Similarly, the events that caused biggest crop damage are

crop_dmg_events[, c("EVTYPE", "crop_dmg")]

Results
Health Impact
The following plot shows top dangerous weather event types.

library(ggplot2)
library(gridExtra)

p1 <- ggplot(data=fatal_events,
             aes(x=reorder(EVTYPE, fatalities), y=fatalities, fill=fatalities)) +
    geom_bar(stat="identity") +
    coord_flip() +
    ylab("Total number of fatalities") +
    xlab("Event type") +
    theme(legend.position="none")

p2 <- ggplot(data=injury_events,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    ylab("Total number of injuries") +
    xlab("Event type") +
    theme(legend.position="none")

grid.arrange(p1, p2, main="Top deadly weather events in the US (1950-2011)")

Conclusions
Tornadoes cause most number of deaths and injuries among all event types. There are more than 5,000 deaths and more than 10,000 injuries in the last 60 years in US, due to tornadoes.

The other event types that are most dangerous with respect to population health are excessive heat and flash floods.


Economic impact of weather events
The following plot shows the most severe weather event types with respect to economic cost that they have costed since 1950s.

library(ggplot2)
library(gridExtra)
# Set the levels in order
p1 <- ggplot(data=prop_dmg_events,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
    geom_bar(stat="identity") +
    coord_flip() +
    xlab("Event type") +
    ylab("Property damage in dollars (log-scale)") +
    theme(legend.position="none")

p2 <- ggplot(data=crop_dmg_events,
             aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg)) +
    geom_bar(stat="identity") +
    coord_flip() + 
    xlab("Event type") +
    ylab("Crop damage in dollars") + 
    theme(legend.position="none")

grid.arrange(p1, p2, main="Weather costs to the US economy (1950-2011)")

Conclusion
The data shows that flash floods and thunderstorm winds cost the largest property damages among weather-related natural diseasters.

The most severe weather event in terms of crop damage is the drought. In the last half century, the drought has caused more than 10 billion dollars damage. Other severe crop-damage-causing event types are floods and hails.



rpub.com/jmlee/storm

