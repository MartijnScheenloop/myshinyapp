## global.Rr ##

library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(varhandle)
library(rworldmap)
library(googleVis)
library(dplyr)

# Load the data
dt.athletes.events <- fread("athlete_events.csv")
dt.regions <- fread("noc_regions.csv")

# Replace NAs in region (ROT, TUV, UNK)
dt.regions[is.na(region)]$region = dt.regions[is.na(region)]$notes

# Merge all data
dt.olympics <- merge(dt.athletes.events, dt.regions[, list(NOC, region)],
                     by = 'NOC', 
                     all = TRUE)

# Replace NAs in region for dt.olympics
# NOC of Singapore was changed from SIN to SGP
# and UK to United Kingdom 
# This was not included in dt.regions
dt.olympics[NOC == "SGP"]$region = "Singapore"

# Change USA and UK to match rworldmap package
dt.olympics[NOC == "USA"]$region = "United States"
dt.olympics[NOC == "UK"]$region = "United Kingdom"

# Remove rows with NA as Event, are useless
dt.olympics <- dt.olympics[!is.na(Event)]


### Here the data for the worldmap is made ###

# Create new data table to modify for the world map
dt.all.data <- dt.olympics

# We first trasnform the medal column into 3 columns
# containing binary data for each medal sort
medals <- to.dummy(dt.all.data $Medal, "medals")
dt.country.medals <-cbind(dt.all.data , medals)

# Next we calculate the total medals per country and rename
# the column
dt.bronze <- aggregate(dt.country.medals$medals.Bronze,
                       by=list(region=dt.country.medals$region), FUN=sum)
names(dt.bronze)[names(dt.bronze)== "x"] <- "Bronze_medals"

dt.silver <- aggregate(dt.country.medals$medals.Silver,
                       by=list(region=dt.country.medals$region), FUN=sum)
names(dt.silver)[names(dt.silver)== "x"] <- "Silver_medals"

dt.gold <- aggregate(dt.country.medals$medals.Gold,
                     by=list(region=dt.country.medals$region), FUN=sum)
names(dt.gold)[names(dt.gold)== "x"] <- "Gold_medals"

# We know combine the separate medal data tables back into one data table
dt.country.medals <- cbind(dt.bronze, dt.silver, dt.gold)

dt.country.medals <- subset(dt.country.medals, select = 
                              c(region, Bronze_medals, Silver_medals, Gold_medals))

dt.country.medals$Total_medals = rowSums(dt.country.medals
                                         [,c("Bronze_medals", "Silver_medals", "Gold_medals")])

# Change the column names for rworldmap compatability
colnames(dt.country.medals)[1] = "Country"
dt.country.medals$Country = as.character(dt.country.medals$Country)
dt.country.medals$Country



