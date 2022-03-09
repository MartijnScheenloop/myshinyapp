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


# New data table containing medals per country
dt.country.medals <- na.omit(dt.olympics)
dt.country.medals <- group_by(dt.country.medals, region)
dt.country.medals <- count(dt.country.medals, Medal)

# Change column name for rworldmap compatability
colnames(dt.country.medals)[1] = "Country"
dt.country.medals$Country = as.character(dt.country.medals$Country)
dt.country.medals$Country

# create and add string of medals to dt.country.medals
dt.country.medals$s_medals <- paste0(dt.country.medals$Medal, " : ", dt.country.medals$n)

dt.country.medals.final <- group_by(dt.country.medals, Country)
dt.country.medals.final <- mutate(dt.country.medals.final,
                                 country_medals_string = paste0(s_medals, collapse = " "))
dt.country.medals.final <- select(dt.country.medals.final, country_medals_string, Country)
dt.country.medals.final <- unique(dt.country.medals.final)



