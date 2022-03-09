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
# This was not included in dt.regions
dt.olympics[NOC == "SGP"]$region = "Singapore"


# New data table containing medals per country
dt.country.medals = dt.olympics %>% na.omit() %>% group_by(., region) %>% count(., Medal)

# Change column name for rworldmap
colnames(dt.country.medals)[1] = "Country"
dt.country.medals$Country = as.character(dt.country.medals$Country)
dt.country.medals$Country

# Change name of USA to match rworldmap name
dt.country.medals[dt.country.medals$Country == "USA", ]$Country = "United States"
dt.country.medals[dt.country.medals$Country == "UK", ]$Country = "United Kingdom"
dt.country.medals$Country

# create and add string of medals to dt.country.medals
dt.country.medals$s_medals = paste0(dt.country.medals$Medal, " : ", dt.country.medals$n)

dt.country.medals.final = dt.country.medals %>%
  group_by(., Country) %>%
  mutate(., country_medals_string = paste0(s_medals, collapse = " ")) %>%
  select(., country_medals_string, Country) %>%
  unique()
