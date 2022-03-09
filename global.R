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

# Merge all data
dt.olympics <- merge(dt.athletes.events, dt.regions[, list(NOC, region)],
                     by = 'NOC', 
                     all = TRUE)

# New data table containing medals per country
country_medals = dt.olympics %>% na.omit() %>% group_by(., region) %>% count(., Medal)
colnames(country_medals)[1] = "Country"
country_medals$Country = as.character(country_medals$Country)
head(country_medals)
country_medals$Country

country_medals[country_medals$Country == "USA", ]$Country = "United States"
country_medals$Country

country_medals$s_medals = paste0(country_medals$Medal, " : ", country_medals$n)

country_medals_final = country_medals %>%
  group_by(., Country) %>%
  mutate(., country_medals_string = paste0(s_medals, collapse = " ")) %>%
  select(., country_medals_string, Country) %>%
  unique()

country_medals_final

