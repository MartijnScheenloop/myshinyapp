## global.Rr ##

library(shinydashboard)
library(shiny)
library(shinyWidgets)

library(data.table)

# Load the data
dt.athletes.events <- fread("athlete_events.csv")
dt.regions <- fread("noc_regions.csv")

# Merge all data
dt.olympics <- merge(dt.athletes.events, dt.regions[, list(NOC, region)],
                     by = 'NOC', 
                     all = TRUE)

