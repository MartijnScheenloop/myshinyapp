## global.Rr ##

library(shinydashboard)
library(shiny)
library(shinyWidgets)

# Load the data
athletes_events = read.csv(file = "athlete_events.csv")
noc_regions = read.csv(file = "noc_regions.csv")

# Transform data into data frame
df.athletes = data.frame(athletes_events)
df.nocregions = data.frame(noc_regions)

# Merge the data frames
df.all.data.merged = merge(df.athletes, df.nocregions)
