## global.R ##

library(data.table)

# Load the data
dt.athletes.events <- fread("athlete_events.csv")
dt.regions <- fread("noc_regions.csv")

# Merge all data
dt.olympics <- merge(dt.athlete.events, dt.regions[, list(NOC, region)],
                     by = 'NOC', 
                     all = TRUE)

