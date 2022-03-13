## global.R ##

# Loading libraries
library(visNetwork)
library(DT)
library(igraph)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(varhandle)
library(rworldmap)
library(googleVis)
library(dplyr)
library(highcharter)
library(tidyverse)

### LOADING AND PREPARING THE DATA ###
# Reading data
dt.athletes.events <- fread("athlete_events.csv")
dt.regions <- fread("noc_regions.csv")

# Replace NAs in region (i.e. ROT, TUV, UNK)
dt.regions[is.na(region)]$region = dt.regions[is.na(region)]$notes

# Merge participation data with region data
dt.olympics <- merge(dt.athletes.events, 
                     dt.regions[, list(NOC, region)],
                     by = 'NOC', 
                     all = TRUE)

# Replace NAs in region for dt.olympics
# NOC of Singapore was changed from SIN to SGP
# This was not included in dt.regions
dt.olympics[NOC == "SGP"]$region = "Singapore"

# Change USA and UK region to match rworldmap package
dt.olympics[NOC == "USA"]$region = "United States"
dt.olympics[NOC == "UK"]$region = "United Kingdom"

# Remove rows with NA as Event, these are useless
dt.olympics <- dt.olympics[!is.na(Event)]



### HERE THE DATA FOR THE WORLDMAP IS MADE ###
# Create new data table to modify for the world map
dt.all.data <- dt.olympics

# We first transform the medal column into 3 columns
# containing binary data for each medal sort
medals <- to.dummy(dt.all.data $Medal, "medals")
dt.country.medals <- cbind(dt.all.data , medals)

# Next we calculate the total medals per country per kind and rename the column
dt.bronze <- aggregate(dt.country.medals$medals.Bronze,
                       by = list(region = dt.country.medals$region), FUN = sum)
names(dt.bronze)[names(dt.bronze) == "x"] <- "Bronze_medals"

dt.silver <- aggregate(dt.country.medals$medals.Silver,
                       by = list(region = dt.country.medals$region), FUN = sum)
names(dt.silver)[names(dt.silver) == "x"] <- "Silver_medals"

dt.gold <- aggregate(dt.country.medals$medals.Gold,
                     by = list(region = dt.country.medals$region), FUN = sum)
names(dt.gold)[names(dt.gold) == "x"] <- "Gold_medals"

# We know combine the separate medal data tables back into one data table
dt.country.medals <- cbind(dt.bronze, dt.silver, dt.gold)

dt.country.medals <- subset(dt.country.medals, select = 
                              c(region, Bronze_medals, Silver_medals, Gold_medals))

dt.country.medals$Total_medals <- rowSums(dt.country.medals[, c("Bronze_medals","Silver_medals", "Gold_medals")])

# Change the column names for rworldmap compatability
colnames(dt.country.medals)[1] <- "Country"
dt.country.medals$Country <- as.character(dt.country.medals$Country)
dt.country.medals$Country



### NETWORK EXPLORATION ###
# Network 1: Games and athletes after 2010 for boxing
# Creating the right data frame
dt.graph.games.final <- dt.olympics[Sport == "Boxing",]
dt.graph.games <- dt.graph.games.final[Year >= 2010,]

# Building the graph
dt.all.athletes <- dt.graph.games[, list(name = unique(Name), type = TRUE)]
dt.all.games <- dt.graph.games[, list(name = unique(Games), type = FALSE)]
dt.vertices <- rbind(dt.all.athletes, dt.all.games)
g.olympics.boxing.2010 <- graph.data.frame(dt.graph.games[, list(Games, Name)],
                               directed = F,
                               vertices = dt.vertices)

# Calculating centrality measures (degree, closenss, betweenness, eigenvector)
V(g.olympics.boxing.2010)$degree      <- degree(g.olympics.boxing.2010)
V(g.olympics.boxing.2010)$closeness   <- closeness(g.olympics.boxing.2010)
V(g.olympics.boxing.2010)$betweenness <- betweenness(g.olympics.boxing.2010)
V(g.olympics.boxing.2010)$evcent      <- evcent(g.olympics.boxing.2010)$vector


# Network 2: Games and athletes football after 2010
# Creating the right data frame
dt.graph.football <- dt.olympics[Sport == "Football",]
dt.graph.football.2010 <-dt.graph.football[Year >= 2010]

# Obtaining the vertices
dt.all.athletes.football.2010 <- 
  dt.graph.football.2010[, list(name = unique(Name), type = TRUE)]

dt.all.games.football.2010 <- 
  dt.graph.football.2010[, list(name = unique(Games), type = FALSE)]

# Get all vertices and combine them
dt.vertices.football.2010 <- rbind(dt.all.athletes.football.2010,
                                   dt.all.games.football.2010)

# Building the graph
g.olympics.football.2010 <- 
  graph.data.frame(dt.graph.football.2010[, list(Games, Name)],
                                             directed = F,
                                             vertices = dt.vertices.football.2010)

# Calculating centrality measures (degree, closenss, betweenness, eigenvector)
V(g.olympics.football.2010)$degree      <- degree(g.olympics.football.2010)
V(g.olympics.football.2010)$closeness   <- closeness(g.olympics.football.2010)
V(g.olympics.football.2010)$betweenness <- betweenness(g.olympics.football.2010)
V(g.olympics.football.2010)$evcent      <- evcent(g.olympics.football.2010)$vector

# Show only the athletes that participated in both olympics: boxing
g.olympics.boxing.2010.subgraph <- 
  induced.subgraph(g.olympics.boxing.2010, V(g.olympics.boxing.2010)[degree > 1])

# Show only the athletes that participated in both olympics: Football
g.olympics.football.2010.subgraph <- 
  induced.subgraph(g.olympics.football.2010, V(g.olympics.football.2010)[degree > 1])

# Tables descriptive athletes boxing
dt.desciptives.table.boxing <- 
  data.table(get.data.frame(g.olympics.boxing.2010, "vertices"))

# Tables descriptive athletes football
dt.desciptives.table.football <- 
  data.table(get.data.frame(g.olympics.football.2010, "vertices"))
