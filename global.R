## global.R ##

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



### HERE THE DATA FOR THE WORLDMAP IS MADE ###

# Create new data table to modify for the world map
dt.all.data <- dt.olympics

# We first transform the medal column into 3 columns
# containing binary data for each medal sort
medals <- to.dummy(dt.all.data $Medal, "medals")
dt.country.medals <- cbind(dt.all.data , medals)

# Next we calculate the total medals per country and rename the column
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

dt.country.medals$Total_medals <- rowSums(dt.country.medals
                                         [,c("Bronze_medals", "Silver_medals", "Gold_medals")])

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
g.olympics <- graph.data.frame(dt.graph.games[, list(Games, Name)],
                               directed = F,
                               vertices = dt.vertices)

# Centralities of the network
# Degree 
V(g.olympics)$degree <- degree(g.olympics)

# closeness centrality
V(g.olympics)$closeness <- closeness(g.olympics)

# betweenness centrality
V(g.olympics)$betweenness <- betweenness(g.olympics)

# eigenvector centrality
V(g.olympics)$evcent <- evcent(g.olympics)$vector


# Network 2: Games and athletes football after 2010
# Creating the right data frame
dt.graph.football <- dt.olympics[Sport == "Football",]
dt.graph.football.2010 <-dt.graph.football[Year >= 2010]

# Building the graph
dt.all.athletes.football.2010 <- dt.graph.football.2010[, list(name = unique(Name),
                                                               type = TRUE)]
dt.all.games.football.2010 <- dt.graph.football.2010[, list(name = unique(Games),
                                                            type = FALSE)]
dt.vertices.football.2010 <- rbind(dt.all.athletes.football.2010,
                                   dt.all.games.football.2010)
g.olympics.football.2010 <- graph.data.frame(dt.graph.football.2010[, list(Games, Name)],
                                             directed = F,
                                             vertices = dt.vertices.football.2010)

# Centralities of the network
# Degree centrality
V(g.olympics.football.2010)$degree <- degree(g.olympics.football.2010)

# closeness centrality
V(g.olympics.football.2010)$closeness <- closeness(g.olympics.football.2010)

# betweenness centrality
V(g.olympics.football.2010)$betweenness <- betweenness(g.olympics.football.2010)

# eigenvector centrality
V(g.olympics.football.2010)$evcent <- evcent(g.olympics.football.2010)$vector

# Show only the athletes that participated in both olympics: boxing
g.olympics.subgraph <- induced.subgraph(g.olympics, V(g.olympics)[degree > 1])

# Show only the athletes that participated in both olympics: Football
g.olympics.football.2010.subgraph <- induced.subgraph(g.olympics.football.2010, V(g.olympics.football.2010)[degree > 1])

# Tables descriptive athletes
dt.desciptives.table.boxing <- data.table(get.data.frame(g.olympics, "vertices"))
dt.desciptives.table.football <- data.table(get.data.frame(g.olympics.football.2010, "vertices"))



### NETWORK ANALYSIS: REGION-EVENTS NETWORK ###
## Creating a network with regions and common events between regions as edges ##

# Get all unique regions and events and combine them
dt.vertices.regions.events <- rbind(dt.olympics[, list(name = unique(region), type = FALSE)],
                                    dt.olympics[, list(name = unique(Event), type = TRUE)])

# Bipartite graph of Olympic regions and the events they compete in
g.regions.events.bipartite <- graph_from_data_frame(dt.olympics[,list(region, Event)],
                                                    directed = FALSE,
                                                    vertices = dt.vertices.regions.events)

# Projection into the region space
g.regions.events <- bipartite.projection(g.regions.events.bipartite)$proj1

# Calculating centrality measures
V(g.regions.events)$degree      <- degree(g.regions.events)
V(g.regions.events)$closeness   <- closeness(g.regions.events)
V(g.regions.events)$betweenness <- betweenness(g.regions.events)
V(g.regions.events)$evcent      <- evcent(g.regions.events)$vector

# Creating a data.table with centrality measures
dt.regions.events.centr <- data.table(get.data.frame(g.regions.events, what = "vertices"))

# Creating data.table that can be filtered
dt.olympics.filtered <- filter(dt.olympics)#, Games == input$games, Sport == input$sport)

# Obtaining number of athletes, medals, unique events and sports per region
dt.region.numbers <- 
  dt.olympics.filtered[, .(n_athletes = length(unique(ID)),
                           n_medals = sum(!is.na(Medal)),
                           n_unique_events = length(unique(Event)),
                           n_unique_sports = length(unique(Sport)),
                           n_games = length(unique(Games))), 
                       by = region][order(region)]

# Merging the numbers per region with the centrality data.table
dt.regions.events.centr <- merge(dt.regions.events.centr, dt.region.numbers,
                                 by.x = 'name', by.y = 'region')
