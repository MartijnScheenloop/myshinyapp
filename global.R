## global.R ##


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

# We first transform the medal column into 3 columns
# containing binary data for each medal sort
medals <- to.dummy(dt.all.data $Medal, "medals")
dt.country.medals <- cbind(dt.all.data , medals)
dt.country.medals <- na.omit(dt.country.medals)

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

