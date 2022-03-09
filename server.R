## server.R r##

shinyServer(function(input, output, body){
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dt.olympics
    
    # year, gender and season filter
    if (input$sex != "All") {
      data <- data[data$Sex == input$sex,]
    }
    if (input$season != "All") {
      data <- data[data$Season == input$season,]
    }
    data <- data[data$Year >= min(input$years) & data$Year <= max(input$years),]
    
    # presentation of data set, following steps:
    # 1. convert medal column to 3 binary columns per medal sort
    medals <- to.dummy(data $Medal, "medals")
    data <-cbind(data , medals)
    
    # 2. initializing dt.n.games for later use (at 6)
    dt.n.games <- data[, list(Games = unique(Games)), by = region]
    
    # 3. using aggregate function to find totals per country and change name
    dt.bronze <- aggregate(data$medals.Bronze, by=list(region=data$region), FUN=sum)
    names(dt.bronze)[names(dt.bronze)=="x"] <- "Bronze_medals"
    dt.silver <- aggregate(data$medals.Silver, by=list(region=data$region), FUN=sum)
    names(dt.silver)[names(dt.silver)=="x"] <- "Silver_medals"
    dt.gold <- aggregate(data$medals.Gold, by=list(region=data$region), FUN=sum)
    names(dt.gold)[names(dt.gold)=="x"] <- "Gold_medals"
    
    # 4. combine three aggregate functions datasets per sort of medal back into one dt
    data <- cbind(dt.bronze, dt.silver, dt.gold)
    data <- subset(data, select = c(region, Bronze_medals, Silver_medals, Gold_medals))
    data$Total_medals = rowSums(data[,c("Bronze_medals", "Silver_medals", "Gold_medals")])
    
    # 5. make separate dt consisting of countries and number of olympic events joined
    dt.n.games <- dt.n.games[, list(n_games = .N), by=region][order(region)]
    dt.n.games <- na.omit(dt.n.games)
    data <- merge(data,dt.n.games,by="region")
    
    # 6. add average of games played column
    data$Average_per_game <- data$Total_medals/data$n_games
    data$Average_per_game <- round(data$Average_per_game, digits =1)
    
    # 7. order from high to low
    data <- data[order(-data$Total_medals),]
    
    data
    
  }))
  
  dt.country.medals.final$country_medals_string = paste(dt.country.medals.final$Country[1:133], dt.country.medals.final$country_medals_string[1:133], sep = " - ")
  
  output$wm = renderGvis ( {
    gvisGeoChart(
      dt.country.medals.final,
      "Country",
      hovervar = "country_medals_string",
      options = list(region = "world", displayMode = "regions",
                     width = "1000",
                     height = "700"
      )
    )
  })

}
)
