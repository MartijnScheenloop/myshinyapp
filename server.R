## server.R r##

shinyServer(function(input, output, body){
  
  ### DESCRIPTIVE STATISTICS ###
  # Filters, reactive and passed to renderTexts and table
  tbinput <- reactive({
    
    data <- dt.all.data
    
    if (input$sex != "All") {
      data <- data[data$Sex == input$sex,]
    }
    
    if (input$season != "All") {
      data <- data[data$Season == input$season,]
    }
    
    if (input$games != "All") {
      data <- data[data$Games == input$games,]
    }
    
    data <- data[data$Year >= min(input$years) & data$Year <= max(input$years),]
    data
    
  })
  
  # Upper statistics of descriptive page
  output$athletes <- renderText(paste({
    n_distinct(tbinput()$ID)
  }))
  
  output$regions <- renderText(paste({
    n_distinct(tbinput()$region)
  }))
  
  output$games <- renderText(paste({
    n_distinct(tbinput()$Games)
  }))
  
  output$sports <- renderText(paste({
    n_distinct(tbinput()$Sport)
  }))
  
  output$hosting_cities <- renderText(paste({
    n_distinct(tbinput()$City)
  }))
  
  output$total_medals <- renderText(paste({
    dt.med <- tbinput()
    dt.med <- dt.med[!is.na(Medal)]
    nrow(dt.med)
  }))
  
  # Reactive data table, reloads when input changes (therefore not in global)
  output$table <- DT::renderDataTable(DT::datatable({
    data <- tbinput()
    
    # 1. Convert medal column to 3 binary columns per medal sort
    medals <- to.dummy(data$Medal, "medals")
    data <- cbind(data , medals)
    
    # 2. Initializing dt.n.games for later use (at 6)
    dt.n.games <- data[, list(Games = unique(Games)), by = region]
    
    # 3. Using aggregate function to find totals per country and change name
    dt.bronze <- aggregate(data$medals.Bronze, by = list(region=data$region), FUN=sum)
    names(dt.bronze)[names(dt.bronze) == "x"] <- "Bronze_medals"
    
    dt.silver <- aggregate(data$medals.Silver, by = list(region = data$region), FUN = sum)
    names(dt.silver)[names(dt.silver) == "x"] <- "Silver_medals"
    
    dt.gold <- aggregate(data$medals.Gold, by = list(region = data$region), FUN = sum)
    names(dt.gold)[names(dt.gold) == "x"] <- "Gold_medals"
    
    # 4. Combine three aggregate functions datasets per sort of medal back into one dt
    data <- cbind(dt.bronze, dt.silver, dt.gold)
    data <- subset(data, select = c(region, Bronze_medals, Silver_medals, Gold_medals))
    data$Total_medals = rowSums(data[,c("Bronze_medals", "Silver_medals", "Gold_medals")])
    
    # 5. Make separate dt consisting of countries and number of olympic events joined
    dt.n.games <- dt.n.games[, list(n_games = .N), by = region][order(region)]
    dt.n.games <- na.omit(dt.n.games)
    data <- merge(data,dt.n.games,by = "region")
    
    # 6. Add average of games played column
    data$Average_per_games <- data$Total_medals/data$n_games
    data$Average_per_games <- round(data$Average_per_games, digits =1)
    
    # 7. Order from high to low
    data <- data[order(-data$Total_medals),]
    data
  }))
  
  # Reactive top 10 medals per region throughout the years
  output$top10plotyears <- renderPlot({
    dt.x <- tbinput()
    
    # Filter on all rows that scored medals
    dt.x <- dt.x[!(dt.x$Medal == "NA")]
    
    # Filter on rows top 10 countries of given reactive input
    dt.x [, n_medals := .N, by=region]
    dt.top <- unique(dt.x, by = "region")
    dt.top <- dt.top[order(-dt.top$n_medals),]
    dt.top <- head(dt.top, 10)
    f <- dt.top$region
    dt.x <- subset(dt.x, region %in% f)
    
    # Initializing plot
    ggplot(dt.x %>% 
             group_by(region) %>% 
             arrange(Year) %>% 
             mutate(rn = row_number())) + 
      geom_smooth(method = loess, aes(x = Year, y = rn, color = region)) +
      theme_classic() +
      labs(y = "Total medals", x = "Year") + 
      ggtitle("Top 10 regions with most medals through the years") +theme(
        plot.title = element_text(size = 17, hjust = 0.5))
  })
  
  output$top10plot <- renderPlot({
    dt.x <- tbinput()
    
    # Filter on all rows that scored medals
    dt.x <- dt.x[!(dt.x$Medal=="NA")]
    
    # Filter on rows top 10 countries of given reactive input
    dt.x [, n_medals := .N, by=region]
    dt.top <- unique(dt.x, by = "region")
    dt.top <- dt.top[order(-dt.top$n_medals),]
    dt.top <- head(dt.top, 10)
    f <- dt.top$region
    dt.x <- subset(dt.x, region %in% f)
    
    # Initializing plot
    ggplot(dt.x, aes(x = fct_rev(fct_infreq(region)))) +
      geom_bar(stat = "count", fill="steelblue") + theme_classic() +
      coord_flip() + labs(y = "Total medals", x = "Region") + 
      ggtitle("Top 10 regions with most medal") +  theme(
        plot.title = element_text(size = 18, hjust = 0.5))
  })
  
  
  
  ### WORLD MAP ###
  dt.country.medals$Total_medals =
      paste(dt.country.medals$Country[1:209],
            dt.country.medals$Total_medals[1:209], sep = " - ")
  
  output$wm <- renderGvis ( {
    gvisGeoChart(
      dt.country.medals,
      "Country",
      hovervar = "Total_medals",
      options = list(region = "world", displayMode = "regions",
                     width = "1200",
                     height = "700"
      )
    )
  })

  
  
  ### NETWORK EXPLORATION ###
  output$centralities <- renderTable({
    if (input$network == "Bipartite network: Events and Athletes, boxing"){
      if (input$centrality == "Degree centrality") {
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics)$degree),
                                                          max(V(g.olympics)$degree),
                                                          median(V(g.olympics)$degree),
                                                          mean(V(g.olympics)$degree),
                                                          sd(V(g.olympics)$degree)))
      }
      
      if (input$centrality == "Closeness centrality") {
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics)$closeness),
                                                          max(V(g.olympics)$closeness),
                                                          median(V(g.olympics)$closeness),
                                                          mean(V(g.olympics)$closeness),
                                                          sd(V(g.olympics)$closeness)))
      }
      
      if (input$centrality == "Betweenness centrality") {
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics)$betweenness),
                                                          max(V(g.olympics)$betweenness),
                                                          median(V(g.olympics)$betweenness),
                                                          mean(V(g.olympics)$betweenness),
                                                          sd(V(g.olympics)$betweenness)))
      }
      
      if (input$centrality == "Eigenvector centrality") {
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics)$evcent),
                                                          max(V(g.olympics)$evcent),
                                                          median(V(g.olympics)$evcent),
                                                          mean(V(g.olympics)$evcent),
                                                          sd(V(g.olympics)$evcent)))
      }}
    
    else if(input$network == "Bipartite network: Events and Athletes, Football") {
      if (input$centrality == "Degree centrality") {
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics.football.2010)$degree),
                                                          max(V(g.olympics.football.2010)$degree),
                                                          median(V(g.olympics.football.2010)$degree),
                                                          mean(V(g.olympics)$degree),
                                                          sd(V(g.olympics.football.2010)$degree)))
      }
      
      if (input$centrality == "Closeness centrality") {
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics.football.2010)$closeness),
                                                          max(V(g.olympics.football.2010)$closeness),
                                                          median(V(g.olympics.football.2010)$closeness),
                                                          mean(V(g.olympics.football.2010)$closeness),
                                                          sd(V(g.olympics.football.2010)$closeness)))
      }
      
      if (input$centrality == "Betweenness centrality"){
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics.football.2010)$betweenness),
                                                          max(V(g.olympics.football.2010)$betweenness),
                                                          median(V(g.olympics.football.2010)$betweenness),
                                                          mean(V(g.olympics.football.2010)$betweenness),
                                                          sd(V(g.olympics.football.2010)$betweenness)))
      }
      
      if (input$centrality == "Eigenvector centrality"){
        dt.centrality.statistics <- data.frame(statistic = c('Minimum',
                                                             'Maximum',
                                                             'Median',
                                                             'Mean',
                                                             'Standard deviation'),
                                               Value = c( min(V(g.olympics.football.2010)$evcent),
                                                          max(V(g.olympics.football.2010)$evcent),
                                                          median(V(g.olympics.football.2010)$evcent),
                                                          mean(V(g.olympics.football.2010)$evcent),
                                                          sd(V(g.olympics.football.2010)$evcent)))
      }
    }
    dt.centrality.statistics
  })
  
  output$descriptives <- renderTable({ 
    if (input$network == "Bipartite network: Events and Athletes, boxing") {
      dt.descriptives.network <- data.frame(statistic = c('Number of Nodes',
                                                          'Number of Edges',
                                                          'Average degree',
                                                          'Clustering coefficient',
                                                          'Average path length',
                                                          'Diameter'),
                                            Value = c( gorder(g.olympics),
                                                       gsize(g.olympics),
                                                       mean(V(g.olympics)$degree),
                                                       transitivity(g.olympics, type = "average"),
                                                       mean(diameter(g.olympics)),
                                                       diameter(g.olympics)))
    }
    
    if (input$network == "Bipartite network: Events and Athletes, Football") {
      dt.descriptives.network <- data.frame(statistic = c('Number of Nodes',
                                                          'Number of Edges',
                                                          'Average degree',
                                                          'Clustering coefficient',
                                                          'Average path length',
                                                          'Diameter'),
                                            Value = c( gorder(g.olympics.football.2010),
                                                       gsize(g.olympics.football.2010),
                                                       mean(V(g.olympics.football.2010)$degree),
                                                       transitivity(g.olympics.football.2010, type = "average"),
                                                       mean(diameter(g.olympics.football.2010)),
                                                       diameter(g.olympics.football.2010)))
    }
    dt.descriptives.network
  })
  
  output$distribution <- renderPlot({ 
    if (input$network == "Bipartite network: Events and Athletes, boxing"){
      plot_distribution <- qplot(V(g.olympics)$degree)
    }
    
    if (input$network == "Bipartite network: Events and Athletes, Football"){
      plot_distribution <- qplot(V(g.olympics.football.2010)$degree)
    }
    plot_distribution
  })
  
  output$mynetwork <- renderVisNetwork({
    if (input$network_choice == "Bipartite network: Events and Athletes, boxing visualization"){
      g.output <- visIgraph(g.olympics)
    }
    
    if (input$network_choice == "Bipartite network: Events and Athletes, Football visualization"){
      g.output <- visIgraph(g.olympics.football.2010)
    }
    
    if (input$network_choice == "Boxers that participated in both olympic events"){
      g.output <- visIgraph(g.olympics.subgraph)
    }
    
    if (input$network_choice == "Football players that participated in both olympic events"){
      g.output <- visIgraph(g.olympics.football.2010.subgraph)
    }
    g.output
  })
  
  output$descriptives_table <- DT::renderDataTable(DT::datatable({
    if (input$network == "Bipartite network: Events and Athletes, boxing"){
      dt.descriptive.table <- data.table(get.data.frame(g.olympics, "vertices"))
    }
    
    if (input$network == "Bipartite network: Events and Athletes, Football"){
      dt.descriptive.table <- data.table(get.data.frame(g.olympics.football.2010, "vertices"))
    }
    dt.descriptive.table
  }))
  
  output$athletes1 <- DT::renderDataTable(DT::datatable({
    if (input$network_choice == "Boxers that participated in both olympic events"){
      dt.subgraph <- data.table(get.data.frame(g.olympics.subgraph, "vertices"))
      dt.subgraph[, type := NULL][, degree := NULL][, closeness := NULL][, betweenness := NULL][, evcent := NULL]
    }
    
    if (input$network_choice == "Football players that participated in both olympic events"){
      dt.subgraph <- data.table(get.data.frame(g.olympics.football.2010.subgraph, "vertices"))
      dt.subgraph[, type := NULL][, degree := NULL][, closeness := NULL][, betweenness := NULL][, evcent := NULL]
    }
    
    if (input$network_choice == "Bipartite network: Events and Athletes, boxing visualization"){
      dt.subgraph <- NULL
    }
    
    if (input$network_choice == "Bipartite network: Events and Athletes, Football visualization"){
      dt.subgraph <- data.table(get.data.frame(g.olympics.football.2010.subgraph, "vertices"))
      dt.subgraph <- NULL
    }
    dt.subgraph
  }))

  ### NETWORK ANALYSIS: REGION-EVENTS NETWORK ###
  # Reactive filters for Network Analysis
  tbinput2 <- reactive({

    data2  <- dt.olympics
    
    if (input$games2 != "All") {
      data2 <- data2[data2$Games == input$games2,]
    }
    
    if (input$sport != "All") {
      data2 <- data2[data2$Sport == input$sport,]
    }
    data2
  })
  
  output$regions.events.graph.table <- renderDataTable( {
    
    ## Creating a network with regions and common events between regions as edges ##
    dt.network.analysis <- tbinput2()
    
    # Get all unique regions and events and combine them
    dt.vertices.regions.events <- rbind(dt.network.analysis[, list(name = unique(region), type = FALSE)],
                                        dt.network.analysis[, list(name = unique(Event), type = TRUE)])
    
    # Bipartite graph of Olympic regions and the events they compete in
    g.regions.events.bipartite <- graph_from_data_frame(dt.network.analysis[,list(region, Event)],
                                                        directed = FALSE,
                                                        vertices = dt.vertices.regions.events)
    
    # Projection into the region space
    g.regions.events <- bipartite.projection(g.regions.events.bipartite)$proj1
    
    # Calculating centrality measures (degree, closenss, betweenness, eigenvector)
    V(g.regions.events)$degree      <- degree(g.regions.events)
    V(g.regions.events)$closeness   <- closeness(g.regions.events)
    V(g.regions.events)$betweenness <- betweenness(g.regions.events)
    V(g.regions.events)$evcent      <- evcent(g.regions.events)$vector
    
    # Creating a data.table with centrality measures
    dt.regions.events.centr <- data.table(get.data.frame(g.regions.events, what = "vertices"))
    
    # Obtaining number of athletes, medals, unique events and sports per region
    dt.region.numbers <- 
      dt.network.analysis[, .(n_athletes = length(unique(ID)),
                               n_medals = sum(!is.na(Medal)),
                               n_unique_events = length(unique(Event)),
                               n_unique_sports = length(unique(Sport)),
                               n_games = length(unique(Games))), 
                           by = region][order(region)]
    
    # Merging the numbers per region with the centrality data.table
    dt.regions.events.centr <- merge(dt.regions.events.centr, dt.region.numbers,
                                     by.x = 'name', by.y = 'region')

    if (input$centrality.na1 == "Degree") {
      dt.regions.events.centr[, list(name, degree, n_unique_events, n_athletes, 
                                     n_unique_sports, n_games, n_medals)]
    }
    
    else if (input$centrality.na1 == "Closeness") {
      dt.regions.events.centr[, list(name, closeness, n_unique_events, n_athletes, 
                                     n_unique_sports, n_games, n_medals)]
    }
    
    else if (input$centrality.na1 == "Betweenness") {
      dt.regions.events.centr[, list(name, betweenness, n_unique_events, n_athletes, 
                                     n_unique_sports, n_games, n_medals)]
    }
    
    else if (input$centrality.na1 == "Eigenvector") {
      dt.regions.events.centr[, list(name, evcent, n_unique_events, n_athletes, 
                                     n_unique_sports, n_games, n_medals)]
    }
  })
  
  output$regions.events.graph.plot <- renderVisNetwork( {
    
    ## Creating a network with regions and common events between regions as edges ##
    dt.network.analysis <- tbinput2()
    
    # Get all unique regions and events and combine them
    dt.vertices.regions.events <- rbind(dt.network.analysis[, list(name = unique(region), type = FALSE)],
                                        dt.network.analysis[, list(name = unique(Event), type = TRUE)])
    
    # Bipartite graph of Olympic regions and the events they compete in
    g.regions.events.bipartite <- graph_from_data_frame(dt.network.analysis[,list(region, Event)],
                                                        directed = FALSE,
                                                        vertices = dt.vertices.regions.events)
    
    # Projection into the region space
    g.regions.events <- bipartite.projection(g.regions.events.bipartite)$proj1
    
    if (input$games2 != "All" & input$sport != "All") {
      
      output.na1 <- visIgraph(g.regions.events)
      
    }
  })
  
  output$popular.sports.events.plot <- renderPlot({
    
    # Creating data.tables with number of participations per sport and event
    dt.sport.count <- head(as.data.table(count(dt.olympics, Sport))[order(-n)], 5)
    dt.event.count <- head(as.data.table(count(filter(dt.olympics, Sport == input$sport2), Event))[order(-n)], 5)
    
    if (input$sport2 == "All"){
      # PLot of the most popular Sports
      ggplot(dt.sport.count, aes(x = reorder(Sport, n), y = n)) + geom_bar(stat = 'identity', fill = "red") + 
        coord_flip() + xlab("Sport") + ylab("Number of Participations")
    } else {
      # PLot of the most popular Events
      ggplot(dt.event.count, aes(x = reorder(Event, n), y = n)) + geom_bar(stat = 'identity', fill = "blue") + 
        coord_flip() + xlab("Event") + ylab("Number of Participations")
    }
  })
  
  output$popular.sports.table <- renderDataTable({
    
    dt.p.s.input <- dt.olympics
    
    if (input$region != "All") {
      dt.p.s.input <- dt.p.s.input[dt.p.s.input$region == input$region,]
    }
    
    if (input$games3 != "All") {
      dt.p.s.input <- dt.p.s.input[dt.p.s.input$Games == input$games3,]
    } 
    
    # Data.table with no. of participations per sport
    dt.popular.sports <- setnames(
      as.data.table(count(dt.p.s.input, Sport))[order(-n)], "n", "#Participations")
  })
  
  output$popular.events.table <- renderDataTable({
    
    dt.p.e.input <- dt.olympics
    
    if (input$region != "All") {
      dt.p.e.input <- dt.p.e.input[dt.p.e.input$region == input$region,]
    }
    
    if (input$games3 != "All") {
      dt.p.e.input <- dt.p.e.input[dt.p.e.input$Games == input$games3,]
    } 
    
    # Data.table with no. of participations per event
    dt.popular.events <- setnames(
      as.data.table(count(dt.p.e.input, Event))[order(-n)], "n", "#Participations")
  })
  
}
)