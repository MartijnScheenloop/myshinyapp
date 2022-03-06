## server.R r##

shinyServer(function(input, output, body){
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dt.olympics
    
    # filter
    if (input$sex != "All") {
      data <- data[data$Sex == input$sex,]
    }
    if (input$season != "All") {
      data <- data[data$Season == input$season,]
    }
    data <- data[data$Year >= min(input$years) & data$Year <= max(input$years),]
    
    # opmaak
    data  <- subset(data , select = c(region, Medal))
    medals <- to.dummy(data $Medal, "medals")
    data <-cbind(data , medals)
    dt.bronze <- aggregate(data$medals.Bronze, by=list(region=data$region), FUN=sum)
    names(dt.bronze)[names(dt.bronze)=="x"] <- "Bronze_medals"
    dt.silver <- aggregate(data$medals.Silver, by=list(region=data$region), FUN=sum)
    names(dt.silver)[names(dt.silver)=="x"] <- "Silver_medals"
    dt.gold <- aggregate(data$medals.Gold, by=list(region=data$region), FUN=sum)
    names(dt.gold)[names(dt.gold)=="x"] <- "Gold_medals"
    data <- cbind(dt.bronze, dt.silver, dt.gold)
    data <- subset(data, select = c(region, Bronze_medals, Silver_medals, Gold_medals))
    data$Total_medals = rowSums(data[,c("Bronze_medals", "Silver_medals", "Gold_medals")])
    data <- data[order(-data$Total_medals),]
    
    data
    
  }))

}
)
