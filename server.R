## server.R r##

shinyServer(function(input, output, body){
  
  output$ss <- renderPlot({
    ggplot(dt.olympics, aes(x = Year)) + geom_histogram() + 
      xlab("Year") + ylab("Participations") + ggtitle("Athlete Participations per Year")
    
  })
})

