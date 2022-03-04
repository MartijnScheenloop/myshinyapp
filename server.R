## server.R ##

shinyServer(function(input, output, session){
  
  observeEvent(input$tabs, {
    if (input$tabs == "dd") {
      sendSweetAlert(
        session = session,
        title = "done!",
        text = "Miauw",
        type = "success"
      )
    }
  })
  
})

