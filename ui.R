## ui.R ##

library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "Olympics Network Analytics"),
  dashboardSidebar(
    sidebarUserPanel("NDA Group 1"),
    
    sidebarMenu(
      menuItem("Homepage", tabName = "hp", icon = icon("home")),
      
      menuItem("Descriptive Statistics", tabName = "ds",
               menuSubItem()
    )
  )
)))
