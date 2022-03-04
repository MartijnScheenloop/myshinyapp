## ui.Rr ##

library(shinydashboard)
library(shiny)

# Create the dashboard
shinyUI(dashboardPage(
  dashboardHeader(title = "Olympics Network Analytics"),
  dashboardSidebar(
    sidebarUserPanel("NDA Group 1"),
    
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Homepage",
         tabName = "hp", icon = icon("home")),
      
      menuItem(
        "General Descriptive Statistics",
         tabName = "ds",
         menuSubItem("Description of Dataset", tabName = 'dd'),
         menuSubItem("Summary Statistics", tabName = "ss")
        ),
      
      menuItem(
        "Network Exploration", 
         tabName = "ne",
         menuSubItem("Network Visualization", tabName = "nv"),
         menuSubItem("Network Descriptive Statistics", tabName = "nds")
        ),
      
      menuItem(
        "Network Analysis", 
        tabName = "na",
        menuSubItem("Network Visualization 1", tabName = "nv1"),
        menuSubItem("Network Visualization 2", tabName = "nv2")
        ),
      
      menuItem(
        "About the Creators",
         tabName = "atc")
  )),

  
  dashboardBody(
    tabItems(
      tabItem(tabName = "hp",
              h1("Welcome to our shiny application"),
              h4("This application will offer you the opportunity to explore the data from all olympic games (1986 - 2016) through network analytics."),
              br(),
              p("The dataset to explore includes variables regarding every athletes: gender, name, weight, height, team and age"),
              tags$head(tags$style("h1 {color: black; font-weight: bold; text-align: center;")),
              tags$head(tags$style("h4 {color: black; font-weight: italic; text-align: center;")),
      ),
      
      tabItem(tabName = "hp"),
      
      tabItem(tabName = "ds"),
      tabItem(tabName = "dd",
              h2("beter komt het er te staan")),
      tabItem(tabName = "ss"),
      
      tabItem(tabName = "ne"),
      tabItem(tabName = "nv"),
      tabItem(tabName = "nds"),
      
      tabItem(tabName = "nv1"),
      tabItem(tabName = "nv2")
    )
  )
))
