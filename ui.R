## ui.R ##

library(shinydashboard)
library(shiny)

# Create the dashboard
shinyUI(dashboardPage(
  dashboardHeader(title = "Olympics Network Analytics"),
  dashboardSidebar(
    sidebarUserPanel("NDA Group 1"),
    
    sidebarMenu(
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
    tabItem(tabName = "hp"),
  
    
    tabItem(tabName = "ds"),
    tabItem(tabName = "dd"),
    tabItem(tabName = "ss"),
    
    tabItem(tabName = "ne"),
    tabItem(tabName = "nv"),
    tabItem(tabName = "nds"),
    
    tabItem(tabName = "nv1"),
    tabItem(tabName = "nv2"),
  )
))
