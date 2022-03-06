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
      ## Homepage with welcome message and general information
      tabItem(tabName = "hp",
              h1("Welcome to our shiny application!"),
              img(src = "hpimage.png", height = 500, width = 1100),
              h4("This application will offer you the opportunity to explore the data from all olympic games (1896 - 2016) through network analytics."),
              br(),
              p("We have merged two seperate csv files derived from Kaggle in order to provide a most wholsesome interpretation of the history of the olympic games.
                In the merged dataset one can find variables regarding an athletes age, gender, name, weight and team. Furthermore,
                information regarding NOC year, city, sport, event, games and medal can be found."),
              p("There is no data from 1916, 1940 and 1944 because in those years the olypmic games got cancelled as a results of WW1 and WW2.
                It must be noted that due to a US boycot in 1980 the data from that year is limited. Another noteable observation is that the summer
                and winter olympics where held in the same year up untill 1994, after that the summer and winter olympic began alternating every two years."),
              tags$head(tags$style("h1 {color: black; font-weight: bold; text-align: center;")),
              tags$head(tags$style("h4 {color: black; font-weight: italic; text-align: center;")),
      ),
      
      tabItem(tabName = "dd",
              h2("Description of Dataset"),
              br(),
              h3("Each row representing an athlete competing in an olympic event"),
              br(),
              
              fluidRow(
                column(width = 3, align="center", 
                       wellPanel(
                         h4("Rows"),
                         h3("271.116")),
                ),
                column(width = 3,  align="center", 
                       wellPanel(
                         h4("Columns"),
                         h3("15")),
                ),
              ),
              
              h3("With Variables"),
              br(),
              fluidRow(
                column(width = 2,
                       wellPanel(
                         h4("ID"),
                         h6("Unique athlete"),
                         tags$head(tags$style("h6 {color:black; text-align:center;}"))),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Name"),
                         h6("Athlete's name")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Sex"),
                         h6("Male or Female")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Age"),
                         h6("Integer")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Height"),
                         h6("In centimemters")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Weight"),
                         h6("In kilograms")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Team"),
                         h6("Team name")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("NOC"),
                         h6("National 3-letter code")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Games"),
                         h6("Year and season")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Year"),
                         h6("Integer")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Season"),
                         h6("Summer or Winter")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("City"),
                         h6("Host city")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Sport"),
                         h6("Sport")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Event"),
                         h6("Event")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Medal"),
                         h6("Gold, Silver, Bronze, or NA")),
                ),
              ),
      ),
              tabItem(tabName = "ss",
                      h2("Descriptive Statistics"),
                      
                      # Games dropdown filter
                      pickerInput(inputId = "games",
                                  label = "Select the Games",
                                  choices = sort(unique(dt.olympics[!is.na(Games)]$Games), decreasing = FALSE),
                                  width = "40%",
                                  options = list(`actions-box` = TRUE),
                                  multiple = T
                      ),
                      
                      # Sport dropdown filter
                      pickerInput(inputId = "sport",
                                  label = "Select a sport",
                                  choices = sort(unique(dt.olympics[!is.na(Sport)]$Sport), decreasing = FALSE),
                                  width = "40%",
                                  options = list(`actions-box` = TRUE),
                                  multiple = T
                      ),
                      
                      # Region dropdown filter
                      pickerInput(inputId = "region",
                                  label = "Select a region",
                                  choices = sort(unique(dt.olympics[!is.na(region)]$region), decreasing = FALSE),
                                  width = "40%",
                                  options = list(`actions-box` = TRUE),
                                  multiple = T
                      ),
                      
                      # Gender dropdown filter
                      pickerInput(inputId = "num",
                                  label = "Select a gender",
                                  choices = sort(unique(dt.olympics[!is.na(Sex)]$Sex), decreasing = FALSE),
                                  width = "40%",
                                  options = list(`actions-box` = TRUE),
                                  multiple = T
                      ),
                      
                      # Season dropdown filter
                      pickerInput(inputId = "season",
                                  label = "Select a season",
                                  choices = sort(unique(dt.olympics[!is.na(Season)]$Season), decreasing = FALSE),
                                  width = "40%",
                                  options = list(`actions-box` = TRUE),
                                  multiple = T
                      ),
                      
                      # Age slider filter
                      sliderInput(inputId = "num", 
                                  label = "Choose an age", 
                                  value = 20,
                                  min = min(dt.olympics[!is.na(Age)]$Age), 
                                  max = max(dt.olympics[!is.na(Age)]$Age)),
                      
                      plotOutput("ss")
                      
              ),
              
              
      tabItem(tabName = "ss",
              h2("Descriptive Statistics"),
              
              # Games dropdown filter
              pickerInput(inputId = "games",
                          label = "Select the Games",
                          choices = sort(unique(dt.olympics[!is.na(Games)]$Games), decreasing = FALSE),
                          width = "40%",
                          options = list(`actions-box` = TRUE),
                          multiple = T
              ),
              
              # Sport dropdown filter
              pickerInput(inputId = "num",
                          label = "Select a sport",
                          choices = sort(unique(dt.olympics[!is.na(Sport)]$Sport), decreasing = FALSE),
                          width = "40%",
                          options = list(`actions-box` = TRUE),
                          multiple = T
              ),
              
              # Region dropdown filter
              pickerInput(inputId = "num",
                          label = "Select a region",
                          choices = sort(unique(dt.olympics[!is.na(region)]$region), decreasing = FALSE),
                          width = "40%",
                          options = list(`actions-box` = TRUE),
                          multiple = T
              ),
              
              # Gender dropdown filter
              pickerInput(inputId = "num",
                          label = "Select a gender",
                          choices = sort(unique(dt.olympics[!is.na(Sex)]$Sex), decreasing = FALSE),
                          width = "40%",
                          options = list(`actions-box` = TRUE),
                          multiple = T
              ),
              
              # Season dropdown filter
              pickerInput(inputId = "num",
                          label = "Select a season",
                          choices = sort(unique(dt.olympics[!is.na(Season)]$Season), decreasing = FALSE),
                          width = "40%",
                          options = list(`actions-box` = TRUE),
                          multiple = T
              ),
              
              # Age slider filter
              sliderInput(inputId = "num", 
                          label = "Choose an age", 
                          value = 20,
                          min = min(dt.olympics[!is.na(Age)]$Age), 
                          max = max(dt.olympics[!is.na(Age)]$Age)),
              
              plotOutput("ss")
              
      ),
      
      tabItem(tabName = "nv"),
      tabItem(tabName = "nds"),
      
      tabItem(tabName = "nv1"),
      tabItem(tabName = "nv2"),
      
      # About us page content #
      tabItem(tabName = "atc",
              h1("About Us"),
              h3("We are the team of scholars that have created this application. We have had interest from many parties so invest now while you still can!"),
              br(),
              tags$head(tags$style("h1 {color:black; font-weight: bold;}")),
              tags$head(tags$style("h3 {color:black; font-weight: italic; text-align: center;}")),
              br(),
              
              fluidRow(
                column(width = 3,
                       h4("Tijmen Pauli"),
                       img(src = "tijmen.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("624333tp"),
                       h5("Favorite Sporting Event: Figure Skating"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                       ),
                column(width = 3,
                       h4("Martijn Scheenloop"),
                       img(src = "martijn.jpg", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("460707"),
                       h5("Favorite Sporting Event: 100M Dash"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
                column(width = 3,
                       h4("Stephan Bos"),
                       img(src = "stephan.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("610219hb"),
                       h5("Favorite Sporting Event: 200M hordes"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
                column(width = 3,
                       h4("Rijck Dijksterhuis"),
                       img(src = "rijck.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("424395nd"),
                       h5("Favorite Sporting Event: Skeleton"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
              )
              )
    )
  )
))
