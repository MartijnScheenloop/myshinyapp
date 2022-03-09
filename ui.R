## ui.Rr ##

library(shinydashboard)
library(shiny)

# Create the dashboard
shinyUI(dashboardPage(
  skin = "green",
  dashboardHeader(title = "Olympics Network Analytics"),
  dashboardSidebar(
    sidebarUserPanel(img(src = "olympicrings.png", style="display: block; margin-left: auto; margin-right: auto;")),
    
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Homepage",
         tabName = "hp", icon = icon("home")),
      
      menuItem(
        "Descriptive Statistics",
        icon = icon("fa-solid fa-calculator"),
         tabName = "ds",
         menuSubItem("Description of Dataset", tabName = 'dd'),
         menuSubItem("Summary Statistics", tabName = "ss"),
         menuSubItem("World Map", tabName = "wm")
        ),
      
      menuItem(
        "Network Exploration",
        icon = icon("bar-chart-o"), 
         tabName = "ne",
         menuSubItem("Network Visualization", tabName = "nv"),
         menuSubItem("Network Descriptive Statistics", tabName = "nds")
        ),
      
      menuItem(
        "Network Analysis",
        icon = icon("fa-solid fa-globe"),
        tabName = "na",
        menuSubItem("Network Visualization 1", tabName = "nv1"),
        menuSubItem("Network Visualization 2", tabName = "nv2")
        ),
      
      menuItem(
        "About the Creators",
         tabName = "atc",
        icon = icon("fa-solid fa-address-book"))
  )),

  
  dashboardBody(
    tabItems(
      tabItem(tabName = "wm",
              fluidRow(box(
                htmlOutput("wm"), width = 10
              ))),
      ## Homepage with welcome message and general information
      tabItem(tabName = "hp",
              h1("Welcome to our shiny application!"),
              img(src = "hpimage-modified.png", height = 500, width = "100%", inline = FALSE),
              h4("This application will offer you the opportunity to explore the data from all olympic games (1896 - 2016) through network analytics."),
              br(),
              p("This application runs on a merged csv file derived from Kaggle. The first csv file contained variables regarding an athletes age,
              gender, name, weight and team. The second csv file contained vatiables regarding NOC year, city, sport, event, games and medals.
              The csv files have been merged into a single file in order to provide a more wholesome interpretation of the history of the
              olympic games. This application provides an interactive exploration of the dataset through descriptive statistics, network
              exploration and one elaborate network analysis. Noticeable observations are that there is no data from 1916, 1940 and
              1944 because in those years the olypmic games got cancelled as a results of WW1 and WW2. It furthermore must be noted that
              due to a US boycot in 1980 the data from that year is limited. Another noteable observation is that the summer
              and winter olympics where held in the same year up untill 1994, after that the summer and winter olympic began alternating
              every two years."),
              tags$head(tags$style("h1 {color: black; font-weight: bold; text-align: center;")),
              tags$head(tags$style("h4 {color: black; font-weight: italic; text-align: center;")),
              tags$head(tags$style("p {color:black; font-size: 15px; font-weight: italic; border-style: double;}")),
      ),
      
      tabItem(tabName = "dd",
              h2("Description of Dataset"),
              br(),
              h3("Each row representing an athlete competing in an olympic event"),
              br(),
              
              fluidRow(

                column(width = 3, 
                       wellPanel(
                         h4("Rows"),
                         h3("271.116")),
                ),
                column(width = 3, 
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
              
              fluidRow(
                column(4,
                       wellPanel(
                         sliderInput(
                           inputId = "years",
                           label = "Years",
                           value = c(min(dt.olympics$Year, na.rm = TRUE), max(dt.olympics$Year, na.rm = TRUE)),
                           min = min(dt.olympics$Year, na.rm = TRUE),
                           max = max(dt.olympics$Year, na.rm = TRUE),
                           step = 1,
                           sep = ""))
                ),
                column(2,
                       wellPanel(
                         pickerInput(
                           inputId = "sex",
                           label = "Select a gender",
                           choices = c("All",sort(unique(dt.olympics[!is.na(Sex)]$Sex))),
                           width = "40%",
                           options = list(`actions-box` = TRUE),
                           multiple = T,
                           selected = "All"))
                ),
                column(2,
                       wellPanel(
                         pickerInput(inputId = "season",
                                     label = "Select a season",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Season)]$Season))),
                                     width = "40%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = T,
                                     selected = "All"))
                ),
                DT::dataTableOutput("table"),
                column(width = 9,
                       p("It first must be noted that the medals counts found in the tabel are the sum of the all the medals received
                         by a country's atheletes. Meaning that if a football teams wins gold this counts as 11 gold medals instead of
                         1. While exploring the descriptve statistics of the olympic games dataset it can be seen that the USA momentarely
                         has the most medals out of all the participating countries. It can further more be inferred that the USA has
                         the highest amount of average medals per olympic games, closely followed by Russia. It seems logical that 
                         the USA and Russia have the highest total and average amount of medals when taking the size and competition
                         between these two countries into account. While taking this into account it becomes less apparent that China
                         does not belong to the top of the table. It must be mentioned that China has considerably less total participations
                         than the USA and China but when looking at China's average amount of medals per game the country does not even
                         come close to the USA and China.")
                       ),
              )
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
                       img(src = "tijmen-modified.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("624333tp"),
                       h5("Favorite Sporting Event: Figure Skating"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                       ),
                column(width = 3,
                       h4("Martijn Scheenloop"),
                       img(src = "martijn-modified.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("460707"),
                       h5("Favorite Sporting Event: 100M Dash"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
                column(width = 3,
                       h4("Stephan Bos"),
                       img(src = "stephan-modified.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("610219hb"),
                       h5("Favorite Sporting Event: 200M hordes"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
                column(width = 3,
                       h4("Rijck Dijksterhuis"),
                       img(src = "rijck-modified.png", height = 200, width = 200, style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("424395nd"),
                       h5("Favorite Sporting Event: Skeleton"),
                       p("Rijck was formely set out to major in Finance. During the corona pandemic he started a period of self-reflection
                         which led him to reconsider his decision and choose to major in Business Information Management at the 
                         Rotterdam School of Management"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
              )
              )
    )
  )
))
