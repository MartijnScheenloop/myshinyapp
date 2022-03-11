## ui.Rr ##

library(shinydashboard)
library(shiny)

# Create the dashboard including tabs #
shinyUI(dashboardPage(
  skin = "green",
  dashboardHeader(title = "Olympics Network Analytics"),
  dashboardSidebar(
    sidebarUserPanel("NDA group 1"),
    
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
        menuSubItem("Regions-Events Network", tabName = "na1"),
        menuSubItem("Regions-Sports Network", tabName = "na2")
        ),
      
      menuItem(
        "About the Creators",
         tabName = "atc",
        icon = icon("fa-solid fa-address-book"))
  )),

  
  dashboardBody(
    tabItems(
      ## Homepage with welcome message and general information ##
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
              tags$head(tags$style("p {font-size: 15px; font-weight: italic; border-style: double;}")),
      ),
      
      tabItem(tabName = "dd",
              h1("Description of Dataset"),
              br(),
              h3("Each row represents an athlete competing in an olympic event"),
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
                         h6("Unique athlete ID"),
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
                         h6("Athlete's age")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Height"),
                         h6("In centimeters")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Weight"),
                         h6("In kilograms")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Team"),
                         h6("National team")),
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
                         h6("Year of Games")),
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
                         h6("Sports category")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Event"),
                         h6("Sport's event")),
                ),
                column(width = 2,
                       wellPanel(
                         h4("Medal"),
                         h6("Gold, Silver, Bronze, or NA")),
                ),
              ),
      ),
      
      tabItem(tabName = "ss",
              h1("Descriptive Statistics"),
              
              fluidRow(
                column(2, 
                       h4("Athletes")
                ),
                column(2,
                       h4("Regions")
                ),
                column(2,
                       h4("Games")
                ),
                column(2, 
                       h4("Sports")
                ),
                column(2, 
                       h4("Hosting cities")
                ),
                column(2, 
                       h4("Total medals")
                )
              ),
              
              fluidRow(
                column(2, 
                       wellPanel (
                         div(textOutput("athletes"),style = "font-size:125%"),
                         align = "center"
                       )
                ),
                column(2,
                       wellPanel (
                         div(textOutput("regions"),style = "font-size:125%"),
                         align = "center"
                       )
                ),
                column(2,
                       wellPanel (
                         div(textOutput("games"),style = "font-size:125%"),
                         align = "center"
                       )
                ), 
                column(2,
                       wellPanel (
                         div(textOutput("sports"),style = "font-size:125%"),
                         align = "center"
                       )
                ),     
                column(2, 
                       wellPanel(
                         div(textOutput("hosting_cities"),style = "font-size:125%"),
                         align = "center"
                       )
                ),
                column(2, 
                       wellPanel(
                         div(textOutput("total_medals"),style = "font-size:125%"),
                         align = "center"
                       )
                )
              ),
              
              fluidRow(
                column(6,
                       sliderInput(
                         inputId = "years",
                         label = "Years",
                         value = c(min(dt.olympics$Year, na.rm = TRUE), max(dt.olympics$Year, na.rm = TRUE)),
                         min = min(dt.olympics$Year, na.rm = TRUE),
                         max = max(dt.olympics$Year, na.rm = TRUE),
                         step = 1,
                         width = "100%",
                         sep = "")
                ),
                column(3,
                       pickerInput(
                         inputId = "sex",
                         label = "Select a gender",
                         choices = c("All",sort(unique(dt.olympics[!is.na(Sex)]$Sex))),
                         width = "100%",
                         options = list(`actions-box` = TRUE),
                         multiple = F,
                         selected = "All")
                ),
                column(3,
                       pickerInput(inputId = "season",
                                   label = "Select a season",
                                   choices = c("All",sort(unique(dt.olympics[!is.na(Season)]$Season))),
                                   width = "100%",
                                   options = list(`actions-box` = TRUE),
                                   multiple = F,
                                   selected = "All")
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
      
      # Worldmap is created here #
      tabItem(tabName = "wm",
              h1("World Map"),
              fluidRow(box(
                p("This tab displays the world map. When you hover over a country the total
                  amount of medals won by that country will be displayed."),
                htmlOutput("wm"), width = "100%"
              ))),
      
      tabItem(tabName = "nv",
              h1("Network Visualization"),
              ),
      
      tabItem(tabName = "nds",
              h1("Network Descriptive Statistics"),
              
              p("On this page, two networks have been created that contain sporting events and the 
                athletes that compete in these. The filters below alow the user to select which network
                they wish to see: sporting events and the athletes for the sport of Boxing or Football."),
              
              fluidRow( 
                
                column(3,
                       wellPanel(
                         pickerInput(
                           inputId = "network",
                           label = "Choose a network to display",
                           choices = c("Bipartite network: Events and Athletes, boxing",
                                       "Bipartite network: Events and Athletes, Football"),
                           width = "100%",
                           options = list(`actions-box` = TRUE),
                           multiple = F,
                           selected = "Bipartite network: Events and Athletes, boxing"))
                ),
                
                column(3,
                       wellPanel(
                         pickerInput(
                           inputId = "centrality",
                           label = "Centralities of the network",
                           choices = c("Degree centrality",
                                       "Closeness centrality",
                                       "Betweenness centrality",
                                       "Eigenvector centrality"),
                           width = "100%",
                           options = list(`actions-box` = TRUE),
                           multiple = F,
                           selected = "Degree centrality"))
                ),
                
                HTML(strrep(br(), 7)),
                
                column(4,
                       h2("Network Descriptives"),
                       tableOutput("descriptives")
                ),
                
                column(4,
                       h2("Centralities Summary"),
                       tableOutput("centralities")
                ),
                
                HTML(strrep(br(), 15)),

                h2("Degree Distribution"),
                plotOutput("distribution")
              )
              
              
      ),
      
      tabItem(tabName = "na1",
              h1("Regions-Events Network Analysis"),
              
              p("This page shows a network analysis on the dataset of this app. The network aims to discover the relations
                between the various regions in the dataset. Every region participates in several sports and events with
                their athletes, which is what this network is about. This network shows the regions as nodes that are
                connected to each other when they have participated in the same sporting event."),
              
              fluidRow(
                column(3,
                       wellPanel(
                         pickerInput(inputId = "event",
                                     label = "Select an event",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Event)]$Event))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = T,
                                     selected = "All")
                         )
                       ),
                
                column(3,
                       wellPanel(
                         pickerInput(inputId = "sport",
                                     label = "Select a sport",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Sport)]$Sport))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = T,
                                     selected = "All")
                       )
                ),
                
                column(3,
                       wellPanel(
                         pickerInput(inputId = "games",
                                     label = "Select the Games",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Games)]$Games))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = T,
                                     selected = "All")
                       )
                ),
                
                column(3,
                       wellPanel(
                         pickerInput(
                           inputId = "centrality.na1",
                           label = "Centrality measure",
                           choices = c("Degree",
                                       "Closeness",
                                       "Betweenness",
                                       "Eigenvector"),
                           width = "100%",
                           options = list(`actions-box` = TRUE),
                           multiple = F,
                           selected = "Degree"))
                ),
                
                HTML(strrep(br(), 6)),
                
                h2("Region-Events Network Centralities"),
                dataTableOutput("regions.events.graph.table"),
                
                HTML(strrep(br(), 2)),
                
                h2("Region-Events Network plot"),
                dataTableOutput("regions.events.graph.plot")
                
                )
      ),

      tabItem(tabName = "na2",
              h1("Regions-Sports Network Analysis"),
              
              p("This page shows a network analysis on the dataset of this app. The network aims to discover the relations
                between the various regions in the dataset. Every region participates in several sports and events with
                their athletes, which is what this network is about. This network shows the regions as nodes that are
                connected to each other when they have participated in the same sport."),
              
              fluidRow(
                column(3,
                       wellPanel(
                         pickerInput(inputId = "event",
                                     label = "Select an event",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Event)]$Event))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = T,
                                     selected = "All")
                       )
                ),
                
                column(3,
                       wellPanel(
                         pickerInput(inputId = "sport",
                                     label = "Select a sport",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Sport)]$Sport))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = T,
                                     selected = "All")
                       )
                ),
                
                column(3,
                       wellPanel(
                         pickerInput(inputId = "games",
                                     label = "Select the Games",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Games)]$Games))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = T,
                                     selected = "All")
                       )
                ),
                
                column(3,
                       wellPanel(
                         pickerInput(
                           inputId = "centrality.na2",
                           label = "Centrality measure",
                           choices = c("Degree",
                                       "Closeness",
                                       "Betweenness",
                                       "Eigenvector"),
                           width = "100%",
                           options = list(`actions-box` = TRUE),
                           multiple = F,
                           selected = "Degree"))
                ),
                
                HTML(strrep(br(), 6)),
                
                h2("Region-Sports Network Centralities"),
                dataTableOutput("regions.sports.graph.table"),
                
                HTML(strrep(br(), 2)),
                
                h2("Region-Sports Network plot"),
                dataTableOutput("regions.sports.graph.plot")
                
              )
              ),
      
      # About us page content #
      tabItem(tabName = "atc",
              h1("About Us"),
              h3("We are the team of students that have created this application.
                 We have had interest from many parties so invest now while you still can!"),
              br(),
              tags$head(tags$style("h1 {color:black; font-weight: bold;}")),
              tags$head(tags$style("h3 {color:black; font-weight: italic; text-align: center;}")),
              br(),
              
              fluidRow(
                column(width = 3,
                       h4("Tijmen Pauli"),
                       img(src = "tijmen-modified.png", height = 200, width = 200,
                           style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("624333tp"),
                       h5("Favorite Sporting Event: Figure Skating"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                       ),
                column(width = 3,
                       h4("Martijn Scheenloop"),
                       img(src = "martijn-modified.png", height = 200, width = 200,
                           style="display: block; margin-left: auto; margin-right: auto;"),
                       h4("460707"),
                       h5("Favorite Sporting Event: 100M Dash"),
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
                column(width = 3,
                       h4("Stephan Bos"),
                       img(src = "stephan-modified.png", height = 200, width = 200,
                           style="display: block; margin-left: auto; margin-right: auto;"),
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
