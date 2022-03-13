## ui.Rr ##

library(shinydashboard)
library(shiny)

# Create the dashboard including tabs #
shinyUI(dashboardPage(
  skin = "green",
  dashboardHeader(title = "OlympicLinks"),
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
         menuSubItem("Network Descriptives", tabName = "nds")
        ),
      
      menuItem(
        "Network Analysis",
        icon = icon("fa-solid fa-globe"),
        tabName = "na",
        menuSubItem("Centrality Analysis", tabName = "na1"),
        menuSubItem("Popular Events", tabName = "na2")
        ),
      
      menuItem(
        "About Us",
         tabName = "atc",
        icon = icon("fa-solid fa-address-book"))
  )),

  
  dashboardBody(
    tabItems(
      ## Homepage with welcome message and general information ##
      tabItem(tabName = "hp",
              h1("Welcome to OlympicLinks!"),
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
      
      ## Page with Description of Dataset
      tabItem(tabName = "dd",
              h1("Description of Dataset"),
              br(),
              h3("Each row represents an athlete competing in an olympic event"),
              br(),
              
              fluidRow(
                column(width = 6,
                       wellPanel(
                         h4("Rows"),
                         h3("271.116")),
                       align = "center"
                ),
                column(width = 6,
                       wellPanel(
                         h4("Columns"),
                         h3("15")),
                       align = "center"
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
      
      
      
      ## Page with Summary Statistics
      tabItem(tabName = "ss",
              h1("Summary Statistics"),
              
              # Set up information columns
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
              
              # Output for information columns set up
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
                column(2,
                       pickerInput(
                         inputId = "sex",
                         label = "Gender",
                         choices = c("All",sort(unique(dt.olympics[!is.na(Sex)]$Sex))),
                         width = "100%",
                         options = list(`actions-box` = TRUE),
                         multiple = F,
                         selected = "All")
                ),
                column(2,
                       pickerInput(inputId = "season",
                                   label = "Season",
                                   choices = c("All",sort(unique(dt.olympics[!is.na(Season)]$Season))),
                                   width = "100%",
                                   options = list(`actions-box` = TRUE),
                                   multiple = F,
                                   selected = "All")
                ),
                column(2,
                       pickerInput(inputId = "games",
                                   label = "Individual game",
                                   choices = c("All",sort(unique(dt.olympics[!is.na(Games)]$Games))),
                                   width = "100%",
                                   options = list(`actions-box` = TRUE),
                                   multiple = F,
                                   selected = "All")
                ),
                
                HTML(strrep(br(), 5)),
                
                h2("Medal Statistics"),
                  DT::dataTableOutput("table"),
                br(),
                fluidRow(
                  column(6,
                         wellPanel(
                           plotOutput("top10plot"))),
                  column(6,
                         wellPanel(
                           plotOutput("top10plotyears")))),
                
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
      
      
      
      ## Page with Worldmap
      tabItem(tabName = "wm",
              h1("World Map"),
              fluidRow(box(
                p("This tab displays the world map. When you hover over a country the total
                  amount of medals won by that country will be displayed."),
                htmlOutput("wm"), width = "100%"
              ))),
      
      tabItem(tabName = "nv",
              h1("Network Visualization"),
              sidebarLayout(
                sidebarPanel(
                  pickerInput(
                    inputId = "network_choice",
                    label = "Choose a network to visualize",
                    choices = c("Bipartite network: Events and Athletes, boxing visualization",
                                "Bipartite network: Events and Athletes, Football visualization",
                                "Boxers that participated in both olympic events",
                                "Football players that participated in both olympic events"),
                    width = "100%",
                    options = list(`actions-box` = TRUE),
                    multiple = F,
                    selected = "Bipartite network: Events and Athletes, boxing visualization")),
                
                mainPanel(
                  h2("Visualization of the network"),  
                  visNetworkOutput("mynetwork"),
                  DT::dataTableOutput("athletes1")
                )
              )
      ),
      
      
      
      ## Page with Network Descriptives
      tabItem(tabName = "nds",
              h1("Network Descriptives"),
              p("This page displays the descriptives of the olympic network.
                The olympic network was to big to create (shiny could not
                cope with it), so we decided to use the data of two events:
                Boxing and Football from 2010 and onwards.THe user is able to 
                view the basis descriptions of each network, the centrality
                measures of each network, an histogram with the degree distributions
                plotted, and a table that contains all of the centrality measures
                for each athlete in the data set"),
              sidebarLayout(
                sidebarPanel(
                  pickerInput(
                    inputId = "network",
                    label = "Choose a network to display",
                    choices = c("Bipartite network: Events and Athletes, boxing",
                                "Bipartite network: Events and Athletes, Football"),
                    width = "100%",
                    options = list(`actions-box` = TRUE),
                    multiple = F,
                    selected = "Bipartite network: Events and Athletes, boxing"),
                  
                  pickerInput(
                    inputId = "centrality",
                    label = "Centralities of the Network",
                    choices = c("Degree centrality",
                                "Closeness centrality",
                                "Betweenness centrality",
                                "Eigenvector centrality"),
                    width = "100%",
                    options = list(`actions-box` = TRUE),
                    multiple = F,
                    selected = "Degree centrality")),
                
                mainPanel(
                  column(4,
                         h2("Descriptive Statistics"),
                         tableOutput("descriptives")
                         ),
                  
                  column(4,h2("Centrality Statistics"),
                  tableOutput("centralities"),
                  ),
                  
                  HTML(strrep(br(), 18)),
                  
                  h2("Degree distribution"),
                  plotOutput("distribution"),
                  
                  HTML(strrep(br(), 2)),
                  
                  h2("Centrality statistics for each node"),
                  DT::dataTableOutput("descriptives_table")
                )
              )),
      
      
      
      ## Page with Network Analysis
      tabItem(tabName = "na1",
              h1("Regions-Events Centrality Analysis"),
              p("This page shows the Regions-Events Network of the dataset. This network aims to discover
                the relations between the various regions in the dataset and the events they participate in.
                Every region participates in several sports and events with their athletes, which is what 
                this network is about. This network shows the regions as nodes that are connected to each 
                other when they have participated in the same sporting event."),
              p("The table and plot below allows you to investigate how countries are related to other countries 
                in terms of sporting events. The table also shows interesting statistics per country, regarding their
                participation in the Olympic Games. Countries with a degree of 208 are countries that are 
                linked to all other countries, meaning that have at least one sporting event in common with 
                every other country. What is remarkable to see here are high-degree countries with a low
                number of unique events, such as American Samoa. This indicates that the events they participate
                in must be quite popular, as they are connected to many countries while having participated in 
                few events."),
              fluidRow(
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
                column(3,
                       wellPanel(
                         pickerInput(inputId = "games2",
                                     label = "Select the Games",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Games)]$Games))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = F,
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
                                     multiple = F,
                                     selected = "All")
                       )
                ),
                HTML(strrep(br(), 6)),
                h2("Network Centralities"),
                dataTableOutput("regions.events.graph.table"),
                HTML(strrep(br(), 2)),

                h2("Region-Events Network plot"),
                p("This plot shows the Regions-Events Network, but only when not all sports and not all Games
                  are selected. When this is the case, the network is too large and therefore not clearly
                  visible, which is useless when analysing it."),
                visNetworkOutput("regions.events.graph.plot")
                )
      ),

      
      
      ## Page with further Network Analysis
      tabItem(tabName = "na2",
              h1("Regions-Events Network Popular Sports/Events"),
              p("This page serves as an auxiliary tool for the previous page. On the previous page, the various
                centrality measures of Olympic regions could be investigated, along with some participation
                statistics regarding these countries. This information comes from the Regions-Events Network
                and can raise questions regarding the events and sports these regions participate in. Therefore,
                this page shows the most popular events and sports of the Olympics, based on the number of athlete 
                participations. Given this fact though, it must be accounted for that team sporting events naturally
                have many parcticipations, due to their nature, and will thus prove to be quite popular. 
                The filters allow the users to investigate this for every region, sport and Games edition."),
              p("The plot below shows the most popular sports by default. Once a sport is selected with the filter
                though, the most popular events of this sport will show."),
              fluidRow(
                column(3,
                       wellPanel(
                         pickerInput(inputId = "sport2",
                                     label = "Select a sport",
                                     choices = c("All", sort(unique(dt.olympics[!is.na(Sport)]$Sport))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = F,
                                     selected = "All")
                       )
                ),
                HTML(strrep(br(), 3)),
                
                h2("Popular Sports/Events plot (All regions)"),
                plotOutput("popular.sports.events.plot"),
                HTML(strrep(br(), 2)),
                
                h2("Popular Sports/Events tables"),
                column(3,
                       wellPanel(
                         pickerInput(inputId = "region",
                                     label = "Select a region",
                                     choices = c("All", sort(unique(dt.olympics[!is.na(region)]$region))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = F,
                                     selected = "All")
                       )
                ),
                column(3,
                       wellPanel(
                         pickerInput(inputId = "games3",
                                     label = "Select the Games",
                                     choices = c("All",sort(unique(dt.olympics[!is.na(Games)]$Games))),
                                     width = "100%",
                                     options = list(`actions-box` = TRUE),
                                     multiple = F,
                                     selected = "All")
                       )
                ),
                HTML(strrep(br(), 8)),
                column(6,
                       dataTableOutput("popular.sports.table")
                       ),
                column(6,
                         dataTableOutput("popular.events.table")
                       )
                  )
      ),
      
      
      
      ## Page with information regarding the creators
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
                       tags$head(tags$style("h4 {color:black; font-weight: bold; text-align:center;}")),
                       tags$head(tags$style("h5 {color:black; font-weight: italic; text-align:center;}")),
                ),
              )
              )
    )
  )
))
