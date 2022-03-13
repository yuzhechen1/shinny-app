ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "morph"),
  h1("Champions League Network Data Analytics"),
  p(style = "font-family:Impact",
    "Dashboard for Champions League from 1955 to 2015"),
    navbarPage(title = "CCL",
               tabPanel("Welcome",
                        h1(strong("Welcome to Champions of the Champions League (CCL)"), style = "font-size:50px;"),
                        p("If you have wanted to know everything about the Champions League this application is perfect for you!
             Here, you are able to discover not only which European football clubs and countries have perforemed the best, 
             but also explore everything about the matches played and even predictions on who might play eachother in the furture, 
             all over the entire 65+ years of champions league history. Now go out there and explore the past and potentional future of 
             the Champions League!"), style = "font-size: 20px;"),
               navbarMenu(title = "Descriptive Analytics",
                        tabPanel(title = "Data retrievals",
                                 p("In the whole data set with 66 unique seasons, 
                                   5508 matches, played by 360 unique clubs which belong to
                                   45 unique countries, are included."),
                                 dataTableOutput("raw_data"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "season",
                                                 label = "Choose a season",
                                                 choices = all_seasons$Season)
                                   ),
                                   mainPanel(
                                     p("In this season, teams in the final round and winners are as below:"),
                                     tableOutput("top_teams"),
                                     tableOutput("Winner"),
                                   )
                                 ),# select a season to find the best performing team
                                 
                        ),
                        tabPanel(title = "Teams summary",
                                 p("Know about the performance of clubs that you're interested in:"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "club",
                                       label = "Choose a club",
                                       choices = all_teams$`Team 1`,
                                       selected = c(all_teams$`Team 1`[1:10]),
                                       multiple = TRUE
                                     ),
                                     
                                   ),
                                   mainPanel(
                                     tableOutput("summary_club"), #descriptive analysis group by league
                                   ),
                                   
                                 ),
                        ),
                        tabPanel(title = "League summary",
                                 p("Know about the performance of countries that you're interested in:"),
                                 # sidebarLayout(
                                 #   sidebarPanel(
                                 #     selectInput(
                                 #       inputId = "league",
                                 #       label = "Choose a country",
                                 #       choices = all_countries$`Country Team 1`
                                 #     )
                                 #   ),
                                 #   mainPanel(
                                 #     tableOutput("league_table"),
                                 #   ),
                                 # ),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "country",
                                       label = "Choose a country",
                                       choices = all_countries$`Country Team 1`,
                                       selected = c(all_countries$`Country Team 1`[1:10]),
                                       multiple = TRUE
                                     ),
                                     sliderInput("club_number","Choose the number of clubs that this country owns:",
                                                     value = c(0,20),min = 0,max = 35
                                                   ),
                                   ),
                                   mainPanel(
                                     tableOutput("summary_league"), #descriptive analysis group by league
                                   ),
                                   
                                 ),
                                 p("How many trophies does each country win in all seasons?"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     checkboxGroupInput("opts","Which period are you interested in?"
                                                        ,choices = c("all years","last ten years"))
                                     # radioButtons("opts","Which period are you interested in?"
                                     #              ,choices = c("all years","last ten years")),
                                   ),
                                   mainPanel(
                                     p("Bar chart with number of trophies of each country"),
                                       plotOutput("bar_chart_by_league"),
                                     ),
                                   ),
                        ),
                        ),# Descriptive part
             navbarMenu(title = "Network Analysis",
                        tabPanel(title = "Team data network",
                                 p("Summary of degrees of the team network"),
                                 dataTableOutput("degree_table"),
                                 p("Degree histogram of all teams:"),
                                 plotOutput("Degree_distribution"),
                                 
                                 ),
                        tabPanel(title = "Country data network",
                                 p("Summary of degrees of the country network"),
                                 dataTableOutput("degree_table.c"),
                                 p("Degree histogram of all countries:"),
                                 plotOutput("Degree_distribution_c")
                                 ),
                        tabPanel(title = "Projection into the Teams Space",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "Seasons",
                                       label = "Choose seasons for projection:",
                                       choices = all_seasons$Season,
                                       multiple = TRUE
                                     ),
                                     sliderInput("min_games","Choose the minimum games played:",
                                                 value = c(10),min = 0,max = 35
                                     ),
                                   ),
                                   mainPanel(
                                     plotOutput("projection")
                                   )
                                 ),
                                 ),
                        tabPanel(title = "Projection into the Countries Space",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "Seasons.c",
                                       label = "Choose seasons for projection:",
                                       choices = all_seasons$Season,
                                       multiple = TRUE
                                     ),
                                     sliderInput("min_games.c","Choose the minimum games played:",
                                                 value = c(50),min = 0,max = 200
                                     ),
                                   ),
                                   mainPanel(
                                     plotOutput("projection.c")
                                   )
                                 ),
                        ),
                        ),# Network Attributes part
             navbarMenu(title = "Deep Dive Analytics",
                        tabPanel(title = "Link prediction",
                                 ## need to input team names & minimum threshold & TBD
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "club1",
                                       label = "Choose a club that you want to predict:",
                                       choices = all_teams$`Team 1`,
                                     ),
                                     selectInput(
                                       inputId = "club2",
                                       label = "Choose another club:",
                                       choices = all_teams$`Team 1`,
                                     )
                                   ),
                                   mainPanel(
                                     p("The Jaccard Index of these two teams are:"),
                                     textOutput("similarity.1")
                                   )
                                 ),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "club3",
                                       label = "Choose a club that you want to predict:",
                                       choices = all_teams$`Team 1`,
                                     ),
                                     selectInput(
                                       inputId = "club4",
                                       label = "Choose another club:",
                                       choices = all_teams$`Team 1`,
                                     )
                                   ),
                                   mainPanel(
                                     p("The Jaccard Index of these two teams are:"),
                                     textOutput("similarity.2"),
                                     p(" "),
                                     p(" ")
                                   )
                                 ),
                                 textOutput("name_club1"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "input1",
                                       label = "Choose a club that you want to compare:",
                                       choices = all_teams$`Team 1`,
                                     ),
                                   ),
                                   mainPanel(
                                     p("Top 20 likely linked clubs:"),
                                     textOutput("outputtable")
                                   )
                                 ),
                                 ),
                        tabPanel(title = "Inference",
                                 p("The following graph shows the linear correlation of average score and degree centrality of countries"),
                                 plotOutput("Regression_results"),
                                 p("Statistical significance of this model:"),
                                 tableOutput("significance"),
                                 p("The following graph shows the linear correlation of average score and number of games played of countries"),
                                 plotOutput("Regression_results.1"),
                                 p("Statistical significance of this model:"),
                                 tableOutput("significance.1")
                                 ## need to input variable names(such as centrality and average goals)
                                 
                          
                        )
                        ),# Deep dive analytics part
             ),
             
    )
