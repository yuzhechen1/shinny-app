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
                                 dataTableOutput("raw_data"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "season",
                                                 label = "Choose a season",
                                                 choices = all_seasons$Season)
                                   ),
                                   mainPanel(
                                     p("In this season, teams in the final round are as below:"),
                                     tableOutput("top_teams"),
                                     tableOutput("Winner"),
                                   )
                                 ),# select a season to find the best performing team
                                 
                        ),
                        tabPanel(title = "Teams summary",
                                 tableOutput("summary_team"), #descriptive analysis group by team
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
                                       multiple = TRUE
                                     ),
                                     sliderInput("club_number","Choose the number of clubs:",
                                                     value = c(0,10),min = 0,max = 35
                                                   ),
                                   ),
                                   mainPanel(
                                     tableOutput("summary_league"), #descriptive analysis group by league
                                   ),
                                   
                                 ),
                                 sidebarLayout(
                                   sidebarPanel(
                                     # sliderInput("period","Choose a period:",
                                     #   value = c(1955,1965),min = 1955,max = 2016
                                     # ),
                                   ),
                                   mainPanel(
                                     p("How many trophies does each country win in all seasons?"),
                                     plotOutput("bar_chart_by_learegue")
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
                                     )
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
                                     )
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
                                 plotOutput("predicted_link")
                                 ),
                        tabPanel(title = "Inference",
                                 plotOutput("Regression_results"),
                                 tableOutput("significance")
                                 ## need to input variable names(such as centrality and average goals)
                                 
                          
                        )
                        ),# Deep dive analytics part
             ),
             
    )
