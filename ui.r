ui <- fluidPage(
  h1("Champions League Network Data Analytics"),
  p(style = "font-family:Impact",
    "Dashboard for Champions League from 1955 to 2015"),
    navbarPage(title = "Home",
             navbarMenu(title = "Descriptive Analytics",
                        tabPanel(title = "Top teams and goals",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "season",
                                                 label = "Choose a season",
                                                 choices = all_seasons$Season)
                                   ),
                                   mainPanel(
                                     p("In this season,"),
                                     tableOutput("top_teams"),
                                   )
                                 ),# select a season to find the best performing team
                                 
                        ),
                        tabPanel(title = "Teams summary",
                                 tableOutput("summary_team"), #descriptive analysis group by team
                        ),
                        tabPanel(title = "League summary",
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
                                     )
                                   ),
                                   mainPanel(
                                     tableOutput("summary_league"), #descriptive analysis group by league
                                   ),
                                   
                                 ),
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput("period","Choose a period:",
                                       value = c(1955,1965),min = 1955,max = 2016
                                     ),
                                   ),
                                   mainPanel(
                                     plotOutput("bar_chart_by_learegue")
                                   ),
                                 ),
                                 
                        ),
                        ),# Descriptive part
             navbarMenu(title = "Network Analysis",
                        tabPanel(title = "Network descriptive analysis",
                                 p("This is the top 30 teams ordered by degree:"),
                                 tableOutput("degree_table"),
                                 p("This is the degree histogram of all teams:"),
                                 plotOutput("Degree_distribution"),
                                 p("This is the degree histogram of all countries:"),
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
                                 ## need to input team names/season/other statistics
                                 plotOutput("Projection")
                                 )
                        ),# Network Attributes part
             navbarMenu(title = "Deep Dive Analytics",
                        tabPanel(title = "Link prediction",
                                 ## need to input team names & minimum threshold & TBD
                                 plotOutput("predicted_link")
                                 ),
                        tabPanel(title = "Inference",
                                 ## need to input variable names(such as centrality and average goals)
                                 plotOutput("Regression_results")
                          
                        )
                        ),# Deep dive analytics part
             ),
             
    )
