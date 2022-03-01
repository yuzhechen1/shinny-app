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
                                     tableOutput("top_teams"),
                                   )
                                 ),# select a season to find the best performing team
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "round",
                                                 label = "Choose a round",
                                                 choices = all_rounds$Round)
                                   ),
                                   mainPanel(
                                     tableOutput("most_goals"),
                                   )
                                 ), # select a round to find the most goals
                        ),
                        tabPanel(title = "Season summary",
                                 plotOutput("summary_season"), #descriptive analysis group by season
                                 
                        ),
                        tabPanel(title = "Teams summary",
                                 plotOutput("summary_team"), #descriptive analysis group by team
                        ),
                        ),# Descriptive part
             navbarMenu(title = "Network Analysis",
                        tabPanel(title = "Network Statistics",
                                 plotOutput("Network_statistics")
                                 ),
                        tabPanel(title = "Projection into the Teams Space",
                                 ## need to input team names/season/other statistics
                                 plotOutput("Projection")
                                 )
                        ),# Network Attributes part
             navbarMenu(title = "Deep dive Analytics",
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
