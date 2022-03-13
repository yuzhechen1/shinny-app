ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  h1("Champions League Network Data Analytics"),
  p(style = "font-family:Impact",
    "Dashboard for Champions League from 1955 to 2021"),
  navbarPage(title = "CCL",
             tabPanel("Welcome",
                      h1(strong("Welcome to Champions of the Champions League (CCL)"), style = "font-size:50px;"),
                      p("If you have wanted to know everything about the Champions League this application is perfect for you!
             At CCL, you are able to discover not only which European football clubs and countries have perforemed the best, 
             but also explore everything about the matches played and even predictions on who might play eachother in the furture, 
             all over the entire 65+ years of Champions League history. "), style = "font-size: 20px;",
                      p("Now go out there and explore the past and potentional future of 
             the Champions League!"),
                      h2("About the development of the app"),
                      p("This application is developed in relation to an assignment for the course Network Data Analytics, 
                          at Erasmus University Rotterdam. The team is composed of four students in the MSc Program Business
                          Information Management: Yuzhe Chen, Bram van de Water, Thom Hudepohl and Kwint Jansen. By using the
                          R platform and multiple of the tools provided in R, such as Shiny, the team was able to build out a
                          fully functioning application. The team has a proven track record in the data science and tech field,
                          across APAC and EMEA. Due to the team’s diversity the app will be valuable for multiple target audiences, 
                          such as football fans and individuals who have no relationship to the Champion’s League yet.",
                        p("The app was developed by using a dataset that contains data containing 4.890 matches of 61 seasons 
                            of the Champions League. For each row, the date, the round, both the clubs that played each other, 
                            the goals scored from both teams and the final result are specified.",
                          p("Each Champions League season progresses in a similar manner. Before 1994, the clubs first played
                              each other in round 1, the club that won progressed to the round of 16, then the quarter-finals, 
                              semi-finals, and finally, the last row of the tournament is the final. After 1994 round one got 
                              replaced by the group stage. In total, the dataset contains information about 360 unique clubs 
                              and represents 45 countries.")))),
             
             
             
             navbarMenu(title = "Descriptive Analytics",
                        tabPanel("Data retrievals",
                                 p("Here you’ll find the entire dataset that includes all the matches played in the history of the 
                                 Champions League. Each row in the dataset represents a unique match. For each of these matches, the 
                                 round within the Champions League, the clubs that faced each other during the match, the countries 
                                 both clubs are from, and the goals scored by both clubs are specified. If you wish to search for a
                                 specific match you’ll be able to use the search filter placed above the table."), style = "font-size: 20px;",
                                 p("Furthermore, below the table, you will be able to search for the best performing clubs and countries
                                   from a particular season. You can do this by selecting a season that you are interested in in the 
                                   search bar located on the left side of the page. The clubs and countries that will be displayed are 
                                   on ones that made it to the semi-final of the tournament."),
                                 
                                 
                                 dataTableOutput("raw_data"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "season",
                                                 label = "Choose a season",
                                                 choices = all_seasons$Season)
                                   ),
                                   
                                   mainPanel(
                                     wellPanel(
                                       p("In this season, clubs in the final round and winners are as below:"),
                                       tableOutput("top_teams"),
                                       br(),
                                       tableOutput("Winner"),
                                     )
                                   )
                                 ),
                                 
                        ),
                        tabPanel(title = "Clubs summary",
                                 p("In the clubs summary section, you will be able to analyse the performance
                                   of the clubs that you are interested in. The table you see below will display 
                                   different interesting information about the clubs that you select. For each club,
                                   you will be able to get an understanding of their performance in the Champions 
                                   League by looking at the country of the club, the number of matches played, in how many different 
                                   seasons they played, how many trophies they won, their win ans loss record, and the number of
                                   goals scored and conceided."), style = "font-size: 20px;",
                                 p("You can use the search function at the top of the table to analyse the performance of 
                                   your favorite clubs "),
                                 
                                 
                                 
                                 
                                 dataTableOutput("summary_club"), #descriptive analysis group by league
                                 
                                 
                        ),
                        tabPanel(title = "County summary",
                                 p("In the country summary section, you will be able to analyse the performance of the countries that you
                                   are interested in. As you can see, we aldready added a few countries, but feel free to add your favorites.
                                   The table you see below will display different interesting information about the 
                                   countries that you select. For each country, the total number of unique clubs that have at least played 
                                   in the Champions League ones is displayed. Furthermore, the total number of games played by clubs from 
                                   the country and the number of participants are shown as well. Finally, you will be able to see the average 
                                   number of participants from these countries and the number of trophies won by them."), style = "font-size: 20px;",
                                 p("Below the table, a histogram will be displayed showing the number of trophies won by the different countries."),
                                 
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
                                     wellPanel(
                                       tableOutput("summary_league"), #descriptive analysis group by league
                                     )
                                   ),
                                   
                                 ),
                                 p("How many trophies does each country win in all seasons?"),
                                 sidebarLayout(
                                   
                                   
                                   
                                   plotOutput("bar_chart_by_league"),
                                   p(""), 
                                 ),
                        ),
             ),
             
             # Descriptive part
             navbarMenu(title = "Network Analysis",
                        tabPanel(title = "Club data network",
                                 p("Welcome to the network descriptive analytics of the clubs. On this page, you will be able to find all 
                                   kinds of information on the network of all the different clubs that have participated in the Champions 
                                   League. A table is added that displays the different clubs and their specific degree, closeness,
                                   betweenness, and ecvent. To analyse which club has the highest or lowest degree, closeness, betweenness, and 
                                   ecvent, you can click the arrows next to the headers and the table will sort the information for you.
                                   To search for specific clubs, you can use the search bar located above the data table."), style = "font-size: 20px;",
                                 p("Above this table, you can find some attribute information on the number of nodes, number of edges, mean distance, 
                                   transitivity, the diameter, and the average degree of the
                                   network. Furthermore, the degree distribution of the network is displayed in the form of a bar chart."),
                                 
                                 p("Attributes of the whole network:"),
                                 p("number of nodes: 360"),
                                 p("number of edges: 5508"), 
                                 p("mean distance: 2.74"), 
                                 p("transitivity: 0.39"),
                                 p("diameter: 6"),
                                 p("average degree: 30.6"),
                                 
                                 dataTableOutput("degree_table"),
                                 br(),
                                 
                                 
                                 br(),
                                 p("Degree histogram of all clubs:"),
                                 
                                 plotOutput("Degree_distribution"),
                                 
                        ),
                        
                        
                        tabPanel(title = "Country data network",
                                 p("Welcome to the network descriptive analytics of the countries. On this page, you will be able to find all 
                                   kinds of information on the network of all the different countries that have participated in the Champions 
                                   League. A table is added that displays the different countries and their specific degree, closeness,
                                   betweenness, and ecvent. To analyse which country has the highest or lowest degree, closeness, betweenness, and 
                                   ecvent, you can click the arrows next to the headers and the table will sort the information for you.
                                   To search for specific countries, you can use the search bar located above the data table."), style = "font-size: 20px;",
                                 p("Above this table, you can find some attribute information on the number of nodes, number of edges, mean distance, 
                                   transitivity, the diameter, and the average degree of the
                                   network. Furthermore, the degree distribution of the network is displayed in the form of a bar chart."),
                                 
                                 p("Attributes of the whole network:"),
                                 p("number of nodes: 45"),
                                 p("number of edges: 5508"), 
                                 p("mean distance: 1.546"), 
                                 p("transitivity: 0.81"),
                                 p("diameter: 3"),
                                 p("average degree: 23"),
                                 dataTableOutput("degree_table.c"),
                                 br(), 
                                 br(),
                                 p("Degree histogram of all countries:"),
                                 plotOutput("Degree_distribution_c")
                        ),
                        
                        
                        
                        tabPanel(title = "Projection into the clubs Space",
                                 p("By creating a projection into the clubs space, users can easily see what opponents
                                   their favourite club has been playing against and how centrally located their club is in the network. More 
                                   centrally located clubs play more matches, and can thus be seen as more successful in the Champions League."), style = "font-size: 20px;",
                                 p("On the left side of your screen you can choose a single season or multiple seasons that you are interested 
                                   in."),
                                 
                                 
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "Seasons",
                                       label = "Choose seasons for projection:",
                                       choices = all_seasons$Season,
                                       multiple = TRUE
                                     ),
                                     
                                   ),
                                   mainPanel(
                                     wellPanel(
                                       plotOutput("projection")
                                     )
                                   )
                                 ),
                        ),
                        
                        
                        
                        tabPanel(title = "Projection into the Countries Space",
                                 p("Over the many years that the Champion’s League has been taking place, many
                                   countries have had different clubs playing matches. By creating a projection 
                                   into the country space, fans can identify how their home country or country of 
                                   choice has been competing against other countries, regardless of the football 
                                   club they support. Furthermore, the more central and bigger the country is 
                                   displayed in the network, the more games they have played."), style = "font-size: 20px;",
                                 p("On the left side of your screen you can choose a single season or multiple seasons that you are interested 
                                   in."),
                                 
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "Seasons.c",
                                       label = "Choose seasons for projection:",
                                       choices = all_seasons$Season,
                                       multiple = TRUE
                                     ),
                                     
                                   ),
                                   mainPanel(
                                     wellPanel(
                                       plotOutput("projection.c")
                                     )
                                   )
                                 ),
                        ),
             ),# Network Attributes part
             navbarMenu(title = "Deep Dive Analytics",
                        tabPanel(title = "Link prediction",
                                 p("The link prediction shown below can give you a better understanding of the possibilities
                                   that two clubs play each other in the future. You can do this by selecting a club you want
                                   to analyse on the left side of your screen under “Choose a club that you want to predict”.
                                   To illustrate how it works we select AC Milan as an example. Subsequently, to be able to 
                                   analyse who is more likely to play each other, select two different clubs under “Choose 
                                   opponent 1 and choose opponent 2”."), style = "font-size: 20px;",
                                 p("In our example, we choose Sporting CP and Real Madrid CF. The output that is shown is a 
                                   Jaccard index of your selected clubs. The output shows a ratio of all the clubs played by 
                                   club 1, that club 2 also played against.  In our case, the Jaccard index between Ac Milan 
                                   and Sporting CP is 0.1494253 and from AC Milan and Real Madrid CF is 0.3488372. This shows 
                                   that AC Milan and Real Madrid CF play against similar clubs and thus the match between AC 
                                   Milan and Real Madrid CF is more likely to happen."),
                                 
                                 
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "club1",
                                       label = "Choose a club that you want to predict:",
                                       selected = "AC Milan", 
                                       choices = all_teams$`Team 1`,
                                     ),
                                     selectInput(
                                       inputId = "club2",
                                       label = "Choose opponent 1:",
                                       selected = "Sporting CP",
                                       choices = all_teams$`Team 1`,
                                     )
                                   ),
                                   
                                   mainPanel(
                                     wellPanel(
                                       p("The Jaccard Index of these two clubs are:"),
                                       textOutput("similarity.1")
                                     )
                                   )
                                 ),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(
                                       inputId = "club3",
                                       label = "Repeat the club that you want to predict:",
                                       selected = "AC Milan",
                                       choices = all_teams$`Team 1`,
                                     ),
                                     selectInput(
                                       inputId = "club4",
                                       label = "Choose opponent 2:",
                                       choices = all_teams$`Team 1`,
                                       selected = "Real Madrid CF", 
                                     )
                                   ),
                                   mainPanel(
                                     wellPanel(
                                       p("The Jaccard Index of these two clubs are:"),
                                       textOutput("similarity.2"),
                                       p(" "),
                                       p(" ")
                                     )
                                   )
                                 ),
                                 textOutput("name_club1"),
                                 
                        ),
                        
                        
                        tabPanel(title = "Inference",
                                 p("The inference shows a regression about the goals scored and
                                   the number of matches played by a club. As you can see in the
                                   regression plot, clubs who have played more matches in the Champions
                                   League tend to also score more goals per match."),
                                 
                                 plotOutput("Regression_results.1"),
                                 br(),
                                 p("Statistical significance of this model: p = 0.001",
                                   p("This show that the difference in average goals scored per match between clubs few and many games is
                                     statistically significant. Therefore, the amount of matches that a club has played is a good indicator of the amount of 
                                     goals scored.")),
                     
                        )
             ),
  ),
  
)

server <- function(input, output,session) {
  
  
  output$raw_data <- renderDataTable({
    
    dt <- datatable(
      Champions_League_Data_1955_2015, 
      rownames = FALSE, 
      escape = -1,
      options = list(
        columnDefs = list(
          list(visible = TRUE, targets = c( 1,2,3,4,5,6,7,8)),
          list(orderable = FALSE, className = 'details-control', targets = 0)
        )
      ),
      callback = JS("
      table.column(1).nodes().to$().css({cursor: 'pointer'});
      var format = function(d) {
        return '<table cellpadding=\"5\" cellspacing=\"0\" border=\"0\" style=\"padding-left:50px;\">'+
            '<tr>'+
                '<td>qsec:</td>'+
                '<td>'+d[7]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>vs:</td>'+
                '<td>'+d[8]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>am:</td>'+
                '<td>'+d[9]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>gear:</td>'+
                '<td>'+d[10]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>carb:</td>'+
                '<td>'+d[11]+'</td>'+
            '</tr>'+
        '</table>';
      };
      "
      ))
  })
  
  
  
  
  output$top_teams <- renderTable(
    
    unique(team_round[which(team_round$Season == input$season & team_round$Round == "Semifinals"),1])
  )
  
  output$Winner <- renderTable(
    country_final[which(country_final$Season == input$season & country_final$Round == "Final"),1:2]
  )
  
  
  
  output$summary_club <- renderDataTable({
    
    dt <- datatable(
      club.statistics, 
      rownames = FALSE, 
      escape = -1,
      options = list(
        columnDefs = list(
          list(visible = TRUE, targets = c( 1,2,3,4,5,6,7,8)),
          list(orderable = FALSE, className = 'details-control', targets = 0)
        )
      ),
      callback = JS("
      table.column(1).nodes().to$().css({cursor: 'pointer'});
      var format = function(d) {
        return '<table cellpadding=\"5\" cellspacing=\"0\" border=\"0\" style=\"padding-left:50px;\">'+
            '<tr>'+
                '<td>qsec:</td>'+
                '<td>'+d[7]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>vs:</td>'+
                '<td>'+d[8]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>am:</td>'+
                '<td>'+d[9]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>gear:</td>'+
                '<td>'+d[10]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>carb:</td>'+
                '<td>'+d[11]+'</td>'+
            '</tr>'+
        '</table>';
      };
      "
      ))
  })
  
  output$league_table <- renderTable(
    league_statistics_table
  )
  
  output$summary_league <- renderTable(
    league_statistics[which(league_statistics$country %in% input$country & league_statistics$`number of clubs` > input$club_number[1] & league_statistics$`number of clubs` < input$club_number[2])  ,]
  )
  
  
  
  
  output$bar_chart_by_league <- renderPlot(
    ggplot(number_trophies_league,aes(Country,`Trophies won`))+geom_bar(stat = "identity",fill = "steelblue")+geom_text(aes(label=`Trophies won`),vjust=-0.2)
  )
  
  output$degree_table <- renderDataTable({
    
    dt <- datatable(
      degree_table, 
      rownames = FALSE, 
      escape = -1,
      options = list(
        columnDefs = list(
          list(visible = TRUE, targets = c( 1,2,3,4)),
          list(orderable = FALSE, className = 'details-control', targets = 0)
        )
      ),
      callback = JS("
      table.column(1).nodes().to$().css({cursor: 'pointer'});
      var format = function(d) {
        return '<table cellpadding=\"5\" cellspacing=\"0\" border=\"0\" style=\"padding-left:50px;\">'+
            '<tr>'+
                '<td>qsec:</td>'+
                '<td>'+d[7]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>vs:</td>'+
                '<td>'+d[8]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>am:</td>'+
                '<td>'+d[9]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>gear:</td>'+
                '<td>'+d[10]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>carb:</td>'+
                '<td>'+d[11]+'</td>'+
            '</tr>'+
        '</table>';
      };
      "
      ))
    
  })
  
  output$Degree_distribution <- renderPlot(
    ggplot(dt.g.matches, aes(degree)) + geom_histogram(fill = "grey", colour = "black", binwidth = 1)
  )
  
  output$degree_table.c <- renderDataTable({
    
    dt <- datatable(
      degree_table.c, 
      rownames = FALSE, 
      escape = -1,
      options = list(
        columnDefs = list(
          list(visible = TRUE, targets = c( 1,2,3,4)),
          list(orderable = FALSE, className = 'details-control', targets = 0)
        )
      ),
      callback = JS("
      table.column(1).nodes().to$().css({cursor: 'pointer'});
      var format = function(d) {
        return '<table cellpadding=\"5\" cellspacing=\"0\" border=\"0\" style=\"padding-left:50px;\">'+
            '<tr>'+
                '<td>qsec:</td>'+
                '<td>'+d[7]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>vs:</td>'+
                '<td>'+d[8]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>am:</td>'+
                '<td>'+d[9]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>gear:</td>'+
                '<td>'+d[10]+'</td>'+
            '</tr>'+
            '<tr>'+
                '<td>carb:</td>'+
                '<td>'+d[11]+'</td>'+
            '</tr>'+
        '</table>';
      };
      "
      ))
  }
  )
  
  output$Degree_distribution_c <- renderPlot(
    ggplot(dt.g.matches.c, aes(degree)) + geom_histogram(fill = "grey", colour = "black", bins = 15)
  )
  
  projection_data <- reactive({
    Champions_League_Data_1955_2015 %>%
      filter(Champions_League_Data_1955_2015$Season %in% input$Seasons)
  })
  
  
  
  
  projection_graph <- reactive({
    graph_from_data_frame(projection_data()[, c("Team 1", "Team 2")],
                          directed=FALSE)
  })
  
  projection_graph_1 <- reactive({
    induced.subgraph(projection_graph(), V(projection_graph())[degree >= input$min_games])
  })
  
  output$projection <- renderPlot(
    plot(projection_graph(),main = "Team projection network of selected seasons",
         ,edge.curved=0,vertex.label.family = "sans", vertex.label.color	= "black")
  )
  
  projection_data.c <- reactive({
    Champions_League_Data_1955_2015 %>%
      filter(Champions_League_Data_1955_2015$Season %in% input$Seasons.c )
  })
  
  projection_graph.c <- reactive({
    graph_from_data_frame(projection_data.c()[,c("Country Team 1","Country Team 2")],
                          directed =FALSE)
  })
  
  output$projection.c <- renderPlot(
    plot(projection_graph.c(),main = "Country projection network of selected seasons",edge.curved=0,vertex.label.family = "sans", vertex.label.color	= "black")
  )
  
  output$similarity.1 <- renderText(
    similarity(g.matches,v = c(input$club1,input$club2),method = "jaccard")[1,2]
  )
  
  observe({
    if(!is.null(input$club1))
      updateSelectInput(session,"club3",
                        choices = input$club1,
                        selected = isolate(input$club3))
  })
  
  output$similarity.2 <- renderText(
    similarity(g.matches,v = c(input$club1,input$club4),method = "jaccard")[1,2]
  )
  
  output$name_club1 <- renderText({
    
    paste(sep = "","You have chosen: ",input$club1,". According to the Jaccard distance measurement,",
          ifelse(
            similarity(g.matches,v = c(input$club1,input$club2),method = "jaccard")[1,2] > similarity(g.matches,v = c(input$club1,input$club4),method = "jaccard")[1,2],
            ' the first match is more likely to happen.',ifelse(
              similarity(g.matches,v = c(input$club1,input$club2),method = "jaccard")[1,2] == similarity(g.matches,v = c(input$club1,input$club4),method = "jaccard")[1,2],
              ' these two matches are equally to happen',ifelse(
                similarity(g.matches,v = c(input$club1,input$club2),method = "jaccard")[1,2] < similarity(g.matches,v = c(input$club1,input$club4),method = "jaccard")[1,2],
                ' the second match is more likely to happen'
              )
            )
          )
    )
  }) 
  
  input2 <- reactive(
    pull(dplyr::filter(list.clubs, `Team 1` != input1 ), `Team 1`)
  )
  
  score <- reactive(
    for(i in 1:length(input2())){
      output <- round(similarity(g.matches, v=c(input1, input2[i]))[1,2], 2)
      score <- c(score, output)
    }
  )
  
  output$outputtable <- renderTable(
    cbind(input2(), data.table(score())) %>%
      select(Club = input2, Score = score())
  )
  
  
  
  output$significance <- renderTable(
    stargazer(lm(
      formula = average_score~degree,
      data = rg_table), type = "text"
    )
  )
  
  output$Regression_results <- renderPlot(
    rg_table %>% 
      ggplot(aes(x=degree,y=average_score))+
      geom_point(alpha = 0.25)+geom_smooth(method ="lm",
                                           se = FALSE)
  )
  
  output$Regression_results.1 <- renderPlot(
    rg_table %>%
      ggplot(aes(x=`number of games`,y=average_score))+
      geom_point(alpha = 0.25)+geom_smooth(method = "lm",
                                           se = FALSE)
  )
  
  output$significance.1 <- renderTable(
    stargazer(lm(
      formula = average_score~`number of games`,
      data = rg_table
    ),type = "text")
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
