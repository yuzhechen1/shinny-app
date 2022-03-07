server <- function(input, output) {
  
  output$top_teams <- renderTable(
    
    team_round[which(team_round$Season == input$season & team_round$Round == "Semifinals"),1]
  )#top teams by season
  # league_statistics_table <- reactive(
  #   league_statistics[which(league_statistics$country == input$league),]
  # )
  output$league_table <- renderTable(
    league_statistics_table
  )
  
  output$summary_league <- renderTable(
    league_statistics[which(league_statistics$country %in% input$country & league_statistics$`number of clubs` > input$club_number[1] & league_statistics$`number of clubs` < input$club_number[2])  ,]
  )#statistics group by country
  
  output$bar_chart_by_learegue <- renderPlot(
    ggplot(number_trophies_league,aes(Country,`Trophies won`))+geom_bar(stat = "identity",fill = "steelblue")+geom_text(aes(label=`Trophies won`),vjust=-0.2)
  )

  output$degree_table <- renderTable(
    degree_table
  )
  
  output$Degree_distribution <- renderPlot(
    ggplot(dt.g.matches, aes(degree)) + geom_histogram(fill = "grey", colour = "black", binwidth = 1)
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
  output$projection <- renderPlot(
    plot(projection_graph(),main = "Projection network of selected seasons")
  )
  
    #plot(graph_from_data_frame(projection_data[, c("Team 1", "Team 2")],directed=FALSE)
         #),
         # , edge.arrow.size=.2, edge.curved=0, vertex.size = V(graph_from_data_frame(projection_data[, c("Team 1", "Team 2")],
         #  directed=FALSE))$degree, vertex.label.cex	= V(graph_from_data_frame(projection_data[, c("Team 1", "Team 2")]
        #directed=FALSE))$degree.font, vertex.label.family = "sans", vertex.label.color	= "black", edge.color	= "gray10") + par(mar = c(0, 0, 0, 0))
  
    
}
