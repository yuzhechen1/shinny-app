server <- function(input, output) {
  
  output$raw_data <- renderTable(
    Champions_League_Data_1955_2015
  )
  
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
    
    team_round[which(team_round$Season == input$season & team_round$Round == "Semifinals"),1]
  )#top teams by season
  
  output$Winner <- renderTable(
    country_final[which(country_final$Season == input$season & country_final$Round == "Final"),1]
  )
  
  output$league_table <- renderTable(
    league_statistics_table
  )
  
  output$summary_league <- renderTable(
    league_statistics[which(league_statistics$country %in% input$country & league_statistics$`number of clubs` > input$club_number[1] & league_statistics$`number of clubs` < input$club_number[2])  ,]
  )#statistics group by country
  
  output$bar_chart_by_learegue <- renderPlot(
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
  output$projection <- renderPlot(
    plot(projection_graph(),main = "Team projection network of selected seasons")
  )
  projection_data.c <- reactive({
    Champions_League_Data_1955_2015 %>%
      filter(Champions_League_Data_1955_2015$Season %in% input$Seasons.c)
  })
  
  projection_graph.c <- reactive({
    graph_from_data_frame(projection_data.c()[,c("Country Team 1","Country Team 2")],
                                        directed =FALSE)
  })
  output$projection.c <- renderPlot(
    plot(projection_graph.c(),main = "Country projection network of selected seasons")
  )
  
  output$significance <- renderTable(
    stargazer(lm(
      formula = average_score~degree,
      data = rg_table,type="text") 
    )
  )
  
  output$Regression_results <- renderPlot(
    rg_table %>% 
      ggplot(aes(x=degree,y=average_score),main = "Linear model of average score and evcent centrality")+
      geom_point(alpha = 0.25)+geom_smooth(method ="lm",
                                           se = FALSE)
  )
  
  
    #plot(graph_from_data_frame(projection_data[, c("Team 1", "Team 2")],directed=FALSE)
         #),
         # , edge.arrow.size=.2, edge.curved=0, vertex.size = V(graph_from_data_frame(projection_data[, c("Team 1", "Team 2")],
         #  directed=FALSE))$degree, vertex.label.cex	= V(graph_from_data_frame(projection_data[, c("Team 1", "Team 2")]
        #directed=FALSE))$degree.font, vertex.label.family = "sans", vertex.label.color	= "black", edge.color	= "gray10") + par(mar = c(0, 0, 0, 0))
  
    
}
