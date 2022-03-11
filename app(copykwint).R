# >> Match Results
# Create a Results Table 
results_table <- Champions_League_Data_1955_2015

# Net Score Column + Team Result Column
results_table$net_score <- (results_table$`Goals Team 1` - results_table$`Goal Team 2`)
results_table$`Team 1 Result` <- results_table$net_score
results_table$`Team 2 Result` <- results_table$net_score
results_table <- as.data.frame(results_table)

# Rename to Winner, Loser or Draw
results_table["Team 1 Result"][results_table["Goals Team 1"] == results_table["Goal Team 2"]] <- "Draw"
results_table["Team 2 Result"][results_table["Goals Team 1"] == results_table["Goal Team 2"]] <- "Draw"
results_table["Team 2 Result"][results_table["Goals Team 1"] > results_table["Goal Team 2"]] <- "Loser"
results_table["Team 1 Result"][results_table["Goals Team 1"] < results_table["Goal Team 2"]] <- "Loser"
results_table["Team 1 Result"][results_table["Goals Team 1"] > results_table["Goal Team 2"]] <- "Winner"
results_table["Team 2 Result"][results_table["Goals Team 1"] < results_table["Goal Team 2"]] <- "Winner"
View(results_table)

# Calculate Points Won per Team
results_table$Team1Points <- results_table$`Team 1 Result`
results_table$Team2Points <- results_table$`Team 2 Result`
results_table["Team1Points"][results_table["Team1Points"] == "Winner"] <- 3
results_table["Team2Points"][results_table["Team2Points"] == "Winner"] <- 3
results_table["Team1Points"][results_table["Team1Points"] == "Draw"] <- 1
results_table["Team2Points"][results_table["Team2Points"] == "Draw"] <- 1
results_table["Team1Points"][results_table["Team1Points"] == "Loser"] <- 0
results_table["Team2Points"][results_table["Team2Points"] == "Loser"] <- 0

#Create Winner Column
results_table <- results_table %>%
  mutate(`Winner` = if_else(`Team 1 Result` == "Winner", `Team 1`, `Team 2`))
results_table["Winner"][results_table["Team2Points"] == 1] <- ""
View(results_table)

#Create Loser Column
results_table <- results_table %>%
  mutate(`Loser` = if_else(`Team 1 Result` != "Winner", `Team 1`, `Team 2`))
results_table["Loser"][results_table["Team2Points"] == 1] <- ""
View(results_table)

# >> Win Total
wins_per_team <- aggregate(results_table$Winner, by=list(type=results_table$Winner),length)
View(wins_per_team)

# >> Loss Total
loss_per_team <- aggregate(results_table$Loser, by=list(type=results_table$Loser),length)
View(loss_per_team)

#Create combined table 
results_per_team <- merge(wins_per_team, loss_per_team, by="type")
names(results_per_team)[1] <- "Club"
names(results_per_team)[2] <- "Matches Won"
names(results_per_team)[3] <- "Matches Lost"
results_per_team <- results_per_team[!(results_per_team$Club==""),]
View(results_per_team)

# Goals Scored and Scored Against

goals_team1 <- Champions_League_Data_1955_2015[,c("Team 1","Goals Team 1")]
names(goals_team1)[1] <- "team name"
names(goals_team1)[2] <- "goals"
goals_team2 <- Champions_League_Data_1955_2015[,c("Team 2","Goal Team 2")]
names(goals_team2)[1] <- "team name"
names(goals_team2)[2] <- "goals"
goals_team <- rbind(goals_team1,goals_team2)
goals_team <- goals_team %>% group_by(`team name`) %>% summarize(`goals` = sum(goals))

goals_against_team1 <- Champions_League_Data_1955_2015[,c("Team 1","Goal Team 2")]
names(goals_against_team1)[1] <- "team name"
names(goals_against_team1)[2] <- "goals"
goals_against_team2 <- Champions_League_Data_1955_2015[,c("Team 2","Goals Team 1")]
names(goals_against_team2)[1] <- "team name"
names(goals_against_team2)[2] <- "goals"
goals_against_team <- rbind(goals_against_team2,goals_against_team2)
goals_against_team <- goals_against_team %>% group_by(`team name`) %>% summarize(`goals` = sum(goals))

goals_merged <- merge(goals_team, goals_against_team, by="team name")
names(goals_merged)[1] <- "Club"
names(goals_merged)[2] <- "Goals Scored"
names(goals_merged)[3] <- "Goals Scored Against"
View(goals_merged)
