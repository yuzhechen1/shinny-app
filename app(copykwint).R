# >> Games Played
#Count number of games played in all seasons of each league
number_game_by_club <- aggregate(Champions_League_Data_1955_2015,by=list(type=Champions_League_Data_1955_2015$`Team 1`),length)
number_game_by_club_2 <- aggregate(Champions_League_Data_1955_2015,by=list(type=Champions_League_Data_1955_2015$`Team 2`),length)
number_game_by_club <- number_game_by_club[,1:2]
number_game_by_club_2 <- number_game_by_club_2[,1:2]
names(number_game_by_club)[1] <- "Club"
names(number_game_by_club)[2] <- "number of games"
names(number_game_by_club_2)[1] <- "Club"
names(number_game_by_club_2)[2] <- "number of games"
number_game_by_club
number_game_by_club_2

## COMBINE DATA TABLES >> HOW? 
  
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
