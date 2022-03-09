install.packages("tidyverse")
install.packages("gapminder")
library(shiny)
library(readxl)
library(igraph)
library(tidyverse)
library(dplyr)
library(gapminder)
setwd("C:/Users/86156/Desktop/硕士/Network data analysis/R working directory/group assignment")
install.packages("readxl")

Champions_League_Data_1955_2015 <- read_excel("C:/Users/86156/Desktop/硕士/Network data analysis/R working directory/group assignment/Source data/Champions League Data 1955-2015.xlsx")

all_rounds <- unique(Champions_League_Data_1955_2015[,3])
all_seasons <- unique(Champions_League_Data_1955_2015[,2])
all_countries <- unique(Champions_League_Data_1955_2015[,5])
all_teams <- unique(Champions_League_Data_1955_2015[,4])

team_round <- Champions_League_Data_1955_2015[,c("Team 1","Round","Season")]
names(team_round)[1] <- "Top teams"#subset of team,round and season, to select top teams in each season

number_club_by_league <- Champions_League_Data_1955_2015 %>% distinct(`Team 1`,.keep_all = TRUE)
number_club_by_league <- aggregate(number_club_by_league$`Team 1`,by=list(type=number_club_by_league$`Country Team 1`),length)
names(number_club_by_league)[1] <- "country"
names(number_club_by_league)[2] <- "number of clubs"
#count number of clubs of each league
number_game_by_league <- aggregate(Champions_League_Data_1955_2015,by=list(type=Champions_League_Data_1955_2015$`Country Team 1`),length)
number_game_by_league <- number_game_by_league[,1:2]
names(number_game_by_league)[2] <- "number of games"
league_statistics <- cbind(number_club_by_league,number_game_by_league$`number of games`)
names(league_statistics)[3] <- "total number of games"
#count number of games played in all seasons of each league
number_participation_by_league_season <- Champions_League_Data_1955_2015 %>% 
  group_by(Season,`Country Team 1`) %>%
  summarise(n = n_distinct(`Team 1`))
names(number_participation_by_league_season)[3] <- "number of participations"
number_participation_by_league <- aggregate(number_participation_by_league_season[,3],by=list(type=number_participation_by_league_season$`Country Team 1`),FUN = sum)
number_participation_by_league <- number_participation_by_league[,c('type','number of participations')]
league_statistics <- cbind(league_statistics,number_participation_by_league$`number of participations`)
names(league_statistics)[4] <- "total number of participations"
average_participation_league <- as.data.frame(league_statistics[,4]/62)
league_statistics <- cbind(league_statistics,average_participation_league[,1])
names(league_statistics)[5] <- "Average number of participations"
#count number of (average) participations of each league

Finalmatches <- Champions_League_Data_1955_2015[which(Champions_League_Data_1955_2015$Round == "Final"),]
t1 <- Finalmatches %>% distinct(Season,.keep_all = TRUE)
Final_winners <- aggregate(Finalmatches,by=list(type=Finalmatches$`Country Team 1`),length)
Final_winners <- Final_winners[,-c(3:10)]
Final_winners <- merge(all_countries,Final_winners,by.x = "Country Team 1",by.y = "type",all.x = TRUE,all.y = FALSE)
Final_winners[is.na(Final_winners)] <- 0
league_statistics <-cbind(league_statistics,Final_winners$Date)
names(league_statistics)[6] <- "Trophies won"
#count number of trophies of each league

match_date <- Champions_League_Data_1955_2015$Date
match_date <- as.data.frame(as.numeric(format(match_date,"%Y")))
Champions_League_Data_1955_2015_year <- cbind(Champions_League_Data_1955_2015,match_date$`as.numeric(format(match_date, "%Y"))`)
names(Champions_League_Data_1955_2015_year)[10] <- "year"

goals_team1 <- Champions_League_Data_1955_2015[,c("Team 1","Goals Team 1")]
names(goals_team1)[1] <- "team name"
names(goals_team1)[2] <- "goals"
goals_team2 <- Champions_League_Data_1955_2015[,c("Team 2","Goal Team 2")]
names(goals_team2)[1] <- "team name"
names(goals_team2)[2] <- "goals"
goals_team <- rbind(goals_team1,goals_team2)

goals_team

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

# >> Trophies Won
trophies_table <- 
  
  # >> Match Results
  results_table <- Champions_League_Data_1955_2015
results_table %>% mutate("Team 1 Result" = 
                           case_when(`Goals Team 1` > `Goal Team 2` ~ "Winner",
                                     `Goals Team 1` == `Goal Team 2` ~ "Draw",
                                     `Goals Team 1` < `Goal Team 2` ~ "Loser")
)

results_table %>% mutate("Team 2 Result" = 
                           case_when(`Goals Team 1` > `Goal Team 2` ~ "Loser",
                                     `Goals Team 1` == `Goal Team 2` ~ "Draw",
                                     `Goals Team 1` < `Goal Team 2` ~ "Winner")
)

shinyApp(ui = ui, server = server)

