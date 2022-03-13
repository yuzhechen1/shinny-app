# install.packages("tidyverse")
# install.packages("gapminder")
# install.packages("readxl")
# install.packages("visNetwork")
install.packages("shinythemes")
install.packages("stringr")
options(encoding = "UTF-8")
library(shiny)
library(readxl)
library(igraph)
library(tidyverse)
library(dplyr)
library(gapminder)
library(data.table)
library(visNetwork)
library(data.table)
library(DT)
library(bslib)
library(shinythemes)
library(stargazer)
library(stringr)
setwd("C:/Users/86156/Desktop/master/Network data analysis/R working directory/group assignment")
#Descriptive analysis part (Thom and Chen)
Champions_League_Data_1955_2015 <- read_excel("C:/Users/86156/Desktop/master/Network data analysis/R working directory/group assignment/shiny app/source data/Champions League Data 1955-2021.xlsx")

all_rounds <- unique(Champions_League_Data_1955_2015[,3])
all_seasons <- unique(Champions_League_Data_1955_2015[,2])
all_countries <- unique(Champions_League_Data_1955_2015[,5])
all_teams <- unique(Champions_League_Data_1955_2015[,4])
match_year <- as.data.frame(as.numeric(substr(Champions_League_Data_1955_2015$Season,1,4)))
Champions_League_Data_1955_2015_year <- cbind(Champions_League_Data_1955_2015,match_year)
names(Champions_League_Data_1955_2015_year)[10] <- "match_year"

team_round <- Champions_League_Data_1955_2015[,c("Team 1","Round","Season")]
names(team_round)[1] <- "Top teams"#subset of team,round and season, to select top teams in each season
country_final <- Champions_League_Data_1955_2015[,c("Country Team 1","Team 1","Round","Season")]
names(country_final)[1] <- "Winning Country"
names(country_final)[2] <- "Winning Team"
#subset of country, round and season, to select winning country in each season

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
number_trophies_league <- Final_winners[which(Final_winners$Date != 0),]
names(number_trophies_league)[1] <- "Country"
names(number_trophies_league)[2] <- "Trophies won"
#bar chart of trophies of each league

Finalmatches_last_10_years <- Champions_League_Data_1955_2015_year[which(Champions_League_Data_1955_2015_year$Round == "Final" & 
                                                                            Champions_League_Data_1955_2015_year$match_year > 2010),]
Final_winners_last_10_years <- aggregate(Finalmatches_last_10_years,by=list(type=Finalmatches_last_10_years$`Country Team 1`),length)
Final_winners_last_10_years <- Final_winners_last_10_years[,-c(3:11)]
Final_winners_last_10_years <- merge(all_countries,Final_winners_last_10_years,by.x = "Country Team 1",by.y = "type",all.x = TRUE,all.y = FALSE)
Final_winners_last_10_years[is.na(Final_winners_last_10_years)] <- 0
number_trophies_league_last_10_years <- Final_winners_last_10_years[which(Final_winners_last_10_years$Date != 0),]
names(number_trophies_league_last_10_years)[1] <- "Country"
names(number_trophies_league_last_10_years)[2] <- "Trophies won"
#analysis for last ten years

match_date <- Champions_League_Data_1955_2015$Date
match_date <- as.data.frame(as.numeric(format(match_date,"%Y")))
Champions_League_Data_1955_2015_year <- cbind(Champions_League_Data_1955_2015,match_date$`as.numeric(format(match_date, "%Y"))`)
names(Champions_League_Data_1955_2015_year)[10] <- "year"
#add year data into the data set

## Descriptive analysis (club part)
dt.matches <- Champions_League_Data_1955_2015_year
dt.matches$winner <- ifelse(dt.matches$`Goals Team 1` > dt.matches$`Goal Team 2`, dt.matches$`Team 1`,
                            ifelse(dt.matches$`Goals Team 1` < dt.matches$`Goal Team 2`, dt.matches$`Team 2`, ""))
dt.matches$loser <- ifelse(dt.matches$`Goals Team 1` < dt.matches$`Goal Team 2`, dt.matches$`Team 1`,
                           ifelse(dt.matches$`Goals Team 1` > dt.matches$`Goal Team 2`, dt.matches$`Team 2`, ""))
# Lists
# ------------------------------------------------------------------------------
# Create unique list of clubs and country and rename it
l.club.country <- unique(dt.matches[, c("Team 1", "Country Team 1")]) %>%
  select('Club' = `Team 1`, 'Country' = `Country Team 1`)

# Create unique list of clubs and country and rename it
l.club.season <- unique(dt.matches[, c("Team 1", "Season")]) %>%
  select('Club' = `Team 1`, 'Season' = `Season`)

# Create lists of clubs that won the final
l.club.final <- unique(dt.matches[, c("Team 1", "Season", "Round")]) %>%
  filter(Round == "Final") %>%
  select('Club' = `Team 1`, 'Season' = `Season`)

# Create list of goals scored per club
l.goals.scored.home <- dt.matches[, c('Team 1', 'Goals Team 1')] %>%
  select(Club = 'Team 1', goals.scored = 'Goals Team 1')
l.goals.scored.away <- dt.matches[, c(Club = 'Team 2', goals ='Goal Team 2')] %>%
  select(Club = 'Team 2', goals.scored = 'Goal Team 2')
l.goals.scored <- rbind(l.goals.scored.home, l.goals.scored.away)

# Create list of goals scored per club
l.goals.against.home <- dt.matches[, c('Team 1', 'Goal Team 2')] %>%
  select(Club = 'Team 1', goals.against = 'Goal Team 2')
l.goals.against.away <- dt.matches[, c(Club = 'Team 2', goals ='Goals Team 1')] %>%
  select(Club = 'Team 2', goals.against = 'Goals Team 1')
l.goals.against <- rbind(l.goals.against.home, l.goals.against.away)

# Data frames
# ------------------------------------------------------------------------------
# Count wins per club
df.win.club <- dt.matches %>%
  group_by(Club = dt.matches$winner) %>%
  summarize(wins = n()) %>%
  arrange(desc(wins))

# Count losses per club
df.lose.club <- dt.matches %>%
  group_by(Club = dt.matches$loser) %>%
  summarize(losses = n()) %>%
  arrange(desc(losses))

# Count seasons per club
df.seasons.club <- l.club.season %>%
  group_by(Club) %>%
  summarize(seasons = n())

# Count trophies per club
df.trophies.club <- l.club.final %>%
  group_by(Club) %>%
  summarize(trophies = n()) %>%
  arrange(desc(trophies))

# Goals scored per club
df.goals.scored <- l.goals.scored %>%
  group_by(Club) %>%
  summarize(goals.scored = sum(goals.scored)) %>%
  arrange(desc(goals.scored))

# Goals against per club
df.goals.against <- l.goals.against %>%
  group_by(Club) %>%
  summarize(goals.against = sum(goals.against)) %>%
  arrange(desc(goals.against))

# Make datatables of dataframes (needed for left join to work)
dt.seasons.club <- data.table(df.seasons.club)
dt.trophies.club <- data.table(df.trophies.club)
dt.wins.club <- data.table(df.win.club)
dt.losses.club <- data.table(df.lose.club)
dt.goals.scored <- data.table(df.goals.scored)
dt.goals.against <- data.table(df.goals.against)

# Create undirected graph
dt.matches <- graph_from_data_frame(dt.matches[, c("Team 1", "Team 2")],
                                    directed=FALSE)

# Calculate matches played
V(dt.matches)$matches <- round(degree(dt.matches), 0)

# Create data table and add countries
dt.matches <- data.table(get.data.frame(dt.matches, "vertices")) %>%
  mutate(Club = name) %>%
  left_join(l.club.country, by = c("Club" = "Club"))

# Select correct columns
club.statistics <- dt.matches[, c('Club', 'Country', 'matches')][order(-matches)]

# Add countries per club
club.statistics <- club.statistics %>% 
  left_join(dt.seasons.club, by = c("Club" = "Club"))

# Add trophies per club
club.statistics <- club.statistics %>% 
  left_join(dt.trophies.club, by = c("Club" = "Club"))

# Add wins per club
club.statistics <- club.statistics %>% 
  left_join(dt.wins.club, by = c("Club" = "Club"))

# Add losses per club
club.statistics <- club.statistics %>% 
  left_join(dt.losses.club, by = c("Club" = "Club"))

# Add goals scored per club
club.statistics <- club.statistics %>% 
  left_join(dt.goals.scored, by = c("Club" = "Club"))

# Add goals against per club
club.statistics <- club.statistics %>% 
  left_join(dt.goals.against, by = c("Club" = "Club"))

# Calculate draws
club.statistics$draws <- club.statistics$matches - club.statistics$wins - club.statistics$
  
  # Select correct columns
  club.statistics <- club.statistics[, c('Club', 'Country', 'seasons', 'matches', 'trophies', 'wins', 'draws', 'losses', 'goals.scored', 'goals.against')]

# Get rid of NA
club.statistics <- club.statistics %>%
  mutate_all(~replace(., is.na(.), 0))



#Network analysis part (Bram)
#build an undirected network of the season 2015/2016
dt.matches.15 <- Champions_League_Data_1955_2015 %>%
  filter(Season == "2015/2016")
g.matches.15 <- graph_from_data_frame(dt.matches.15[, c("Team 1", "Team 2")],
                                      directed=FALSE)
# Calculate values for network graph
V(g.matches.15)$degree <- degree(g.matches.15)

V(g.matches.15)$degree.font <- degree(g.matches.15)/30

# Build network graph
# Ideas for the formatting of the graph: https://kateto.net/netscix2016.html
#plot(g.matches.15, edge.arrow.size=.2, edge.curved=0, vertex.size = V(g.matches.15)$degree, vertex.label.cex	= V(g.matches.15)$degree.font, vertex.label.family = "sans", vertex.label.color	= "black", edge.color	= "gray10") + par(mar = c(0, 0, 0, 0))

# Create undirected graph
g.matches <- graph_from_data_frame(Champions_League_Data_1955_2015[, c("Team 1", "Team 2")],
                                   directed=FALSE)
# Calculate degree
V(g.matches)$degree <- round(degree(g.matches), 0)
V(g.matches)$closeness <- round(closeness(g.matches), 4)
V(g.matches)$betweenness <- round(betweenness(g.matches), 0)
V(g.matches)$evcent <- round(evcent(g.matches)$vector, 3)

# Create data table
g.matches.all <- induced.subgraph(g.matches, V(g.matches))
dt.g.matches <- data.table(get.data.frame(g.matches.all, "vertices"))

dt.club.country <- unique(Champions_League_Data_1955_2015[,c("Team 1","Country Team 1")]) %>%
  select('Club' = `Team 1`,'Country' = `Country Team 1`)


# Order datatable on degree
degree_table <- dt.g.matches[order(-degree)] %>%
 mutate(Club = name) %>%
   left_join(dt.club.country,by = c("Club" = "Club"))
degree_table <- degree_table[,-6]

# Plot for degree distributiom
ggplot(dt.g.matches, aes(degree)) + geom_histogram(fill = "grey", colour = "black", binwidth = 1)

# Create undirected graph
g.matches.c <- graph_from_data_frame(Champions_League_Data_1955_2015[, c("Country Team 1", "Country Team 2")],
                                     directed=FALSE)

# Calculate degree
V(g.matches.c)$degree <- round(degree(g.matches.c), 0)
V(g.matches.c)$closeness <- round(closeness(g.matches.c), 4)
V(g.matches.c)$betweenness <- round(betweenness(g.matches.c), 0)
V(g.matches.c)$evcent <- round(evcent(g.matches.c)$vector, 3)

# Create data table
g.matches.all.c <- induced.subgraph(g.matches.c, V(g.matches.c))
dt.g.matches.c <- data.table(get.data.frame(g.matches.all.c, "vertices"))
degree_table.c <- dt.g.matches.c[order(-degree)]

ggplot(dt.g.matches.c, aes(degree)) + geom_histogram(fill = "grey", colour = "black", bins = 15)


# ------------------------------------------------------------------------------
### LINK PREDICTION TABLE ###
# ------------------------------------------------------------------------------
# Load in data

# Create undirected graph
g.matches <- graph_from_data_frame(Champions_League_Data_1955_2015[, c("Team 1", "Team 2")],
                                   directed=FALSE)

# List of unique clubs
list.clubs <- distinct(Champions_League_Data_1955_2015[, "Team 1"])

# Select club you want to compare
input1 <- "AFC Ajax"

# Exclude that club from the list (use pull to create vector instead of tibble)
input2 <- pull(dplyr::filter(list.clubs, `Team 1` != input1 ), `Team 1`)

# Loop formula which pastes all the outcomes underneath each other
score <- c()
for(i in 1:length(input2)){
  output <- round(similarity(g.matches, v=c(input1, input2[i]))[1,2], 2)
  score <- c(score, output)
}

# Combine outcomes with names
outputtable <- cbind(input2, data.table(score)) %>%
  select(Club = input2, Score = score)

# Rank table show top 20
head(data.table(outputtable[order(-score),]),20)




#Inference analysis part (Chen)
average_score <- as.data.frame(tapply(Champions_League_Data_1955_2015$`Goals Team 1`,Champions_League_Data_1955_2015$`Country Team 1`,mean))  
names(average_score)[1] <- "Average_score"
#dependent variable: average score

dt.g.matches.c<- dt.g.matches.c[order(dt.g.matches.c$name),]
#independent variable: degree

names(number_game_by_league)[1] <- "Country"
#independent variable: number of games

rg_table <- cbind(number_game_by_league,dt.g.matches.c,average_score$Average_score)
rg_table <- rg_table[,-3]
names(rg_table)[7] <- "average_score"

lm1 <- lm(
  formula = average_score~degree,
  data = rg_table)
stargazer(lm1,type="text") 
rg_table %>% 
  ggplot(aes(x=degree,y=average_score))+
  geom_point(alpha = 0.25)+geom_smooth(method ="lm",
                                       se = FALSE)

lm2 <- lm(
  formula = average_score~`number of games`,
  data = rg_table
)

shinyApp(ui = ui, server = server)

