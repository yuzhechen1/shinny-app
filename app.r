install.packages("tidyverse")
install.packages("gapminder")
install.packages("readxl")
install.packages("visNetwork")
library(shiny)
library(readxl)
library(igraph)
library(tidyverse)
library(dplyr)
library(gapminder)
library(data.table)
library(visNetwork)
setwd("C:/Users/86156/Desktop/硕士/Network data analysis/R working directory/group assignment")
#Descriptive analysis part (Thom and Chen)
Champions_League_Data_1955_2015 <- read_excel("C:/Users/86156/Desktop/硕士/Network data analysis/R working directory/group assignment/Source data/Champions League Data 1955-2021.xlsx")

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
number_trophies_league <- Final_winners[which(Final_winners$Date != 0),]
names(number_trophies_league)[1] <- "Country"
names(number_trophies_league)[2] <- "Trophies won"
#bar chart of trophies of each league

match_date <- Champions_League_Data_1955_2015$Date
match_date <- as.data.frame(as.numeric(format(match_date,"%Y")))
Champions_League_Data_1955_2015_year <- cbind(Champions_League_Data_1955_2015,match_date$`as.numeric(format(match_date, "%Y"))`)
names(Champions_League_Data_1955_2015_year)[10] <- "year"
#add year data into the data set


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
plot(g.matches.15, edge.arrow.size=.2, edge.curved=0, vertex.size = V(g.matches.15)$degree, vertex.label.cex	= V(g.matches.15)$degree.font, vertex.label.family = "sans", vertex.label.color	= "black", edge.color	= "gray10") + par(mar = c(0, 0, 0, 0))

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

# Order datatable on degree
degree_table <- head(dt.g.matches[order(-degree)],30)

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
dt.g.matches.c[order(-degree)]

ggplot(dt.g.matches.c, aes(degree)) + geom_histogram(fill = "grey", colour = "black", bins = 15)

shinyApp(ui = ui, server = server)


