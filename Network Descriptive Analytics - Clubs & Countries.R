
# Load in data and packages
library(tidyverse)
library(ggplot2)
library(igraph)
library(data.table)
setwd("~/Studie/Network Data Analytics/Assignments/Group Assignment")
dt.matches <- read_delim("Champions League Data 1955-2021.csv", delim = ";")

# Create unique list of teams and country and rename it
dt.club.country <- unique(dt.matches[, c("Team 1", "Country Team 1")]) %>%
  select('Club' = `Team 1`, 'Country' = `Country Team 1`)

# ------------------------------------------------------------------------------
### CLUBS ANALYTICS ###
# ------------------------------------------------------------------------------
# Filtering variables
select.season <- c("2020/2021")
select.country <- c("England", "Netherlands", "Spain", "Italy", "Germany", "France")
# ------------------------------------------------------------------------------

# Apply filters on
dt.matches.filtered <- dt.matches %>%
  filter(Season %in% select.season)

# Create undirected graph
g.matches.filtered <- graph_from_data_frame(dt.matches.filtered[, c("Team 1", "Team 2")],
                                   directed=FALSE)

# Calculate degree
V(g.matches.filtered)$degree <- round(degree(g.matches.filtered), 0)
V(g.matches.filtered)$closeness <- round(closeness(g.matches.filtered), 4)
V(g.matches.filtered)$betweenness <- round(betweenness(g.matches.filtered), 0)
V(g.matches.filtered)$evcent <- round(evcent(g.matches.filtered)$vector, 3)

# Create data table
dt.g.matches.filtered <- data.table(get.data.frame(g.matches.filtered, "vertices")) %>%
  mutate(Club = name) %>%
  left_join(dt.club.country, by = c("Club" = "Club"))

# Order datatable on degree
network.table.clubs <- dt.g.matches.filtered[, c("Club", "Country", "degree", "closeness", "betweenness", "evcent")][order(-degree)] %>%
  filter(Country %in% select.country)

# ------------------------------------------------------------------------------
# Summary on selected seasons
number.nodes <- gorder(g.matches.filtered)
number.edges <- gsize(g.matches.filtered)
avg.path.length <- mean_distance(g.matches.filtered, directed = TRUE, unconnected = TRUE) # Better understand what this actually means
avg.clustering.coefficient <- transitivity(g.matches.filtered, type = "average")          # Better understand what this actually means
diameter <- diameter(g.matches.filtered)                                                  # Better understand what this actually means
avg.degree <- mean(V(g.matches.filtered)$degree)                                          # Always 7.8125 in current league set up
  
# Top 20 clubs
head(network.table.clubs, 20)

# Degree distribution
ggplot(network.table.clubs, aes(degree)) + geom_histogram(fill = "grey", colour = "black", binwidth = 1)

# ------------------------------------------------------------------------------
### COUNTRY ANALYTICS ###
# ------------------------------------------------------------------------------
# Filtering variables
select.season <- c("2020/2021")
# ------------------------------------------------------------------------------

# Apply filters on
dt.matches.filtered <- dt.matches %>%
  filter(Season %in% select.season)

# Create undirected graph
g.countries.filtered <- graph_from_data_frame(dt.matches.filtered[, c("Country Team 1", "Country Team 2")],
                                            directed=FALSE)

# Calculate degree
V(g.countries.filtered)$degree <- round(degree(g.countries.filtered), 0)
V(g.countries.filtered)$closeness <- round(closeness(g.countries.filtered), 4)
V(g.countries.filtered)$betweenness <- round(betweenness(g.countries.filtered), 0)
V(g.countries.filtered)$evcent <- round(evcent(g.countries.filtered)$vector, 3)

# Create data table
dt.g.countries.filtered <- data.table(get.data.frame(g.countries.filtered, "vertices")) %>%
  mutate(Country = name)

# Order datatable on degree
network.table.countries <- dt.g.countries.filtered[, c("Country", "degree", "closeness", "betweenness", "evcent")][order(-degree)]


# ------------------------------------------------------------------------------
# Summary on selected seasons
number.nodes <- gorder(g.countries.filtered)
number.edges <- gsize(g.countries.filtered)
avg.path.length <- mean_distance(g.countries.filtered, directed = TRUE, unconnected = TRUE) # Better understand what this actually means
avg.clustering.coefficient <- transitivity(g.countries.filtered, type = "average")          # Better understand what this actually means
diameter <- diameter(g.countries.filtered)                                                  # Better understand what this actually means
avg.degree <- mean(V(g.countries.filtered))                                                 # Better understand what this actually means

# Top 10 clubs
head(network.table.countries, 10)

# Degree distribution
ggplot(network.table.countries, aes(degree)) + geom_histogram(fill = "grey", colour = "black", binwidth = 1)