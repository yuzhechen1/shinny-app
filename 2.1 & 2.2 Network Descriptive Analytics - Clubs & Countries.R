
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

# Calculate centrality measures
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
select.season.2 <- c("2020/2021")
# ------------------------------------------------------------------------------

# Apply filters on
dt.matches.filtered.2 <- dt.matches %>%
  filter(Season %in% select.season.2)

# Create undirected graph
g.countries.filtered <- graph_from_data_frame(dt.matches.filtered.2[, c("Country Team 1", "Country Team 2")],
                                            directed=FALSE)

# Calculate centrality measures
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
number.nodes.2 <- gorder(g.countries.filtered)
number.edges.2 <- gsize(g.countries.filtered)
avg.path.length.2 <- mean_distance(g.countries.filtered, directed = TRUE, unconnected = TRUE) # Better understand what this actually means
avg.clustering.coefficient.2 <- transitivity(g.countries.filtered, type = "average")          # Better understand what this actually means
diameter.2 <- diameter(g.countries.filtered)                                                  # Better understand what this actually means
avg.degree.2 <- mean(V(g.countries.filtered))                                                 # Better understand what this actually means

# Top 10 clubs
head(network.table.countries, 10)

# Degree distribution
ggplot(network.table.countries, aes(degree)) + geom_histogram(fill = "grey", colour = "black", binwidth = 1)

# ------------------------------------------------------------------------------
### PROJECTION INTO CLUB SPACE ###
# ------------------------------------------------------------------------------
# Filtering variables
select.season.3 <- c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019", "2019/2020", "2020/2021")
select.min.games <- 20

# Apply filters on
dt.matches.filtered.3 <- dt.matches %>%
  filter(Season %in% select.season.3)

# Create undirected graph
g.matches.filtered.2 <- graph_from_data_frame(dt.matches.filtered.3[, c("Team 1", "Team 2")],
                                            directed=FALSE)

# Calculate centrality measures
V(g.matches.filtered.2)$degree <- round(degree(g.matches.filtered.2), 0)

# Filter on degrees
g.matches.filtered.2.1 <- induced.subgraph(g.matches.filtered.2, V(g.matches.filtered.2)[degree >= select.min.games])

# Calculate variables for lay-out of graph
max.degree <- max(degree(g.matches.filtered.2.1))
size.factor <- 25 / max.degree
V(g.matches.filtered.2.1)$size <- round(degree(g.matches.filtered.2.1), 0) * size.factor
V(g.matches.filtered.2.1)$text <- V(g.matches.filtered.2.1)$size / 25

# Plot graph
plot(g.matches.filtered.2.1, edge.curved=0, vertex.size = V(g.matches.filtered.2.1)$size, vertex.label.cex = V(g.matches.filtered.2.1)$text, vertex.label.family = "sans", vertex.label.color	= "black")

## NICE TO HAVE: CHECK IF WE CAN MAKE EDGES WEIGHTED

# Extra information
# ------------------------------------------------------------------------------
most.succesfull.team <- data.table(get.data.frame(g.matches.filtered.2.1, "vertices"))[order(-degree)][, name][1]
number.nodes.3 <- gorder(g.matches.filtered.2.1)                                                # Clubs included
number.edges.3 <- gsize(g.matches.filtered.2.1)                                                 # This does not mean that much as long as weight edge is not included
avg.path.length.3 <- mean_distance(g.matches.filtered.2.1, directed = TRUE, unconnected = TRUE) # Better understand what this actually means
avg.clustering.coefficient.3 <- transitivity(g.matches.filtered.2.1, type = "average")          # Better understand what this actually means
diameter.3 <- diameter(g.matches.filtered.2.1)                                                  # Better understand what this actually means


# ------------------------------------------------------------------------------
### PROJECTION INTO COUNTRY SPACE ###
# ------------------------------------------------------------------------------
# Filtering variables
select.season.4 <- c("2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019", "2019/2020", "2020/2021")
select.min.games.2 <- 100

# Apply filters on
dt.matches.filtered.4 <- dt.matches %>%
  filter(Season %in% select.season.4)

# Create undirected graph
g.countries.filtered.2 <- graph_from_data_frame(dt.matches.filtered.4[, c("Country Team 1", "Country Team 2")],
                                              directed=FALSE)

# Calculate centrality measures
V(g.countries.filtered.2)$degree <- round(degree(g.countries.filtered.2), 0)

# Filter on degrees
g.countries.filtered.2.1 <- induced.subgraph(g.countries.filtered.2, V(g.countries.filtered.2)[degree >= select.min.games.2])

# Calculate variables for lay-out of graph
max.degree <- max(degree(g.countries.filtered.2))
size.factor <- 25 / max.degree
V(g.countries.filtered.2.1)$size <- round(degree(g.countries.filtered.2.1), 0) * size.factor
V(g.countries.filtered.2.1)$text <- V(g.countries.filtered.2.1)$size / 25

# Clean up edges
g.countries.filtered.2.2 <- simplify(g.countries.filtered.2.1, remove.loops = T)

# Plot graph
plot(g.countries.filtered.2.2, edge.curved=0, vertex.size = V(g.countries.filtered.2.2)$size, vertex.label.cex = V(g.countries.filtered.2.2)$text, vertex.label.family = "sans", vertex.label.color	= "black")

## NICE TO HAVE: CHECK IF WE CAN MAKE EDGES WEIGHTED

# Extra information
# ------------------------------------------------------------------------------
most.succesfull.country <- data.table(get.data.frame(g.countries.filtered.2.2, "vertices"))[order(-degree)][, name][1]
number.nodes.4 <- gorder(g.countries.filtered.2.2)                                                # Countries included
number.edges.4 <- gsize(g.countries.filtered.2.2)                                                 # This does not mean that much as long as weight edge is not included
avg.path.length.4 <- mean_distance(g.countries.filtered.2.2, directed = TRUE, unconnected = TRUE) # Better understand what this actually means
avg.clustering.coefficient.4 <- transitivity(g.countries.filtered.2.2, type = "average")          # Better understand what this actually means
diameter.4 <- diameter(g.countries.filtered.2.2)                                                  # Better understand what this actually means
