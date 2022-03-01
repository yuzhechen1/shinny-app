library(shiny)
library(readxl)
library(igraph)
setwd("C:/Users/86156/Desktop/硕士/Network data analysis/R working directory/group assignment")
install.packages("readxl")

Champions_League_Data_1955_2015 <- read_excel("C:/Users/86156/Desktop/硕士/Network data analysis/R working directory/group assignment/Source data/Champions League Data 1955-2015.xlsx")

all_rounds <- unique(Champions_League_Data_1955_2015[,3])
all_seasons <- unique(Champions_League_Data_1955_2015[,2])

shinyApp(ui = ui, server = server)
