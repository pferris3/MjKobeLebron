library(dplyr)
library(ggplot2)

file <- "advanced_stats.csv"

data <- read.csv("advanced_stats.csv")

lebronData <- filter(data, Player == "Lebron James", RSorPO == "Regular Season")
lebronAges <- lebronData$Age
lebronPoints <- lebronData$PER

View(lebronData)

mikeData <- filter(data, Player == "Michael Jordan", RSorPO == "Regular Season")

View(mikeData)

mikeAges <- mikeData$Age
mikePoints <- mikeData$PER

plot(mikeAges, mikePoints, type = "l")

lines(lebronAges, lebronPoints, type = "l", col = "red")

