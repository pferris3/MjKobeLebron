library(dplyr)
library(ggplot2)

#Read CSV file
data <- read.csv("per_game_stats.csv")

View(data)

#Get Lebron data, break into smaller pieces
lebronData <- filter(data, Player == "Lebron James", RSorPO == "Regular Season")
lebronAges <- lebronData$Age
lebronPoints <- lebronData$PTS
lebronAssists <- lebronData$AST

#View(lebronData)

#Get Mike data, break into smaller pieces
mikeData <- filter(data, Player == "Michael Jordan", RSorPO == "Regular Season")
mikeAges <- mikeData$Age
mikePoints <- mikeData$PTS
mikeAssists <- mikeData$AST


#View(mikeData)

#Get Kobe data, break into smaller pieces
kobeData <- filter(data, Player == "Kobe Bryant", RSorPO == "Regular Season")
kobeAges <- kobeData$Age
kobePoints <- kobeData$PTS
kobeAssists <- kobeData$AST


#Get playoff games played
lebronPlayoffGames <- sum(filter(data, Player == "Lebron James", RSorPO == "Playoffs")$G, na.rm = TRUE)
mikePlayoffGames <- sum(filter(data, Player == "Michael Jordan", RSorPO == "Playoffs")$G, na.rm = TRUE)
kobePlayoffGames <- sum(filter(data, Player == "Kobe Bryant", RSorPO == "Playoffs")$G, na.rm = TRUE)
#print(sum(lebronPlayoffGames, na.rm = TRUE))


#Plot points comparison
plot(mikeAges, mikePoints, type = "l", xlim = c(18,42), ylim = c(5, 40), xlab = "Age", ylab = "PPG")
lines(lebronAges, lebronPoints, type = "l", col = "red")
lines(kobeAges, kobePoints, type = "l", col = "purple")


#Plot assists comparison
plot(mikeAges, mikeAssists, type = "l", xlim = c(18,42), ylim = c(0, 15), xlab = "Age", ylab = "AST")
lines(lebronAges, lebronAssists, type = "l", col = "red")
lines(kobeAges, kobeAssists, type = "l", col = "purple")

#Bar graph playoff games
playoffGamesVec <- c(lebronPlayoffGames, mikePlayoffGames, kobePlayoffGames)
namesVec <- c("Lebron James", "Michael Jordan", "Kobe Bryant")
playerColorsVec <- c("red", "black", "purple")
barplot(playoffGamesVec, main = "Total Playoff Games", names.arg = namesVec, xlab = "Player", ylab = "# of Games",
        col = playerColorsVec)


