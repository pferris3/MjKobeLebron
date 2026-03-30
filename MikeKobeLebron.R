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
lebronRebounds <- lebronData$TRB
lebronFGP <- mean(lebronData$FG.) * 100
lebron3PP <- mean(lebronData$X3P.) * 100
lebronFTP <- mean(lebronData$FT.) * 100


#Get Mike data, break into smaller pieces
mikeData <- filter(data, Player == "Michael Jordan", RSorPO == "Regular Season")
mikeAges <- mikeData$Age
mikePoints <- mikeData$PTS
mikeAssists <- mikeData$AST
mikeRebounds <- mikeData$TRB
mikeFGP <- mean(mikeData$FG.) * 100
mike3PP <- mean(mikeData$X3P.) * 100
mikeFTP <- mean(mikeData$FT.) * 100


#Get Kobe data, break into smaller pieces
kobeData <- filter(data, Player == "Kobe Bryant", RSorPO == "Regular Season")
kobeAges <- kobeData$Age
kobePoints <- kobeData$PTS
kobeAssists <- kobeData$AST
kobeRebounds <- kobeData$TRB
kobeFGP <- mean(kobeData$FG.) * 100
kobe3PP <- mean(kobeData$X3P.) * 100
kobeFTP <- mean(kobeData$FT.) * 100


#Get playoff games played
lebronPlayoffGames <- sum(filter(data, Player == "Lebron James", RSorPO == "Playoffs")$G, na.rm = TRUE)
mikePlayoffGames <- sum(filter(data, Player == "Michael Jordan", RSorPO == "Playoffs")$G, na.rm = TRUE)
kobePlayoffGames <- sum(filter(data, Player == "Kobe Bryant", RSorPO == "Playoffs")$G, na.rm = TRUE)

#Get career averages
#OFFENSE
lebronPointsAvg <- mean(lebronPoints)
lebronAssistsAvg <- mean(lebronAssists)
lebronReboundsAvg <- mean(lebronRebounds)

mikePointsAvg <- mean(mikePoints)
mikeAssistsAvg <- mean(mikeAssists)
mikeReboundsAvg <- mean(mikeRebounds)

kobePointsAvg <- mean(kobePoints)
kobeAssistsAvg <- mean(kobeAssists)
kobeReboundsAvg <- mean(kobeRebounds)

pointsAvgVec <- c(lebronPointsAvg, mikePointsAvg, kobePointsAvg)
assistsAvgVec <- c(lebronAssistsAvg, mikeAssistsAvg, kobeAssistsAvg)
reboundsAvgVec <- c(lebronReboundsAvg, mikeReboundsAvg, kobeReboundsAvg)

careerOffenseMat <- as.matrix(data.frame(pointsAvgVec, assistsAvgVec, reboundsAvgVec))

#SHOOTING
careerFGP <- c(lebronFGP, mikeFGP, kobeFGP)
career3PP <- c(lebron3PP, mike3PP, kobe3PP)
careerFTP <- c(lebronFTP, mikeFTP, kobeFTP)

careerShootingMat <- as.matrix(data.frame(careerFGP, career3PP, careerFTP))


#Plot points comparison
plot(mikeAges, mikePoints, type = "l", lwd = 2, xlim = c(18,42), ylim = c(5, 40), xlab = "Age", ylab = "PPG")
lines(lebronAges, lebronPoints, type = "l", col = "red", lwd = 2)
lines(kobeAges, kobePoints, type = "l", col = "purple", lwd = 2)
legend("topright", legend = namesVec, fill = playerColorsVec)


#Plot assists comparison
plot(mikeAges, mikeAssists, type = "l", lwd = 2, xlim = c(18,42), ylim = c(0, 15), xlab = "Age", ylab = "APG")
lines(lebronAges, lebronAssists, type = "l", col = "red", lwd = 2)
lines(kobeAges, kobeAssists, type = "l", col = "purple", lwd = 2)
legend("topright", legend = namesVec, fill = playerColorsVec)

#Bar graph playoff games
playoffGamesVec <- c(lebronPlayoffGames, mikePlayoffGames, kobePlayoffGames)
namesVec <- c("Lebron James", "Michael Jordan", "Kobe Bryant")
playerColorsVec <- c("red", "black", "purple")
barplot(playoffGamesVec, main = "Total Playoff Games", names.arg = namesVec, xlab = "Player", ylab = "# of Games",
        ylim = c(0, 300),col = playerColorsVec)



#Plot career averages for offense
barplot(careerOffenseMat, names.arg = c("Points", "Assists", "Rebounds"), col = playerColorsVec, ylim = c(0,30),
        beside = TRUE)
legend("topright", legend = namesVec, fill = playerColorsVec)

#Plot career shooting percentages
barplot(careerShootingMat, names.arg = c("Field Goals", "3-Pointers", "Free-throw"), col = playerColorsVec,
        ylim = c(0,100), ylab = "Shooting %", beside = TRUE)
legend("topleft", legend = namesVec, fill = playerColorsVec)