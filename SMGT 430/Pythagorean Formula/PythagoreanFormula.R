library(worldfootballR)
library(dplyr)
buli_2023 <- fb_match_results(country = "GER", gender = "M", season_end_year = 2023, tier = "1st")
dplyr::glimpse(buli_2023)
buli_2023
buli_2023$scorediff <- buli_2023$HomeGoals - buli_2023$AwayGoals
buli_2023$scorediff #Home - Away
halfway <- seq.int(nrow(buli_2023) / 2)
colnames(buli_2023)

buli_2023$winner = with(buli_2023, ifelse(HomeGoals == AwayGoals, "Tie", ifelse(HomeGoals > AwayGoals, Home, Away)))
buli_2023$loser = with(buli_2023, ifelse(HomeGoals == AwayGoals, "Tie", ifelse(HomeGoals > AwayGoals, Away, Home)))
buli_2023$winner_score = with(buli_2023, ifelse(HomeGoals == AwayGoals, HomeGoals, ifelse(HomeGoals > AwayGoals, HomeGoals, AwayGoals)))
buli_2023$loser_score = with(buli_2023, ifelse(HomeGoals == AwayGoals, AwayGoals, ifelse(HomeGoals > AwayGoals, AwayGoals, HomeGoals)))
buli_2023$points_loser <- with(buli_2023, ifelse(HomeGoals > AwayGoals, AwayGoals, HomeGoals))
buli_2023 <- buli_2023[buli_2023$Date < as.Date('2023-05-30'),]
buli_2023
firsthalf <- buli_2023[buli_2023$Date <= as.Date('2023-1-25'),]
secondhalf <- buli_2023[buli_2023$Date > as.Date('2023-1-25') & buli_2023$Date < as.Date('2023-05-30'),]
length(firsthalf$winner)
length(secondhalf$winner)

buli_2023
get_team_data <- function(df){
  idx = 1
  outputdf <- data.frame(Team=character(),
                         Wins=numeric(), 
                         Losses=numeric(), 
                         Ties=numeric(),
                         Scored = numeric(),
                         Conceded = numeric(),
                         Points = numeric(),
                         PossiblePoints = numeric()) 
  for (team in unique(df$Home)){ 
    teamdf <- df[df$Home == team | df$Away == team, ]
    win <- teamdf[teamdf$winner == team, ]
    lose <- teamdf[teamdf$loser == team, ]
    tie <- teamdf[teamdf$winner == "Tie", ]
    wins <- length(win$winner)
    loses <- length(lose$winner)
    ties <- length(tie$winner)
    points <- 3 * wins + ties
    scored <- sum(win$winner_score) + sum(lose$loser_score) + sum(tie$winner_score)
    conceded <- sum(win$loser_score) + sum(lose$winner_score) + sum(tie$winner_score)
    possiblepoints <- 3 * length(teamdf$winner)
    rowappended <- c(team, wins, loses, ties, scored, conceded, points, possiblepoints)
    outputdf[idx, ] <- rowappended
    idx = idx + 1
  }
  outputdf$PointPercent <- as.numeric(outputdf$Points) / as.numeric(outputdf$PossiblePoints)
  outputdf$ScoreRatio <- as.numeric(outputdf$Scored) / as.numeric(outputdf$Conceded)
  return(outputdf)
  
}
first_half_team <- get_team_data(firsthalf)
second_half_team <- get_team_data(secondhalf)
season_team <- get_team_data(buli_2023)
season_team
first_half_team_data <- select(first_half_team, Team, Scored, Conceded, PointPercent, ScoreRatio)
second_half_team_data <- select(second_half_team, Team, Scored, Conceded, PointPercent, ScoreRatio)
season_team_data <- select(season_team, Team, Scored, Conceded, PointPercent, ScoreRatio)
first_half_team_data
season_team_data

getalpha <- function(df){
error <- 100000
found_alpha <- 0
for (alpha in seq(from = 0, to = 10, by = .0001)) {
  pythag <- 1 / (1 + df$ScoreRatio^(-alpha))
  new_error <- sum((df$PointPercent - pythag)^2)
  print(c(alpha, new_error))
  if (new_error < error) {
    error <- new_error
    found_alpha <- alpha
  }
}
print(error)
return(found_alpha)
}
firsthalfalpha <- getalpha(first_half_team_data)
secondhalfalpha <- getalpha(second_half_team_data)
seasonalpha <- getalpha(season_team_data)
print(firsthalfalpha)
print(secondhalfalpha)
print(seasonalpha)
first_half_team_data
first_half_team_data <- first_half_team_data %>% 
  rename(FirstScored = Scored,
         FirstConceded = Conceded,
         FirstPointPercent = PointPercent,
         FirstScoreRatio = ScoreRatio)
second_half_team_data <- second_half_team_data %>% 
  rename(SecondScored = Scored,
         SecondConceded = Conceded,
         SecondPointPercent = PointPercent,
         SecondScoreRatio = ScoreRatio)
second_half_team_data
first_half_team_data$FirstHalfPythag <-1 / (1 + first_half_team_data$FirstScoreRatio^(-seasonalpha))
second_half_team_data$SecondHalfPythag <-1 / (1 + second_half_team_data$SecondScoreRatio^(-seasonalpha)) 
first_and_second <- merge(first_half_team_data,second_half_team_data,by="Team")
first_and_second

first_and_second$FirstResidual <- first_and_second$FirstPointPercent - first_and_second$FirstHalfPythag
first_and_second$SecondResidual <- first_and_second$SecondPointPercent - first_and_second$SecondHalfPythag

first_and_second$FirstResidual
first_and_second$SecondResidual
pointpercentcor <- cor(first_and_second$FirstPointPercent, first_and_second$SecondPointPercent, method = "pearson")
pointpercentcor
pythagcor <- cor(first_and_second$FirstHalfPythag, first_and_second$SecondPointPercent, method = "pearson")
pythagcor
residualcor <- cor(first_and_second$FirstResidual, first_and_second$SecondResidual, method = "pearson")
residualcor






plot(x = first_and_second$FirstPointPercent, y = first_and_second$SecondPointPercent, xlim = c(0.1, .8), ylim = c(0.1,.8),
     xlab="First Half Point %", ylab="Second Half Point %", col = "blue", main = "First Half Point% vs Second Half Point%")
abline(a = 0, b = 1, col = "black")
text(0.3, 0.6, labels=substr(paste("R = ", pointpercentcor),1, 13), cex = .75)
plot(x = first_and_second$FirstHalfPythag, y = first_and_second$SecondPointPercent, xlim = c(0.1, .8), ylim = c(0.1,.8),
     xlab="First Half Pythag Point%", ylab="Second Half Point %", col = "red", main = "First Half Pythag Point% vs Second Half Point%")
abline(a = 0, b = 1, col = "black")
text(0.3, 0.6, labels=substr(paste("R = ", pythagcor),1, 13), cex = .75)
plot(x = first_and_second$FirstResidual, y = first_and_second$SecondResidual, xlim = c(-.15, .1), ylim = c(-.2,.1),
     xlab="First Half Residual", ylab="Second Half Residual", col = "green", main = "First Half Residual vs Second Half Residual")
abline(a = 0, b = 1, col = "black")
text(-0.1, 0.05, labels=substr(paste("R = ", residualcor),1, 13), cex = .75)
resfactor = 3
dev.copy(png, "residualpythag.png",res = 72*resfactor, height=480*resfactor, width=480*resfactor)
dev.off()
