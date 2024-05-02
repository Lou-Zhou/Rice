library(hoopR)
library(dplyr)
library(plyr)
library(ggplot2)
library(png)
library(sportyR)
library(ash)
library(rms)

allshots <- read.csv("/Users/louzhou/Documents/SMGT 430 Files/R Programs/Final Project/allshots.csv")


clean_allshots <- allshots |> dplyr::filter(end_game_seconds_remaining <= 30)
recent <- clean_allshots |> dplyr::filter(season %in% c(2020, 2021, 2022, 2023))
allshots$col <- sample(0:4000, size = nrow(allshots), replace = TRUE)
recent <- recent |> dplyr::arrange(desc(scoring_play)) |> dplyr::mutate(
  Scored = ifelse(scoring_play, "Scored", "Missed"))

#contour(f$x,f$y,f$z,add=TRUE)


shotchart = geom_basketball("nba", display_range = "offense", rotation = 270)  + geom_point(data = recent, aes(x = coordinate_x_raw - 24,
           y = coordinate_y_raw - 43, color = Scored, fill = Scored, shape = Scored)) + scale_shape_manual(values = c("Scored" = 1, "Missed" = 4)) + 
  scale_color_manual(values = c("Scored" = "darkgreen", "Missed" = "red")) + ggtitle("Shotchart of Last Shots While Down 2, Last 4 Seasons") +
  theme(plot.title = element_text(hjust = 0.5))
shotchart


ot <- clean_allshots |> dplyr::filter(OT)
allgames <- hoopR::load_nba_schedule(seq(2002, 2024, 1))
allgames_2 <- allgames |> dplyr::select(game_id, home_id, away_id, home_score, away_score)
won <- allgames_2 |> dplyr::mutate(
  winner = ifelse(home_score > away_score, home_id, away_id)
)
shooters <- ot |> dplyr::select(game_id, possession)
both <- merge(won, shooters, by = c("game_id"))
both <- both |> dplyr::mutate(
  won = ifelse(winner == possession, 1, 0),
  ot = 1
)
sum(both$won) / nrow(both) #0.4794816 chance of winning, about 50 / 50
#no evidence to support "momentum swing" of last shot 


colnames(allgames)
allshots |> dplyr::select(end_game_seconds_remaining, scoring_play)
unique(allshots$scoring_play)

logdata <- clean_allshots |> dplyr::mutate(
  scored = ifelse(scoring_play, 1, 0)
)

ggplot(logdata, aes(x=end_game_seconds_remaining, y=scored)) + geom_point() +
  stat_smooth(method="glm", color="red", se=FALSE, 
              method.args = list(family=binomial)) + ggtitle("Seconds Left vs Scoring") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Seconds Left") + ylab("Did Score")

logistic_model <- lrm(logdata$scored ~ logdata$end_game_seconds_remaining)
logistic_model #psuedo-r^2 of 0.308, coefficient of 0.1680, slightly positive
#more likely to score with more time left


