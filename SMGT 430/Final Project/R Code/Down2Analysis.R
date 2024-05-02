library(hoopR)
library(dplyr)
library(plyr)
library(ggplot2)
library(png)
library(sportyR)
library(ash)
library(rms)
library(stringr)

#Some Cleaning
allshots <- read.csv("/Users/louzhou/Documents/SMGT 430 Files/R Programs/Final Project/allshots.csv")
allgames <- hoopR::load_nba_schedule(seq(2002, 2024, 1))
allshots <- allshots |> dplyr::filter(clock_seconds < 10 & clock_minutes == 0)
allshots <- allshots |> dplyr::filter(!str_detect(type_text,"Free Throw") & season >= 2010)
allgames <- allgames |> dplyr::filter(season >= 2010)
threes <- allshots |> dplyr::mutate(
  x_dist_from_basket = (42.25-abs(coordinate_x)),
  y_dist_from_basket = abs(coordinate_y),
  angle_from_basket = atan(y_dist_from_basket/x_dist_from_basket),
  dist_from_basket = sqrt(x_dist_from_basket^2+y_dist_from_basket^2)
)
threes <- threes |> dplyr::mutate(
  three_pointer = ifelse(
    (!OT & angle_from_basket < asin(22/23.75) &
       dist_from_basket >= 23.75) | (y_dist_from_basket >= 22),1,0
  )
)
threes <- threes |> dplyr::filter(dist_from_basket <= 30)
onlythrees = threes |> dplyr::filter(three_pointer == 1)

colnames(allgames)
scores <- allgames |> dplyr::select(game_id, home_score, away_score,home_id, away_id)
scores <- scores |> dplyr::mutate(
  winner = ifelse(home_score > away_score, home_id, away_id)
)
scores <- scores |> dplyr::rename(
  final_home_score = home_score,
  final_away_score = away_score
)
finals <- merge(threes, scores, by = c("game_id"))
finals <- finals |> dplyr::mutate(
  won = ifelse(possession == winner, 1, 0)
)
threeattempt <- finals |> dplyr::filter(three_pointer == 1)
#Transition Probabilities
otgames <- finals |> dplyr::filter(OT)
twos <- finals |> dplyr::filter(three_pointer == 0)
twosprct <- sum(twos$scoring_play) /nrow(twos) #0.5768322
threeprct <- sum(threeattempt$won) / nrow(threeattempt) #0.1641221
oddsofthree <- sum(allshots$scoring_play) / nrow(allshots) #0.3902116
otwinprct <- sum(otgames$won) / nrow(otgames)
otwinprct
0.3902116
0.5768322 * .5
#Plotting
library('heemod')
library('diagram')
mat_dim <- define_transition(
  state_names = c('Start', 'Two Attempt', 'OT', 'Lose', 'Win', 'Three Attempt'),
  0, 0.610, 0, 0, 0, 0.390,
  0, 0, 0.577, 0.424, 0, 0,
  0, 0, 0, 0.497,0.502, 0,
    0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0.836, 0.164, 0);
#my = c(0,-.1,-00,.10,0,.20)
plot(mat_dim, box.type = 'rect', box.size = 0.15, box.prop = 0.6, mx = c(0,0,0,0,0,0.4), my = c(0,-.1,-00,.10,0,.20))

#Vegas
overtime <- finals |> dplyr::filter(OT)
overtime
overtime <- overtime |> dplyr::mutate(
  is_favored = ifelse(home_favorite & possession == home_id, 1, -1),
  spread = -1 * is_favored * abs(home_team_spread)
)
#spread: positive = underdog
spreads <- overtime |> dplyr::select(spread, won)
rownames(spreads) <- NULL
ggplot(spreads, aes(x=spread, y = won)) + geom_point() +
  stat_smooth(method="glm", color="red", se=FALSE, 
              method.args = list(family=binomial))
x = spreads$spread
y = spreads$won
logit <-lrm(y ~ x)
logit #piss-poor r^2: .021
library(ltm)
biserial.cor(x,y) #0.12596, so spread is not a good predictor of win

#if three_point > two_point * win_ot, go with three_point
#else, go with two_point
#can predict three point / two point using some regularized rasch model
#can predict ot win with bradley terry model
