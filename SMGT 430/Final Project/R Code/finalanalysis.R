library(hoopR)
library(dplyr)
library(plyr)
library(ggplot2)
library(png)
library(sportyR)
library(ash)
library(rms)

allshots <- read.csv("/Users/louzhou/Documents/SMGT 430 Files/R Programs/Final Project/allshots.csv")
linbt <- read.csv("/Users/louzhou/Documents/SMGT 430 Files/R Programs/Final Project/linbt.csv")
twostrengths <- read.csv("/Users/louzhou/Documents/SMGT 430 Files/R Programs/Final Project/twostrengths.csv")
threestrengths <- read.csv("/Users/louzhou/Documents/SMGT 430 Files/R Programs/Final Project/threestrengths.csv")
allshots


nrow(allshots)
allshots <- allshots |> dplyr::filter(season >= 2010)
#2010 onwards
threestrengths |> dplyr::filter(state == "Intercept")
twostrengths |> dplyr::filter(state == "Intercept")

finals <- hoopR::load_nba_schedule(seq(2010, 2024, 1))
colnames(finals)
final <- finals |> dplyr::select(id, home_winner)
colnames(allshots)
allshots
shots <- allshots |> dplyr::mutate(
  x_dist_from_basket = (42.25-abs(coordinate_x)),
  y_dist_from_basket = abs(coordinate_y),
  angle_from_basket = atan(y_dist_from_basket/x_dist_from_basket),
  dist_from_basket = sqrt(x_dist_from_basket^2+y_dist_from_basket^2)
  
)
shots <- shots |> dplyr::mutate(
  three_pointer = ifelse(
    (angle_from_basket < asin(22/23.75) &
       dist_from_basket >= 23.75) | (y_dist_from_basket >= 22),1,0
  )
)

shots <- shots |> dplyr::mutate(
  offense = athlete_id_1,
  defender = ifelse(team_id == home_team_id, as.character(away_team_id), as.character(home_team_id)),
  defender = paste0("d", as.character(defender)),
  scored = ifelse(scoring_play, 1, 0),
  is_home = ifelse(home_team_id == possession, 1, 0)
)
colnames(shots)

rel <- shots |> dplyr::select(three_pointer, season, offense, defender, scored, game_id, is_home, team_id)
total <- merge(rel, final, by.x = c("game_id"), by.y = c("id"))
total <- total |> dplyr::mutate(
  defense = sub('.', '', defender)
)
two <- twostrengths |> dplyr::select(state, id, season, coefficients, name)
two_def <- two |> dplyr::filter(state == "Defender")
two_off <- two |> dplyr::filter(state == "Offense")
two_int <- two |> dplyr::filter(state == "Intercept")
all <- merge(total, two_def, by.x = c("season", "defense"), by.y = c("season", "id"))
all <- all |> dplyr::rename(
  def_two_coefficient = coefficients,
  def_name = name )
all <- merge(all, two_off, by.x = c("season", "offense"), by.y = c("season", "id"))
all <- all |> dplyr::rename(
  off_two_coefficient = coefficients,
  off_name = name )
all <- merge(all, two_int, by.x = c("season"), by.y = c("season"))
all <- all |> dplyr::rename(
  intercept_coefficient = coefficients)


three <- threestrengths |> dplyr::select(state, id, season, coefficients, name)
three_def <- three |> dplyr::filter(state == "Defender")
three_off <- three |> dplyr::filter(state == "Offense")
three_int <- three |> dplyr::filter(state == "Intercept")
all <- merge(all, three_def, by.x = c("season", "defense"), by.y = c("season", "id"))
all <- all[ , !(names(all) %in% c("state.x", "state.y", "name.x", "name.y"))]
all
all <- all |> dplyr::rename(
  def_three_coefficient = coefficients)
all <- merge(all, three_off, by.x = c("season", "offense"), by.y = c("season", "id"))
all <- all |> dplyr::rename(
  off_three_coefficient = coefficients)
all <- merge(all, three_int, by.x = c("season"), by.y = c("season"))
all <- all |> dplyr::rename(
  three_intercept_coefficient = coefficients)
colnames(all)
final_all <- all |> dplyr::select(three_pointer,home_winner, is_home, season, offense, defense, game_id, defender,team_id, scored, home_winner, def_two_coefficient,
                     off_two_coefficient, off_name, def_name, intercept_coefficient, def_three_coefficient, off_three_coefficient, three_intercept_coefficient)
final_all <- final_all |> dplyr::rename(
  off_team = team_id
)
final_all

colnames(linbt)
final_bt <- merge(linbt, final_all, by.x = c("season", "ids"), by.y = c("season", "off_team"))
final_bt <- final_bt |> dplyr::rename(
  off_bt_coeff = lin_coefficients,
  offense_team = ids
)
final_bt <- merge(linbt, final_bt, by.x = c("season", "ids"), by.y = c("season", "defense"))
final_bt <- final_bt |> dplyr::rename(
  def_bt_coeff = lin_coefficients,
  defense = ids
) 
intercepts <- linbt |> dplyr::filter(ids == -1)
intercepts
final_bt
final <- merge(final_bt,intercepts, by = c("season"))
colnames(final)
final_sel <- final |> dplyr::select(three_pointer, home_winner, is_home, season, offense,off_name, offense_team, defense, def_name, off_two_coefficient,
                       off_three_coefficient, def_two_coefficient, def_three_coefficient, intercept_coefficient,
                       three_intercept_coefficient, off_bt_coeff, def_bt_coeff, lin_coefficients)
final_sel <- final_sel |> dplyr::rename(
  bt_coeff = lin_coefficients
)
final_sel <- final_sel |> dplyr::mutate(
  two_aeta = off_two_coefficient + def_two_coefficient + intercept_coefficient,
  three_aeta = off_three_coefficient + def_three_coefficient + three_intercept_coefficient,
  subtract = ifelse(is_home, 1, -1),
  bt_aeta = off_bt_coeff - def_bt_coeff + bt_coeff * subtract,
  two_chance = exp(two_aeta) / (1 + exp(two_aeta)),
  three_chance = exp(three_aeta) / (1 + exp(three_aeta)),
  ot_chance = exp(bt_aeta) / (1 + exp(bt_aeta))
)
final_sel
nrow(test)
test <- final_sel |> dplyr::mutate(
  two_win = two_aeta * ot_chance,
  three_win = three_aeta,
  shouldthree = ifelse(three_win > two_win, 1, 0),
  won = ifelse(is_home == 1 & home_winner | is_home == 0 & !home_winner, 1, 0),
  didfollow = ifelse(shouldthree == three_pointer, 1, 0)
)
mean(test$shouldthree)
followed <- test |> dplyr::filter(didfollow == 1)
not_followed <- test |> dplyr::filter(didfollow == 0)
nrow(followed) #0.3076923
nrow(not_followed)
mean(not_followed$won)
allshots |> dplyr::filter(OT & season == 2023)
unique(allshots$season)
