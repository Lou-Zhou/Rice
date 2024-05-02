library(hoopR)
library(plyr)
library(dplyr)
library(tidyverse)
allpbp <- hoopR::load_nba_pbp(seq(2009, 2024, 1))

shots <- allpbp |> dplyr::filter(shooting_play & !str_detect(type_text,"Free Throw"))
nrow(shots)
colnames(shots)
clutch = shots |> dplyr::mutate(score_diff = abs(home_score - away_score)) |>
                                dplyr::filter(clock_minutes <= 5 & score_diff <= 5 & period_number >= 4)
nrow(clutch)
mean(clutch$scoring_play)
notclutch = shots |> dplyr::mutate(score_diff = abs(home_score - away_score)) |>
  dplyr::filter(!(clock_minutes <= 5 & score_diff <= 5 & period_number >= 4))
mean(notclutch$scoring_play)
nrow(notclutch)
length(unique(shots$season))
shots <- shots |> dplyr::mutate(
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
  offense = paste0("s",as.character(athlete_id_1)),
  defender = ifelse(team_id == home_team_id, as.character(away_team_id), as.character(home_team_id)),
  defender = paste0("d", as.character(defender)),
  scored = ifelse(scoring_play, 1, 0)
)
colnames(shots_new)
shots_new <- shots |> dplyr::filter(!is.na(offense) & !is.na(defender)) |> dplyr::mutate(
  score_diff = abs(home_score - away_score)
)

shots_new <- shots_new |> dplyr::filter((period == 4 & clock_minutes <= 5 & score_diff <= 5))
mean(shots_new$scoring_play)
#splitting into threes and twos
threes <- shots_new |> dplyr::filter(three_pointer == 1)
twos <- shots_new |> dplyr::filter(three_pointer == 0)
mean(twos$scored)#clutch: .479844, all: .5039
mean(threes$scored) #clutch: .34454, all: 
get_strengths <- function(df, year){
#start of function
testseason = df |> dplyr::filter(season == year)
print(unique(testseason$season))
print("Matrix...")
threes_model_matrix <- model.matrix(~ 0 + defender + offense, data = testseason)
threes_model_matrix <- as(threes_model_matrix, "sparseMatrix") 
print(threes_model_matrix)
print(colnames(threes_model_matrix))
print("Model...")
testseason$test = sample(c(0,1), size = nrow(testseason), replace = TRUE, prob = c(0.99, .01))
print(mean(testseason$test))
#model <- glmnet::glmnet(x = threes_model_matrix, y = testseason$scored, lambda = .Machine$double.xmin)
model <- glmnet::cv.glmnet(x = threes_model_matrix, y = testseason$test, standardize = FALSE, alpha = 0)
print("Model Finished")
coefficients <- data.frame(
  name = rownames(coef(model)),
  coefficients = coef(model, s = "lambda.min")[,1],
  season = year
  )
coefficients <- coefficients |> dplyr::mutate(
  state  = case_when(
    str_detect(name,"defenderd") ~ "Defender",
    str_detect(name,"offenses") ~ "Offense",
    str_detect(name,"Intercept") ~ "Intercept"
),
  id = parse_number(name, na = c("(Intercept)","defenderdNA", "offensesNA")))
return(coefficients)
}
8.381219e-03
0.008302889
exp(9.507342e-03) / (1 + exp( 9.507342e-03))
mean(twos$scored)
alltwostrengths = data.frame()
allthreestrengths = data.frame()
get_strengths(twos, 2010)
test <- twos |> dplyr::filter(season == 2010)
mean(test$scored)
for(season in seq(2009, 2024, 1)){
  print(nrow(alltwostrengths))
  print(nrow(allthreestrengths))
  threestrengths <- get_strengths(threes, season)
  twostrengths <- get_strengths(twos, season)
  alltwostrengths = rbind(alltwostrengths, twostrengths)
  allthreestrengths = rbind(allthreestrengths, threestrengths)
  
}
exp(0.4874142879) / (1 + exp(0.4874142879))
player_names <- hoopR::load_nba_player_box(seq(2009, 2024, 1))
team_names <- hoopR::load_nba_team_box(seq(2009, 2024, 1))
team_name <- team_names |> dplyr::select(team_id, team_display_name) |> dplyr::filter(!duplicated(team_id))
name <- names <- player_names |> dplyr::select(athlete_id, athlete_display_name) 
name <- name |> dplyr::filter(!duplicated(athlete_id))
name$state = "Offense"
team_name$state = "Defender"
name <- name |> dplyr::rename(
  id = athlete_id,
  name = athlete_display_name
)
team_name <- team_name |> dplyr::rename(
  id = team_id,
  name = team_display_name
)
allnames <- rbind(name, team_name)
allnames <- allnames |> add_row(id = NA, name = "Intercept", state = "Intercept")
three_names <- merge(allthreestrengths, allnames, by = c("id", "state"))
two_names <- merge(alltwostrengths, allnames, by = c("id", "state"))
two_names <- two_names |> dplyr::rename(
  nameid = name.x,
  name = name.y
)
three_names <- three_names |> dplyr::rename(
  nameid = name.x,
  name = name.y
)

twostrengths

write.csv(two_names, "twostrengths.csv")
write.csv(three_names, "threestrengths.csv")


mean(threes$scoring_play)
mean(twos$scoring_play)

twostrengths |> dplyr::filter(state == "Intercept")
exp(0.4725633) / (1 + exp(0.4725633))

