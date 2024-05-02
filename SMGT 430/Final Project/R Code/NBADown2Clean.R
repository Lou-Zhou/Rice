library(hoopR)
library(dplyr)
library(plyr)
allgames <- hoopR::load_nba_schedule(seq(2002, 2024, 1))
allgames_new <- allgames |>
  dplyr::mutate(
    scorediff = abs(home_score - away_score)
  )
allgames_reg <- allgames_new |> dplyr::filter(status_period == 4, scorediff < 8) #get all OT games and games within 2
allgames_ot <- allgames_new |> dplyr::filter(status_period > 4)


games_reg <- allgames_reg$id
games_ot <- allgames_ot$id
allpbp <- hoopR::load_nba_pbp(seq(2002, 2024, 1))

pbpgames_reg <-  allpbp |> dplyr::filter(game_id %in% games_reg)
pbpgames_ot <- allpbp |> dplyr::filter(game_id %in% games_ot)
pbpgames_ot
#PLAN: get all regulation games and find the last possession the losing team got. Include 
# possessions if the score difference is two at the time of the possessions, find winning probabilities 
#from that... enough for today.

#can get possessions by merging the rosters and the pbp to get the team of the player doing an event
#if that team lost the game, using allgames_ret, mark that column as true, and vice versa

#need to move on to overtime games, too



rosters <- hoopR::load_nba_player_box(seq(2002, 2024, 1))

rosters

pbpgames_reg

rosters <- rosters |> dplyr::select(game_id, athlete_id, athlete_display_name, team_id)

rosters <- rosters |>
  dplyr::rename(
    athlete_id_1 = athlete_id,
    player_1_team_id = team_id
  )
unique(rosters$game_id)

clean_data <- function(pbp){
regrostered <- merge(data.frame(pbp), data.frame(rosters), by = c("game_id", "athlete_id_1"))

regrostered_2 <- regrostered |>
  dplyr::mutate(
    possession = ifelse(player_1_team_id == home_team_id, home_team_id, away_team_id),
    current_winner = case_when(
      home_score > away_score ~ home_team_id,
      home_score < away_score ~ away_team_id,
      home_score == away_score ~ -1
    ),
    is_losing = ifelse(current_winner != player_1_team_id, TRUE, FALSE),
    score_diff = abs(home_score - away_score)
  )
regrostered

regrostered_2 <- regrostered_2 |> dplyr::arrange(sequence_number) |> dplyr::filter(period_number == 4)

lastmin <- regrostered_2 |> dplyr::arrange(game_id, sequence_number)

lastmin <- lastmin |> dplyr::mutate(
  shooterhome = ifelse(player_1_team_id == home_team_id, TRUE, FALSE),
  before_shot_home = ifelse(shooterhome, home_score - score_value, home_score),
  before_shot_away = ifelse(!shooterhome, away_score - score_value, away_score),
  before_diff = abs(before_shot_home - before_shot_away),
  possession = ifelse(player_1_team_id == home_team_id, home_team_id, away_team_id),
  current_winner = case_when(
    before_shot_home > before_shot_away ~ home_team_id,
    before_shot_home < before_shot_away ~ away_team_id,
    before_shot_home == before_shot_away ~ -1
  ),
  is_losing = case_when(current_winner == -1 ~ FALSE,
                        current_winner == player_1_team_id ~ FALSE,
                        current_winner != player_1_team_id ~ TRUE
                        )
)
test <- allpbp |> dplyr::select(type_id, text)

lastmin_shots <- lastmin |> dplyr::filter(before_diff == 2 & period_number == 4 & !grepl("Free Throw", text, fixed = TRUE))
test_help <- lastmin_shots |> dplyr::filter(game_id == 401585617)

#score is new score AFTER result of shot, dealing with offensive rebounds
lastmin <- lastmin |> dplyr::arrange(game_id, game_play_number)

lastmin$game_play_number

lastmin <- lastmin |> dplyr::mutate(
  new_possession = ifelse(lag(possession) == possession, FALSE, TRUE),
  new_possession = ifelse(is.na(new_possession), FALSE, new_possession)
)
lastmin <- lastmin |> dplyr::group_by(game_id) |> dplyr::mutate(
  num_possession = c(cumsum(new_possession)))
lastmin <- lastmin |> dplyr::group_by(game_id, num_possession) |> dplyr::mutate(
  contains_shot = ifelse(TRUE %in% shooting_play, TRUE, FALSE),
)
lastmin <- lastmin |> dplyr::group_by(game_id) |> dplyr::mutate(
  num_shot_possession = c(cumsum(contains_shot)))
lastmin |> dplyr::filter(game_id == 401585617) 

lastminposs <-lastmin |> dplyr::group_by(game_id) |> dplyr::filter(is_losing) |>
  dplyr::mutate(
    last_num = max(as.numeric(num_shot_possession)),
    last_possession = ifelse(num_shot_possession == last_num, TRUE, FALSE)
  )
lastminposs |> dplyr::filter(game_id == 401332963)
print(lastminposs |> dplyr::filter(game_id == 401332963) |> select(text, last_possession, game_play_number, is_losing, score_diff, current_winner, before_diff), n = 500)


test <- lastminposs |> dplyr::filter(last_possession & shooting_play & !grepl("Free Throw", text, fixed = TRUE) & before_diff == 2)
#knocks out a lot of shots
test #49200 games, only 177(now 869) games where a team took a shot while down(exactly) two?
test |> dplyr::filter(game_id == 401332963)
regshots <- test |> dplyr::group_by(game_id) |> dplyr::mutate(
  first_shot = ifelse(text == text[1], TRUE, FALSE),
  shots = n()
)
finalshots <- regshots |> dplyr::filter(first_shot)
return(finalshots)
}
regulation <- clean_data(pbpgames_reg)
overtime <- clean_data(pbpgames_ot)
#OT SHOTS
regulation #904
overtime #723



#weird shit happening with the ot data, might be flawed and not the function -- see 290313023, game goes to ot while ending 77-79 in the fourth??
#espn data is fucking ass... how the hell does a game go to ot while ending 103 - 101 in the 4th?????
#function seems to work but the espn data is questionable at best -- i think we need to throw out OT data because it is ass
#nvm i fixed it, still some broken ones in the overtime df but i think we can just get rid

overtime_clean <- overtime |> dplyr::filter(away_score == home_score)

colnames(allshots)
regulation$OT = FALSE
overtime_clean$OT = TRUE

allshots = rbind(overtime_clean, regulation)
#scoring_play tells if scored
write.csv(allshots, "allshots.csv")

