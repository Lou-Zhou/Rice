library(tibble)
library(GGally)
library(grid)
library(tidyverse)
library(shadowtext)
library(ggpubr)
nhldatapath <- "/Users/louzhou/Documents/SMGT 430 Files/Data/NHL/nhldata.csv"
nhlstandingpath <- "/Users/louzhou/Documents/SMGT 430 Files/Data/NHL/nhlstandings.csv"
nhldata <- read.csv(nhldatapath)
nhlstanding <- read.csv(nhlstandingpath)
nhldata_played <- nhldata[!is.na(nhldata$Visitor_G), ]
nhldata_played
#code taken from R demonstration
data <- nhldata_played |>
  dplyr::mutate(
    home_win = ifelse(Home_G > Visitor_G, TRUE, FALSE),
    points_winner = ifelse(home_win, Home_G, Visitor_G),
    points_loser = ifelse(home_win, Visitor_G, Home_G),
    score_diff = ifelse(home_win, points_winner - points_loser, points_loser - points_winner)
  )
  matrix_home <- model.matrix(~ Home, data = data) 
  matrix_away <- model.matrix(~ Visitor, data = data)
  total_matrix <- matrix_home - matrix_away
data
team_data <- as.data.frame(total_matrix[, -1])
names(team_data) <- sort(unique(data$Home))[-1]
names(team_data) #Anaheim Ducks as 0

linear_model <- lm(data$score_diff ~ ., data = team_data)
total_matrix
summary(linear_model)
winby <- coef(linear_model)["(Intercept)"] + coef(linear_model)["`San Jose Sharks`"] - coef(linear_model)["`Nashville Predators`"] 
coef(linear_model)["`San Jose Sharks`"]
coef(linear_model)["`Nashville Predators`"] 
coef(linear_model)["(Intercept)"]
winby
coefficients <- summary(linear_model)
coefficients <- coef(linear_model)
coeff_names <- names(coefficients)
coefficients
# Create a dataframe
coeff_df <- data.frame(
  Variable = coeff_names,
  Coefficient = coefficients,
  row.names = NULL
)
coeff_df[1,] = c("`Anaheim Ducks`",0)
# Print or use the dataframe
print(coeff_df)
coeff_df <- coeff_df %>% arrange(desc(Coefficient))
coeff_df
row.names(coeff_df) <- NULL
coeff_df
coeff_df <- rownames_to_column(coeff_df)
coeff_df_new <- coeff_df %>% 
  rename(
    BT_Ranking = rowname,
    Team = Variable
  )



nhlstanding<- select(nhlstanding, c("Team","PTS","Conference","Division"))
bt_ranking <- select(coeff_df_new, c("BT_Ranking","Team"))
bt_ranking$Team <- substring(bt_ranking$Team, 2, nchar(bt_ranking$Team) - 1)
bt_ranking$Team
east <- nhlstanding[nhlstanding["Conference"] == "Eastern", ]
west <- nhlstanding[nhlstanding["Conference"] == "Western", ]
east
bt_ranking
clean <- function(df){
  print(df)
  row.names(df) <- NULL
  df
  df <- rownames_to_column(df)
  df <- df %>% 
    rename(
      Actual_Ranking = rowname,
    )
  dfmerged <- merge(bt_ranking, df, by=c("Team"))
  dfmerged <- dfmerged %>% mutate(Bradley_Terry_Ranking = dense_rank(as.numeric(BT_Ranking)))
  return(dfmerged)
}
eastern <- clean(east)
western <- clean(west)


#comparing bradley-terry to actual standings?
plotrankings <- function(df, title){
dfselect <- select(df, c("Team","Actual_Ranking", "Bradley_Terry_Ranking"))
dfselect <- dfselect %>% 
  mutate(
    Actual_Ranking = as.numeric(Actual_Ranking),
  )
dfselect <- dfselect %>% arrange(desc(Actual_Ranking))
print(dfselect)
ggparcoord(dfselect,
           columns = 2:3, groupColumn = 1,  
           scale="globalminmax", 
           showPoints = TRUE, 
           title = title,
) + scale_y_reverse(breaks = 1:16)
}
plotrankings(eastern, "Eastern Actual vs Bradley-Terry Rankings")
plotrankings(western, "Western Actual vs Bradley-Terry Rankings")


getwins <- function(df, team){
  teamdf <- df[df["Home"] == team | df["Visitor"] == team, ]
  wins <- teamdf[(teamdf["Home"] == team | teamdf["home_win"]) | (teamdf["Visitor"] == team | !teamdf["home_win"]), ]
  return(wins)
}
oilers <- getwins(data, "Edmonton Oilers")
jets <- getwins(data, "Winnipeg Jets")
mean(abs(oilers$score_diff))
mean(abs(jets$score_diff))
mean(abs(data$score_diff))

eastern
coeff_df_new$Team <- substring(coeff_df_new$Team, 2, nchar(coeff_df_new$Team) - 1)
eastmerge <- merge(eastern, coeff_df_new, on="Team")
westmerge <- merge(western, coeff_df_new, on="Team")

mean(as.numeric(eastmerge$Coefficient))
mean(as.numeric(westmerge$Coefficient))


?group_by
