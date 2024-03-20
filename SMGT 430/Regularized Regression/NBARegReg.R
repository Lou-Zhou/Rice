library("glmnet")
library("lubridate")
library("stringr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("tibble")
#data <- read.csv("path1")
#colnames(data)
#data["Date"]
#sapply(data, class)
#as.Date(data$Date)
#data$nice_dates = as.Date(data$nice_dates, format = "%m-%d-%Y")
#season <- data[data$nice_dates > as.Date("2021-10-19"), ]
#write.csv(season, "2021Data.csv")
data <- read.csv("/Users/louzhou/Documents/SMGT 430 Files/Data/NBAPBP/2021Data.csv")
data <- data %>%
  filter(str_detect(HomeEvent, "makes") | str_detect(HomeEvent, "misses") |
         str_detect(AwayEvent, "makes") | str_detect(AwayEvent, "misses")
         )
data$HomeEvent #Defense could force bad shots, good contest, etc.
noft <- data %>%
  filter(!str_detect(HomeEvent, "free throw") & !str_detect(AwayEvent, "free throw")) 
#Offensive: Shooter, Defensive: Defensive Players
noft$ActivePlayers
noft$Shooter <- strsplit(gsub("[\\[\\]']", "", noft$ActivePlayers, perl = TRUE), ", ")
noft$Shooter <- sapply(noft$Shooter, function(x) x[1])
noft <- noft %>%
  dplyr::mutate(
    isHome = ifelse(str_detect(H1, Shooter) | str_detect(H2, Shooter) | str_detect(H3, Shooter) | str_detect(H4, Shooter) | str_detect(H5, Shooter), 1, 0)
  )
getn <- function(lst, n){
  sapply(lst, `[`, n)
}
noft <- noft %>%
  dplyr::mutate(
    Def_1 = ifelse(isHome, H1, A1),
    Def_2 = ifelse(isHome, H2, A2),
    Def_3 = ifelse(isHome, H3, A3),
    Def_4 = ifelse(isHome, H4, A4),
    Def_5 = ifelse(isHome, H5, A5),
    didMake = ifelse(str_detect(HomeEvent, "makes") | str_detect(AwayEvent, "makes"), 1, 0),
    ShotDistance = getn(ifelse(HomeEvent == "", strsplit(AwayEvent, "\\D+"), strsplit(HomeEvent, "\\D+")), 3),
    ShotDistance = ifelse(is.na(ShotDistance), 1, ShotDistance),
    ShotDistance = as.numeric(ShotDistance),
    ShotDistance_Binned = cut(ShotDistance, breaks=c(0, 5, 10, 15, 25, 30, 99))
  )


select(noft, Def_1, Def_2, Def_3, Def_4, Def_5)

player_names <- unique(unlist(noft[, c("Def_1", "Def_2", "Def_3", "Def_4", "Def_5")]))
player_names

defenders <- sapply(player_names, function(player) {
  player_defending <- rowSums(noft[, c("Def_1", "Def_2", "Def_3", "Def_4", "Def_5")] == player) > 0
  as.numeric(player_defending)
})

defenders
length(noft[,1])

#make it so that instead of def_1, def_2,.. just 1 if isdefender, 0 if not
#combine the sparse matrix??
noft
model_matrix <- model.matrix(~ 0 + ShotDistance_Binned + Shooter + defenders, data = noft)
model_matrix
colnames(model_matrix)
#accounting for weights?

players




model_matrix <- as(model_matrix, "sparseMatrix") 
colnames(model_matrix)

model <- glmnet::glmnet(x = model_matrix, y = noft$didMake, lambda = .Machine$double.xmin) 
?glmnet
beta_2 <- coef(model)[grepl("Shooter", rownames(coef(model)))] 
coef(model)
shooting <- noft |>
  dplyr::group_by(Shooter) |>
  dplyr::summarize(
    player_shots = dplyr::n(),
    player_shooting = sum(didMake) / length(didMake),
    #Shooter = paste0("Shooter", Shooter, sep = "")
  )


defense <- noft %>%
  pivot_longer(cols = c("Def_1","Def_2","Def_3","Def_4","Def_5")) %>%
  count(name, value)
head(coefficients)
defense <- defense |>
  dplyr::group_by(value) |>
  dplyr::summarize(
  possessions = sum(n),
  #value = paste0("defenders", value, sep = "")
  )

defense <- defense |>
  dplyr::summarize(
    value = paste0("defenders", value, sep = ""),
    possessions = possessions,
    player_shooting = 0
  )

shooting <- shooting |>
  dplyr::summarize(
    value = paste0("Shooter", Shooter, sep = ""),
    possessions = player_shots,
    player_shooting = player_shooting
  )


defense
shooting
combined <- rbind(defense, shooting)
combined

library(broom)
coefs <- tidy(model)
coefs
coefs[order(coefs$estimate, decreasing = TRUE),]
model_coefs <-select(coefs, term, estimate)
model_coefs
head(model_coefs, 7)
model_coefs_players <- tail(model_coefs, -7)
model_coefs_players
#dealing with defenders very weirdly... how to fix?? Fixed!!
#sample size issue, how to sample for # of possessions

withappearances <- merge(combined, model_coefs_players, by.x = "value", by.y = "term")
withappearances
withappearancesfiltered <- withappearances[withappearances["possessions"] > 49, ]
withappearancesfiltered
player_data <- withappearancesfiltered |> arrange(desc(estimate))

#offensive vs defensive

player_data <- player_data |>
  dplyr::mutate(
  isShooting = ifelse(str_detect(value, "Shooter"), 1, 0)
)
player_data
select(player_data, value, isShooting)

shooters <- player_data[player_data["isShooting"] == 1, ]
defenders <- player_data[player_data["isShooting"] == 0, ]
defenders
shooters$value = substring(shooters$value, 8)
shooters$value
defenders$value = substring(defenders$value, 10)
defenders$value

shooters
defenders
shooters <- shooters %>% 
  rename(
    off_possessions = possessions,
    off_estimate = estimate
  )
defenders <- defenders %>% 
  rename(
    def_possessions = possessions,
    def_estimate = estimate
  )

colnames(shooters)
colnames(defenders)

shootersanddef <- merge(shooters, defenders, by.x = "value", by.y = "value")

shootersanddef

model_matrix <- model.matrix(~ 0 + ShotDistance_Binned + Shooter + defenders, data = noft)                           #
colnames(model_matrix)
#accounting for weights?

players




model_matrix <- as(model_matrix, "sparseMatrix") 
colnames(model_matrix)

model <- glmnet::glmnet(x = model_matrix, y = noft$didMake, lambda = .Machine$double.xmin) 
ridgemodel <- glmnet::cv.glmnet(x = model_matrix, y = noft$didMake, standardize = FALSE, alpha = 0) 
library(broom)
ridgecoefs <- coef(ridgemodel)
rownames(coef(ridgemodel))
reg_coefficients <- data.frame(
  name = rownames(coef(ridgemodel)),
  coefficients = coef(ridgemodel, s = "lambda.min")[,1]
)

ridgecoefs_players <- tail(reg_coefficients, -7)
ridgecoefs_players
withappearances <- merge(combined, ridgecoefs_players, by.x = "value", by.y = "name")
player_data <- withappearances |> arrange(desc(coefficients))

#offensive vs defensive

player_data <- player_data |>
  dplyr::mutate(
    isShooting = ifelse(str_detect(value, "Shooter"), 1, 0)
  )
shooters <- player_data[player_data["isShooting"] == 1, ]
defenders <- player_data[player_data["isShooting"] == 0, ]
defenders
shooters$value = substring(shooters$value, 8)
shooters$value
defenders$value = substring(defenders$value, 10)
defenders$value

shooters
defenders
shooters <- shooters %>% 
  rename(
    off_possessions = possessions,
    off_estimate = coefficients
  )
defenders <- defenders %>% 
  rename(
    def_possessions = possessions,
    def_estimate = coefficients
  )

colnames(shooters)
colnames(defenders)
shootersanddef <- merge(shooters, defenders, by = "value")

shootersanddef <- shootersanddef |>
  dplyr::summarize(
    player = value,
    off_possessions = off_possessions,
    player_shooting = player_shooting.x,
    off_estimate = off_estimate,
    def_possessions = def_possessions,
    def_estimate = def_estimate
  )
shootersanddef |> arrange(desc(off_estimate)) #best scorers
shootersanddef |> arrange(def_estimate) #best defenders
nonshooter <- head(reg_coefficients, 7)
nonshooter

playerids <- read.csv("/Users/name/Documents/SMGT 430 Files/Data/NBAPBP/NBA_Player_IDs.csv")
playerids <- select(playerids, BBRefName, BBRefID)
playerids
colnames(shootersanddef)
named = merge(playerids, shootersanddef, by.x = "BBRefID", by.y = "player")
named <-named |> 
  dplyr::mutate(
    def_estimated_graph = -1 * def_estimate,
    combined = (def_estimated_graph + off_estimate) / 2
  )
ggplot(data = named, aes(off_estimate, def_estimated_graph))  + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  #geom_text(aes(label=ifelse(off_estimate > 0.02,as.character(BBRefName),'')),hjust=0.5,vjust=-.5, size = 3) +
  #geom_text(aes(label=ifelse(def_estimated_graph > 0.011,as.character(BBRefName),'')),hjust=0.5,vjust=-.5, size = 3) +
  geom_text(aes(label=ifelse(combined > 0.0075,as.character(BBRefName),'')),hjust=0.5,vjust=-.5, size = 3)  + ggtitle("Defensive and Offensive Strengths, 20-21 NBA Season") +
  xlab("Offensive Strength") + ylab("Defensive Strength") + labs(color = "Overall Strength") + geom_smooth(method='lm') + geom_point(aes(color = combined))+ scale_color_viridis_c() 
mean(named$def_estimate)
mean(named$off_estimate)
named |> arrange(desc(off_estimate)) #best scorers
named |> arrange(def_estimate) #best defenders
cor(named$off_estimate, named$def_estimated_graph, method = "pearson")
# -0.3276891
?pivot_wider
