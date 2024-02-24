library(magrittr)

data <- read.csv("/Users/louzhou/Downloads/uslc_stats_assignment_2.csv")
filtered = data[c("Player","Team","Season","Position","Shots", "SoT")]
filtered <- filtered[filtered$Shots > 0, ]
totalSoTPercent <- sum(filtered$SoT) / sum(filtered$Shots)
totalSoTPercent #0.3473254
filtered <- filtered %>% mutate(SoTPercent = SoT / Shots)
filtered <- filtered %>% mutate(noise_variance = totalSoTPercent * (1 - totalSoTPercent) / Shots) 
pop_variance <- function(observed, pop_mean, noise_variance){
  previous_pop_var <- -1
  pop_var <- 0
  iter <- 0
  var <- (observed - pop_mean)**2 - noise_variance
  while((abs(pop_var) - previous_pop_var) > .000000001 & iter < 10000){
    previous_pop_var <- pop_var
    iter <- iter + 1
    pop_var <- weighted.mean(var, w = (pop_var + noise_variance)**{-2})
  }
  return(pop_var)
  }
test_var <- pop_variance(filtered$SoTPercent, totalSoTPercent, filtered$noise_variance)
print(test_var)#need 0.05548214
regressiontothemean <- function(n, var, obs, popvar, popmean){
  numerator = (n / var) * obs + (1 / popvar) * popmean
  denominator = (n / var) + (1 / popvar)
  return(numerator / denominator)
}
filtered <- filtered |>                                                                                   #
  dplyr::mutate(true_talent = regressiontothemean(Shots, noise_variance, SoTPercent, test_var, totalSoTPercent))
filtered
defense <- filtered[filtered$Position %in% c('DM','CB','FB'),]
def_totalSoTPercent <- sum(defense$SoT) / sum(defense$Shots)
defense <- defense %>% mutate(noise_variance = def_totalSoTPercent * (1 - def_totalSoTPercent) / Shots) 
def_var <- pop_variance(defense$SoTPercent, def_totalSoTPercent, defense$noise_variance)
defense <- defense |>                                                                                   #
  dplyr::mutate(true_talent = regressiontothemean(Shots, noise_variance, SoTPercent, def_var, def_totalSoTPercent))
defense
print(def_var)
attacker <- filtered[filtered$Position %in% c('CM','AM','W','ST'),]
att_totalSoTPercent <- sum(attacker$SoT) / sum(attacker$Shots)
attacker <- attacker %>% mutate(noise_variance = att_totalSoTPercent * (1 - att_totalSoTPercent) / Shots) 
att_var <- pop_variance(attacker$SoTPercent, att_totalSoTPercent, attacker$noise_variance)
attacker <- attacker |>                                                                                   #
  dplyr::mutate(true_talent = regressiontothemean(Shots, noise_variance, SoTPercent, att_var, att_totalSoTPercent))
attacker
att_var
#plotting
graph_pos <- function(df, num, color){
  points(x = df$SoTPercent,y = df$true_talent, col = color)
  sorted <- df |>                                                                                           #
    dplyr::arrange(-true_talent)   
  numplayers = length(df$true_talent)
  if (num > 0){
  for (idx in seq(from = 1, to = num, by = 1)){
  #text(sorted$SoTPercent[idx], sorted$true_talent[idx]+.05, labels=sorted$Player[idx], cex = .35)
  #text(sorted$SoTPercent[numplayers - idx], sorted$true_talent[numplayers - idx]-.05, labels=sorted$Player[numplayers - idx], cex = .35)
  }
  print(head(sorted,num))
  print("=")
  print(tail(sorted,num))
  }
}
plot(x = c(), y = c(), xlim = c(0, 1), ylim = c(0,1),
     xlab="Actual SoT%", ylab="Estimated True SoT%", col = "blue", main = "Defensive Estimated SoT% vs Actual SoT%")
abline(a = 0, b = 1, col = "black")
graph_pos(defense, 3, "blue")
graph_pos(attacker, 3, "red")
mean(defense$true_talent)
mean(attacker$true_talent)
resfactor = 3
dev.copy(png, "regressdef.png",res = 72*resfactor, height=480*resfactor, width=480*resfactor)
dev.off()
match("Simeon Betapudi", attacker$Player)
attacker[attacker$Player == "Simeon Betapudi", ]
attacker$Player[304]
text(attacker$SoTPercent[304]+.04, attacker$true_talent[304]+.05, labels=expression("Simeon Betapudi" ^ "1"), cex = .75)

