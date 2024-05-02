library(rvest)
library(magrittr)
library(dplyr)

total_data <- data.frame()

for (year in 2002:2024) {
  # URLs of the pages to scrape
  if (year != 2005 && year != 2006 && year != 2012 && year != 2021) {
    oct_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-october.html")
    oct_page <- read_html(oct_url)
    oct_table <- html_nodes(oct_page, "table#schedule")
    oct_data <- html_table(oct_table, fill = TRUE)[[1]]
    total_data <- rbind(total_data,oct_data)
  }
  if (year != 2012 && year != 2021) {
    nov_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-november.html")
    nov_page <- read_html(nov_url)
    nov_table <- html_nodes(nov_page, "table#schedule")
    nov_data <- html_table(nov_table, fill = TRUE)[[1]]
    total_data <- rbind(total_data,nov_data)
  }
  dec_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-december.html")
  dec_page <- read_html(dec_url)
  dec_table <- html_nodes(dec_page, "table#schedule")
  dec_data <- html_table(dec_table, fill = TRUE)[[1]]
  total_data <- rbind(total_data,dec_data)
  jan_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-january.html")
  jan_page <- read_html(jan_url)
  jan_table <- html_nodes(jan_page, "table#schedule")
  jan_data <- html_table(jan_table, fill = TRUE)[[1]]
  total_data <- rbind(total_data,jan_data)
  feb_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-february.html")
  feb_page <- read_html(feb_url)
  feb_table <- html_nodes(feb_page, "table#schedule")
  feb_data <- html_table(feb_table, fill = TRUE)[[1]]
  total_data <- rbind(total_data,feb_data)
  mar_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-march.html")
  mar_page <- read_html(mar_url)
  mar_table <- html_nodes(mar_page, "table#schedule")
  mar_data <- html_table(mar_table, fill = TRUE)[[1]]
  total_data <- rbind(total_data,mar_data)
  if (year != 2020) {
    apr_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-april.html")
    apr_page <- read_html(apr_url)
    apr_table <- html_nodes(apr_page, "table#schedule")
    apr_data <- html_table(apr_table, fill = TRUE)[[1]]
    if (year < 2020) {
      stop_row <- which(apr_data$Date == "Playoffs")[1]
      apr_data <- apr_data[1:(stop_row - 1), ]
    }
    if (year > 2021 && year != 2024) {
      stop_row <- which(apr_data$Notes == "Play-In Game")[1]
      apr_data <- apr_data[1:(stop_row - 1), ]
    }
    total_data <- rbind(total_data,apr_data)
  }
  if (year == 2021) {
    may_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-may.html")
    may_page <- read_html(may_url)
    may_table <- html_nodes(may_page, "table#schedule")
    may_data <- html_table(may_table, fill = TRUE)[[1]]
    stop_row <- which(may_data$Notes == "Play-In Game")[1]
    may_data <- may_data[1:(stop_row - 1), ]
    total_data <- rbind(total_data,may_data)
  }
  if (year == 2020) {
    jul_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-jul.html")
    jul_page <- read_html(jul_url)
    jul_table <- html_nodes(jul_page, "table#schedule")
    jul_data <- html_table(jul_table, fill = TRUE)[[1]]
    total_data <- rbind(total_data,jul_data)
    aug_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",year,"_games-aug.html")
    aug_page <- read_html(aug_url)
    aug_table <- html_nodes(aug_page, "table#schedule")
    aug_data <- html_table(aug_table, fill = TRUE)[[1]]
    stop_row <- which(aug_data$Notes == "Play-In Game")[1]
    aug_data <- aug_data[1:(stop_row - 1), ]
    total_data <- rbind(total_data,aug_data)
  }
  
  data_editor <- function(total_data) {
    
    colnames(total_data)[3] <- "Visitor"
    colnames(total_data)[4] <- "Vis_PTS"
    colnames(total_data)[5] <- "Home"
    colnames(total_data)[6] <- "Hom_PTS"
    
    total_data <- total_data[, -which(names(total_data) == "")]
    
    return(total_data)
  }
  
  total_data <- data_editor(total_data)
  var_name <- paste0("total_data_",year)
  assign(var_name, total_data)
  total_data <- total_data |>
    dplyr::mutate(
      Hom_PTS = as.numeric(Hom_PTS),
      Vis_PTS = as.numeric(Vis_PTS),
      score_diff = Hom_PTS - Vis_PTS
    )
  
  team_matrix_home <- model.matrix(~ total_data$Home)
  team_matrix_away <- model.matrix(~ total_data$Visitor)
  team_matrix <- team_matrix_home - team_matrix_away
  
  team_data <- as.data.frame(team_matrix[, -1])
  team_names <- sort(unique(total_data$Home))[-1]
  names(team_data) <- team_names
  
  linear_model <- lm(total_data$score_diff ~ ., data = team_data)                                         #
  
  logistic_model <- glm(total_data$score_diff > 0 ~ ., data = team_data, family = binomial())             #
  
  lin_coefficients <- coef(linear_model)
  
  lin_coefficients_df <- data.frame(
    names = names(lin_coefficients),
    lin_coefficients = as.numeric(lin_coefficients),
    stringsAsFactors = FALSE
  )
  
  log_coefficients <- coef(logistic_model)
  
  log_coefficients_df <- data.frame(
    names = names(log_coefficients),
    log_coefficients = as.numeric(log_coefficients),
    stringsAsFactors = FALSE
  )
  
  lin_coefficients_df <- lin_coefficients_df[-1,]
  lin_coefficients_df <- rbind(lin_coefficients_df,c("`Atlanta Hawks`",0))
  lin_coefficients_df$lin_coefficients <- as.numeric(lin_coefficients_df$lin_coefficients)
  lin_coefficients_df <- lin_coefficients_df[order(lin_coefficients_df$lin_coefficients,decreasing = TRUE),]
  
  var_name <- paste0("lin_",year)
  assign(var_name,lin_coefficients_df)
  
  log_coefficients_df <- log_coefficients_df[-1,]
  log_coefficients_df <- rbind(log_coefficients_df,c("`Atlanta Hawks`",0))
  log_coefficients_df$log_coefficients <- as.numeric(log_coefficients_df$log_coefficients)
  log_coefficients_df <- log_coefficients_df[order(log_coefficients_df$log_coefficients,decreasing = TRUE),]
  
  var_name <- paste0("log_",year)
  assign(var_name,log_coefficients_df)
}
colnames(total_data)
colnames(oct_data)
