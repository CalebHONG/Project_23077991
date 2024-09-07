
initialize_elo <- function(teams, initial_elo = 1500) {
  elo_scores <- setNames(rep(initial_elo, length(teams)), teams)
  return(elo_scores)
}

expected_win_probability <- function(home_elo, away_elo, theta = 400, home_advantage = 40, is_home = 1) {
  if (is_home == 1) {
    home_elo <- home_elo + home_advantage
  }
  exp_home <- 1 / (1 + 10^((away_elo - home_elo) / theta))
  exp_away <- 1 - exp_home
  return(c(home = exp_home, away = exp_away))
}


update_elo <- function(home_elo, away_elo, home_win, K = 40, theta = 400, home_advantage = 40, is_home = 1) {
  exp <- expected_win_probability(home_elo, away_elo, theta, home_advantage, is_home)
  
  if (home_win) {
    obs_home <- 1
    obs_away <- 0
  } else {
    obs_home <- 0
    obs_away <- 1
  }
  
  new_home_elo <- home_elo + K * (obs_home - exp[1])
  new_away_elo <- away_elo + K * (obs_away - exp[2])
  
  return(c(home = new_home_elo, away = new_away_elo))
}


decay_elo <- function(elo_scores, global_mean, days_diff, threshold = 200, decay_factor = 0.05) {
  for (team in names(elo_scores)) {
    if (days_diff > threshold) {

      #elo_scores[team] <- elo_scores[team] * (decay_factor ^ (days_diff - threshold))
      #elo_scores[team] <- elo_scores[team] * (exp(-0.1))
      #elo_scores[team]=global_mean + (elo_scores[team] - global_mean) * (0.1)
      elo_scores[team] = global_mean + (elo_scores[team] - global_mean) * (1-decay_factor)
    } else {

      #elo_scores[team] <- elo_scores[team] - (elo_scores[team] - global_mean) * (days_diff / threshold)
      elo_scores[team] = global_mean + (elo_scores[team] - global_mean) * (1-decay_factor)
      #elo_scores[team]=elo_scores[team]
    }
  }
  return(elo_scores)
}


teams <- unique(c(bbl_data$team1, bbl_data$team2))


elo_scores <- initialize_elo(teams)

correct_predictions <- 0
brier_score_sum <- 0
last_match_date <- as.Date(bbl_data$date[1])  
bbl_data$predictions = NA
bbl_data$bs = NA


for (i in 1:nrow(bbl_data)) {
  home_team <- bbl_data$team1[i]
  away_team <- bbl_data$team2[i]
  winner <- bbl_data$winner[i]
  is_home <- bbl_data$home[i]  
  current_match_date <- as.Date(bbl_data$date[i])
  days_diff <- as.numeric(difftime(current_match_date, last_match_date, units = "days"))
  
  global_mean_elo <- mean(elo_scores)
  elo_scores <- decay_elo(elo_scores, global_mean_elo, days_diff)
  
  home_elo <- elo_scores[home_team]
  away_elo <- elo_scores[away_team]
  

  exp <- expected_win_probability(home_elo, away_elo, home_advantage = 0, is_home = is_home)
  

  predicted_winner <- ifelse(exp[1] > exp[2], home_team, away_team)
  

  if (predicted_winner == winner) {
    bbl_data$predictions[i] = 1
    correct_predictions <- correct_predictions + 1
  } else {
    bbl_data$predictions[i] = 0
  }
  

  home_win <- (home_team == winner)
  new_elo <- update_elo(home_elo, away_elo, home_win, home_advantage = 0, is_home = is_home)
  
  elo_scores[home_team] <- new_elo[1]
  elo_scores[away_team] <- new_elo[2]
  

  if (home_win) {
    brier_score_sum <- brier_score_sum + (exp[1] - 1)^2
    bbl_data$bs[i] =  (exp[1] - 1)^2
  } else {
    brier_score_sum <- brier_score_sum + (exp[2] - 1)^2
    bbl_data$bs[i] =  (exp[2] - 1)^2
  }
  

  last_match_date <- current_match_date
}


accuracy <- correct_predictions / nrow(bbl_data)
print(paste("Final prediction accuracy:", round(accuracy * 100, 2), "%"))


brier_score <- brier_score_sum / nrow(bbl_data)
print(paste("Final Brier score:", round(brier_score, 4)))


final_correct_predictions <- sum(bbl_data$predictions[490:544])
final_brier_score <- mean(bbl_data$bs[490:544])

print(paste("Prediction accuracy for last 55 matches:", final_correct_predictions))
print(paste("Brier score for last 55 matches:", round(final_brier_score, 4)))


##################################################################################################

initialize_elo <- function(teams, initial_elo = 1500) {
  elo_scores <- setNames(rep(initial_elo, length(teams)), teams)
  return(elo_scores)
}


expected_win_probability <- function(home_elo, away_elo, theta = 400) {
  exp_home <- 1 / (1 + 10^((away_elo - home_elo) / theta))
  exp_away <- 1 - exp_home
  return(c(home = exp_home, away = exp_away))
}


update_elo <- function(home_elo, away_elo, home_win, K, theta = 400) {
  exp <- expected_win_probability(home_elo, away_elo, theta)
  
  if (home_win) {
    obs_home <- 1
    obs_away <- 0
  } else {
    obs_home <- 0
    obs_away <- 1
  }
  
  new_home_elo <- home_elo + K * (obs_home - exp[1])
  new_away_elo <- away_elo + K * (obs_away - exp[2])
  
  return(c(home = new_home_elo, away = new_away_elo))
}


teams <- unique(c(bbl_data$team1, bbl_data$team2))


elo_scores <- initialize_elo(teams)


correct_predictions <- 0
brier_score_sum <- 0

bbl_data$predictions = NA
bbl_data$bs = NA


season_k_values <- c(
  "T20 2011/12" = 40,
  "T20 2012/13" = 45,
  "T20 2013/14" = 50,
  "T20 2014/15" = 55,
  "T20 2015/16" = 60,
  "T20 2016/17" = 65,
  "T20 2017/18" = 70,
  "T20 2018/19" = 75,
  "T20 2019/20" = 80,
  "T20 2020/21" = 85,
  "T20 2021/22" = 90,
  "T20 2022/23" = 95,
  "T20 2023/24" = 100
)


for (i in 1:nrow(bbl_data)) {
  home_team <- bbl_data$team1[i]
  away_team <- bbl_data$team2[i]
  winner <- bbl_data$winner[i]
  season <- bbl_data$season[i]
  
  home_elo <- elo_scores[home_team]
  away_elo <- elo_scores[away_team]
  
  
  current_match_date <- as.Date(bbl_data$date[i])
  days_diff <- as.numeric(difftime(current_match_date, last_match_date, units = "days"))
  

  global_mean_elo <- mean(elo_scores)
  elo_scores <- decay_elo(elo_scores, global_mean_elo, days_diff)

  K <- season_k_values[season]
  

  exp <- expected_win_probability(home_elo, away_elo)

  predicted_winner <- ifelse(exp[1] > exp[2], home_team, away_team)
  
  if (predicted_winner == winner) {
    bbl_data$predictions[i] = 1
    correct_predictions <- correct_predictions + 1
  } else {
    bbl_data$predictions[i] = 0
  }
  

  home_win <- (home_team == winner)
  new_elo <- update_elo(home_elo, away_elo, home_win, K)
  
  elo_scores[home_team] <- new_elo[1]
  elo_scores[away_team] <- new_elo[2]
  
  if (home_win) {
    brier_score_sum <- brier_score_sum + (exp[1] - 1)^2
    bbl_data$bs[i] =  (exp[1] - 1)^2
  } else {
    brier_score_sum <- brier_score_sum + (exp[2] - 1)^2
    bbl_data$bs[i] =  (exp[2] - 1)^2
  }
}

accuracy <- correct_predictions / nrow(bbl_data)
print(paste("Final prediction accuracy:", round(accuracy * 100, 2), "%"))


brier_score <- brier_score_sum / nrow(bbl_data)
print(paste("Final Brier score:", round(brier_score, 4)))

final_correct_predictions <- sum(bbl_data$predictions[490:544])
final_brier_score <- mean(bbl_data$bs[490:544])

print(paste("Prediction accuracy for last 55 matches:", final_correct_predictions))
print(paste("Brier score for last 55 matches:", round(final_brier_score, 4)))

