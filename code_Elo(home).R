


initialize_elo <- function(teams, initial_elo = 1500) {
  elo_scores <- setNames(rep(initial_elo, length(teams)), teams)
  return(elo_scores)
}


expected_win_probability <- function(home_elo, away_elo, theta = 400, home_advantage = 80, is_home = 1) {
  if (is_home == 1) {
    home_elo <- home_elo + home_advantage
  }
  exp_home <- 1 / (1 + 10^((away_elo - home_elo) / theta))
  exp_away <- 1 - exp_home
  return(c(home = exp_home, away = exp_away))
}


update_elo <- function(home_elo, away_elo, home_win, K = 40, theta = 400, home_advantage = 80, is_home = 1) {
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


decay_to_average <- function(elo_scores, decay_factor = 0.995) {
  average_elo <- mean(elo_scores)
  return(average_elo + (elo_scores - average_elo) * decay_factor)
}

teams <- unique(c(bbl_data$team1, bbl_data$team2))

elo_scores <- initialize_elo(teams)

correct_predictions <- 0
brier_score_sum <- 0

bbl_data$predictions = NA
bbl_data$bs = NA


for (i in 1:nrow(bbl_data)) {
  home_team <- bbl_data$team1[i]
  away_team <- bbl_data$team2[i]
  winner <- bbl_data$winner[i]
  is_home <- bbl_data$home[i]  
  
  home_elo <- elo_scores[home_team]
  away_elo <- elo_scores[away_team]
  

  exp <- expected_win_probability(home_elo, away_elo, home_advantage = 80, is_home = is_home)
  

  predicted_winner <- ifelse(exp[1] > exp[2], home_team, away_team)
  

  if (predicted_winner == winner) {
    bbl_data$predictions[i] = 1
    correct_predictions <- correct_predictions + 1
  } else {
    bbl_data$predictions[i] = 0
  }

  home_win <- (home_team == winner)
  new_elo <- update_elo(home_elo, away_elo, home_win, home_advantage = 80, is_home = is_home)
  
  elo_scores[home_team] <- new_elo[1]
  elo_scores[away_team] <- new_elo[2]
  
  #elo_scores <- decay_to_average(elo_scores, decay_factor = 0.995)

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
