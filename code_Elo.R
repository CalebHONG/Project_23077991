#######Elo
#################original Elo


# initial Elo rating
initialize_elo <- function(teams, initial_elo = 1500) {
  elo_scores <- setNames(rep(initial_elo, length(teams)), teams)
  return(elo_scores)
}

# cal win rate
expected_win_probability <- function(home_elo, away_elo, theta = 400) {
  exp_home <- 1 / (1 + 10^((away_elo - home_elo) / theta))
  exp_away <- 1 - exp_home
  return(c(home = exp_home, away = exp_away))
}

# update Elo rating
update_elo <- function(home_elo, away_elo, home_win, K = 60, theta = 400) {
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

for (i in 1:nrow(bbl_data)) {
  home_team <- bbl_data$team1[i]
  away_team <- bbl_data$team2[i]
  winner <- bbl_data$winner[i]
  
  home_elo <- elo_scores[home_team]
  away_elo <- elo_scores[away_team]
  

  exp <- expected_win_probability(home_elo, away_elo)
  

  predicted_winner <- ifelse(exp[1] > exp[2], home_team, away_team)
  
  if (predicted_winner == winner) {
    bbl_data$predictions[i] = 1
    correct_predictions <- correct_predictions + 1
  } else {
    bbl_data$predictions[i] = 0
  }
  
  home_win <- (home_team == winner)
  new_elo <- update_elo(home_elo, away_elo, home_win)
  
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
sum(bbl_data$predictions[490:544])
mean(bbl_data$bs[490:544])
################################################################################
##################################3
####################################################################

elo_scores <- initialize_elo(teams)


train_data <- bbl_data[1:(nrow(bbl_data) - 55), ]
test_data <- bbl_data[(nrow(bbl_data) - 54):nrow(bbl_data), ]


for (i in 1:nrow(train_data)) {
  home_team <- train_data$team1[i]
  away_team <- train_data$team2[i]
  winner <- train_data$winner[i]
  
  home_elo <- elo_scores[home_team]
  away_elo <- elo_scores[away_team]
  
  home_win <- (home_team == winner)
  new_elo <- update_elo(home_elo, away_elo, home_win)
  
  elo_scores[home_team] <- new_elo[1]
  elo_scores[away_team] <- new_elo[2]
}


correct_predictions <- 0
brier_score_sum <- 0

for (i in 1:nrow(test_data)) {
  home_team <- test_data$team1[i]
  away_team <- test_data$team2[i]
  winner <- test_data$winner[i]
  
  home_elo <- elo_scores[home_team]
  away_elo <- elo_scores[away_team]
  

  exp <- expected_win_probability(home_elo, away_elo)
  

  predicted_winner <- ifelse(exp[1] > exp[2], home_team, away_team)
  
  if (predicted_winner == winner) {
    correct_predictions <- correct_predictions + 1
  }
  

  if (home_team == winner) {
    brier_score_sum <- brier_score_sum + (exp[1] - 1)^2
  } else {
    brier_score_sum <- brier_score_sum + (exp[2] - 0)^2
  }
}


accuracy <- correct_predictions / nrow(test_data)
print(paste("Accuracy for the last 55 matches:", round(accuracy * 100, 2), "%"))

brier_score <- brier_score_sum / nrow(test_data)
print(paste("Brier score for the last 55 matches:", round(brier_score, 4)))
#######################################################################################
####################################Elo
######################################################################################

initialize_elo <- function(teams, initial_elo = 1500) {
  elo_scores <- setNames(rep(initial_elo, length(teams)), teams)
  return(elo_scores)
}

expected_win_probability <- function(home_elo, away_elo, theta = 200) {
  exp_home <- 1 / (1 + 10^((away_elo - home_elo) / theta))
  exp_away <- 1 - exp_home
  return(c(home = exp_home, away = exp_away))
}

update_elo <- function(home_elo, away_elo, home_win, K = 30, theta = 200, lambda = 0.01) {
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
  
  new_home_elo <- new_home_elo * (1 - lambda) + 1500 * lambda
  new_away_elo <- new_away_elo * (1 - lambda) + 1500 * lambda
  
  return(c(home = new_home_elo, away = new_away_elo))
}

teams <- unique(c(bbl_data$team1, bbl_data$team2))


elo_scores <- initialize_elo(teams)


correct_predictions <- 0

bbl_data$predictions <- NA

for (i in 1:nrow(bbl_data)) {
  home_team <- bbl_data$team1[i]
  away_team <- bbl_data$team2[i]
  winner <- bbl_data$winner[i]
  
  home_elo <- elo_scores[home_team]
  away_elo <- elo_scores[away_team]
  

  exp <- expected_win_probability(home_elo, away_elo)
  

  predicted_winner <- ifelse(exp[1] > exp[2], home_team, away_team)
  

  if (predicted_winner == winner) {
    bbl_data$predictions[i] <- 1
    correct_predictions <- correct_predictions + 1
  } else {
    bbl_data$predictions[i] <- 0
  }
  

  home_win <- (home_team == winner)
  new_elo <- update_elo(home_elo, away_elo, home_win, K = 30, lambda = 0.01)
  
  elo_scores[home_team] <- new_elo[1]
  elo_scores[away_team] <- new_elo[2]
}

accuracy <- correct_predictions / nrow(bbl_data)
print(paste("Final prediction accuracy:", round(accuracy * 100, 2), "%"))

##########################################################################################################
####
##################################
theta_values <- seq(300, 500, by = 50)
k_values <- seq(20, 60, by = 10)
param_grid <- expand.grid(theta = theta_values, K = k_values)

time_series_cv <- function(theta, K, min_train_size = 100) {
  n <- nrow(bbl_data[1:490,])
  correct_predictions <- c()
  

  teams <- unique(c(bbl_data$team1, bbl_data$team2))
  elo_scores <- initialize_elo(teams)
  
  for (i in seq(min_train_size, n - 1)) {

    home_team <- bbl_data$team1[i]
    away_team <- bbl_data$team2[i]
    winner <- bbl_data$winner[i]
    
    home_elo <- elo_scores[home_team]
    away_elo <- elo_scores[away_team]
    

    exp <- expected_win_probability(home_elo, away_elo, theta)
    

    predicted_winner <- ifelse(exp[1] > exp[2], home_team, away_team)
    

    correct_predictions <- c(correct_predictions, predicted_winner == winner)
    

    home_win <- (home_team == winner)
    new_elo <- update_elo(home_elo, away_elo, home_win, K = K, theta = theta)
    
    elo_scores[home_team] <- new_elo[1]
    elo_scores[away_team] <- new_elo[2]
  }
  
  return(mean(correct_predictions))
}


results <- data.frame()
for (i in 1:nrow(param_grid)) {
  theta <- param_grid$theta[i]
  K <- param_grid$K[i]
  
  score <- time_series_cv(theta, K)
  results <- rbind(results, data.frame(theta = theta, K = K, accuracy = score))
}

best_params <- results[which.max(results$accuracy), ]
print(best_params)