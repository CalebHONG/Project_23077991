install.packages("PlayerRatings")
library(PlayerRatings)
install.packages("elo")
library(elo)

# 
elo_data <- data.frame(
  Date = as.numeric(as.Date(bbl_data$date)),
  Player1 = bbl_data$team1,
  Player2 = bbl_data$team2,
  Result = ifelse(bbl_data$winner == bbl_data$team1, 1, 0)
)

elo_ratings <- elo(elo_data, kFactor = 40)


print(elo_ratings)

final_ratings <- final(elo_ratings)
print(final_ratings)


#######################################################################################
matches <- data.frame(
  Date = as.numeric(as.Date(bbl_data$date)), 
  Player1 = bbl_data$team1, 
  Player2 = bbl_data$team2, 
  Result = ifelse(bbl_data$winner == bbl_data$team1, 1, 0)
)
result <- elo(matches, init = 1500, k = 40, history = TRUE)
print(result)
# Glicko-2
ratings <- glicko2(matches)

final_ratings <- ratings$ratings
print(final_ratings)
