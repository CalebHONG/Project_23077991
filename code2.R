# Create two data frames corresponding to team1 and team2
team1_data = select(bbl_data, season, team = team1, score = runs_team1, target=target_team1,wicket=wickets_team1, balls=balls_team1,max_ball=max_balls_team1)

team2_data = select(bbl_data, season, team = team2, score = runs_team2, target=target_team2,wicket=wickets_team2, balls=balls_team2,max_ball=max_balls_team2)

# Merge data frames
combined_data = bind_rows(team1_data, team2_data)

# Grouping data
grouped_data = group_by(combined_data, season, team)

# take average of score
rates_table = summarise(grouped_data, scoring_rate = sum(score)/sum(balls))

print(rates_table, n = 104)
########################################################################################
# bbl_winner transform to character
bbl_data$winner=as.character(bbl_data$winner)

# Fill in missing values
for (i in missing_winner_positions){
  # fixed season
  fixed_season = bbl_data[i,"season"]
  
  # team1 and team2
  first_team = bbl_data[i,"team1"]
  second_team = bbl_data[i,"team2"]
  
  # transform to character
  first_team = as.character(first_team)
  second_team = as.character(second_team)
  
  # Calculating averages, controlling seasons and teams
  averagescore_first_team = rates_table[rates_table$season == fixed_season
                                           & rates_table $team == first_team, ]
  averagescore_second_team = rates_table[rates_table$season == fixed_season
                                            & rates_table $team == second_team, ]
  
  missing_winner=ifelse(averagescore_first_team$scoring_rate > averagescore_second_team$scoring_rate, 
                        first_team,  second_team)
  bbl_data[i,"winner"] =missing_winner
}

#############################################
#############################################
#############################################
#############################################
# Create two data frames corresponding to team1 and team2
team1_data = select(bbl_data, season, team = team1, score = runs_team1, balls=balls_team1)
team2_data = select(bbl_data, season, team = team2, score = runs_team2, balls=balls_team2)

# Merge data frames
combined_data = bind_rows(team1_data, team2_data)

# Grouping data
grouped_data = group_by(combined_data, season, team)

# take average of score
rates_table = summarise(grouped_data, scoring_rate = sum(score)/sum(balls))

print(rates_table, n = 104)
########################################################################################
# bbl_winner transform to character
bbl_data$winner=as.character(bbl_data$winner)

# Fill in missing values
for (i in missing_winner_positions){
  # fixed season
  fixed_season = bbl_data[i,"season"]
  
  # team1 and team2
  first_team = bbl_data[i,"team1"]
  second_team = bbl_data[i,"team2"]
  
  # transform to character
  first_team = as.character(first_team)
  second_team = as.character(second_team)
  
  # Calculating averages, controlling seasons and teams
  scoring_rate_first_team = rates_table[rates_table$season == fixed_season
                                        & rates_table$team == first_team, ]
  scoring_rate_second_team = rates_table[rates_table$season == fixed_season
                                         & rates_table$team == second_team, ]
  
  missing_winner=ifelse(scoring_rate_first_team$scoring_rate > scoring_rate_second_team$scoring_rate, 
                        first_team,  second_team)
  bbl_data[i,"winner"] =missing_winner
}

bbl_data[missing_winner_positions,] #missing value


###################################################over all seanson 
# Grouping data
grouped_data = group_by(combined_data, team)
# take average of score
rates_table = summarise(grouped_data, scoring_rate = sum(score)/sum(balls))

for(i in missing_winner_positions[c(1,9)]){
  
  # team1 and team2
  first_team = bbl_data[i,"team1"]
  second_team = bbl_data[i,"team2"]
  
  # transform to character
  first_team = as.character(first_team)
  second_team = as.character(second_team)
  
  # Calculating averages, controlling seasons and teams
  scoring_rate_first_team = rates_table[rates_table$team == first_team, ]
  scoring_rate_second_team = rates_table[rates_table$team == second_team, ]
  
  missing_winner=ifelse(scoring_rate_first_team$scoring_rate > scoring_rate_second_team$scoring_rate, 
                        first_team,  second_team)
  bbl_data[i,"winner"] = missing_winner
}
bbl_data[missing_winner_positions,]
#############################################################################
column_sums = colSums(winner_toss)


cum_heights = apply(winner_toss, 2, cumsum) 


for (i in 1:ncol(winner_toss)) {
  for (j in 1:nrow(winner_toss)) {

    part_percentage <- round((winner_toss[j, i] / column_sums[i]) * 100, 1)  

    label_y_position <- if (j == 1) {
      winner_toss[j, i] / 2
    } else {
      cum_heights[j - 1, i] + winner_toss[j, i] / 2
    }

    text(bar_positions[i], label_y_position, labels = paste(part_percentage, "%", sep=""), cex = 0.9)
  }
}


###############
bbl_data$odds = NA
 bbl_data$odds = ifelse(bbl_data$dbMktPriceTeam1>bbl_data$dbMktPriceTeam2,0,1)
 bbl_data$com =NA
 bbl_data$new_winner = NA
 bbl_data$new_winner[as.character(bbl_data$winner)==as.character(bbl_data$team1)] = 1
 bbl_data$new_winner[as.character(bbl_data$winner) == as.character(bbl_data$team2)] = 0
 bbl_data$com = ifelse(bbl_data$odds==bbl_data$new_winner,"yes","no")
 table(bbl_data$season,bbl_data$com)
 ####################################################################################
 
 # (Batting Performance, BP)
 calculate_BP <- function(R, B, total_runs, total_balls) {
   SR <- (R / B) * 100  
   MSR <- (total_runs / total_balls) * 100 
   BP <- (SR / MSR) * 0.5  
   return(BP)
 }
 
 #  (Adjusted Combined Bowling Rate, ACBR)
 calculate_ACBR <- function(R, B, W, RPB_i, RPB_match) {
   # Harmonic mean of bowling average, economy rate, and strike rate
   bowling_average <- R / W
   economy_rate <- (R / B) * 6
   strike_rate <- B / W
   
   # Calculate ACBR
   ACBR <- (3 * (R / B)) / ((1 / W) + (6 / B) + (B / W))
   return(ACBR)
 }
 
 # 标准化函数
 normalize <- function(value, min_value, max_value) {
   normalized_value <- (value - min_value) / (max_value - min_value)
   return(normalized_value)
 }
 
 # (Team Strength, ki)
 calculate_team_strength <- function(BP, ACBR, w1, w2) {
   Y_BP <- normalize(BP, min(BP), max(BP)) 
   Y_ACBR <- normalize(max(ACBR) - ACBR, 0, max(ACBR) - min(ACBR))  
   
   ki <- w1 * Y_BP + w2 * Y_ACBR  
   return(ki)
 }
 
 
 
 
 # Sort by date
 bbl_data = bbl_data[order(bbl_data$date), ]
 
 # Defining new variables
 bbl_data$team1_recent_results = vector("list", nrow(bbl_data))
 bbl_data$team2_recent_results = vector("list", nrow(bbl_data))
 
 # Calculate the results (1 for win, 0 for loss) in the last five games for the top two teams in each game
 for (i in 1:nrow(bbl_data)) {
   # Current tournament dates and teams
   current_date = bbl_data$date[i]
   team1 = bbl_data$team1[i]
   team2 = bbl_data$team2[i]
   
   # Find the games prior to the current game
   past_matches = bbl_data[bbl_data$date < current_date, ]
   
   # Recording team1's last five game results
   team1_matches = past_matches[past_matches$team1 == team1 | past_matches$team2 == team1, ]
   if (nrow(team1_matches) > 5) {
     team1_matches <- tail(team1_matches, 5)
   }
   team1_results <- ifelse(team1_matches$winner == team1, 1, 0)
   # If less than 5 matches, pad with NA
   team1_results <- c(rep(NA, 5 - length(team1_results)), team1_results)
   bbl_data$team1_recent_results[[i]] <- team1_results
   
   # Recording team2's last five game results
   team2_matches <- past_matches[past_matches$team1 == team2 | past_matches$team2 == team2, ]
   if (nrow(team2_matches) > 5) {
     team2_matches <- tail(team2_matches, 5)
   }
   team2_results <- ifelse(team2_matches$winner == team2, 1, 0)
   # If less than 5 matches, pad with NA
   team2_results <- c(rep(NA, 5 - length(team2_results)), team2_results)
   bbl_data$team2_recent_results[[i]] = team2_results
 }
 
 bbl_data$CF1 = NA
 bbl_data$CF2 = NA
 current= bbl_data[100:544,]
 t=1:5
 weights1 <- (1 - current$team1_strength)
 weights2 <- (1 - current$team2_strength)
current$CF1 = NA
current$CF2 = NA
 for(i in 1:nrow(current)){
 current$CF1[i] =( sum(weights1[i] * current$team1_recent_results[[i]][5]+weights1[i]^2 *current$team1_recent_results[[i]][4]+
                        weights1[i]^3 * current$team1_recent_results[[i]][3]+
                      weights1[i]^4 * current$team1_recent_results[[i]][2]+weights1[i]^5 * current$team1_recent_results[[i]][1])
 /sum(weights1[i]+weights1[i]^2 +
        weights1[i]^3 +
        weights1[i]^4 +weights1[i]^5))
 current$CF2[i] = (sum(weights2[i] * current$team2_recent_results[[i]][5]+weights2[i]^2 * current$team2_recent_results[[i]][4]+
                        weights2[i]^3 * current$team1_recent_results[[i]][3]+
                      weights2[i]^4 * current$team1_recent_results[[i]][2]+weights2[i]^5 * current$team1_recent_results[[i]][1])
 /sum(weights2[i]+weights2[i]^2 +
        weights2[i]^3 +
        weights2[i]^4 +weights2[i]^5))
 }
CF=ifelse(current$CF1>current$CF2,1,0)
sum(tail(CF,55)==tail(bbl_data$new_winner,55))
sum(tail(CF,55)==tail(bbl_data$market,55))
 # (Current Form, CF)
 calculate_CF <- function(recent_results, ki) {
   t <- 1:length(recent_results)  
   weights <- (1 - ki)^(t - 1)
   CF <- sum(weights * recent_results)  
   return(CF)
 }