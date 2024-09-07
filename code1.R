library(dplyr)


# Read data with appropriate names
bbl_odds = read.csv("bbl_odds.csv", header=TRUE, sep = '')
bbl_data = read.csv("bbl_dataset.csv", header=TRUE)

#  select right term sequence
right_term=bbl_data[,c("id","team1","team2")]

# merge right term and bbl_odds 
merged_data = merge(right_term, bbl_odds, by = "id",all.x = TRUE)

# match "id" variable according to bbl_dataset
match_data = merged_data[match(bbl_data$id, merged_data$id), ]

# Alter the term order and dbMktPrice according to "bbl_dataset"
for (i in 1:nrow(match_data)) {
  if (!is.na(match_data[i, 4]) && match_data[i, 2] != match_data[i, 4]) {
      temp = match_data[i, 7]
      match_data[i, 7] = match_data[i, 6]
      match_data[i, 6] = temp
    }
}

# new "bbl_odds"
adjust_odds=match_data[,c(1,2,3,6,7)]

# Harmonized column names
colnames(adjust_odds) = c("id", "team1", "team2", "dbMktPriceTeam1", "dbMktPriceTeam2")
############################################################################
############################################################################
# Splitting the data into two parts
n = length(bbl_data$winner)
training_data = bbl_data[1:(0.7*n),] #data for predicting
testing_data = bbl_data[-(1:(0.7*n)),] #data for training and validation

# transform to factor
bbl_data$id=as.factor(bbl_data$id)

bbl_data$season=as.factor(bbl_data$season)
bbl_data$ground=as.factor(bbl_data$ground)
bbl_data$timezone=as.factor(bbl_data$timezone)

bbl_data$team1=as.factor(bbl_data$team1)
bbl_data$team2=as.factor(bbl_data$team2)
bbl_data$neutral_venue=as.factor(bbl_data$neutral_venue)

bbl_data$day_night_game=as.factor(bbl_data$day_night_game)
bbl_data$night_game=as.factor(bbl_data$night_game)

bbl_data$first_innings=as.factor(bbl_data$first_innings)
bbl_data$winner=as.factor(bbl_data$winner)
bbl_data$toss_winner=as.factor(bbl_data$toss_winner)

# number of rows and columns
num_of_row = nrow(bbl_data)
num_of_col = ncol(bbl_data)

# team_list
team_list = unique(bbl_data$team1)

# View the structure of the bbl_data
str(bbl_data)

# Get a summary of the bbl_data
summary(bbl_data)

# View the first few rows of the bbl_data
head(bbl_data)

############################################################################
############################################################################
######################Fill in missing values################################
############################################################################
# find missing value in each variable
colSums(is.na(bbl_data))

# find missing positions of row in "winner" variable
missing_winner_positions = which(is.na(bbl_data$winner))
bbl_data[missing_winner_positions,]


#########################################################################################
# Create two data frames corresponding to team1 and team2
team1_data = select(bbl_data, season, team = team1, score = runs_team1, target=target_team1,wicket=wickets_team1, balls=balls_team1,max_ball=max_balls_team1)

# omit wickst=10
team1_data = filter(team1_data, max_ball !=120 | (balls!=120 & wicket!=10))
team1_data = filter(team1_data,  score <target | is.na(target) |(score>=target & max_ball != 120))

team2_data = select(bbl_data, season, team = team2, score = runs_team2, target=target_team2,wicket=wickets_team2, balls=balls_team2,max_ball=max_balls_team2)

# omit wickst=10
team2_data = filter(team2_data, max_ball !=120 | (balls!=120 & wicket!=10))
team2_data = filter(team2_data, score <target | is.na(target) | (score>=target & max_ball != 120))

# Merge data frames
combined_data = bind_rows(team1_data, team2_data)

# Grouping data
grouped_data = group_by(combined_data, season, team)

# take average of score
rates_table = summarise(grouped_data, scoring_rate = sum(score)/sum(balls))

print(rates_table, n = 89)
########################################################################################
# bbl_winner transform to character
bbl_data$winner=as.character(bbl_data$winner)

# Fill in missing values
for (i in missing_winner_positions[1:8]){
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
  bbl_data[i,"winner"] = missing_winner
}

###################################################over all seanson 
# Grouping data
grouped_data = group_by(combined_data, team)
# take average of score
rates_table = summarise(grouped_data, scoring_rate = sum(score)/sum(balls))

for(i in missing_winner_positions){
  
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
#########################################################
########################################################
#################################################################EDA
bbl_data$winner = as.factor(bbl_data$winner)

win_counts = table(bbl_data$winner[bbl_data$winner!="Tie"])[-9]
tie_data = bbl_data[bbl_data$winner=="Tie",]
tie_counts = table(tie_data$team1)+table(tie_data$team2)
lost_counts = table(bbl_data$team1)+table(bbl_data$team2)-win_counts-tie_counts

results_matrix = rbind(win_counts, tie_counts, lost_counts)

Total_game = table(bbl_data$team1)+table(bbl_data$team2)
win_per = win_counts/Total_game
tie_per = tie_counts/Total_game
lost_per = lost_counts/Total_game

bar_positions = barplot(results_matrix, 
        beside = FALSE, 
        col = c("lightgreen", "pink", "lightblue"), 
        #names.arg = team_list,
        names.arg = c("Adelaide\nStrikers","Brisbane\nHeat ","Hobart\nHurricanes",
                      "Melbourne\nRenegades","Melbourne\nStars","Perth\nScorchers",
                      "Sydney\nSixers","Sydney\nThunder"),
        #names.arg = c("Adelaide\na","Brisbane","Hobart","Renegades","Stars","Perth","Sixers","Thunder"),
        legend = c("Wins","Ties", "Losses" ),
        main = "Team Results", 
        xlab = "Team", 
        ylab = "Number of Results",
        args.legend = list(x = "bottomright", inset = c(0, 0.012), cex = 0.8),
        cex.names = 0.85
        )
bardata_matrix = rbind(win_per, tie_per, lost_per)
column_sums = colSums(results_matrix)


cum_heights = apply(results_matrix, 2, cumsum)  


for (i in 1:ncol(results_matrix)) {
  for (j in 1:nrow(results_matrix)) {
  
    part_percentage <- round((results_matrix[j, i] / column_sums[i]) * 100, 1) 
    
  
    label_y_position <- if (j == 1) {
      results_matrix[j, i] / 2
    } else {
      cum_heights[j - 1, i] + results_matrix[j, i] / 2
    }
 
    text(bar_positions[i], label_y_position, labels = paste(part_percentage, "%", sep=""), cex = 0.9)
  }
}
##############################toss and winner
########################################################################################################################
########################################################################################################################
###################################################################
Total_game = table(bbl_data$team1)+table(bbl_data$team2)
table_data = table(bbl_data$toss_winner,bbl_data$winner)
table_toss = table(bbl_data$toss_winner)

# Create an empty data frame with 2 rows and 8 columns
winner_toss = data.frame(matrix(ncol = 8, nrow = 2))

# Set row names and column names
row.names(winner_toss) = c( "toss_loser","toss_winner")
names(winner_toss) = c("Adelaide Strikers","Brisbane Heat ","Hobart Hurricanes",
                       "Melbourne Renegades","Melbourne Stars","Perth Scorchers",
                       "Sydney Sixers","Sydney Thunder")
toss_winner = diag(table_data[,1:8])/table_toss
toss_loser = (colSums(table_data[1:8,1:8])-diag(table_data[,1:8]))/(Total_game-table_toss)
#############
total_toss_winner = sum(diag(table_data[,1:8]))

total_toss_loser = sum(colSums(table_data[1:8,1:8])-diag(table_data[,1:8]))

midpoints = barplot(c(total_toss_loser,total_toss_winner), 
        beside = FALSE, 
        col = c("lightgreen", "lightblue"),
        #names.arg = team_list,
        names.arg = c("Toss loser","Toss winner"),
        main = "Coin Toss Winner vs. Match Winner",
        xlab = "result of Toss", 
        ylab = "Number of Wins",
        ylim = c(0,310),

)
text(x = midpoints, y = c(total_toss_loser,total_toss_winner) + 5, 
     labels = c(total_toss_loser,total_toss_winner))
########################################################################################################################

winner_toss[1,] = toss_loser
winner_toss[2,] = toss_winner

bar_positions = barplot(as.matrix(winner_toss), 
                        beside = TRUE, 
                        col = c("lightgreen","lightblue"),
                        #names.arg = team_list,
                        names.arg = c("Adelaide\nStrikers","Brisbane\nHeat ","Hobart\nHurricanes",
                                      "Melbourne\nRenegades","Melbourne\nStars","Perth\nScorchers",
                                      "Sydney\nSixers","Sydney\nThunder"),
                        #names.arg = c("Adelaide\na","Brisbane","Hobart","Renegades","Stars","Perth","Sixers","Thunder"),
                        legend = c("Toss Losses","Toss Wins"),
                        main = "Impact of Toss Winner on Win Rate by Team",
                        xlab = "Team", 
                        ylab = "Win Rate",
                        args.legend = list(x = "bottomright", inset = c(0, 0.012), cex = 0.8),
                        cex.names = 0.85
)

##############################first innings and winner
########################################################################################################################
########################################################################################################################
###################################################################
Total_game = table(bbl_data$team1)+table(bbl_data$team2)
table_data = table(bbl_data$first_innings,bbl_data$winner)
table_first_innings = table(bbl_data$first_innings)

# Create an empty data frame with 2 rows and 8 columns
winner_first_innings = data.frame(matrix(ncol = 8, nrow = 2))

# Set row names and column names
row.names(winner_first_innings) = c( "first innings","second innings")
names(winner_first_innings) = c("Adelaide Strikers","Brisbane Heat ","Hobart Hurricanes",
                       "Melbourne Renegades","Melbourne Stars","Perth Scorchers",
                       "Sydney Sixers","Sydney Thunder")
first_innings_winner = diag(table_data[,1:8])/table_first_innings
second_innings_winner = (colSums(table_data[1:8,1:8])-diag(table_data[,1:8]))/(Total_game-table_first_innings)
#############
total_first_innings_winner = sum(diag(table_data[,1:8]))

total_second_innings_winner = sum(colSums(table_data[1:8,1:8])-diag(table_data[,1:8]))

midpoints = barplot(c(total_first_innings_winner,total_second_innings_winner), 
                    beside = FALSE, 
                    col = c("lightgreen", "lightblue"),
                    #names.arg = team_list,
                    names.arg = c("first innings","second innings"),
                    main = "innings order vs. Match Winner",
                    xlab = "innings order", 
                    ylab = "Number of Wins",
                    ylim = c(0,310),
                    
)
text(x = midpoints, y = c(total_first_innings_winner,total_second_innings_winner) + 5, 
     labels = c(total_first_innings_winner,total_second_innings_winner))
########################################################################################################################

winner_first_innings[1,] = first_innings_winner
winner_first_innings[2,] = second_innings_winner

bar_positions = barplot(as.matrix(winner_first_innings), 
                        beside = TRUE, 
                        col = c("lightgreen","lightblue"),
                        #names.arg = team_list,
                        names.arg = c("Adelaide\nStrikers","Brisbane\nHeat ","Hobart\nHurricanes",
                                      "Melbourne\nRenegades","Melbourne\nStars","Perth\nScorchers",
                                      "Sydney\nSixers","Sydney\nThunder"),
                        #names.arg = c("Adelaide\na","Brisbane","Hobart","Renegades","Stars","Perth","Sixers","Thunder"),
                        legend = c("first innings","second innings"),
                        main = "innings order vs. Match Winner",
                        xlab = "Team", 
                        ylab = "per of Wins",
                        args.legend = list(x = "bottomright", inset = c(0, 0.012), cex = 0.8),
                        cex.names = 0.85
)
##############################home advantage and winner
########################################################################################################################
########################################################################################################################
###################################################################
home_data = bbl_data[bbl_data$neutral_venue==0,c("team1","team2","neutral_venue","winner")]

winner_home = sum(as.character(home_data$team1) == as.character(home_data$winner))

winner_nohome = sum(as.character(home_data$team2) == as.character(home_data$winner))

midpoints = barplot(c(winner_home,winner_nohome), 
                    beside = FALSE, 
                    col = c("lightgreen", "lightblue"),
                    #names.arg = team_list,
                    names.arg = c("home","not home"),
                    main = "home vs. Match Winner",
                    xlab = "home", 
                    ylab = "Number of Wins",
                    ylim = c(0,280),
)
text(x = midpoints, y = c(winner_home,winner_nohome) + 5, 
     labels = c(winner_home,winner_nohome))
########################################################################################################################
########################################################################################################################
########################################################################################################
Total_game = table(bbl_data$team1)+table(bbl_data$team2)
table_data = table(home_data$team1,home_data$winner)
table_home = table(home_data$team1)

# Create an empty data frame with 2 rows and 8 columns
winner_home = data.frame(matrix(ncol = 8, nrow = 2))

# Set row names and column names
row.names(winner_home) = c( "home","not home")
names(winner_home) = c("Adelaide Strikers","Brisbane Heat ","Hobart Hurricanes",
                                "Melbourne Renegades","Melbourne Stars","Perth Scorchers",
                                "Sydney Sixers","Sydney Thunder")
home_winner = diag(table_data[,1:8])/table_home
nohome_winner = (colSums(table_data[1:8,1:8])-diag(table_data[,1:8]))/(Total_game-table_home)

winner_home[1,] = home_winner
winner_home[2,] = nohome_winner

bar_positions = barplot(as.matrix(winner_home), 
                        beside = TRUE, 
                        col = c("lightgreen","lightblue"),
                        #names.arg = team_list,
                        names.arg = c("Adelaide\nStrikers","Brisbane\nHeat ","Hobart\nHurricanes",
                                      "Melbourne\nRenegades","Melbourne\nStars","Perth\nScorchers",
                                      "Sydney\nSixers","Sydney\nThunder"),
                        #names.arg = c("Adelaide\na","Brisbane","Hobart","Renegades","Stars","Perth","Sixers","Thunder"),
                        legend = c("home","not home"),
                        main = "Impact of Home Advantage on Win Rate by Team",
                        xlab = "Team", 
                        ylab = "Win Rate",
                        args.legend = list(x = "bottomright", inset = c(0, 0.012), cex = 0.8),
                        cex.names = 0.85
)
##############################season and winner
########################################################################################################################
########################################################################################################################
###################################################################

seasons = unique(bbl_data$season)
teams = unique(bbl_data$team1)
#teams=teams[c(1,4,7,8)]
win_rate_data = data.frame(matrix(nrow = length(teams), ncol = length(seasons)))
#win_rate_data=win_rate_data[c(1,4,7,8),]
# Set row names and column names
row.names(win_rate_data) = teams

names(win_rate_data) = c("2011/12","2012/13","2013/14","2014/15","2015/16","2016/17","2017/18",
                       "2018/19","2019/20","2020/21","2021/22","2022/23","2023/24")

for (i in 1:length(teams)) {
  for (j in 1:length(seasons)) {
    subset_data = subset(bbl_data, bbl_data$season == seasons[j] & ((bbl_data$team1 == teams[i]) |(bbl_data$team2 == teams[i])))
    total_games = nrow(subset_data)
    wins = sum(as.character(subset_data$winner) == teams[i])
    win_rate = wins / total_games * 100
    win_rate_data[i,j] = win_rate
  }
}

seasons = colnames(win_rate_data)
teams = rownames(win_rate_data)

plot(1:length(seasons), win_rate_data[1, ],lwd=2, type = 'l', ylim = c(0, 110), 
     xlab = "Season", ylab = "Win Rate (%)", xaxt = "n", main = "Team Win Rates Over Seasons")
axis(1, at = 1:length(seasons), labels = seasons)

colors = rainbow(nrow(win_rate_data))
for (i in seq_len(nrow(win_rate_data))) {
  lines(1:length(seasons), win_rate_data[i, ],lwd=2, col = colors[i], type = "o", pch = i)
}
abline(h = 50, , lwd = 2) 

legend("topright", legend = teams, col = colors, lwd=2,lty = 1, pch = 1:19,cex=1,ncol=2,text.width = 2.5)

###############################################################################################
###########################day/night winner
##########################################
day_night = ifelse(bbl_data$day_night_game==0 & bbl_data$night_game== 0, "day",
                     ifelse(bbl_data$day_night_game==1 & bbl_data$night_game== 0, "day_night",
                            ifelse(bbl_data$day_night_game==0 & bbl_data$night_game== 1, "night", NA)))
# Create one data frame corresponding to team
team_daynight_data = cbind(bbl_data[,c("team1","team2","winner")],day_night)

# Create an empty data frame with 2 rows and 8 columns
winner_daynight = data.frame(matrix(ncol = 8, nrow = 3))

# Set row names and column names
row.names(winner_daynight) = c( "day","day_night","night")
names(winner_daynight) = c("Adelaide Strikers","Brisbane Heat","Hobart Hurricanes",
                       "Melbourne Renegades","Melbourne Stars","Perth Scorchers",
                       "Sydney Sixers","Sydney Thunder")
unique_day =  c("day","day_night","night")
teams = c("Adelaide Strikers","Brisbane Heat","Hobart Hurricanes",
          "Melbourne Renegades","Melbourne Stars","Perth Scorchers",
          "Sydney Sixers","Sydney Thunder")
for(i in 1:3){
  for(j in 1:8){
    subset_data = subset(team_daynight_data, team_daynight_data$day_night == unique_day[i]
                         & ((team_daynight_data$team1 == teams[j]) |(team_daynight_data$team2 == teams[j])))
    total_games = nrow(subset_data)
    wins = sum(as.character(subset_data$winner) == teams[j])
    win_rate = wins / total_games * 100
    winner_daynight[i,j] = win_rate}
}
bar_positions = barplot(as.matrix(winner_daynight), 
                        beside = TRUE, 
                        col = c("lightgreen","pink","lightblue"),
                        #names.arg = team_list,
                        names.arg = c("Adelaide\nStrikers","Brisbane\nHeat ","Hobart\nHurricanes",
                                      "Melbourne\nRenegades","Melbourne\nStars","Perth\nScorchers",
                                      "Sydney\nSixers","Sydney\nThunder"),
                        #names.arg = c("Adelaide\na","Brisbane","Hobart","Renegades","Stars","Perth","Sixers","Thunder"),
                        legend = c("day","day night","night"),
                        main = "day/night vs. Match Winner",
                        xlab = "Team", 
                        ylab = "per of Wins(%)",
                        args.legend = list(x = "bottomright", inset = c(0, 0.012), cex = 0.8),
                        cex.names = 0.85
)


chisq.test(table(bbl_data$winner, bbl_data$toss_winner)[1:8,1:8])
chisq.test(table(bbl_data$winner, bbl_data$first_innings)[1:8,1:8])
chisq.test(table(bbl_data$winner, bbl_data$ground)[-9,])
chisq.test(table(bbl_data$winner, bbl_data$season)[-9,])
chisq.test(table(team_daynight_data$winner, team_daynight_data$day_night)[-9,])


result = relief(formula = bbl_data$winner ~ ., data = bbl_data, neighbours.count = 5)
print(result)

##############################perth win rate

perth_data <- subset(bbl_data, team1 == "Perth Scorchers" | team2 == "Perth Scorchers")


perth_data$home_away <- ifelse(perth_data$team1 == "Perth Scorchers" & perth_data$neutral_venue == 0, "Home", 
                               ifelse(perth_data$team2 == "Perth Scorchers" & perth_data$neutral_venue == 0, "Away", "neutral"))


perth_data <- subset(perth_data, home_away != "neutral")


perth_data$perth_win <- ifelse((perth_data$team1 == "Perth Scorchers" & perth_data$winner == "Perth Scorchers") |
                                 (perth_data$team2 == "Perth Scorchers" & perth_data$winner == "Perth Scorchers"), 1, 0)


perth_data$season <- gsub("T20 ", "", perth_data$season)
win_rates <- aggregate(perth_win ~ season + home_away, data = perth_data, FUN = mean)

#print
print(win_rates)
library(ggplot2)

ggplot(win_rates, aes(x = season, y = perth_win, color = home_away, group = home_away)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Perth Scorchers Home and Away Win Rates by Season",
       x = "Season",
       y = "Win Rate",
       color = "Venue") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
###########################################################home advantage

bbl_data <- subset(bbl_data, neutral_venue == 0)


calculate_overall_win_rates <- function(data) {
  
  home_win_rate <- aggregate(winner == team1 ~ team1, data = data, mean)
  colnames(home_win_rate) <- c("Team", "Home_Win_Rate")
  
  
  away_win_rate <- aggregate(winner == team2 ~ team2, data = data, mean)
  colnames(away_win_rate) <- c("Team", "Away_Win_Rate")
  
  
  win_rates <- merge(home_win_rate, away_win_rate, by = "Team")
  
  return(win_rates)
}


team_win_rates <- calculate_overall_win_rates(bbl_data)


print(team_win_rates)


library(ggplot2)
library(reshape2)


win_rates_melt <- melt(team_win_rates, id.vars = "Team", variable.name = "Venue", value.name = "Win_Rate")






labels <- c("Adelaide\nStrikers", "Brisbane\nHeat", "Hobart\nHurricanes", 
            "Melbourne\nRenegades", "Melbourne\nStars", "Perth\nScorchers", 
            "Sydney\nSixers", "Sydney\nThunder")

ggplot(win_rates_melt, aes(x = Team, y = Win_Rate, fill = Venue)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +  
  labs(title = "Home and Away Win Rates by Team",
       x = "Team",
       y = "Win Rate",
       fill = "Venue") + 
  scale_fill_manual(values = c("Home_Win_Rate" = "lightgreen", "Away_Win_Rate" = "lightblue"),
                    labels = c("Home_Win_Rate" = "Home", "Away_Win_Rate" = "Away")) +  
  scale_x_discrete(labels = labels) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),  
        plot.title = element_text(hjust = 0.5)) 


