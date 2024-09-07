# Read data with appropriate names
bbl_odds = read.csv("bbl_odds.csv", header=TRUE, sep = '')
bbl_data = read.csv("bbl_dataset.csv", header=TRUE)



############################################################################
############################################################################
# Splitting the data into two parts
n = length(bbl_data$winner)
training_data = bbl_data[1:(0.7*n),] #data for predicting
testing_data = bbl_data[-(1:(0.7*n)),] #data for training and validation

# number of rows and columns
num_of_row = nrow(bbl_data)
num_of_col = ncol(bbl_data)

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

# team_list and season list
team_list = unique(bbl_data$team1)
season_list = unique(bbl_data$season)

# View the structure of the bbl_data
str(bbl_data)

# Get a summary of the bbl_data
summary(bbl_data)

############################################################################
######################missing values################################
############################################################################
# find missing value in each variable
colSums(is.na(bbl_data))

# find missing positions of row in "winner" variable
missing_winner_positions = which(is.na(bbl_data$winner))
bbl_data[missing_winner_positions,]

# omit the case which winner is NA or Tie
bbl_data = subset(bbl_data, !is.na(bbl_data$winner) & bbl_data$winner != "Tie")

#################################################################EDA#################################################################
##################################################################################################################################
##################################################################################################################################
Total_game = table(bbl_data$team1)+table(bbl_data$team2)
bbl_data$winner = as.character(bbl_data$winner)
win_counts = table(bbl_data$winner)
lost_counts = Total_game - win_counts

results_matrix = rbind(win_counts, lost_counts)


win_per = win_counts/Total_game
lost_per = lost_counts/Total_game

bar_positions = barplot(results_matrix, 
                        beside = FALSE, 
                        col = c( "pink", "lightblue"), 
                        #names.arg = team_list,
                        names.arg = c("Adelaide\nStrikers","Brisbane\nHeat ","Hobart\nHurricanes",
                                      "Melbourne\nRenegades","Melbourne\nStars","Perth\nScorchers",
                                      "Sydney\nSixers","Sydney\nThunder"),
                        #names.arg = c("Adelaide\na","Brisbane","Hobart","Renegades","Stars","Perth","Sixers","Thunder"),
                        legend = c("Wins", "Losses" ),
                        main = "Team Results", 
                        xlab = "Team", 
                        ylab = "Number of Results",
                        args.legend = list(x = "bottomright", inset = c(0, 0.012), cex = 0.8),
                        cex.names = 0.85)
bardata_matrix = rbind(win_per, lost_per)
column_sums = colSums(results_matrix)

# add percent percentage
cum_heights = apply(results_matrix, 2, cumsum)  # Cumulative height

# 循环每个分类和组成部分，添加标签
for (i in 1:ncol(results_matrix)) {
  for (j in 1:nrow(results_matrix)) {
    # 计算每个部分的百分比
    part_percentage = round((results_matrix[j, i] / column_sums[i]) * 100, 1)  # 百分比，保留一位小数
    
    # 计算每个标签的具体位置
    label_y_position = if (j == 1) {
      results_matrix[j, i] / 2
    } else {
      cum_heights[j - 1, i] + results_matrix[j, i] / 2
    }
    # add lable
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
                    col = c("pink", "lightblue"),
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
                        col = c("pink","lightblue"),
                        #names.arg = team_list,
                        names.arg = c("Adelaide\nStrikers","Brisbane\nHeat ","Hobart\nHurricanes",
                                      "Melbourne\nRenegades","Melbourne\nStars","Perth\nScorchers",
                                      "Sydney\nSixers","Sydney\nThunder"),
                        #names.arg = c("Adelaide\na","Brisbane","Hobart","Renegades","Stars","Perth","Sixers","Thunder"),
                        legend = c("Toss Losses","Toss Wins"),
                        main = "Coin Toss Winner vs. Match Winner",
                        xlab = "Team", 
                        ylab = "per of Wins",
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
                    col = c("pink", "lightblue"),
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
                        col = c("pink","lightblue"),
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
                    col = c("pink", "lightblue"),
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
                        main = "home vs. Match Winner",
                        xlab = "Team", 
                        ylab = "per of Wins",
                        args.legend = list(x = "bottomright", inset = c(0, 0.012), cex = 0.8),
                        cex.names = 0.85
)
##############################season and winner
########################################################################################################################
########################################################################################################################
###################################################################
# 获取所有独特的赛季和队伍组合
seasons = unique(bbl_data$season)
teams = unique(bbl_data$team1)

win_rate_data = data.frame(matrix(nrow = length(teams), ncol = length(seasons)))

# Set row names and column names
row.names(win_rate_data) = teams

names(win_rate_data) = c("2011/12","2012/13","2013/14","2014/15","2015/16","2016/17","2017/18",
                         "2018/19","2019/20","2020/21","2021/22","2022/23","2023/24")
# 计算每个赛季每个队伍的胜率
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

# 设置绘图区域
plot(1:length(seasons), win_rate_data[1, ], type = 'l', ylim = c(0, 110), 
     xlab = "Season", ylab = "Win Rate (%)", xaxt = "n", main = "Team Win Rates Over Seasons")
axis(1, at = 1:length(seasons), labels = seasons)

# 为每个队伍添加折线
colors = rainbow(nrow(win_rate_data))
for (i in seq_len(nrow(win_rate_data))) {
  lines(1:length(seasons), win_rate_data[i, ], col = colors[i], type = "o", pch = i)
}

# 添加图例
legend("topright", legend = teams, col = colors, lty = 1, pch = 1:19,cex=0.8,ncol=2,text.width = 2)
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




bbl_data=bbl_data %>%
  filter(bbl_data$home == 1)


chi_test_result <- chisq.test(table(a$day_night, a$new_winner))
print(chi_test_result)
library(vcd)
# 使用Cramér's V
cramers_v_result <- assocstats(table(a$new_first_innings, a$new_winner))
print(cramers_v_result)
cramers_v_matrix <- matrix(c(
  1.00, 0.167, 0.262, 0.024, 0.155,
  0.167, 1.00, 0.126, 0.061, 0.045,
  0.262, 0.126, 1.00, 0.12, 0.067,
  0.024, 0.061, 0.12, 1.00, 0.012,
  0.155, 0.045, 0.022, 0.012, 1.00
), nrow = 5, byrow = TRUE)

# 设置矩阵行和列名称
rownames(cramers_v_matrix) <- colnames(cramers_v_matrix) <- c("home advantage", "match start time", "toss winner", "first innings team", "match winner")

# 转换矩阵为数据框用于 ggplot2
cramers_v_df <- melt(cramers_v_matrix, varnames = c("Variable1", "Variable2"), value.name = "CramersV")

# 绘制热图
ggplot(cramers_v_df, aes(Variable1, Variable2, fill = CramersV)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Cramér's V") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Heatmap of Cramér's V between Categorical Variables",
       x = "Variable 1",
       y = "Variable 2")