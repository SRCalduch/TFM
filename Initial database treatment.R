# DATABASE TREATMENT

library(readr)

# Import data base
ATP_doubles_full_ord_ok <- read_csv("TFM/ATP_doubles_full_ord_ok.csv")

# Ajust of the column l_bpFaced
ATP_doubles_full_ord_ok <- ATP_doubles_full_ord_ok[,-1]
ATP_doubles_full_ord_ok$l_bpFaced <- gsub(",", "", ATP_doubles_full_ord_ok$l_bpFaced)
ATP_doubles_full_ord_ok$l_bpFaced <- ifelse(ATP_doubles_full_ord_ok$l_bpFaced == "", NA, ATP_doubles_full_ord_ok$l_bpFaced)
ATP_doubles_full_ord_ok$l_bpFaced <- as.numeric(ATP_doubles_full_ord_ok$l_bpFaced)

# The 'Retirement' column, which is the target variable, is created from the Score column
library(dplyr)
library(stringr)
ATP_doubles_full_ord_ok <- mutate(
  ATP_doubles_full_ord_ok, Retirement=case_when(
    str_detect(ATP_doubles_full_ord_ok$score, "RET") ~ 'YES',
    TRUE ~ 'NO')
)

# The 'Tourney level' column is improved
table(ATP_doubles_full_ord_ok$tourney_level)
ATP_doubles_full_ord_ok <- ATP_doubles_full_ord_ok[ATP_doubles_full_ord_ok$tourney_level!="D",] #Copa Davis matches are removed
table(ATP_doubles_full_ord_ok$tourney_level)

ATP_doubles_full_ord_ok$tourney_level <- replace(ATP_doubles_full_ord_ok$tourney_level,ATP_doubles_full_ord_ok$tourney_level=="A","Other levels")
ATP_doubles_full_ord_ok$tourney_level <- replace(ATP_doubles_full_ord_ok$tourney_level,ATP_doubles_full_ord_ok$tourney_level=="F","Tour Finals")
ATP_doubles_full_ord_ok$tourney_level <- replace(ATP_doubles_full_ord_ok$tourney_level,ATP_doubles_full_ord_ok$tourney_level=="G","Grand Slam")
ATP_doubles_full_ord_ok$tourney_level <- replace(ATP_doubles_full_ord_ok$tourney_level,ATP_doubles_full_ord_ok$tourney_level=="M","Masters 1000")
ATP_doubles_full_ord_ok$tourney_level <- ifelse(ATP_doubles_full_ord_ok$tourney_level == 'Tour Finals','Other levels',ATP_doubles_full_ord_ok$tourney_level)


table(ATP_doubles_full_ord_ok$tourney_level)

filter_data <- function(data){
  data <- data %>%
    filter(!(ATP_doubles_full_ord_ok$round == "RR" & ATP_doubles_full_ord_ok$tourney_name == "Dusseldorf"))
  return(data)
}
ATP_doubles_full_ord_ok <- filter_data(ATP_doubles_full_ord_ok) #ATP World Team Cup in Dusseldorf are removed

#A new variable with the mean of the age of both players for winners and losers is created
ATP_doubles_full_ord_ok$winner_age_mean <- ((ATP_doubles_full_ord_ok$winner1_age + ATP_doubles_full_ord_ok$winner2_age)/2)
ATP_doubles_full_ord_ok$loser_age_mean <- ((ATP_doubles_full_ord_ok$loser1_age + ATP_doubles_full_ord_ok$loser2_age)/2)

#A new variable with the mean of the height of both players for winners and losers is created
ATP_doubles_full_ord_ok$winner_ht_mean <- ((ATP_doubles_full_ord_ok$winner1_ht + ATP_doubles_full_ord_ok$winner2_ht)/2)
ATP_doubles_full_ord_ok$loser_ht_mean <- ((ATP_doubles_full_ord_ok$loser1_ht + ATP_doubles_full_ord_ok$loser2_ht)/2)

#A new variable with the mean of the rank of both players for winners and losers is created
ATP_doubles_full_ord_ok$winner_team_rank <- (ATP_doubles_full_ord_ok$winner1_rank + ATP_doubles_full_ord_ok$winner2_rank)/2
ATP_doubles_full_ord_ok$loser_team_rank <- (ATP_doubles_full_ord_ok$loser1_rank + ATP_doubles_full_ord_ok$loser2_rank)/2

# ID of the team
ATP_doubles_full_ord_ok$winner_team_id <- ATP_doubles_full_ord_ok$winner1_id * ATP_doubles_full_ord_ok$winner2_id
ATP_doubles_full_ord_ok$loser_team_id <- ATP_doubles_full_ord_ok$loser1_id * ATP_doubles_full_ord_ok$loser2_id


#A new variable with the dominant hand of both players for winners and losers is created
# If one player is Unknown (U), is considered UU
ATP_doubles_full_ord_ok$winner_team_hand <- paste(ATP_doubles_full_ord_ok$winner1_hand,ATP_doubles_full_ord_ok$winner2_hand,sep = "")
ATP_doubles_full_ord_ok$winner_team_hand <- ifelse(ATP_doubles_full_ord_ok$winner_team_hand == 'RL' , 'LR' , ATP_doubles_full_ord_ok$winner_team_hand)
ATP_doubles_full_ord_ok$winner_team_hand <- ifelse(ATP_doubles_full_ord_ok$winner_team_hand == 'LU' , 'UU' , ATP_doubles_full_ord_ok$winner_team_hand)
ATP_doubles_full_ord_ok$winner_team_hand <- ifelse(ATP_doubles_full_ord_ok$winner_team_hand == 'UL' , 'UU' , ATP_doubles_full_ord_ok$winner_team_hand)
ATP_doubles_full_ord_ok$winner_team_hand <- ifelse(ATP_doubles_full_ord_ok$winner_team_hand == 'RU' , 'UU' , ATP_doubles_full_ord_ok$winner_team_hand)
ATP_doubles_full_ord_ok$winner_team_hand <- ifelse(ATP_doubles_full_ord_ok$winner_team_hand == 'UR' , 'UU' , ATP_doubles_full_ord_ok$winner_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- paste(ATP_doubles_full_ord_ok$loser1_hand, ATP_doubles_full_ord_ok$loser2_hand, sep = "")
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'RL' , 'LR' , ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'LU' , 'UU' , ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'UL' , 'UU' , ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'RU' , 'UU' , ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'UR' , 'UU' , ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'NAR' , NA , ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'RNA' , NA , ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok$loser_team_hand <- ifelse(ATP_doubles_full_ord_ok$loser_team_hand == 'NANA' , NA , ATP_doubles_full_ord_ok$loser_team_hand)

# The entry variable is filtered and organized 
ATP_doubles_full_ord_ok$winner_entry <- ifelse(ATP_doubles_full_ord_ok$winner_entry == '1 AL' , 'AL' , ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok$winner_entry <- ifelse(ATP_doubles_full_ord_ok$winner_entry == '2 AL' , 'AL' , ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok$winner_entry <- ifelse(ATP_doubles_full_ord_ok$winner_entry == '10 WC' , 'WC' , ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok$winner_entry <- ifelse(ATP_doubles_full_ord_ok$winner_entry == '2 PR' , 'PR' , ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok$winner_entry <- ifelse(ATP_doubles_full_ord_ok$winner_entry == '4 Q' , 'Q' , ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok$winner_entry <- ifelse(ATP_doubles_full_ord_ok$winner_entry == '4 WC' , 'WC' , ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok$winner_entry <- ifelse(ATP_doubles_full_ord_ok$winner_entry == 'Alt' , 'AL' , ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '1 AL' , 'AL' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '1 WC' , 'WC' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '10 WC' , 'WC' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '11 WC' , 'WC' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '2 WC' , 'WC' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '2 AL' , 'AL' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '3 AL' , 'AL' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '4 AL' , 'AL' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == '4 Q' , 'Q' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == 'Alt' , 'AL' , ATP_doubles_full_ord_ok$loser_entry)
ATP_doubles_full_ord_ok$loser_entry <- ifelse(ATP_doubles_full_ord_ok$loser_entry == 'I' , 'WC' , ATP_doubles_full_ord_ok$loser_entry)


# The rounds are grouped in three different groups: Qualifying, Final and First rounds
ATP_doubles_full_ord_ok$round_group <- ATP_doubles_full_ord_ok$round
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'BR','Final',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'R128','First',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'Q2','Qualifying',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'Q1','Qualifying',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'RR','Final',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'F','Final',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'SF','Final',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'R64','First',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'R32','First',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'QF','Final',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'R16','First',ATP_doubles_full_ord_ok$round_group)

# A new variable of group with Final or no Final rounds
ATP_doubles_full_ord_ok$round_group2 <- ATP_doubles_full_ord_ok$round_group
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'First','No Final',ATP_doubles_full_ord_ok$round_group)
ATP_doubles_full_ord_ok$round_group <- ifelse(ATP_doubles_full_ord_ok$round_group == 'Qualifying','No Final',ATP_doubles_full_ord_ok$round_group)


# A new variable with the total numbre of games of the match is created
ATP_doubles_full_ord_ok$score_noRET <- ATP_doubles_full_ord_ok$score
ATP_doubles_full_ord_ok$score_noRET <- gsub("\\(.*?\\)", "", ATP_doubles_full_ord_ok$score_noRET)
ATP_doubles_full_ord_ok$score_noRET <- gsub(" RET", "", ATP_doubles_full_ord_ok$score_noRET)

ATP_doubles_full_ord_ok$Total_Games <- sapply(strsplit(ATP_doubles_full_ord_ok$score_noRET, " "), function(x) {
  set_scores <- strsplit(x, "-")
  total_games <- sum(as.integer(set_scores[[1]]))
  if (length(set_scores) > 1) {
    total_games <- total_games + sum(as.integer(set_scores[[2]]))
  }
  if (length(set_scores) > 2) {
    total_games <- total_games + sum(as.integer(set_scores[[3]]))
  }
  if (length(set_scores) > 3) {
    total_games <- total_games + sum(as.integer(set_scores[[4]]))
  }
  if (length(set_scores) > 4) {
    total_games <- total_games + sum(as.integer(set_scores[[5]]))
  }
  total_games
})


# MISSING DATA

library(naniar)
gg_miss_var(ATP_doubles_full_ord_ok[c(1:26)]) + labs(y = "Missings")
gg_miss_var(ATP_doubles_full_ord_ok[c(27:52)]) + labs(y = "Missings")
gg_miss_var(ATP_doubles_full_ord_ok[c(53:80)]) + labs(y = "Missings")

gg_miss_fct(ATP_doubles_full_ord_ok[c(1:40)], year) + labs(x = "Year") + labs(y = "Variable")
ATP_doubles_full_ord_ok$year2 <- ATP_doubles_full_ord_ok$year
gg_miss_fct(ATP_doubles_full_ord_ok[c(41:81)], year2) + labs(x = "Year") + labs(y = "Variable")

DoublesATP <- ATP_doubles_full_ord_ok[, c("year","tourney_level","surface","round_group","minutes","Retirement","winner_team_hand","loser_team_hand","winner_age_mean","loser_age_mean","winner_ht_mean","loser_ht_mean","Total_Games")] 

write.csv(DoublesATP, file = "/Users/USER/Desktop/TFM/TFM/DoublesATP.csv")

# Graphic number of different pairs per year

ATP_doubles_full_ord_ok$winners <- paste(ATP_doubles_full_ord_ok$winner1_name, ATP_doubles_full_ord_ok$winner2_name, sep = " y ")
ATP_doubles_full_ord_ok$losers <- paste(ATP_doubles_full_ord_ok$loser1_name, ATP_doubles_full_ord_ok$loser2_name, sep = " y ")
df_winners <- ATP_doubles_full_ord_ok[, c("year","winners")]
df_losers <- ATP_doubles_full_ord_ok[, c("year","losers")]
colnames(df_winners) <- c("year", "team")
colnames(df_losers) <- c("year", "team")
df_win_los <- rbind(df_winners, df_losers)
df_win_los_uniq <- unique(df_win_los[, c("year", "team")])


team_year <- table(df_win_los_uniq$year)
team_year

# Crear el gráfico de barras
barplot(team_year, main = "Number of couples per year", xlab = "Year", ylab = "Numner of couples")
