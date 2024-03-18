# 1. DATABASE TREATMENT

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

# Name of both winners and both losers
ATP_doubles_full_ord_ok$winners <- paste(ATP_doubles_full_ord_ok$winner1_name, ATP_doubles_full_ord_ok$winner2_name, sep = " y ")
ATP_doubles_full_ord_ok$losers <- paste(ATP_doubles_full_ord_ok$loser1_name, ATP_doubles_full_ord_ok$loser2_name, sep = " y ")


DoublesATP <- ATP_doubles_full_ord_ok[, c("year","tourney_level","surface","round_group","Retirement","winner_team_hand","loser_team_hand","winner_age_mean","loser_age_mean","Total_Games")] 


# MISSING DATA

library(visdat)
library(naniar)
vis_miss(DoublesATP,sort_miss = TRUE) 
gg_miss_fct(DoublesATP,year)

# 2. EDA UNIVARIATE

### QUANTITATIVE VARIABLES
Quantitative_var <- ATP_doubles_full_ord_ok[, c("winner1_ht","winner1_age","winner2_ht","winner2_age","winner_age_mean","loser_age_mean","winner_ht_mean","loser_ht_mean","loser1_ht","loser1_age","loser2_ht","loser2_age","winner_team_rank","loser_team_rank","winner1_rank","winner1_rank_points","winner2_rank","winner2_rank_points","loser1_rank","loser1_rank_points","loser2_rank","loser2_rank_points","minutes","draw_size")] 

library(skimr)
skim(Quantitative_var)


The variables are graphically represented below in order to provide a more detailed analysis.

library(ggplot2)
library(brinton)
Quantitative_var <- as.data.frame(Quantitative_var)
gridExtra::grid.arrange(plotup(Quantitative_var, "minutes", "color histogram", output = 'console'))


# Minutes
plot_minutes <- ggplot(Quantitative_var, aes(x=minutes, fill=..count..)) +
  geom_histogram(bins = 15) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='Minutes') +
  ylab("Number of Observations") +
  ggtitle("Minutes of the match") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

plot_minutes


# Height

par(mfrow = c(2, 2))
# Winner 1
hist(Quantitative_var$winner1_ht, probability = TRUE, ylab = "Frequency", xlab = "Height", col = "grey",
     axes = FALSE, main = "Height Winner 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner1_ht, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Winner 2
hist(Quantitative_var$winner2_ht, probability = TRUE, ylab = "", xlab = "Height", col = "grey",
     axes = FALSE, main = "Height Winner 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner2_ht, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 1
Quantitative_var$loser1_ht <- ifelse(Quantitative_var$loser1_ht < 150, NA, Quantitative_var$loser1_ht)

hist(Quantitative_var$loser1_ht, probability = TRUE, ylab = "Frequency", xlab = "Height", col = "grey",
     axes = FALSE, main = "Height Loser 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser1_ht, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 2
hist(Quantitative_var$loser2_ht, probability = TRUE, ylab = "", xlab = "Height", col = "grey",
     axes = FALSE, main = "Height Loser 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser2_ht, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))


# Height Mean

#Plot Mean of the height of the winners
plot_winner_ht_mean <- ggplot(Quantitative_var, aes(x=winner_ht_mean, fill=..count..)) +
  geom_histogram(bins = 15) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='Height mean (cms)') +
  ylab("Number of Observations") +
  ggtitle("Winners Height") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

#Plot Mean of the height of the losers
plot_loser_ht_mean <- ggplot(Quantitative_var, aes(x=loser_ht_mean, fill=..count..)) +
  geom_histogram(bins = 15) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='Height mean (cms)') +
  ylab("Number of Observations") +
  ggtitle("Losers Height") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

require(gridExtra)
grid.arrange(plot_winner_ht_mean, plot_loser_ht_mean, ncol=2)


#Age

par(mfrow = c(2, 2))
# Winner 1
hist(Quantitative_var$winner1_age, probability = TRUE, ylab = "Frequency", xlab = "Age", col = "grey",
     axes = FALSE, main = "Winner 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner1_age, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Winner 2
hist(Quantitative_var$winner2_age, probability = TRUE, ylab = "", xlab = "Age", col = "grey",
     axes = FALSE, main = "Winner 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner2_age, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 1
hist(Quantitative_var$loser1_age, probability = TRUE, ylab = "Frequency", xlab = "Age", col = "grey",
     axes = FALSE, main = "Loser 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser1_age, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 2
hist(Quantitative_var$loser2_age, probability = TRUE, ylab = "", xlab = "Age", col = "grey",
     axes = FALSE, main = "Loser 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser2_age, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))


# Age sum

#Plot Mean of the age of the winners
plot_winner_age_mean <- ggplot(Quantitative_var, aes(x=winner_age_mean, fill=..count..)) +
  geom_histogram(bins = 15) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='Years mean') +
  ylab("Number of Observations") +
  ggtitle("Winners Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

#Plot Mean of the age of the losers
plot_loser_age_mean <- ggplot(Quantitative_var, aes(x=loser_age_mean, fill=..count..)) +
  geom_histogram(bins = 15) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='Years mean') +
  ylab("Number of Observations") +
  ggtitle("Losers Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

require(gridExtra)
grid.arrange(plot_winner_age_mean, plot_loser_age_mean, ncol=2)


# RANK

par(mfrow = c(2, 2))
# Winner 1
hist(Quantitative_var$winner1_rank, probability = TRUE, ylab = "Frequency", xlab = "Rank", col = "grey",
     axes = FALSE, main = "Winner 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner1_rank, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Winner 2
hist(Quantitative_var$winner2_rank, probability = TRUE, ylab = "", xlab = "Rank", col = "grey",
     axes = FALSE, main = "Winner 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner2_rank, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 1
hist(Quantitative_var$loser1_rank, probability = TRUE, ylab = "Frequency", xlab = "Rank", col = "grey",
     axes = FALSE, main = "Loser 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser1_rank, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 2
hist(Quantitative_var$loser2_rank, probability = TRUE, ylab = "", xlab = "Rank", col = "grey",
     axes = FALSE, main = "Loser 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser2_rank, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))


# RANK MEAN

#Plot Mean of the rank of the winners
plot_winner_team_rank <- ggplot(Quantitative_var, aes(x=winner_team_rank, fill=..count..)) +
  geom_histogram(bins = 15) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='Rank mean') +
  ylab("Number of Observations") +
  ggtitle("Winners Rank") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

#Plot Mean of the rank of the losers
plot_loser_team_rank <- ggplot(Quantitative_var, aes(x=loser_team_rank, fill=..count..)) +
  geom_histogram(bins = 15) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='Rank mean') +
  ylab("Number of Observations") +
  ggtitle("Losers Rank") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

require(gridExtra)
grid.arrange(plot_winner_team_rank, plot_loser_team_rank, ncol=2)


# RANK POINTS

par(mfrow = c(2, 2))
# Winner 1
hist(Quantitative_var$winner1_rank_points, probability = TRUE, ylab = "Frequency", xlab = "Rank Points", col = "grey",
     axes = FALSE, main = "Winner 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner1_rank_points, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Winner 2
hist(Quantitative_var$winner2_rank_points, probability = TRUE, ylab = "", xlab = "Rank Points", col = "grey",
     axes = FALSE, main = "Winner 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$winner2_rank_points, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 1
hist(Quantitative_var$loser1_rank_points, probability = TRUE, ylab = "Frequency", xlab = "Rank Points", col = "grey",
     axes = FALSE, main = "Loser 1")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser1_rank_points, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
# Loser 2
hist(Quantitative_var$loser2_rank_points, probability = TRUE, ylab = "", xlab = "Rank Points", col = "grey",
     axes = FALSE, main = "Loser 2")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$loser2_rank_points, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))



# DRAW SIZE


par(mfrow = c(1, 1))
hist(Quantitative_var$draw_size, probability = TRUE, ylab = "Frequency", xlab = "Draw Size", col = "grey",
     axes = FALSE, main = "Draw Size")
axis(1)
par(new = TRUE)
boxplot(Quantitative_var$draw_size, horizontal = TRUE, axes = FALSE,
        lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))



# QUALITATIVE VARIABLES

# TOURNEY

library(descr)
library(scales)
library(ggplot2)
Tourney_table <- data.frame(freq((ATP_doubles_full_ord_ok$tourney_name), plot = FALSE))
knitr::kable(Tourney_table)

ATP_doubles_full_ord_ok[['tourney_name']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['tourney_name']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=tourney_name, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

### Surface

Surface_table <- data.frame(freq((ATP_doubles_full_ord_ok$surface), plot = FALSE))
knitr::kable(Surface_table)

ATP_doubles_full_ord_ok[['surface']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['surface']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=surface, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Surface') +
  ylab("Matches") +
  ggtitle("Surface Matches") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')


### Tournament Level

Tourney_level_table <- data.frame(freq((ATP_doubles_full_ord_ok$tourney_level), plot = FALSE))
knitr::kable(Tourney_level_table)

ATP_doubles_full_ord_ok[['tourney_level']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['tourney_level']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=tourney_level, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(5, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Levels') +
  ylab("Matches") +
  ggtitle("Tournament Level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

### ENTRY


winner_entry_table <- data.frame(freq((ATP_doubles_full_ord_ok$winner_entry), plot = FALSE))
knitr::kable(winner_entry_table)

loser_entry_table <- data.frame(freq((ATP_doubles_full_ord_ok$loser_entry), plot = FALSE))
knitr::kable(loser_entry_table)

ATP_doubles_full_ord_ok$winner_entry <-  as.character(ATP_doubles_full_ord_ok$winner_entry)
ATP_doubles_full_ord_ok[['winner_entry']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['winner_entry']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=winner_entry, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(5, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Entry') +
  ylab("Number of Observations") +
  ggtitle("Winner entry") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

ATP_doubles_full_ord_ok[['loser_entry']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['loser_entry']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=loser_entry, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(5, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Entry') +
  ylab("Number of Observations") +
  ggtitle("Loser entry") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')


### RETIREMENT


Retirement_table <- data.frame(freq((ATP_doubles_full_ord_ok$Retirement), plot = FALSE))
knitr::kable(Retirement_table)

ATP_doubles_full_ord_ok[['Retirement']] <- factor(ATP_doubles_full_ord_ok[['Retirement']], levels = unique(ATP_doubles_full_ord_ok[['Retirement']]))
ggplot(ATP_doubles_full_ord_ok, aes(x=Retirement, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  coord_flip() +
  ggtitle("Retirement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')


# ROUND


Round_table <- data.frame(freq((ATP_doubles_full_ord_ok$round), plot = FALSE))
knitr::kable(Round_table, )

ATP_doubles_full_ord_ok$round <-  as.character(ATP_doubles_full_ord_ok$round)
ATP_doubles_full_ord_ok[['round']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['round']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=round, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(5, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Round') +
  ylab("Number of Observations") +
  ggtitle("Rounds") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')


### Best of sets

BestofSets_table <- data.frame(freq((ATP_doubles_full_ord_ok$best_of), plot = FALSE))
knitr::kable(BestofSets_table)

ATP_doubles_full_ord_ok$best_of <-  as.character(ATP_doubles_full_ord_ok$best_of)
ATP_doubles_full_ord_ok[['best_of']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['best_of']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=best_of, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(5, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Set') +
  ylab("Number of Observations") +
  ggtitle("Best of Sets") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

### Dominant Hand

# Winner team dominant hand
ATP_doubles_full_ord_ok$winner_team_hand <-  as.character(ATP_doubles_full_ord_ok$winner_team_hand)
ATP_doubles_full_ord_ok[['winner_team_hand']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['winner_team_hand']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=winner_team_hand, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(5, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Dominant Hand') +
  ylab("Number of Observations") +
  ggtitle("Winner Team Dominant Hand") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')

# Loser team dominant hand
ATP_doubles_full_ord_ok$loser_team_hand <-  as.character(ATP_doubles_full_ord_ok$loser_team_hand)
ATP_doubles_full_ord_ok[['loser_team_hand']] <- forcats::fct_infreq(ATP_doubles_full_ord_ok[['loser_team_hand']], ordered = TRUE)
ggplot(ATP_doubles_full_ord_ok, aes(x=loser_team_hand, fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(5, 'Spectral')))(3)) +
  coord_flip() +
  labs(x='Dominant Hand') +
  ylab("Number of Observations") +
  ggtitle("Loser Team Dominant Hand") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(family='mono'))+
  theme(plot.title = element_text(face = 'bold'))+
  theme(plot.title = element_text(size = 16))+
  theme(panel.grid = element_line(colour = NA),
        axis.ticks = element_line(color = 'black'),
        legend.position='none')


# 3.EDA BIVARIATE

EDA_df <- ATP_doubles_full_ord_ok[, c("tourney_level","surface","round_group","minutes","Retirement","winner_team_hand","loser_team_hand","winner_age_mean","loser_age_mean","winner_ht_mean","loser_ht_mean")] 

# Creation of descriptive table
library(compareGroups)
export2word(tab <- descrTable(Retirement ~., EDA_df, show.all=TRUE, method=2, byrow = TRUE,hide.no="no", max.xlev = 30, simplify=TRUE,
                              show.ratio=TRUE, # OR and p-valor .
),file='eda_descriptive_tab2.docx',
header.labels = c("all"="All","p.overall"="p-overall","p.ratio"="p-value"), caption="Descriptives by retirement")


library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

# Retirement vs Winner age mean

EDA_df %>% 
  ggplot(aes(x = factor(EDA_df$Retirement), y = EDA_df$winner_age_mean, fill = factor(EDA_df$Retirement))) +
  
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25
  ) +
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Winner Age Mean vs Retirement",
    x = "Retirement",
    y = "Winner age mean",
    fill = "Retirement"
  ) +
  coord_flip()


# Retirement vs loser age mean

EDA_df %>% 
  ggplot(aes(x = factor(EDA_df$Retirement), y = EDA_df$loser_age_mean, fill = factor(EDA_df$Retirement))) +
  
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25
  ) +
  scale_fill_tq() +
  theme_tq() +
  labs(
    title = "Loser Age Mean vs Retirement",
    x = "Retirement",
    y = "Loser age mean",
    fill = "Retirement"
  ) +
  coord_flip()


# Retirement vs Winner height mean

EDA_df %>% 
  ggplot(aes(x = factor(EDA_df$Retirement), y = EDA_df$winner_ht_mean, fill = factor(EDA_df$Retirement))) +
  
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25
  ) +
  scale_fill_tq() +
  theme_tq() +  labs(
    title = "Winners Height Mean vs Retirement",
    x = "Retirement",
    y = "Winner height mean",
    fill = "Retirement"
  ) +
  coord_flip()


# Retirement vs loser height mean

EDA_df %>% 
  ggplot(aes(x = factor(EDA_df$Retirement), y = EDA_df$loser_ht_mean, fill = factor(EDA_df$Retirement))) +
  
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25
  ) +
  scale_fill_tq() +
  theme_tq() +  labs(
    title = "Losers Height Mean vs Retirement",
    x = "Retirement",
    y = "Losers height mean",
    fill = "Retirement"
  ) +
  coord_flip()


# 4. DESCRIPTIVES

library(readr)
DoublesATP_full<- ATP_doubles_full_ord_ok

# Graphic number of different pairs per year
df_winners <- DoublesATP_full[, c("year","winners")]
df_losers <- DoublesATP_full[, c("year","losers")]
colnames(df_winners) <- c("year", "team")
colnames(df_losers) <- c("year", "team")
df_win_los <- rbind(df_winners, df_losers)
df_win_los_uniq <- unique(df_win_los[, c("year", "team")])


team_year <- table(df_win_los_uniq$year)

barplot(team_year, main = "Number of couples per year", xlab = "year", ylab = "Number of couples",col = "lightblue")

# Top Retirements of Couples
library(stringr)
df_RET <- DoublesATP_full[str_detect(DoublesATP_full$score, "RET"), ]
df_RET <- df_RET[!is.na(df_RET$tourney_id),]

df_RET_ord <- df_RET %>%
  group_by(losers) %>%
  summarise(Frecuency = n()) %>%
  arrange(desc(Frecuency))

df_RET_ord_winners <- df_RET %>%
  group_by(winners) %>%
  summarise(Frecuency = n()) %>%
  arrange(desc(Frecuency))

top_4 <- df_RET_ord %>%
  top_n(22, Frecuency)

ggplot(top_4, aes(x = losers, y = Frecuency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Retired Couple") +
  ylab("Frecuency") +
  ggtitle("Top of Couple's Retirements")

top_4$losers <- factor(top_4$losers, levels = top_4$losers[order(top_4$Frecuency, decreasing = FALSE)])
top_4$Color <- ifelse(top_4$Frecuency == max(top_4$Frecuency), "#E60000",
                      ifelse(top_4$Frecuency == min(top_4$Frecuency), "#336699", "#FFD700"))
ggplot(top_4, aes(x = losers, y = Frecuency, fill = Color)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  labs(x = "Retired Couple", y = "Frequency", title = "Top of Couple's Retirements") +
  scale_fill_identity() +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.y = element_text(hjust = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")


top_ret_win <- df_RET_ord_winners %>%
  top_n(10, Frecuency)

ggplot(top_ret_win, aes(x = winners, y = Frecuency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Retired Couple") +
  ylab("Frecuency") +
  ggtitle("Top of Couple's Retirements")


# 5.EPIDEMIOLOGY
# Import data base

colnames(DoublesATP)[1] <- "matches"

library("SmartEDA")
library("epiR")

##Tournament vs Retirement
# Number of Retirements by tournament level
table(DoublesATP$Retirement, DoublesATP$tourney_level)

# Number of games per tournament level
aggregate(Total_Games~tourney_level, data = DoublesATP, sum)

# Comparing Grand Slams vs Masters 1000
GS_M<-c(88,136524,40,110079)
epi.2by2(dat = GS_M, method = "cohort.time",
         conf.level = 0.95, units = 10000, interpret = FALSE, outcome = "as.columns")

# Comparing Other Levels vs Masters 1000
OL_M<-c(133,387818,40,110079)
epi.2by2(dat = OL_M, method = "cohort.time",
         conf.level = 0.95, units = 10000, interpret = FALSE, outcome = "as.columns")

## Surface vs Retirement
table(DoublesATP$Retirement, DoublesATP$surface)
aggregate(Total_Games~surface, data = DoublesATP, sum)

# Comparing Carpet vs Hard
Ca_H<-c(12,12088,118,333130)
epi.2by2(dat = Ca_H, method = "cohort.time",
         conf.level = 0.95, units = 10000, interpret = FALSE, outcome = "as.columns")

# Comparing Clay vs Hard
Cl_H<-c(94,195719,118,333130)
epi.2by2(dat = Cl_H, method = "cohort.time",
         conf.level = 0.95, units = 10000, interpret = FALSE, outcome = "as.columns")

# Comparing Grass vs Hard
Gr_H<-c(37,93484,118,333130)
epi.2by2(dat = Gr_H, method = "cohort.time",
         conf.level = 0.95, units = 10000, interpret = FALSE, outcome = "as.columns")

## Round vs Retirement
table(DoublesATP$Retirement, DoublesATP$round_group)
aggregate(Total_Games~round_group, data = DoublesATP, sum)

# Comparing No Final vs Final
NF_F<-c(197,411265,64,223156)
epi.2by2(dat = NF_F, method = "cohort.time",
         conf.level = 0.95, units = 10000, interpret = FALSE, outcome = "as.columns")

## PLOTS

incidence <- DoublesATP %>%
  group_by(year) %>%
  summarise(retires = sum(Retirement == "YES"), matches = n_distinct(matches)) %>%
  mutate(incidence = retires / matches)

library(ggplot2)

ggplot(incidence, aes(x = year, y = incidence)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  xlab("Year") +
  ylab("Retirement incidence") +
  ggtitle("Incidence of retirements per year")


library(dplyr)

retires_year_surface <- DoublesATP %>%
  group_by(year, surface) %>%
  summarise(retires = sum(Retirement == "YES"), .groups = "drop")


ggplot(retires_year_surface, aes(x = year, y = retires, fill = surface)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Year") +
  ylab("Total retirements") +
  ggtitle("Retirements per year and surface") +
  scale_fill_discrete(name = "Surfaces")


