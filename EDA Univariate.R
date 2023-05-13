# EDA Univariant

# Import data base
ATP_doubles_full_ord_ok <- read_csv("TFM/ATP_doubles_full_ord_ok.csv")


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
