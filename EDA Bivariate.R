## EDA BIVARIATE

DoublesATP <- read_csv("TFM/DoublesATP.csv")
EDA_df <- DoublesATP[, c("tourney_level","surface","round_group","minutes","Retirement","winner_team_hand","loser_team_hand","winner_age_mean","loser_age_mean","winner_ht_mean","loser_ht_mean")] 

# Creation of descriptive table
library(compareGroups)
export2word(tab <- descrTable(Retirement ~., EDA_df, show.all=TRUE, method=2, byrow = TRUE,hide.no="no", max.xlev = 30, simplify=TRUE,
                              show.ratio=TRUE, # OR and p-valor .
),file='eda_descriptive_tab2.docx',
header.labels = c("all"="All","p.overall"="p-overall","p.ratio"="p-value"), caption="Descriptives by retirement")


install.packages("tidyverse")
install.packages("tidyquant")
install.packages("ggdist")
install.packages("ggthemes")
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
