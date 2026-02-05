
# Import data base

library(readr)
DoublesATP<- read_csv("Data/DoublesATP.csv")
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

incidence$lower <- incidence$upper <- NA 
for(i in 1:nrow(incidence)){
  incidence$lower[i] <- prop.test(incidence$retires[i], incidence$matches[i])$conf.int[1]
  incidence$upper[i] <- prop.test(incidence$retires[i], incidence$matches[i])$conf.int[2]
}

library(ggplot2)

ggplot(incidence, aes(x = year, y = incidence)) +
  geom_line(color='darkblue') +
  geom_point(color='darkblue') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, color='darkblue') +
  xlab("Year") +
  ylab("Retirement incidence") +
  scale_x_continuous(minor_breaks = 2000:2020) +
  ggtitle("Incidence of retirements per year")

ggsave(filename = 'Graphics/Epidemiology/Incidence of retirements per year.jpeg',
       width = 10, height = 6) # width = 7.3, height = 6.4



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


