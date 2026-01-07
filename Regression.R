#Libraries
library(readr)
library(AER)

# Loading data
DoublesATP<- read_csv("Data/DoublesATP.csv")

# DoublesATP <- DoublesATP %>% filter(year <=2010) # Year<2010
# DoublesATP <- DoublesATP %>% filter(year > 2010) # Year<2010


colnames(DoublesATP)[1] <- "matches"

DoublesATP$Retirement    <- as.factor(DoublesATP$Retirement)
DoublesATP$surface       <- factor(DoublesATP$surface,
                                   levels = c("Grass","Carpet","Clay","Hard"))
DoublesATP$tourney_level <- factor(DoublesATP$tourney_level,
                                   levels = c("Masters 1000","Grand Slam","Other levels"))
DoublesATP$year_bin <- factor(ifelse(DoublesATP$year<=2010,"Before 2010","After 2010"),
                          levels = c("Before 2010","After 2010"))


# Poisson Regression Model
DoublesATP$Ret_count <- as.numeric(DoublesATP$Retirement) - 1
# aggregate data by surface and tourney_level and round_group
d_agg <- aggregate(cbind(Ret_count, Total_Games) ~ surface + tourney_level + round_group + year_bin,
                   data = DoublesATP, sum)
Reg_Ret_Surf_p0 <- glm(Ret_count ~ surface + tourney_level + round_group + year_bin, 
                      family = poisson,
                      offset = log(Total_Games),
                      data   = d_agg)
summary(Reg_Ret_Surf_p0)

Reg_Ret_Surf_p <- glm(Ret_count ~ surface + tourney_level + round_group, 
                       family = poisson,
                       offset = log(Total_Games),
                       data   = d_agg)
summary(Reg_Ret_Surf_p)

# Overdispersion
dispersiontest(Reg_Ret_Surf_p, trafo=1)
dispersiontest(Reg_Ret_Surf_p, trafo=2)

# Results table
coefficients <- summary(Reg_Ret_Surf_p)$coefficients
estimates <- coefficients[, "Estimate"]
std_errors <- coefficients[, "Std. Error"]
irr <- exp(estimates)
lower_ci <- exp(estimates - 1.96 * std_errors)
upper_ci <- exp(estimates + 1.96 * std_errors)

results <- data.frame(
  Variables = rownames(coefficients),
  Estimate = estimates,
  SE = std_errors,
  IRR = irr,
  Lower_CI95 = lower_ci,
  Upper_CI95 = upper_ci,
  p_value = coefficients[, "Pr(>|z|)"]
)

print(results)



# Sensitivity analysis by period

# Period ≤2010
d_agg_pre2010 <- subset(d_agg, year_bin == "Before 2010")

Reg_pre2010 <- glm(
  Ret_count ~ surface + tourney_level + round_group,
  family = poisson,
  offset = log(Total_Games),
  data = d_agg_pre2010
)

summary(Reg_pre2010)


# Period >2010
d_agg_post2010 <- subset(d_agg, year_bin == "After 2010")

Reg_post2010 <- glm(
  Ret_count ~ surface + tourney_level + round_group,
  family = poisson,
  offset = log(Total_Games),
  data = d_agg_post2010
)

summary(Reg_post2010)
