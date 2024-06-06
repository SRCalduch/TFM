# Data
library(readr)
DoublesATP<- read_csv("TFM/DoublesATP.csv")
colnames(DoublesATP)[1] <- "matches"

#Transforming data
DoublesATP$surface <- factor(DoublesATP$surface)
DoublesATP$tourney_level <- factor(DoublesATP$tourney_level)
DoublesATP$round_group <- factor(DoublesATP$round_group)
DoublesATP$Retirement <- factor(DoublesATP$Retirement)
DoublesATP$Retirement <- as.numeric(DoublesATP$Retirement)

# Poisson Regression Model
DoublesATP$surface <- relevel(DoublesATP$surface, ref = "Grass")
DoublesATP$tourney_level <- relevel(DoublesATP$tourney_level, ref = "Masters 1000")
Reg_mod <- glm(formula = Retirement ~ surface + tourney_level + round_group, 
                  family = "poisson", 
                  offset = log(Total_Games), 
                  data = DoublesATP)
summary(Reg_mod)

# Results table
coefficients <- summary(Reg_mod)$coefficients
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

# Dispersion
library(AER)
dispersiontest(Reg_mod, trafo=1)
dispersiontest(Reg_mod, trafo=2)
