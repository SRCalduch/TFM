#Libraries
library(readr)
library(AER)

# Loading data
DoublesATP<- read_csv("data/DoublesATP.csv")
colnames(DoublesATP)[1] <- "matches"

DoublesATP$Retirement    <- as.factor(DoublesATP$Retirement)
DoublesATP$surface       <- factor(DoublesATP$surface,
                                   levels = c("Grass","Carpet","Clay","Hard"))
DoublesATP$tourney_level <- factor(DoublesATP$tourney_level,
                                   levels = c("Masters 1000","Grand Slam","Other levels"))

# Poisson Regression Model
DoublesATP$Ret_count <- as.numeric(DoublesATP$Retirement) - 1
Reg_Ret_Surf_p <- glm(Ret_count ~ surface + tourney_level + round_group, 
                      family = "poisson",
                      offset = log(Total_Games),
                      data   = DoublesATP)
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
