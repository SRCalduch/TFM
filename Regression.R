library(readr)
DoublesATP<- read_csv("TFM/DoublesATP.csv")
colnames(DoublesATP)[1] <- "matches"
DoublesATP$Retirement <- as.factor(DoublesATP$Retirement)
DoublesATP$surface <- as.factor(DoublesATP$surface)
DoublesATP$tourney_level <- as.factor(DoublesATP$tourney_level)


# Reg simple Surface
Reg_Ret_Surf<-glm(Retirement~surface, family = "binomial", data = DoublesATP)
summary(Reg_Ret_Surf)
statistically significant

# Reg simple Tourney Level
DoublesATP$tourney_level<-relevel(DoublesATP$tourney_level, ref = "Masters 1000")
Reg_Ret_Tourney<-glm(Retirement~tourney_level, family = "binomial", data = DoublesATP)
summary(Reg_Ret_Tourney)
statistically significant

# Reg simple Round
Reg_Ret_round<-glm(Retirement~round_group, family = "binomial", data = DoublesATP)
summary(Reg_Ret_round)
statistically significant


# Regression model
Reg_mod <- glm(formula = Retirement ~ surface + tourney_level + round_group, family = "binomial", data = DoublesATP)
Reg_mod$aic
summary(Reg_mod)
step(Reg_mod)
par(mfrow=c(2,2))
plot(Reg_mod_ok)

#Normalidad
residues<-rstandard(Reg_mod)
par(mfrow=c(1,3))
hist(residues)
boxplot(residues)
qqnorm(residues)
qqline(residues)

#ANOVA
anova_reg_mod <- anova(Reg_mod,test="Chi")
anova_reg_mod

