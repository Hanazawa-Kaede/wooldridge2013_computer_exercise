sink("problem_15_output.txt")
# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)

# load data
data("wage1")

# (1) 
lm_model <- lm(lwage ~ female + educ + female:educ + exper + I(exper^2) + tenure + I(tenure^2), data = wage1)
summary(lm_model)

# 式(7.18)を用いて、educ = 12.5のときの男女差を推定する。
female_educ12.5 <- summary(lm_model)$coefficients["female", "Estimate"] + summary(lm_model)$coefficients["educ", "Estimate"]*12.5
male_educ12.5 <- summary(lm_model)$coefficients["educ", "Estimate"]*12.5

female_educ12.5-male_educ12.5

# educ = 0の時の男女それぞれについて計算
female_educ0 <- summary(lm_model)$coefficients["female", "Estimate"]
male_educ0 <- 0


# (2)
lm_model2 <- lm(lwage ~ female + educ + I(female*(educ-12.5)) + exper + I(exper^2) + tenure + I(tenure^2), data = wage1)
summary(lm_model2)

sink()