sink("problem_10_output.txt")

# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)

# load data
data("wage1")

# regress educ on exper and tenure
lm_model <- lm(educ ~ exper + tenure, data = wage1)
summary(lm_model)

# save the uhat
uhat <- lm_model$residuals

# regress log(wage) on uhat
lm_model2 <- lm(lwage ~ uhat, data = wage1)
summary(lm_model2)
coef_uhat <- lm_model2$coefficients[[2]]

# original regression
lm_original <- lm(lwage ~ educ + exper + tenure , data = wage1)
summary(lm_original)
coef_educ <- lm_original$coefficients[[2]]

# compare coefficients
coef_uhat - coef_educ

sink()