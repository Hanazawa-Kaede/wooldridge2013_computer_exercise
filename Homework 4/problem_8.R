sink("problem_8_output.txt")

# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)

# load data
data('wage2')


# (1) Find the average salary and average IQ. What is the standard deviation of IO?
# average salary
mean(wage2$wage)

# average IQ
mean(wage2$IQ)

# standard deviation of IQ
sd(wage2$IQ)

# (2) Estimate a simple regression model where a one-point increase in IQ changes wage by a constant dollar amount.
# estimate a simple regression model
lm_model <- lm(wage ~ IQ, data = wage2)
summary(lm_model)

# Use this to find the predicted increase in wage for an increase in IQ of 15 points.
# predicted increase in wage for an increase in IQ of 15 points
coef(lm_model)[[2]] * 15

# R^2
summary(lm_model)$r.squared


# (3) estimate a model where each one-point increase in IQ has the same percentage effect on wage.
lm_model2 <- lm(lwage ~ IQ, data = wage2)
coef(lm_model2)[[2]] * 15

sink()
