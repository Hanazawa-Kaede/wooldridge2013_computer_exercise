sink("problem_16_output.txt")
# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)
library(estimatr)

# load data
data("hprice1")

# (1) obtain heteroskedasticity-robust standard erros
lm_model <- lm_robust(price ~ lotsize + sqrft + bdrms, data = hprice1, se_type = 'stata')
summary(lm_model)

# (2) 
lm_model2 <- lm_robust(lprice ~ llotsize + lsqrft + bdrms, data = hprice1, se_type = 'stata')
summary(lm_model2)

sink()