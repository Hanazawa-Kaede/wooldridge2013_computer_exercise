sink("problem_8_output.txt")
# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)
library(AER)
library(tidyverse)

# load data
data("wage2")

# (1)
iv_model1 <- ivreg(lwage ~ educ + exper + tenure + black | sibs + exper + tenure + black, data = wage2)
summary(iv_model1)

# (2)
# 1st stage:
lm_model_1st <- lm(educ ~ sibs + exper + tenure + black, data = wage2)
eduhat <- predict(lm_model_1st)

# 2nd stage:
lm_model_2nd <- lm(lwage ~ eduhat + exper + tenure + black, data = wage2)
summary(lm_model_2nd)

# (3)
# 1st step:
lm_model_1st <- lm(educ ~ sibs, data = wage2)
summary(lm_model_1st)
eduhat2 <- predict(lm_model_1st)

# 2nd step:
lm_model_2nd <- lm(lwage ~ eduhat2 + exper + tenure + black, data = wage2)
summary(lm_model_2nd)

sink()