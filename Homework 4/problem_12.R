sink("problem_12_output.txt")

# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)
library(tidyverse)

# load data
data("wage2")

# add exprtenure
wage2 <- wage2 |> 
  mutate(exprtenure = exper + tenure)

# (2) regress log(wage) on educ, expr and exprtenure
lm_model <- lm(lwage ~ educ + exper + exprtenure, data = wage2)
summary(lm_model)

sink()