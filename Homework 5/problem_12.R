sink("problem_12_output.txt")
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
data("mroz")

# (1)
# wageがNAになっているものを0に変換（lwageも0に変換）
mroz <- mroz |> 
  mutate(
    wage = if_else(is.na(wage), 0, wage),
    lwage = if_else(is.na(lwage), 0, lwage),
    lhours = if_else(hours == 0, 0, log(hours))
  )

iv_model <- ivreg(lhours ~ lwage + educ + age + kidslt6 + nwifeinc
                  | educ + age + kidslt6 + nwifeinc + exper + expersq, data = mroz)
summary(iv_model)

# (2)
iv_model2 <- ivreg(lhours ~ lwage + educ + age + kidslt6 + nwifeinc
                  | motheduc + fatheduc + age + kidslt6 + nwifeinc + exper + expersq, data = mroz)
summary(iv_model2)

# (3)
# Regressing the 2SLS residuals on all exogenous variables
mroz <- mroz |> 
  mutate(uhat = residuals(iv_model2))


# regress uhat on exper expersq motheduc fatheduc age kidslt6 nwifeinc
lm_model <- lm(uhat ~ expersq + motheduc + fatheduc + age + kidslt6 + nwifeinc, data = mroz)
summary(lm_model)

sink()