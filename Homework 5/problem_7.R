sink("problem_7_output.txt")
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
data("card")

# (1)
# 7th editionでは、exper^2の項が追加されている
lm_formula <- paste0("educ ~ nearc4 + exper + I(exper^2) + black + smsa + south + smsa66 +",
                     paste0('reg', 662:669, collapse = ' + '))
lm_model <- lm(lm_formula, data = card)
summary(lm_model)

# 残差の取得
v2 <- residuals(lm_model)

# lwageを被説明変数に、v2を説明変数に追加
lm_formula <- paste0("lwage ~ nearc4 + exper + I(exper^2) + black + smsa + south + smsa66 + v2 + ",
                     paste0('reg', 662:669, collapse = ' + '))

# Run regression
lm_model <- lm(as.formula(lm_formula), data = card)
summary(lm_model)

# (2)
lm_formula <- paste0("lwage ~ nearc4 + exper + I(exper^2) + black + smsa + south + smsa66 +",
                     paste0('reg', 662:669, collapse = ' + '))
iv_formula <- paste0("~nearc2 + nearc4 + exper + I(exper^2) + black + smsa + south + smsa66 + ", paste0('reg', 662:669, collapse = ' + '))

iv_reg <- ivreg(lwage ~ educ + exper + expersq + black + smsa + south +
                  smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669
                | nearc4 + exper + expersq + black + smsa + south + south66 +
                  smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 +
                  reg668 + reg669,
                data = card)
summary(iv_reg)


# (3)
card <- card |> 
  mutate(u2 = residuals(iv_reg))

# regress u2 on all exogenous variables
lm_model3 <- lm(u2 ~ nearc2 + nearc4 + exper + expersq + black + smsa + south + south66 +
                  smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 +
                  reg668 + reg669,
                data = card)

summary(lm_model3)
nrow(card)*summary(lm_model3)$r.squared




sink()








