sink("problem_14_output.txt")
# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)

# load data
data("wage2")

# (1) regress log(wage) on educ and exper, tenure, married, black, south, and urban
lm_model <- lm(lwage ~ educ + exper + tenure + married + black + south + urban, data = wage2)
summary(lm_model)

# (2) add exper^2 and tenure^2
lm_model2 <- lm(lwage ~ educ + exper + I(exper^2) + tenure + I(tenure^2) + married + black + south + urban, data = wage2)
summary(lm_model2)

# (3)Extend the original model to allow the return to education to depend on race.
lm_model3 <- lm(lwage ~ educ + exper + I(exper^2) + tenure + I(tenure^2) + married + black + south + urban + educ:black, data = wage2)
summary(lm_model3)


# (4) make married_black and married_nonblack(both of them are dummy variables.)
wage2 <- wage2 |> 
  mutate(
    married_black = ifelse(married == 1 & black == 1, 1, 0),
    married_nonblack = ifelse(married == 1 & black == 0, 1, 0)
  )

# このformulaだと、marriedの係数がNAになってしまう（多重共線性？）
# lm_model4 <- lm_robust(lwage ~ educ + exper + tenure + married + black + south + urban + married_black + married_nonblack, data = wage2, se_type = 'stata')

# original modelから、blackとmarriedを落として回帰分析
lm_model4 <- lm_robust(lwage ~ educ + exper + tenure + south + urban + married_black + married_nonblack, data = wage2, se_type = 'stata')

summary(lm_model4)

sink()