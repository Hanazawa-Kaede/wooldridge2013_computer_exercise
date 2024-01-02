sink("problem_11_output.txt")
# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)

# load data
data("wage2")

# (1) regress IQ on educ
lm_model <- lm(IQ ~ educ, data = wage2)
summary(lm_model)

delta_1_hat <- coef(lm_model)[[2]]

# (2) regress log(wage) on educ
lm_model2 <- lm(lwage ~ educ, data = wage2)
summary(lm_model2)

beta_tilde_1 <- coef(lm_model2)[[2]]


# (3) regress log(wage) on educ and IQ
lm_model3 <- lm(lwage ~ educ + IQ, data = wage2)
summary(lm_model3)

beta_hat_1 <- coef(lm_model3)[[2]]
beta_hat_2 <- coef(lm_model3)[[3]]

# (4) 
(beta_hat_1 + beta_hat_2 * delta_1_hat) == beta_tilde_1

sink()