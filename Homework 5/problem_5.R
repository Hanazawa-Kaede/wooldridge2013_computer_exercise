sink("problem_5_output.txt")
# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)
library(AER)

# load data
data("wage2")

# (1) regress lwage on sibs
lm_model <- lm(lwage ~ sibs, data = wage2)
summary(lm_model)

# (2) regress educ on brthord
lm_model2 <- lm(educ ~ brthord, data = wage2)
summary(lm_model2)

# (3) AER::ivreg() で推定
iv_model3 <- ivreg(lwage ~ educ | brthord, data = wage2)
summary(iv_model3)

# (4) regress educ on sibs and brthord
lm_model4 <- lm(educ ~ sibs + brthord, data = wage2)
summary(lm_model4)

# (5) IV regression
iv_model5 <- ivreg(lwage ~ educ + sibs | brthord + sibs, data = wage2)
summary(iv_model5)

# (6) educ_hatとwage2$sibsの相関係数を計算
educ_hat <- predict(lm_model4)
cor(as.numeric(educ_hat), (wage2$sibs[1:length(educ_hat)]))

sink()