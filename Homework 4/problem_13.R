sink("problem_13_output.txt")

# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)

# load data
data("k401ksubs")

# (1) How many single-person households are there in the data set?
sum(k401ksubs$marr == 0)

# (2) regress nettfa on inc and age. Use only single-person households.
lm_model <- lm(nettfa ~ inc + age, data = k401ksubs[k401ksubs$marr == 0, ])
summary(lm_model)

# (4)
# 帰無仮説の設定: H_0: beta2 = 1, H_1: beta_2 < 1
null_hypothesis_value <- 1

summary_lm <- summary(lm_model)
summary_lm$coefficients['age','Estimate']
summary_lm$coefficients['age','Std. Error']

test_statistics <- (summary_lm$coefficients['age','Estimate']-null_hypothesis_value)/(summary_lm$coefficients['age','Std. Error'])

p_value <- pt(-abs(test_statistics), df = length(lm_model$residuals) - length(lm_model$coefficients), lower.tail = TRUE)

# 有意水準を設定
alpha <- 0.01

# 帰無仮説の検定
if (p_value < alpha) {
  cat("1%有意水準で有意 (p-value =", p_value, ")")
} else {
  cat("1%有意水準で有意ではない (p-value =", p_value, ")")
}

sink()