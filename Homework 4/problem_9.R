sink("problem_9_output.txt")

# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)

# load data
data('hprice1')

# (1)
# regress price on sqrft and bdrms
lm_model <- lm(price ~ sqrft + bdrms, data = hprice1)

summary(lm_model)

# sample size
nrow(hprice1)


# (2)
lm_model$coefficients[[3]]*1

# (3)
lm_model$coefficients[[2]]*140 + lm_model$coefficients[[3]]*1

# (4)
summary(lm_model)$r.squared

# (5) 1行目を使ってpredict
predict(lm_model, hprice1[1,], se.fit = TRUE)

# (6)
predict(lm_model, hprice1[1,], se.fit = TRUE)[[1]] - 300

sink()