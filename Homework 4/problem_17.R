sink("problem_17_output.txt")
# my R version
# R.version.string
# [1] "R version 4.3.2 (2023-10-31)"

# preparation
# source : https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf　
# install.packages("wooldridge")
library(wooldridge)
library(modelsummary)
library(lmtest)

# load data
data('vote1')

# (1)
lm_model <- lm(voteA ~ prtystrA + democA + lexpendA + lexpendB, data = vote1)
summary(lm_model)

vote1 <- vote1 |> 
  mutate(
    uhat = summary(lm_model)$residual
  )

lm_model2 <- lm(uhat ~ prtystrA + democA + lexpendA + lexpendB, data = vote1)
summary(lm_model2)


summary(lm_model2)$r.squared

# (2) Breusch-Pagan testをF statistic versionで実行する
lm_model_BP <- lm(uhat^2 ~ prtystrA + democA + lexpendA + lexpendB, data = vote1)
summary(lm_model_BP)$fstatistic

# p-valueを計算する関数。
# summaryの結果から直接抽出できないため使用（summaryの出力結果を見ればp-valueは書いてある）
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# p-valueの抽出
overall_p(lm_model_BP)


# (3) Compute the special case of the White test for heteroskedasticity
vote1 <- vote1 |> 
  mutate(yhat = predict(lm_model))

lm_model_White <- lm(uhat^2 ~ yhat+I(yhat^2), data = vote1)
summary(lm_model_White)

# p-valueの計算：0.06449618
overall_p(lm_model_White)

sink()

