sink("problem_6_output.txt")
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

# (2) regress IQ on nearc4
lm_model2 <- card |> 
  filter(!is.na(IQ)) |> 
  (\(x) lm(IQ ~ nearc4, data = x))()

summary(lm_model2)

# (3) regress IQ on nearc4, smasa66, and reg662-reg669
lm_formula <- paste0("IQ ~ nearc4 + smsa66 +", paste0('reg', 662:669, collapse = ' + '))
lm_model3 <- card |> 
  filter(!is.na(IQ)) |> 
  (\(x) lm(lm_formula, data = x))()

summary(lm_model3)
sink()
