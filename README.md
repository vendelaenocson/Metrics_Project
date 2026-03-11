```r
library(tidyverse)
library (stargazer)
library(readxl)
library(modelsummary)
library(car)
library(lmtest)
library(fixest)
library(ivreg)
library(gmm)
library(flextable)
library(estimatr)
library(ggplot2)
dataset <- read_xlsx("earnings.xlsx")
data_clean <- subset(dataset, wage != 0 & !is.na(married) & !is.na(libcrd14))
# summary(data_clean)
# head(data_clean)


#create experience squared
data_clean$expersq <- data_clean$exper^2

#create factor dummied for married (we use only married1)
data_clean$married <- factor(data_clean$married)
dummies <- model.matrix(~married-1,data=data_clean)
data_clean <- cbind(data_clean,dummies)

#run a test OLS
ols_test = lm(lnwage ~ educ + exper + expersq + black + south + smsa + south66 +I(south66*south) , data = data_clean)
stargazer(ols_test, type="text")
# summary(ols)

#run ols without south66
ols_linear = lm(wage ~ educ + exper + expersq + black + south + smsa, data = data_clean)

#check for potential heteroskedastic errors
autoplot(ols_linear)+
  theme_bw(base_size = 14)

#create log of wage, used to to get rid of right skewed errors
data_clean$lnwage = log(data_clean$wage)

#regression, same as what we are given in the exercise
ols = lm(lnwage ~ educ + exper + expersq + black + south + smsa, data = data_clean)

#check regression
stargazer(ols, type="text")

#chek errors again, looks good
autoplot(ols)+
  theme_bw(base_size = 14)

#regress education on exogenous and its insrtumetnts
stepeduc <- lm(educ ~ exper + expersq + black + south + smsa + libcrd14 + momdad14, data = data_clean)
#rediduals
v_1 <- stepeduc$res
#wu test for educ instruments
wu_test_educ <- lm_robust(lnwage ~ educ + exper + expersq + black + south + smsa  + v_1, data = data_clean)
summary(wu_test_educ)

#same process for experience
stepexp <- lm(exper ~ educ + black + south + smsa  +  nearc4 + married1+ smsa66, data = data_clean)
summary(stepexp)

v_2 <- stepexp$res

wu_test_exp <- lm_robust(lnwage ~ educ + exper + expersq + black  + south + smsa + v_2, data = data_clean)
summary(wu_test_exp)

#same process for experience squared
stepexpsq <- lm(expersq ~ educ + black + south + smsa + nearc4 + married1 + smsa66, data = data_clean)

v_3 <- stepexpsq$res

wu_test_expsq <- lm_robust(lnwage ~ educ + exper + expersq + black + south + smsa + v_3, data = data_clean)
summary(wu_test_expsq)

#iv-regression
iv_reg <- ivreg(lnwage ~ educ + exper + expersq + black + south + smsa | black + south + smsa + libcrd14 + momdad14 + nearc4 + married1 + smsa66, data = data_clean)
summary(iv_reg, diagnostics = TRUE)


iv <- ivreg(lnwage ~ educ + exper + expersq + black + south + smsa |  black + south + smsa + libcrd14 + momdad14 + nearc4 + married1 + smsa66, data = data_clean)
res_iv <- iv$residuals
sargan <- lm(res_iv ~ black + south + smsa  + libcrd14 + momdad14 + nearc4 + married1 + smsa66, data = data_clean)
summary(sargan)

#N*R^2 
2990*0.0002067
#= 0.618033, df= 5-3 (5 instruments - 3 exogenous variables)=2

#Wald strong test educ
ols_educ <- lm(educ~ exper + expersq+ black + south + smsa + libcrd14 + momdad14, data=data_clean)
Hnull_educ <- c("libcrd14=0", "momdad14=0")
linearHypothesis(ols_educ, Hnull_educ, test="F")

#Wald strong test exper
ols_exper <- lm(exper~ educ + black + south + smsa + nearc4 + married1 + smsa66, data=data_clean)
Hnull_exper <- c("nearc4=0", "married1=0", "smsa66=0")

#Wald strong test expersq
ols_expersq <- lm(expersq~ educ + black + south + smsa + nearc4 + married1 + smsa66, data=data_clean)
Hnull_expersq <- c("nearc4=0", "married1=0", "smsa66=0")
linearHypothesis(ols_expersq, Hnull_expersq, test="F")

#chek iv regression
summary(iv_reg)

#see difference between coefficients of iv and ols regression
modelplot(list("OLS"=ols, "IV"=iv_reg),
          coef_omit = 'Interc')+
  theme_minimal()+
  scale_color_manual(values = c("OLS" = "black", "IV" = "green")) +
  theme_minimal() 

iv2 <- feols(lnwage ~ exper+exper^2+black + south + smsa | educ~ exper+expersq+black + south + smsa + libcrd14 + momdad14, data = data_clean)
modelsummary(list(ols,iv_reg, iv2), stars=TRUE)

modelsummary(list(ols, iv_reg))

```



