# Metrics_Project

dataset <- read_xlsx("earnings.xlsx")

summary(dataset)   # to get summary stats of all dataset

data_clean <- subset(dataset, wage != 0 & !is.na(married))

datasummary_skim(data_clean, output = "default", histogram=T)

data_clean$lnwage = log(data_clean$wage)

ols = lm (lnwage ~ educ + exper + I(exper^2) + black + south + smsa + smsa66 + I(smsa*smsa66) + married, data = data_clean) # should we add married
summary(ols)

# educ - momdad14 (parents at home, not dealing with finances, able to focus on school), south66 (education in the south not as strong) 
# exper & exper^2 - nearc4 (more educated people in the neighborhood to learn from and work under), enroll (in school later = less experience) 
instr1_ed <- data_clean$momdad14
instr2_ed <- data_clean$south66
instr1_exp <- data_clean$nearc4
instr2_exp <- data_clean$enroll

# test for endogeneity of educ  (test on all exog + instrument to see if its strong); see if variables are different if we drop NAs or keep them (Tutorial 3)
stepeduc <- lm(educ ~ exper + I(exper^2) + black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr2_ed + instr1_ed, data = data_clean)
v_1 <- stepeduc$res
wu_test_educ <- lm_robust(lnwage ~ educ + exper + I (exper^2)+ black + south + smsa + + smsa66 + I(smsa*smsa66) + married + v_1, data = data_clean)
summary(wu_test_educ)
# fail to reject the null --> cant say educ is endogenous

stepexp <- lm(exper ~ educ + black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr1_exp + instr2_exp, data = data_clean)
v_2 <- stepexp$res
wu_test_exp <- lm_robust(lnwage ~ educ + exper + I (exper^2) + black + south + smsa + + smsa66 + I(smsa*smsa66) + married + v_2, data = data_clean)
summary(wu_test_exp)
# reject the null that exp is exog

stepexpsq <- lm(I(exper^2) ~ educ + black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr1_exp + instr2_exp, data = data_clean)
v_3 <- stepexpsq$res
wu_test_expsq <- lm_robust(lnwage ~ educ + exper + I (exper^2) + black + south + smsa + + smsa66 + I(smsa*smsa66) + married + v_3, data = data_clean)
summary(wu_test_expsq)
# reject the null that exp_sq is exog

# fail to reject null based on coeff on v, so educ can be exog and instrument invalid, so do a sargan test (use when model is overidentified)
iv <- ivreg(lnwage ~ educ + exper + I(exper^2) + black + south + smsa + smsa66 + I(smsa*smsa66) + married|  black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr2_ed + instr1_ed + instr1_exp + instr2_exp, data = data_clean)
res_iv <- iv$residuals
sargan <- lm(res_iv ~ black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr2_ed + instr1_ed + instr1_exp + instr2_exp, data = data_clean)
summary(sargan)
# N*R^2 = 3010*0.0001759 = 0.529 compare to chi sq, dof = overidentifying instruments = 1

strong1 <- lm(educ ~ exper + I(exper^2) + black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr2_ed + instr1_ed, data = data_clean)
summary(strong1)
Hnull <-c("instr1_ed=0","instr2_ed=0")
linearHypothesis(strong1, Hnull, test="F")
# F-stat is 19.7 > 10 --> strong instrument

strong2 <- lm(exper ~ educ + black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr1_exp + instr2_exp, data = data_clean)
summary(strong2)
Hnull2 <-c("instr1_exp=0","instr2_exp=0")
linearHypothesis(strong2, Hnull2, test="F")
# F-stat is  < 10 --> not strong instrument

strong3 <- lm(I(exper^2) ~ educ + black + south + smsa + smsa66 + I(smsa*smsa66) + married + instr1_exp + instr2_exp, data = data_clean)
summary(strong3)
Hnull3 <-c("instr1_exp=0","instr2_exp=0")
linearHypothesis(strong3, Hnull3, test="F")
# F-stat is  < 10 --> not strong instrument


# instrument needs to be strong and valid
