# Metrics_Project

dataset <- read_xlsx("earnings.xlsx")

summary(dataset)   # to get summary stats of all dataset

data_clean <- subset(dataset, wage != 0 & !is.na(married) & !is.na(libcrd14))

data_clean$expersq <- data_clean$exper^2

datasummary_skim(data_clean, output = "default", histogram=T)

data_clean$lnwage = log(data_clean$wage)

ols = lm (lnwage ~ educ + exper + expersq + black + south + smsa + south66 + I(south*south66), data = data_clean) # should we add factor(married)
summary(ols)
stargazer(ols, type = "text")

# educ - momdad14 (parents at home, not dealing with finances, able to focus on school), south66 (education in the south not as strong) 
# exper & exper^2 - nearc4 (more educated people in the neighborhood to learn from and work under), enroll (in school later = less experience) 
instr1_ed <- data_clean$libcrd14
instr2_ed <- data_clean$momdad14
instr1_exp <- data_clean$nearc4
instr2_exp <- data_clean$married
instr3_exp <- data_clean$smsa66


# test for endogeneity of educ  (test on all exog + instrument to see if its strong); see if variables are different if we drop NAs or keep them (Tutorial 3)
stepeduc <- lm(educ ~ exper + expersq + black + south + smsa + south66 + I(south*south66)+ instr2_ed + instr1_ed, data = data_clean)
v_1 <- stepeduc$res
wu_test_educ <- lm_robust(lnwage ~ educ + exper + expersq + black + south + smsa  + v_1, data = data_clean)
summary(wu_test_educ)
# reject the null that educ is exog

stepexp <- lm(exper ~ educ + black + south + smsa  +  instr1_exp + instr2_exp, data = data_clean)
v_2 <- stepexp$res
wu_test_exp <- lm_robust(lnwage ~ educ + exper + expersq + black + south + south66 + I(south*south66)+ smsa + v_2, data = data_clean)
summary(wu_test_exp)
# reject the null that exp is exog

stepexpsq <- lm(expersq ~ educ + black + south + smsa + instr1_exp + instr2_exp + instr3_exp, data = data_clean)
v_3 <- stepexpsq$res
wu_test_expsq <- lm_robust(lnwage ~ educ + exper + expersq + black + south  south66 + I(south*south66) + smsa + v_3, data = data_clean)
summary(wu_test_expsq)
# reject the null that exp_sq is exog

# iv_reg
iv_reg <- ivreg(lnwage ~ educ + exper + expersq + black + south + smsa + south66 + I(south*south66) | black + south + smsa + south66 + I(south*south66) + instr1_ed + instr2_ed + instr1_exp + instr2_exp + instr3_exp, data = data_clean)
summary(iv_reg, diagnostics = TRUE)
coeftest(iv_reg, vcov = vcovHC(iv_reg, type = "HC3"))

# fail to reject null based on coeff on v, so educ can be exog and instrument invalid, so do a sargan test (use when model is overidentified)
iv <- ivreg(lnwage ~ educ + exper + expersq + black + south + smsa |  black + south + smsa+ instr2_ed + instr1_ed + instr1_exp + instr2_exp + instr3_exp, data = data_clean)
res_iv <- iv$residuals
sargan <- lm(res_iv ~ black + south + smsa  + married + instr2_ed + instr1_ed + instr1_exp + instr2_exp + instr3_exp, data = data_clean)
summary(sargan)
# N*R^2 = 2981*0.0003662 = 1.18 compare to chi sq, dof = overidentifying instruments = 1

strong1 <- lm(educ ~ exper + expersq + black + south + smsa + south66 + I(south*south66)+ instr2_ed + instr1_ed, data = data_clean)
summary(strong1)
Hnull <-c("instr1_ed=0","instr2_ed=0")
linearHypothesis(strong1, Hnull, test="F")
# F-stat is 19.7 > 10 --> strong instrument

strong2 <- lm(exper ~ educ + black + south + smsa + south66 + I(south*south66) + instr1_exp + instr2_exp + instr3_exp, data = data_clean)
summary(strong2)
Hnull2 <-c("instr1_exp=0","instr2_exp=0", "instr3_exp=0")
linearHypothesis(strong2, Hnull2, test="F")
# F-stat is  < 10 --> not strong instrument

strong3 <- lm(expersq ~ educ + black + south + smsa + south66 + I(south*south66) + instr1_exp + instr2_exp + instr3_exp, data = data_clean)
summary(strong3)
Hnull3 <-c("instr1_exp=0","instr2_exp=0","instr3_exp=0")
linearHypothesis(strong3, Hnull3, test="F")
# F-stat is  < 10 --> not strong instrument
