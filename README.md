# Metrics_Project

dataset <- read_xlsx("earnings.xlsx")

summary(dataset)   # to get summary stats of all dataset

data_clean <- subset(dataset, wage != 0)

datasummary_skim(data_clean, output = "default", histogram=T)

data_clean$lnwage = log(data_clean$wage)

ols = lm (lnwage ~ educ + exper + I(exper^2) + black + south + smsa + smsa66 + I(smsa*smsa66) + married, data = data_clean) # should we add married
summary(ols)

# educ - momdad14 (parents at home, not dealing with finances, able to focus on school), south (education in the south not as strong) 
# exper & exper^2 - nearc4 (more educated people in the neighborhood to learn from and work under), enroll (in school later = less experience) 
