library(lmtest)
library(sandwich)
library(AER)
library("readxl")

# 4. ===========================================================================
cps04 <- read_excel("cps04.xlsx")


# (a) ==========================================================================

# i. ===========================================================================
# U is the error term. It represents the unobserved determinants. It is not 
# uncorrelated with the regressors, e.g wealth affects likelihood of finishing
# high school and finishing college.

# ii. ==========================================================================
# beta_0 is the intercept for the model 
# beta_1 average difference in wages if a person is female
# beta_2 average difference in wages if a person completed college
# beta_3 average difference in wages if a person completed high school

# ==============================================================================
# For rest of (a) and (b) Suppose U is uncorrelated with the regressors and that
# the fourth moments of the dependent variable and each of the regressors exist.

# i. ===========================================================================

cps04_lm1 <- lm(cps04$AHE ~ 1 + Female + College + HS, data = cps)
summary(cps04_lm1)

# Estimate of the model using OLS:
# wage = 12.89 - 4.08fem + 13.26college + 5.30hs

fem_wage <- subset(cps04$AHE, cps04$Female == 1)

male_wage <- subset(cps04$AHE, cps04$Female == 0)

t.test(fem_wage, male_wage, conf.level = 0.95)

# the 95% CI for the effect of being female on income,
# (-3.90, -3.59)
# This means being female results in an expected income that is between -3.90
# and -3.59 less than their male counterparts

# ii. ==========================================================================


# A. ===========================================================================

# Theta is the difference in the effect of completing college and high school 
# vs. only completing high school on wage. 

# B. ===========================================================================
# 

# C. ==========================================================================
# 

# iii. =========================================================================

# (b) ==========================================================================

# (c) ==========================================================================

# 5. ===========================================================================
fertility <- read_excel("fertility.xlsx")

# (a) ==========================================================================
weeksm_lm_mk <- lm(fertility$weeksm1 ~ 1 + fertility$morekids)
summary(weeksm_lm_mk)

ww_more_kids <- subset(fertility$weeksm1, fertility$morekids == 1)

ww_less_kids <- subset(fertility$weeksm1, fertility$morekids == 0)

t.test(ww_more_kids, ww_less_kids)

# Note: 
#     mean hours worked for women with more than 2 kids = 15.68
#     mean hours worked for women with more than 2 kids = 21.07
#     
# Yes, on average women with more than two children work 5.39 less hours than 
# women with less than two children.


# (b) ==========================================================================
# There are many unobserved factors/determinants that make the OLS regression 
# estimated in part (a) inappropriate. For example, wealthier women would be able
# to hire a baby sitter to watch over the kids during the day and thus work more. 
# Therefore if a wealthier family were to have 3 kids, this would be 
# contradicting to the results found in our OLS regression estimation

# (c) ==========================================================================
ss_more_kids <- subset(fertility$samesex == 1, fertility$morekids == 1)
ss_less_kids <- subset(fertility$samesex == 1, fertility$morekids == 0)

weeksm_lm_ss <- lm(fertility$weeksm1 ~ 1 + fertility$samesex)
summary(weeksm_lm_ss)

t.test(ss_more_kids,ss_less_kids)


# Couples whose first two children are of the same sex are more likely to have a 
# third child, than couples whose first two children are not the same sex. 
# The probability that a couple with its first two children being same sex has a 
# third child is 54.99%. While the probability that a couple with its first two 
# children being same sex not having has a third child is 47.83%. 
# The effect is large: 0.01431 - 9.502E-05 = 0.01421.

# This is statistically significant given the p-value ( < 2.2e-16 ) is less than 
# 0.05.

# (d) ==========================================================================
# Samesex may be a valid instrument for the IV regression of weeksworked on 
# morekids because samesex has an effect on morekids, however, it does not have 
# a direct effect on weeksworked. Therefore cov(samesex, morekids) =/= 0

# (e) ==========================================================================

ww_iv_reg <- ivreg(fertility$weeksm1 ~ 1 + fertility$morekids | fertility$samesex)
summary(ww_iv_reg)

# The fertility effect on labor supply is small: 0.01431 - 0.01388 = 0.00043

# (f) ==========================================================================

weeksm_lm_2 <- lm(fertility$weeksm1 ~ 1 + fertility$morekids
                                        + fertility$agem1
                                        + fertility$black
                                        + fertility$hispan
                                        + fertility$othrace)
summary(weeksm_lm_2)

# Yes, our results change when we add the variables agem1, black, hispan, and 
# othrace in the labor supply regression (the multiple R-squared is bigger). This 
# is a result of the additional regressors improving the fit of the model.



