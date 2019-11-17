options(useFancyQuotes = 'False')
library(tableone)
library(emmeans)
library(lsmeans)
library(ggplot2)
#library(car)
#library(olsrr)
library(reshape)
library(lme4)
library(lmerTest)
library(margins)
library(nlme)
library(sjPlot)
library(sjmisc)
library(survival)
library(ipw)
library(MatchIt)
library(lattice)
library(Formula)
library(Hmisc)
library(splines)
library(IPWsurvival)
library(ggpubr)
library(magrittr)
library(survminer)
# singal variable
df <- read.csv("C:/Users/310293241/Desktop/datathon/case_study_data.csv", header = TRUE, sep = ",",na.strings=c("","NA"))
head(df)
cox <- coxph(Surv(hospitaldischargeoffset,unitdischargestatus ) ~ crp_alb, data = df)
summary(cox)

# multi-variables
# age
cox_1 <- coxph(Surv(hospitaldischargeoffset,unitdischargestatus ) ~ crp_alb + age, data = df)
summary(cox_1)

# age+gender
cox_2 <- coxph(Surv(hospitaldischargeoffset,unitdischargestatus ) ~ crp_alb + age + gender, data = df)
summary(cox_2)

# more variables
cox_3 <- coxph(Surv(hospitaldischargeoffset,unitdischargestatus ) ~ crp_alb  + gender    +
                 ALT..SGPT. + AST..SGOT.  + WBC.x.1000 + creatinine  + total.cholesterol + triglycerides, data = df)
summary(cox_3)
# age+gender+apachescore
#cox_3 <- coxph(Surv(hospitaldischargeoffset,unitdischargestatus ) ~ crp_alb + age + gender+apachescore, data = df)
#summary(cox_3)

# plot k-m graph
fu_time <- df[,"hospitaldischargeoffset"]
death <- df[,"unitdischargestatus"]
three_qua <- df[,"three_qua"]
#cox.zph(fit, transform="km", global=TRUE)
#fit <- coxph(Surv(fu_time, death) ~ three_qua) # fit the desired model
#temp <- cox.zph(fit)# apply the cox.zph function to the desired model
#print(temp) # display the results
#plot(temp) # plot the curves
#km_fit <- survfit(Surv(fu_time, death) ~ three_qua) 
#autoplot(km_fit)
#plot(km_fit, xlab = "time", ylab = "Survival probability") # label the axes 


require("survival")
fit <- survfit(Surv(fu_time, death) ~ three_qua, data = df)
ggsurvplot(fit, data = df)

ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = df,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimates of survival curves.
  xlim = c(0,200),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 100,     # break X axis in time intervals by 500.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
  # in legend of risk table
)