library(causaldata)
library(survival)
library(ggplot2)
library(tidyverse)
library(ggsurvfit)
library(timereg)
nhefs = causaldata::nhefs_complete

table(nhefs$qsmk)
hist(nhefs$smokeintensity)

qsmk_mod = glm(qsmk~wt71 + income + school + race + sex + age + smokeintensity + smokeyrs + price82 + tax82,
               data = nhefs, family = binomial)

summary(qsmk_mod)

qsmk_mod_2 = glm(qsmk~race*sex*age*smokeintensity,
               data = nhefs, family = binomial)
summary(qsmk_mod_2)

nhefs %>% group_by(race) %>% summarise(qsmk = sum(qsmk),
                                       nqsmk = sum)

start_date = ISOdate(1983,1,1)
nhefs$death_date = ISOdate(paste0(19,nhefs$yrdth),nhefs$modth,nhefs$dadth)
len_study = as.numeric(ISOdate(1993,1,1) - start_date)
nhefs$surv = as.numeric(nhefs$death_date - start_date)
nhefs$surv[is.na(nhefs$surv)] = len_study


survfit2(Surv(as.numeric(surv),death) ~ 1, data = nhefs) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

survfit2(Surv(as.numeric(surv),death) ~ qsmk, data = nhefs) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

survfit2(Surv(as.numeric(surv),death) ~ race, data = nhefs) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

survfit2(Surv(as.numeric(surv),death) ~ active, data = nhefs) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

ggplot(nhefs) + geom_histogram(aes(x = age,colour = factor(qsmk), y = ..density..))

quantile(nhefs$age[nhefs$qsmk == 1])
quantile(nhefs$age[nhefs$qsmk == 0])

nhefs$survival = with(nhefs,Surv(surv,death))
write.csv(nhefs,"~/R/win-library/4.0/Causal Survival/nhefs.csv")

aalen_model = aareg(survival~wt71 + income + sex + age + smokeintensity + smokeyrs + qsmk,
      data = nhefs)

summary(aalen_model)
plot(aalen_model)
