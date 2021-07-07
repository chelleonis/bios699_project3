#should also imputate
#prepare data partition, testing step and 

library(tableone)
library(survival)
library(caret)
library(mice)
library(survminer)

hn_can <- read.csv("C:/Users/typer321/Desktop/p3.csv")

#descriptive stuff

vars <- c("how_old","gender","race","highest_ed","marital_status",
          "deathstatus","recurstatus","stime","rtime",
          "Aspirin","othchol","statin","metformin","Insulin",
          "cod", "bmi","smoker")

CreateTableOne(vars, data = hn_can)

#basic (non-parametric, kaplan-meier)
# cnnot estimate survival considering covariates
#so it's pretty much useless for this study lol....
test1 <- survfit(Surv(stime, deathstatus)~1, conf.int = 0.95, data = hn_can)
plot(test1)

survdiff(Surv(stime,deathstatus)~Aspirin,data = hn_can) 
#comparing to medicine groups lol could end here xd

#cox regression model is the base we should use
#cox prop method

survthing <- Surv(hn_can$stime,hn_can$deathstatus)

#default efron, (breslow and exact other methods)
cox_test <- coxph(survthing ~ gender + how_old + Aspirin + statin, method = "breslow",
                  data = hn_can)
summary(cox_test)

cox.coxer <- survfit(cox_test)
plot(cox.coxer)

ggforest(cox_test, data = hn_can)


#main question is to differentiate common drug users from non
#then we can fit a full model
