setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/TAVERN/chordtone_data')
rm(list = ls())
library(FactoMineR)
library(ResourceSelection)
library(ROCR)

###########################################MODELS START HERE#####################


#check if empty token

######## Logistic Regression with different columns ##########

dataframe<-read.csv('TAVERN_Factors.csv',header=TRUE, sep=",")
dataframe<-dataframe[2:24]

#Columns:

# 'beat_pos' - col 1
# 'duration' - col 2
# "ArrNum" & "DepNum" col 4 & 5 (take abs())
# "onOffBeat" - beat_pos: onBeat/offBeat - col 6
# "BeatVal" - beat_pos: 0 & 0.25 % 0.5 & 1 - col 7
# "ArrSkiSteLeap" & "DepSkiSteLeap" - Intervals: Skip, step, leap - col 8 & 9
# "IntBeats" - Catagorize strong beat positions to 1,2,3,4, others are 10 - col10
# "ArrDir" & "DepDir" - interval direction - col 11 & 12
# !!!RESOPONSE - col 3



# MODEL 2!


train <- dataframe[1:32000,] 
test <- dataframe[32001:40156,]
prop.table(table(test$response))
model <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat ,family=binomial(link='logit'),data=train)
summary(model)
hoslem.test(train$response, fitted(model))


#### NAT ADDED THIS: predict(model, newdata = test, 'response')

#anova() function on the model to analyze the table of deviance
anova(model, test="Chisq")

graph <- unique(dataframe[, c('onOffBeat', 'ArrSkiSteLeap','DepSkiSteLeap', 'ArrDir', 'DepDir')])
graph
#accuracy on test set
fitted.results <- predict(model,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

###PICK MODEL 2, FIND INTERACTION #####
#A-duration B-abs(ArrNum) C-abs(DepNum) D-onOffBeat


##try a step wise regression
library("olsrr")
lmMod <- glm(response ~ duration + ArrSkiSteLeap * DepSkiSteLeap * onOffBeat , family=binomial(link='logit'), data = train)
#md<-ols_step_backward_aic(lmMod)
#md <- step(lmMod, direction = "backward", trace = 1)
#drop1(lmMod, test = "LRT")
#summary(lmMod)
#anova(lmMod)
##
fitted.results <- predict(md,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(md, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


###another one
library(MASS)
mod <- glm(response ~ duration + ArrSkiSteLeap * DepSkiSteLeap * onOffBeat , family=binomial(link='logit'), data = train)
bc <- step(mod)
formula(bc)
summary(bc)
#try another backwards step wise
# Now using the code below to perform 5 fold CV
library(ggplot2)
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

lmFit_Step <- train(response ~ duration * ArrSkiSteLeap * DepSkiSteLeap * onOffBeat, data = train, "lmStepAIC", scope = 
                      list(lower = response~1, upper = response~.), direction = "backward",trControl=ctrl)

#Fitting a new model with these 8 variables

mod_Step = lm(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data = scaledData)
summary(mod_Step)




# + A:B for two way interaction, is this is best A+B+C+D+A:B+A:C
#put all modesls in anova

####2-WAY INTERACTIONS########

##Model 2_1 + A:B
model2_1 <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat +duration:ArrSkiSteLeap + duration:DepSkiSteLeap+ ArrSkiSteLeap:DepSkiSteLeap +
                  duration:onOffBeat + ArrSkiSteLeap:onOffBeat+ DepSkiSteLeap:onOffBeat + duration:ArrSkiSteLeap:DepSkiSteLeap + duration:ArrSkiSteLeap:onOffBeat 
                + ArrSkiSteLeap:DepSkiSteLeap:onOffBeat +duration:ArrSkiSteLeap:DepSkiSteLeap:onOffBeat  
                ,family=binomial(link='logit'),data=train)
summary(model2_1)
#hoslem.test(train$response, fitted(model2_1))

#anova() function on the model to analyze the table of deviance
anova(model2_1, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2_1,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))


#ROC and AUC
#look at every threshold, plot
p <- predict(model2_1, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##Model 2_2 + A:C
model2_2 <- glm(response ~ duration * ArrSkiSteLeap * DepSkiSteLeap * onOffBeat ,family=binomial(link='logit'),data=train)
summary(model2_2)
#hoslem.test(train$response, fitted(model2_1))

#anova() function on the model to analyze the table of deviance
anova(model2_2, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2_2,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model2_2, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##Model 2_3 + A:D
model2_3 <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat + duration : onOffBeat,family=binomial(link='logit'),data=train)
summary(model2_3)
#hoslem.test(train$response, fitted(model2_1))

#anova() function on the model to analyze the table of deviance
anova(model2_3, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2_3,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model2_3, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##Model 2_4+ B:C
model2_4 <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat + ArrSkiSteLeap : DepSkiSteLeap,family=binomial(link='logit'),data=train)
summary(model2_4)
#hoslem.test(train$response, fitted(model2_1))

#anova() function on the model to analyze the table of deviance
anova(model2_4, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2_4,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model2_4, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##Model 2_5+ B:D
model2_5 <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat + ArrSkiSteLeap : onOffBeat,family=binomial(link='logit'),data=train)
summary(model2_5)
#hoslem.test(train$response, fitted(model2_1))

#anova() function on the model to analyze the table of deviance
anova(model2_5, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2_5,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model2_5, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##Model 2_6+C:D
model2_6 <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat + DepSkiSteLeap : onOffBeat,family=binomial(link='logit'),data=train)
summary(model2_6)

#anova() function on the model to analyze the table of deviance
anova(model2_6, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2_6,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model2_6, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


####3-WAY INTERACTIONS########

##Model 3_1 + A:B:C
model2_1 <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat + duration : ArrSkiSteLeap ,family=binomial(link='logit'),data=train)
summary(model2_1)
#hoslem.test(train$response, fitted(model2_1))

#anova() function on the model to analyze the table of deviance
anova(model2_1, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2_1,newdata=subset(test,select=c(2,8,9,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model2_1, newdata=subset(test,select=c(2,8,9,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

























# MODEL 1!
model2 <- glm(response ~ duration + abs(ArrNum) + abs(DepNum) + onOffBeat ,family=binomial(link='logit'),data=train)
summary(model2)
hoslem.test(train$response, fitted(model2))

#anova() function on the model to analyze the table of deviance
anova(model2, test="Chisq")


#accuracy on test set
fitted.results <- predict(model2,newdata=subset(test,select=c(2,4,5,6)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model2, newdata=subset(test,select=c(2,4,5,6)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##model 3
model3 <- glm(response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + BeatVal ,family=binomial(link='logit'),data=train)
summary(model3)
hoslem.test(train$response, fitted(model3))

#anova() function on the model to analyze the table of deviance
anova(model3, test="Chisq")


#accuracy on test set
fitted.results <- predict(model3,newdata=subset(test,select=c(2,8,9,10)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)


print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model3, newdata=subset(test,select=c(2,8,9,10)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


##Model 4 
model4 <- glm(response ~ duration + onOffBeat + ArrDir + DepDir,family=binomial(link='logit'),data=train)
summary(model4)

hoslem.test(train$response, fitted(model4))

#anova() function on the model to analyze the table of deviance
anova(model4, test="Chisq")


#accuracy on test set
fitted.results <- predict(model4,newdata=subset(test,select=c(2,6,11,12)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
#look at every threshold, plot
p <- predict(model4, newdata=subset(test,select=c(2,6,11,12)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
