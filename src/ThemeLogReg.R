setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/TAVERN-update/chordtone_data')
rm(list = ls())
library(ROCR)
data_test<-read.csv('TAVERN_Factors.csv',header=TRUE, sep=",")[2:24]
data_train<-read.csv('THEME_Factors.csv',header=TRUE, sep=",")[3:25]

data_0<-subset(data_train,data_train$Response==0)
data_0<- subset(data_0,select=c(6,18,19,16))
data_1<-subset(data_train,data_train$Response==1)
data_1<- subset(data_1,select=c(6,18,19,16))

with(data_0, table(paste(ArrSkiSteLeap, DepSkiSteLeap, onOffBeat )))
with(data_1, table(paste(ArrSkiSteLeap, DepSkiSteLeap, onOffBeat )))
######################################Logistic Regression#######################

#####train - themes, test - tavern ####
model1 <- glm(Response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat ,family=binomial(link='logit'),data=data_train)
model1_1  <- glm(Response ~ duration * ArrSkiSteLeap * DepSkiSteLeap * onOffBeat ,family=binomial(link='logit'),data=data_train)
summary(model1)
summary(model1_1)

anova(model1, test="Chisq")

#accuracy on test set
fitted.results1 <- predict(model1,newdata=subset(data_test,select=c(6,18,19,16)),type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.55,1,0) #####!!!choose threshold
misClasificError1 <- mean(fitted.results1 != data_test$Response)
table(fitted.results1,data_test$Response)

print(paste('Accuracy',1-misClasificError1))

#ROC and AUC
p1 <- predict(model1, newdata=subset(data_test,select=c(6,18,19,16)), type="response")
pr1 <- prediction(p1, data_test$Response)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1



######## train and test on themes ###
theme_train = data_train[1:1488,]
theme_test = data_train[1489:1860,]
data_test$DepNum
model2 <- glm(Response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat,family=binomial(link='logit'),data=theme_train)
model2_1  <- glm(Response ~ duration * ArrSkiSteLeap * DepSkiSteLeap * onOffBeat ,family=binomial(link='logit'),data=theme_train)
summary(model2)
summary(model2_1)

anova(model2, test="Chisq")

#accuracy on test set
fitted.results1 <- predict(model2_1,newdata=subset(theme_test,select=c(6,18,19,16)),type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.55,1,0) #####!!!choose threshold
misClasificError1 <- mean(fitted.results1 != (theme_test$Response))
table(fitted.results1,theme_test$Response)

print(paste('Accuracy',1-misClasificError1))

#ROC and AUC
p1 <- predict(model2_1, newdata=subset(theme_test,select=c(6,18,19,16)), type="response")
pr1 <- prediction(p1, theme_test$Response)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1
