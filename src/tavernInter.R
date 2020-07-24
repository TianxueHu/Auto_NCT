setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/TAVERN/chordtone_data')
rm(list = ls())
library(ROCR)

data_train<-read.csv('TAVERN_Factors.csv',header=TRUE, sep=",")[3:28]

for (i in 1:nrow(data_train)){
  if (data_train$ArrSkiSteLeap[i] == "skip"){
    data_train$ArrSkiSteLeap[i]<-"leap"
  }
}
for (i in 1:nrow(data_train)){
  if (data_train$DepSkiSteLeap[i] == "skip"){
    data_train$DepSkiSteLeap[i]<-"leap"
  }
}

data1<-data_train
train1 <- data1[1:36239,] 
test1 <- data1[36239:45299,]
model <- glm(Response ~ duration * ArrSkiSteLeap * DepSkiSteLeap * onOffBeat ,family=binomial(link='logit'),data=train1)
summary(model)
nothing<-glm(Response ~ 1 ,family=binomial(link='logit'),data=train1)

#######################forward stepwise############
##ranking
forwards = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(model)), direction="forward")

# Step:  AIC=34871.22
# Response ~ DepSkiSteLeap + ArrSkiSteLeap + duration + onOffBeat + 
#   ArrSkiSteLeap:duration + DepSkiSteLeap:ArrSkiSteLeap + ArrSkiSteLeap:onOffBeat + 
#   DepSkiSteLeap:onOffBeat + DepSkiSteLeap:duration + duration:onOffBeat + 
#   DepSkiSteLeap:ArrSkiSteLeap:onOffBeat
# 
# Df Deviance   AIC
# <none>                                       34823 34871
# + duration:ArrSkiSteLeap:DepSkiSteLeap  4    34817 34873
# + duration:DepSkiSteLeap:onOffBeat      2    34822 34874
# + duration:ArrSkiSteLeap:onOffBeat      2    34822 34874



model1 <- glm(Response ~ DepSkiSteLeap + ArrSkiSteLeap + duration + onOffBeat + 
                   ArrSkiSteLeap:duration + DepSkiSteLeap:ArrSkiSteLeap + ArrSkiSteLeap:onOffBeat + 
                   DepSkiSteLeap:onOffBeat + DepSkiSteLeap:duration + duration:onOffBeat + 
                   DepSkiSteLeap:ArrSkiSteLeap:onOffBeat ,family=binomial(link='logit'),data=train1)
summary(model1)
summary(model1_1)

anova(model1, test="Chisq")

#accuracy on test set
fitted.results1 <- predict(model1,newdata=subset(test1,select=c(6,19,21,22)),type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.46,1,0) #####!!!choose threshold
misClasificError1 <- mean(fitted.results1 != test1$Response)
table(fitted.results1,test1$Response)
print(paste('Accuracy',1-misClasificError1))
table(test1$Response)

#[1] "Accuracy 0.753669572894824"



p1 <- predict(model1, newdata=subset(test1,select=c(6,19,21,22)), type="response")
pr1 <- prediction(p1, test1$Response)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1

