setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/TAVERN/chordtone_data')
rm(list = ls())
library(ROCR)
library(caret)
dataframe<-read.csv('TAVERN_Factors.csv',header=TRUE, sep=",")
dataframe<-dataframe[2:24]
table(dataframe$Response)


######################################Logistic Regression#######################


###### MODEL 1: beat_pos: onBeat/offBeat
data1 <- dataframe

train1 <- data1[1:36239,] 
test1 <- data1[36239:45299,]

table(data1$onOffBeat)
model1 <- glm(Response ~ duration + ArrSkiSteLeap + DepSkiSteLeap+ onOffBeat ,family=binomial(link='logit'),data=train1)
model1_1  <- glm(Response ~ duration * ArrSkiSteLeap * DepSkiSteLeap * onOffBeat ,family=binomial(link='logit'),data=train1)
summary(model1)
summary(model1_1)

anova(model1, test="Chisq")

#accuracy on test set
fitted.results1 <- predict(model1,newdata=subset(test1,select=c(7,20,22,23)),type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.50,1,0) #####!!!choose threshold
misClasificError1 <- mean(fitted.results1 != test1$Response)
table(fitted.results1,test1$Response)
print(paste('Accuracy',1-misClasificError1))
table(test1$Response)

#ROC and AUC

#look at every threshold, plot

p1 <- predict(model1, newdata=subset(test1,select=c(7,20,22,23)), type="response")
pr1 <- prediction(p1, test1$Response)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1


###### MODEL 2: beat_pos: 0 & 0.25 % 0.5 & 1
data2 <- data
BeatVal <- c("BeatVal") #an empty list record result
data2[,BeatVal] <- NA 
for (i in 1:nrow(data)){
  if (as.numeric(data$beat_pos[i]) %% 1 == 0){
    data2$BeatVal[i] <- "beat1"
  } else if(as.numeric(data$beat_pos[i]) %% 0.5== 0){
    data2$BeatVal[i] <- "beat0.5"
  }else if(as.numeric(data$beat_pos[i]) %% 0.25== 0){
    data2$BeatVal[i] <- "beat0.25"
  } else {
    data2$BeatVal[i] <- "others"
  }
}
data2$BeatVal <-factor(data2$BeatVal)
train2 <- data2[1:32000,] 
test2 <- data2[32001:40156,]
model2 <- glm(response ~ duration + abs(ArrNum) + abs(DepNum) + BeatVal ,family=binomial(link='logit'),data=train2)
summary(model2)
model2_1  <- glm(response ~ duration * abs(ArrNum) * abs(DepNum) * BeatVal ,family=binomial(link='logit'),data=train2)
summary(model2_1)

anova(model2, test="Chisq")

#accuracy on test set
fitted.results2 <- predict(model2,newdata=subset(test2,select=c(2,4,5,6)),type='response')
fitted.results2 <- ifelse(fitted.results2 > 0.5,1,0) #####!!!choose threshold

misClasificError2 <- mean(fitted.results2 != test2$response)
table(fitted.results2,test2$response)

print(paste('Accuracy',1-misClasificError2))

#ROC and AUC
p2 <- predict(model2, newdata=subset(test2,select=c(2,4,5,6)), type="response")
pr2 <- prediction(p2, test2$response)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)

auc2 <- performance(pr2, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2


for (x in -10:10){
  n<-abs(x) == 2 | abs(x) == 1
  print(n)
}


########## MODEL 3: Intervals: Skip, step, leap with on/off beat (data from model1)
data3 <- data1
arrSkiSteLeap<- c("ArrSkiSteLeap") #an empty list record result
data3[,arrSkiSteLeap] <- NA 
depSkiSteLeap<- c("DepSkiSteLeap") #an empty list record result
data3[,depSkiSteLeap] <- NA 
for (i in 1:nrow(data)){
  if (abs(as.numeric(data$ArrNum[i]))==2 | abs(as.numeric(data$ArrNum[i]))==1){
    data3$ArrSkiSteLeap[i] <- "step"
  } else if (abs(as.numeric(data$ArrNum[i]))==3 | abs(as.numeric(data$ArrNum[i]))==4){
    data3$ArrSkiSteLeap[i] <- "skip"
  }else if (abs(as.numeric(data$ArrNum[i]))==0){
    data3$ArrSkiSteLeap[i] <- "unison"
  } else {
    data3$ArrSkiSteLeap[i] <- "leap"
  }
  
  if (abs(as.numeric(data$DepNum[i]))==2 | abs(as.numeric(data$DepNum[i]))==1){
    data3$DepSkiSteLeap[i] <- "step"
  } else if (abs(as.numeric(data$DepNum[i]))==3 | abs(as.numeric(data$DepNum[i]))==4){
    data3$DepSkiSteLeap[i] <- "skip"
  }else if (abs(as.numeric(data$DepNum[i]))==0){
    data3$DepSkiSteLeap[i] <- "unison"
  } else {
    data3$DepSkiSteLeap[i] <- "leap"
  }
}


data3$onOffBeat <-factor(data3$onOffBeat)
train3 <- data3[1:32000,] 
test3 <- data3[32001:40156,]
model3 <- glm(response ~ duration + DepSkiSteLeap + ArrSkiSteLeap + onOffBeat,family=binomial(link='logit'),data=train3)
summary(model3)
model3_1 <- glm(response ~ duration * DepSkiSteLeap * ArrSkiSteLeap * onOffBeat,family=binomial(link='logit'),data=train3)
summary(model3_1)

anova(model3, test="Chisq")

#accuracy on test set
fitted.results3 <- predict(model3,newdata=subset(test3,select=c(2,6,7,8)),type='response')
fitted.results3 <- ifelse(fitted.results3 > 0.5,1,0) #####!!!choose threshold

misClasificError3 <- mean(fitted.results3 != test3$response)
table(fitted.results3,test3$response)

print(paste('Accuracy',1-misClasificError3))

#ROC and AUC
p3 <- predict(model3, newdata=subset(test3,select=c(2,6,7,8)), type="response")
pr3 <- prediction(p3, test3$response)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
plot(prf3)

auc3 <- performance(pr3, measure = "auc")
auc3 <- auc3@y.values[[1]]
auc3


############### MODEL4: Comparing beat positions of strong beats such as 1,2,3,4, build upon model 3.
data4 <- data3
IntBeats<- c("IntBeats") #an empty list record result
data4[,IntBeats] <- NA 

for (i in 1:nrow(data)){
  if (as.numeric(data$beat_pos[i]) %% 1 == 0){
    data4$IntBeats[i] <- as.numeric(data$beat_pos[i])
  }else{
    data4$IntBeats[i] <-'10'
  }
}

####Scenario 1 - treat integer beats as catagories, offBeats are of catagory '10' ##
data4_1<- data4
data4_1$IntBeats <-factor(data4_1$IntBeats)
train4_1 <- data4_1[1:32000,] 
test4_1 <- data4_1[32001:40156,]
model4_1 <- glm(response ~ duration + DepSkiSteLeap + ArrSkiSteLeap + IntBeats,family=binomial(link='logit'),data=train4_1)
summary(model4_1)
model4_1_1 <- glm(response ~ duration * DepSkiSteLeap * ArrSkiSteLeap * IntBeats,family=binomial(link='logit'),data=train4_1)
summary(model4_1_1)

anova(model4_1, test="Chisq")

#accuracy on test set
fitted.results4_1 <- predict(model4_1,newdata=subset(test4_1,select=c(2,7,8,9)),type='response')
fitted.results4_1 <- ifelse(fitted.results4_1 > 0.5,1,0) #####!!!choose threshold

misClasificError4_1 <- mean(fitted.results4_1 != test4_1$response)
table(fitted.results4_1,test4_1$response)

print(paste('Accuracy',1-misClasificError4_1))

#ROC and AUC
p4_1 <- predict(model4_1, newdata=subset(test4_1,select=c(2,7,8,9)), type="response")
pr4_1 <- prediction(p4_1, test4_1$response)
prf4_1 <- performance(pr4_1, measure = "tpr", x.measure = "fpr")
plot(prf4_1)

auc4_1 <- performance(pr4_1, measure = "auc")
auc4_1 <- auc4_1@y.values[[1]]
auc4_1

#####Scenario 2 - treat integer beat position linearly. Toss data that are not on-beat.








data$beat_pos <- factor(data$beat_pos) #treating as catagorical data
#anova() function on the model to analyze the table of deviance
anova(model1, test="Chisq")


###### MODEL 2: treating all beat_pos as different catagories ##
data$beat_pos <- factor(data$beat_pos) #treating as catagorical data
model1 <- glm(response ~ duration * abs(ArrNum) * abs(DepNum) ,family=binomial(link='logit'),data=train)
summary(model1)

#anova() function on the model to analyze the table of deviance
anova(model1, test="Chisq")








#accuracy on test set
fitted.results <- predict(model,newdata=subset(test,select=c(1,2,4,5)),type='response')
fitted.results <- ifelse(fitted.results > 0.9,1,0)

misClasificError <- mean(fitted.results != test$response)
table(fitted.results,test$response)

print(paste('Accuracy',1-misClasificError))

#ROC and AUC
library(ROCR)
#look at every threshold, plot

p <- predict(model, newdata=subset(test,select=c(1,2,4,5)), type="response")
pr <- prediction(p, test$response)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
