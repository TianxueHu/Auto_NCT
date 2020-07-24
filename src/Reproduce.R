##REPRODUCING

setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/Tavern/chordtone_data')
rm(list = ls())
library(ROCR)

data_test1<-read.csv('TAVERN_Factors.csv',header=TRUE, sep=",")[3:28]
data_test2<-read.csv('HAYDN_Factors.csv',header=TRUE, sep=",")[3:25]
data_train<-read.csv('THEME_Factors.csv',header=TRUE, sep=",")[3:28]

####processing
for (i in 1:nrow(data_test1)){
  if (data_test1$ArrSkiSteLeap[i] == "skip"){
    data_test1$ArrSkiSteLeap[i]<-"leap"
  }
}
for (i in 1:nrow(data_test1)){
  if (data_test1$DepSkiSteLeap[i] == "skip"){
    data_test1$DepSkiSteLeap[i]<-"leap"
  }
}
for (i in 1:nrow(data_test2)){
  if (data_test2$ArrSkiSteLeap[i] == "skip"){
    data_test2$ArrSkiSteLeap[i]<-"leap"
  }
}
for (i in 1:nrow(data_test2)){
  if (data_test2$DepSkiSteLeap[i] == "skip"){
    data_test2$DepSkiSteLeap[i]<-"leap"
  }
}
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

data_test1 <- data_test1[36239:45299,]
#* DepByRest * ArrByRest 
model <- glm(Response ~ duration * ArrSkiSteLeap * DepSkiSteLeap * onOffBeat ,family=binomial(link='logit'),data=data_train)
summary(model)
nothing<-glm(Response ~ 1 ,family=binomial(link='logit'),data=data_train)

#######################forward stepwise############
##ranking
forwards = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(model)), direction="forward")
#Step:  AIC=1202.98
#Response ~ DepSkiSteLeap + ArrSkiSteLeap + onOffBeat + duration + 
#  ArrSkiSteLeap:onOffBeat + DepSkiSteLeap:onOffBeat + DepSkiSteLeap:duration

#Df Deviance    AIC
#<none>                             1177.0 1203.0
#+ ArrSkiSteLeap:DepSkiSteLeap  4   1169.5 1203.5
#+ duration:onOffBeat           1   1176.3 1204.3
#+ duration:ArrSkiSteLeap       2   1176.3 1206.3


##Backward step
backwards = step(model) 
#shows overfit warnings


####FINAL MODEL!!!#######
model <- glm(Response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat+ArrSkiSteLeap:onOffBeat+DepSkiSteLeap:onOffBeat+DepSkiSteLeap:duration,family=binomial(link='logit'),data=data_train)
##save model to file
saveRDS(model, "final_model.rds")

########################## test on TAVERN ##########

fitted.results <- predict(model,newdata=subset(data_test1,select=c(6,19,21,22)),type='response') 
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != data_test1$Response)
table(fitted.results,data_test1$Response)
#print(paste('result',i))
print(paste('Accuracy',1-misClasificError))

p1 <- predict(model, newdata=subset(data_test1,elect=c(6,19,21,22)), type="response") #Tavern
pr1 <- prediction(p1, data_test1$Response)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1

###TEST ON TAVERN Summary
###[1] "Accuracy 0.763883605933815"
### AUC= 0.776566

########################## test on HAYDN ##########

fitted.results <- predict(model,newdata=subset(data_test2,select=c(5,16,19,18)),type='response') #HAYDN
#which( colnames(data_test2)=="onOffBeat" )
fitted.results <- ifelse(fitted.results > 0.46,1,0)
misClasificError <- mean(fitted.results != data_test2$Response)
table(fitted.results,data_test2$Response)
#print(paste('result',i))
print(paste('Accuracy',1-misClasificError))

p1 <- predict(model, newdata=subset(data_test2,select=c(5,16,19,18)), type="response") #haydn
pr1 <- prediction(p1, data_test2$Response)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
#plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1

###TEST ON HAYDN
###[1] "Accuracy 0.706008243500317"
### AUC=0.6852714


############Model evaluation on themes######
#Randomly shuffle the data
data_train<-data_train[sample(nrow(data_train)),]
##train test split
train<-data_train[1:1613,] 
test<-data_train[1614:2017,]
table(test$Response)
model <- glm(Response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat+ArrSkiSteLeap:onOffBeat+DepSkiSteLeap:onOffBeat+DepSkiSteLeap:duration,family=binomial(link='logit'),data=train)
fitted.results <- predict(model,newdata=subset(test,select=c(6,19,21,22)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Response)
print(paste('Accuracy',1-misClasificError))

p1 <- predict(model, newdata=subset(test,select=c(6,19,21,22)), type="response") #haydn
pr1 <- prediction(p1, test$Response)
prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
#plot(prf1)

auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1

TN<-table(fitted.results,test$Response)[1,1]
FP<-table(fitted.results,test$Response)[1,2]
FN<-table(fitted.results,test$Response)[2,1]
TP<-table(fitted.results,test$Response)[2,2]

#Precision = TP/TP+FP
PPV<-TP/(TP+FP)
print(paste('PPV',PPV))

#Recall = TP/TP+FN
recall <-TP/(TP+FN)
print(paste('recall',recall))

#f1 score
f1<- 2*(PPV*recall)/PPV+recall
print(paste('f1',f1))

########################Perform 10 fold cross validation
#Randomly shuffle the data
data_cv<-train#[sample(nrow(data_train)),]
table(train$Response)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(data_cv)),breaks=10,labels=FALSE)
acc =list()
ppvlist = list()
recallls= list()
f1ls= list()
AUC=list()

for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test_cv <- data_cv[testIndexes, ]
  train_cv <- data_cv[-testIndexes, ]
  # Fitting
  model <- glm(Response ~ duration + ArrSkiSteLeap + DepSkiSteLeap + onOffBeat+ArrSkiSteLeap:onOffBeat+DepSkiSteLeap:onOffBeat+DepSkiSteLeap:duration,family=binomial(link='logit'),data=train_cv)
  
  fitted.results <- predict(model,newdata=subset(test_cv,select=c(6,19,21,22)),type='response')
  fitted.results <- ifelse(fitted.results > 0.46,1,0)
  misClasificError <- mean(fitted.results != test_cv$Response)
  table(fitted.results,test_cv$Response)
  #print(paste('result',i))
  #print(paste('Accuracy',1-misClasificError))
  
  p1 <- predict(model, newdata=subset(test_cv,select=c(6,19,21,22)), type="response")
  pr1 <- prediction(p1, test_cv$Response)
  prf1 <- performance(pr1, measure = "tpr", x.measure = "fpr")
  #plot(prf1)
  
  auc1 <- performance(pr1, measure = "auc")
  auc1 <- auc1@y.values[[1]]
  AUC[i]<-auc1
  
  TN<-table(fitted.results,test_cv$Response)[1,1]
  FP<-table(fitted.results,test_cv$Response)[1,2]
  FN<-table(fitted.results,test_cv$Response)[2,1]
  TP<-table(fitted.results,test_cv$Response)[2,2]
  
  #Precision = TP/TP+FP
  PPV<-TP/(TP+FP)
  ppvlist[i]<-PPV
  #print(paste('PPV',PPV))
  
  #Recall = TP/TP+FN
  recall <-TP/(TP+FN)
  recallls[i]<-recall
  #print(paste('recall',recall))
  
  #f1 score
  f1<- 2*(PPV*recall)/PPV+recall
  f1ls[i]<-f1
  #print(paste('f1',f1))
  
  # Collecting results
  acc[i] <- 1-misClasificError
}

mean(unlist(acc)) 
mean(unlist(ppvlist)) 
mean(unlist(recallls)) 
mean(unlist(f1ls)) 
mean(unlist(AUC)) 

#> mean(unlist(acc)) 
#[1] 0.8521505
#> mean(unlist(ppvlist)) 
#[1] 0.9525265
#> mean(unlist(recallls)) 
#[1] 0.8788544
#> mean(unlist(f1ls)) 
#[1] 2.636563
#> mean(unlist(AUC)) 
#[1] 0.865517

