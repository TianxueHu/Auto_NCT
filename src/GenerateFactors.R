setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/TAVERN/chordtone_data')
rm(list = ls())
library(FactoMineR)
library(ResourceSelection)
library(ROCR)
#dataframe<-read.csv('71.2%TAVERN.csv',header=TRUE, sep=",")
dataframe<-read.csv('66%HAYDN.csv',header=TRUE, sep=",")
table(dataframe$Response)
# !!!!! Problem - after factorizing a column and use glm(), one factor disappeared. And what does the "intersection" stand for?

#delete some chords that are probabilistic
dataframe<-dataframe[!(dataframe$harm=='I/III'),]
dataframe<-dataframe[!(dataframe$harm=='viioD7/V'),]
dataframe<-dataframe[!(dataframe$harm=='viioD7/IV'),]
dataframe<-dataframe[!(dataframe$harm=='viioD7/vi'),]


table(dataframe$Response)

#data_r<-dataframe[,c(6,7)]
#arriving<-dataframe[,9]
#departure<-dataframe[,12]
#response<-dataframe[,13]
#data = cbind(data_r,response)
#################################linearize intervals##########################3##
arrnumber <- c("ArrNum") 
dataframe[,arrnumber] <- NA 
depnumber <- c("DepNum") 
dataframe[,depnumber] <- NA 


intervals <- c('-A11'= -18,'-A12'=-20,'-A15'=-25,'-A18'=-30,'-A2'=-3,'-A4'=-6,'-A5'=-8,'-A8'=-13,'-A9'=-15,'-d10'=-14,'-d11'=-16,'-d12'=-18,'-d14'=-21,'-d15'=-23,'-d18'=-28,
               '-d3'=-2,'-d4'=-4,'-d5'=-6 ,'-d6'=-7,'-d7'=-9,'-d8'=-11,'-m10'=-15,'-M10'=-16,'-m13'=-20,'-M13'=-21,'-m14'=-22,'-M14'=-23,'-m16'=-25,'-M16'=-26,'-m17'=-27, 
               '-M17'=-28,'-m2'=-1,'-M2'=-2,'-M21'=-35,'-M24'=-40,'-m3'=-3,'-M3'=-4,'-m6'=-8,'-M6'=-9,'-m7'=-10,'-M7'=-11,'-m9'=-13,'-M9'=-14,'-P11'=-17,'-P12'=-19, 
               '-P15'=-24,'-P18'=-29,'-P19'=-31,'-P22'=-36,'-P4'=-5,'-P5'=-7,'-P8'=-12, 
               '+A11'= 18, '+A13'=22,'+A2'=3,'+A3'=5,'+A4'=6,'+A5'=8,'+A8'=13,'+A9'=15,'+d12'=18,'+d15'=23,'+d21'=33,'+d3'=2,'+d4'=4,'+d5'=6, 
               '+d7'=9,'+d8'=11,'+dd11'=15,'+m10'=15,'+M10'=16,'+m13'=20,'+M13'=21,'+m14'=22,'+M14'=23,'+m16'=25,'+M16'=26,'+m17'=27,'+M17'=28,'+m2'=1,'+M2'=2, 
               '+M20'=33,'+m21'=34,'+M21'=35,'+m23'=37,'+M28'=47,'+m3'=3,'+M3'=4,'+m6'=8,'+M6'=9,'+m7'=10,'+M7'=11,'+m9'=13,'+M9'=14,'+P11'=17, 
               '+P12'=19,'+P15'=24,'+P18'=29,'+P19'=31,'+P22'=36,'+P25'=41,'+P26'=43,'+P29'=48,'+P4'=5,'+P5'=7,'+P8'=12,'A1'=1,'AA1'=2,'d1'=-1,'P1'=0)


for (i in 1:nrow(dataframe)){
  dataframe$ArrNum[i]<-as.numeric(intervals[as.character(dataframe[i,]$approaching)])
  dataframe$DepNum[i]<-as.numeric(intervals[as.character(dataframe[i,]$depart)])
}


#fill missing interval data with 0 (first/last note in the piece)
dataframe$ArrNum[is.na(dataframe$ArrNum)] <- 0
dataframe$DepNum[is.na(dataframe$DepNum)] <- 0

data <- dataframe
##############Set Boundary Notes###

boundary <- c("boundary") #an empty list record result
dataframe[,boundary] <- NA 
#data$file[20]!=data$file[21]
for (i in 1:(nrow(data)-1)){
  if (data$file[i]!=data$file[i+1]){
    dataframe$boundary[i] <- 1
    dataframe$boundary[i+1] <- 1
  }
}
dataframe$boundary[is.na(dataframe$boundary)] <- 0
dataframe$boundary<-factor(dataframe$boundary)
################# Make Extra Columns ##############


# 1 - "onOffBeat" - beat_pos: onBeat/offBeat - col 6
onOffBeat <- c("onOffBeat") #an empty list record result
dataframe[,onOffBeat] <- NA 
for (i in 1:nrow(data)){
  if (as.numeric(data$beat_pos[i]) %% 1 == 0){
    dataframe$onOffBeat[i] <- "onbeat"
  }else{
    dataframe$onOffBeat[i] <- "offbeat"
  }
}
dataframe$onOffBeat <-factor(dataframe$onOffBeat)

# 2 - "BeatVal" - beat_pos: 0 & 0.25 % 0.5 & 1 - col 7
BeatVal <- c("BeatVal") #an empty list record result
dataframe[,BeatVal] <- NA 
for (i in 1:nrow(data)){
  if (as.numeric(data$beat_pos[i]) %% 1 == 0){
    dataframe$BeatVal[i] <- "beat1.0"
  } else if(as.numeric(data$beat_pos[i]) %% 0.5== 0){
    dataframe$BeatVal[i] <- "beat0.5"
  }else if(as.numeric(data$beat_pos[i]) %% 0.25== 0){
    dataframe$BeatVal[i] <- "beat0.25"
  }else if(as.numeric(data$beat_pos[i]) %% 0.125 == 0){
    dataframe$BeatVal[i] <- "beat0.125"
  } else {
    dataframe$BeatVal[i] <- "others"
  }
}
dataframe$BeatVal <-factor(dataframe$BeatVal) 

#3 - "ArrSkiSteLeap" & "DepSkiSteLeap" - Intervals: Skip, step, leap - col 8 & 9
arrSkiSteLeap<- c("ArrSkiSteLeap") #an empty list record result
dataframe[,arrSkiSteLeap] <- NA 
depSkiSteLeap<- c("DepSkiSteLeap") #an empty list record result
dataframe[,depSkiSteLeap] <- NA 
for (i in 1:nrow(data)){
  if (abs(as.numeric(data$ArrNum[i]))==2 | abs(as.numeric(data$ArrNum[i]))==1){
    dataframe$ArrSkiSteLeap[i] <- "step"
  } else if (abs(as.numeric(data$ArrNum[i]))==3 | abs(as.numeric(data$ArrNum[i]))==4){
    dataframe$ArrSkiSteLeap[i] <- "skip"
  } else if (abs(as.numeric(data$ArrNum[i]))==0){
    dataframe$ArrSkiSteLeap[i] <- "unison"
  } else {
    dataframe$ArrSkiSteLeap[i] <- "leap"
  }
  
  if (abs(as.numeric(data$DepNum[i]))==2 | abs(as.numeric(data$DepNum[i]))==1){
    dataframe$DepSkiSteLeap[i] <- "step"
  } else if (abs(as.numeric(data$DepNum[i]))==3 | abs(as.numeric(data$DepNum[i]))==4){
    dataframe$DepSkiSteLeap[i] <- "skip"
  } else if (abs(as.numeric(data$DepNum[i]))==0){
    dataframe$DepSkiSteLeap[i] <- "unison"
  } else {
    dataframe$DepSkiSteLeap[i] <- "leap"
  }
}

table(data$beat_pos %% 1)
#4 "IntBeats" - Catagorize strong beat positions to 1,2,3,4, others are 10 - col10
IntBeats<- c("IntBeats") #an empty list record result
dataframe[,IntBeats] <- NA 

for (i in 1:nrow(data)){
  if (as.numeric(data$beat_pos[i]) %% 1 == 0){
    dataframe$IntBeats[i] <- as.numeric(data$beat_pos[i])
  }else{
    dataframe$IntBeats[i] <-'offBeat'
  }
}
dataframe$IntBeats <-factor(dataframe$IntBeats)

#5 "ArrDir" & "DepDir" - interval direction - col 11 & 12
arrdir<- c("ArrDir") #an empty list record result
dataframe[,arrdir] <- NA 
depdir<- c("DepDir") #an empty list record result
dataframe[,depdir] <- NA 
for (i in 1:nrow(data)){
  if (as.numeric(data$ArrNum[i])<0){
    dataframe$ArrDir[i] <- "down"
  } else if (as.numeric(data$ArrNum[i])>0){
    dataframe$ArrDir[i] <- "up"
  } else if (as.numeric(data$ArrNum[i])==0){
    dataframe$ArrDir[i] <- "unison"
  }
  
  if (as.numeric(data$DepNum[i])<0){
    dataframe$DepDir[i] <- "down"
  } else if (as.numeric(data$DepNum[i])>0){
    dataframe$DepDir[i] <- "up"
  } else if (as.numeric(data$DepNum[i])==0){
    dataframe$DepDir[i] <- "unison"
  }
}



#=============================================================

#generate NCT type here
NCT_Type<- c("NCT_Type") #an empty list record result
dataframe[,NCT_Type] <- NA 

for (i in 1:nrow(dataframe)){
  if(as.numeric(dataframe$Response[i])==0){
    #passing & neighbouring
    if(dataframe$ArrSkiSteLeap[i] == 'step' && dataframe$DepSkiSteLeap[i] == 'step'){
      if (dataframe$ArrDir[i] == dataframe$DepDir[i]){
        if(dataframe$onOffBeat[i]=='onbeat'){
          dataframe$NCT_Type[i] <- "AccPsTone"
        }else if(dataframe$onOffBeat[i]=='offbeat'){
          dataframe$NCT_Type[i] <- "UnAccPsTone"
        }
      }else if (dataframe$ArrDir[i] != dataframe$DepDir[i]){
        if(dataframe$onOffBeat[i]=='onbeat'){
          dataframe$NCT_Type[i] <- "AccNBTone"
        }else if(dataframe$onOffBeat[i]=='offbeat'){
          dataframe$NCT_Type[i] <- "UnAccNBTone"
        }
      }
    }else if(dataframe$ArrSkiSteLeap[i] == 'step' && dataframe$DepSkiSteLeap[i] == 'skip' && dataframe$ArrDir[i]!=dataframe$DepDir[i]){
      dataframe$NCT_Type[i] <- "EscapeTone"
    }else if(dataframe$ArrSkiSteLeap[i] == 'skip' && dataframe$DepSkiSteLeap[i] == 'step' && dataframe$ArrDir[i]!=dataframe$DepDir[i] && dataframe$onOffBeat[i]=='onbeat') {
      dataframe$NCT_Type[i] <- "Appoaggiatua"
    }else if(dataframe$ArrSkiSteLeap[i] == 'leap' && dataframe$DepSkiSteLeap[i] == 'step' && dataframe$ArrDir[i]!=dataframe$DepDir[i] && dataframe$onOffBeat[i]=='onbeat') {
      dataframe$NCT_Type[i] <- "Appoaggiatua"
    }else if (dataframe$ArrSkiSteLeap[i] == 'unison' && dataframe$DepSkiSteLeap[i] == "step" && dataframe$onOffBeat[i]=='onbeat'){
      if (dataframe$onOffBeat[i]=='onbeat'){
        dataframe$NCT_Type[i] <- "Susp/Retard"
      }
    }else if (dataframe$DepSkiSteLeap[i] == 'unison'){
      dataframe$NCT_Type[i] <- "Anticipation/Syncopation"
    }else{
      dataframe$NCT_Type[i] <- "Unknown"
    }
  }else{
    dataframe$NCT_Type[i] <- "ChordTone"
  }

}

#sort(with(subset(dataframe, NCT_Type=='Unknown'), table(paste(ArrSkiSteLeap, DepSkiSteLeap, ArrDir, DepDir, onOffBeat )))) (1.94616 + c(.25,.5,.75,1) * .94960)


table(dataframe$NCT_Type)


#write.csv(dataframe,'TAVERN_Factors.csv')
write.csv(dataframe,'HAYDN_Factors.csv')












