

#Raw .krn file should go through (e.g. original file called "BWV347")
#optional processing:
  #extractx -i kern if original score have spines other than **kern
# command: ./melody_extracter (a filename, here is BWV347.krn)
# then it generates a melody.krn file
# command: ./recnum melody.krn > raw_melody_num.krn
# command: ./extractor (a filename, here is raw_melody_num.krn)
# then it generates **_features.krn file 
# command: sed '/^=/ d' raw_melody_num_features.krn > tmp.krn
# command: ridx -GLId tmp.krn > melody4R.krn
#
#R: InputToModel.R


#####################load file melody4R.krn#############
dir= getwd()
setwd(dir)
library(stringi)
rm(list = ls())

filename <- "melody4R.krn"
datafile<-lapply(filename,read.delim, header=F, stringsAsFactors=F)
dataframe <- datafile[[1]]

colnames(dataframe)=c("harm","melody", "index", "beat_pos","duration", "scale_deg","approaching","key")


### Delete null point rows and investigation
dataframe<-dataframe[!(dataframe$duration==0),] #remove dur = 0 data (originally '.' in pieces),they are generated as placeholders to '.'
#also including GRACE NOTE and rests
dataframe<-dataframe[!(dataframe$duration=='.'),] #rests and unknown notes with intervals are in [] form, either starting/end of piece
#so it does not affect intervals 
dataframe<-dataframe[!(dataframe$beat_pos=='.'),] #some rests

#moving up approaching interval to get departure interval
dataframe<-dataframe[!(dataframe$scale_deg=='r'),] 
#placeholder
app<-c(dataframe$approaching)
a<- app[-1]
a<-append(a,'[cc]')
dataframe$depart <- a


### "duration" as numeric
dataframe$duration <-as.numeric(dataframe$duration)

#####################Generate other factor cols########
######linearize intervals####
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

####"onOffBeat" - beat_pos: onBeat/offBeat 
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

#### "ArrSkiSteLeap" & "DepSkiSteLeap" - Intervals: Skip, step, leap - col 8 & 9
arrSkiSteLeap<- c("ArrSkiSteLeap") #an empty list record result
dataframe[,arrSkiSteLeap] <- NA 
depSkiSteLeap<- c("DepSkiSteLeap") #an empty list record result
dataframe[,depSkiSteLeap] <- NA 
for (i in 1:nrow(data)){
  if (abs(as.numeric(data$ArrNum[i]))==2 | abs(as.numeric(data$ArrNum[i]))==1){
    dataframe$ArrSkiSteLeap[i] <- "step"
  } else if (abs(as.numeric(data$ArrNum[i]))==3 | abs(as.numeric(data$ArrNum[i]))==4){
    dataframe$ArrSkiSteLeap[i] <- "leap"
  } else if (abs(as.numeric(data$ArrNum[i]))==0){
    dataframe$ArrSkiSteLeap[i] <- "unison"
  } else {
    dataframe$ArrSkiSteLeap[i] <- "leap"
  }
  
  if (abs(as.numeric(data$DepNum[i]))==2 | abs(as.numeric(data$DepNum[i]))==1){
    dataframe$DepSkiSteLeap[i] <- "step"
  } else if (abs(as.numeric(data$DepNum[i]))==3 | abs(as.numeric(data$DepNum[i]))==4){
    dataframe$DepSkiSteLeap[i] <- "leap"
  } else if (abs(as.numeric(data$DepNum[i]))==0){
    dataframe$DepSkiSteLeap[i] <- "unison"
  } else {
    dataframe$DepSkiSteLeap[i] <- "leap"
  }
}


#### prepare for identification in the next program
prediction <- c("Prediction") #an empty list record result
dataframe[,prediction] <- NA #this line can be placed at the end 

response <- c("Response") # empty list record chord tone/non chord tone response
dataframe[,response] <- NA


###################load model###########################
model <- readRDS("final_model.rds")

# make prediction on the melody data
dataframe$Prediction<-predict(model, newdata = dataframe, 'response')
# set threshold: >=0.5 as chord tones, <0.5 as non chord tones
for (i in 1:nrow(dataframe)){
  if (as.numeric(dataframe$Prediction[i]) >=0.46){ ###CHANGE THRESHOLD!
    dataframe$Response[i] <- 1
  }else{
    dataframe$Response[i] <- 0
  }
}

###AGE$yearsold <- ifelse(AGE$age>60, "OLD", "YOUNG")

#optional - store identified dataframe to .csv file for future use
write.csv(dataframe,'CTNCT.csv')

############ import raw_melody_num.krn, to mark the CT/NCT in score#####

#Simply comment out the line below after saving dataframe
dataframe<-read.csv('CTNCT.csv',header=TRUE, sep=",")[,2:17]

raw <- "raw_melody_num.krn"
count.fields(raw, sep = "\t")
rawfile<-read.delim2(raw, header=F, stringsAsFactors=F, sep = "", quote="\"")
rawfile<-rawfile[,c('V1','V2','V3')]
colnames(rawfile)=c("harm","melody", "index")

mark <- c("mark") # empty list record chord tone/non chord tone response
rawfile[,mark] <- NA

##search
countd = 1 #dataframe counter
for(countr in 1:nrow(rawfile)){ #for loop counting raw file
  print(paste("raw index", countr, rawfile$index[countr]))
  print(paste("data index", countd, dataframe$index[countd]))
  print(dataframe$index[countd]==rawfile$index[countr])
  if(is.na(dataframe$index[countd])){ ###finish counting dataframe
    if (substr(rawfile$index[countr], start = 1, stop = 2) == '*-') {
      #ending mark
      mark <- '*-'
    } else if (as.character(rawfile$index[countr]) == '=') {
      #ending barline
      mark <- rawfile$melody[countr]
    } else {
      mark <-  ''
    }
    countd = countd + 1
  }else{
    if(dataframe$index[countd]==rawfile$index[countr]) {
      if (dataframe$Response[countd] == 0) {
        mark <- 'hotpink'
      } else if (dataframe$Response[countd] == 1) {
        mark <- 'black'
      }
      countd = countd + 1
      #countr = countr + 1
    } else if (dataframe$index[countd] != rawfile$index[countr]) {
      if (substr(rawfile$index[countr], start = 1, stop = 4) == '**re') {
        #define color spine
        mark <- '**color'
      } else if (substr(rawfile$melody[countr], start = 1, stop = 1) == '.') {
        #dot
        mark <- '.'
      } else if (substr(rawfile$melody[countr], start = 1, stop = 1) == '*') {
        #star
        mark <- '*'
      } else if (substr(rawfile$melody[countr], start = 1, stop = 3) == '!!!') {
        #melody start with !!!
        mark <-  ' '
        rawfile$melody[countr] <-
          paste(rawfile$melody[countr], rawfile$index[countr]) ##PLAN: delete the "index" col later
      } else if (as.character(rawfile$index[countr]) == '=') {
        #barline
        mark <- rawfile$melody[countr]
      } else {
        mark <- '.'
      }
    }
  }
  print(paste("mark",mark))
  print(paste("current countr", countr))
  rawfile$mark[countr] <-mark
  print(paste("file",rawfile$mark[countr]))
  #countr  = countr + 1
}

#drop the index column
final<-rawfile[ , !(names(rawfile) %in% 'index')]


write.table(final, file = "final.krn", sep = "\t",
            col.names=FALSE, row.names=FALSE, quote = FALSE)



