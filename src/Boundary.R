###Set Boundary note###
setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/TAVERN/chordtone_data')
rm(list = ls())
data_test<-read.csv('TAVERN_Factors.csv',header=TRUE, sep=",")[3:25]
data_train<-read.csv('THEME_Factors.csv',header=TRUE, sep=",")[3:25]

dataframe<-data_test

dataframe$file[10647]==dataframe$file[10648]


boundary <- c("boundary") #an empty list record result
dataframe[,onOffBeat] <- NA 
for (i in 1:nrow(data)){
  if (as.numeric(data$beat_pos[i]) %% 1 == 0){
    dataframe$onOffBeat[i] <- "onbeat"
  }else{
    dataframe$onOffBeat[i] <- "offbeat"
  }
}