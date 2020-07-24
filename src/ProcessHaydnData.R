setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/haydn_op20_harm/op20/haydn_extracted')
library(stringi)
#source('C:/Users/Claire/Desktop/R/ChordTones/RomanNumerals.R', echo=TRUE)
rm(list = ls())


###read data & assemble
filename<-dir(pattern = "*krn$")
datafiles <- lapply(filename,read.delim, header=F, stringsAsFactors=F)
filename
length(filename)

dataframe <-c()
for (i in 1:length(filename)){
  getdata<-datafiles[[i]]
  getdata$V9 <- c(rep(filename[i],lengths(getdata[1])))
  dataframe<-rbind(dataframe,getdata)
} 

colnames(dataframe)=c("harm","melody", "beat_pos","duration", "scale_deg","approaching","key","file")
dataframe


###new columns for arrving by a rest and depart to a rest
# arrbyrest <- c("ArrByRest") 
# dataframe[,arrbyrest] <- NA
# depbyrest <- c("DepByRest") 
# dataframe[,depbyrest] <- NA
# is.na(dataframe$scale_deg[64554+1])
# for (i in 1:nrow(dataframe)){
#   if (dataframe$scale_deg[i+1]=="r" ){
#     dataframe$DepByRest[i] <- 1
#   }else{
#     dataframe$DepByRest[i] <- 0
#   }
# }
# #check the last dp here
# #dataframe$Depbyrest[2650]<-1

# for (i in 2:nrow(dataframe)){
#   if (dataframe$scale_deg[i-1]=="r" ){
#     dataframe$ArrByRest[i] <- 1
#   }else{
#     dataframe$ArrByRest[i] <- 0
#   }
# }
# #fill the first dp as 1
# dataframe$ArrByRest[1]<-1

# #as.factors?
# dataframe$ArrByRest <-factor(dataframe$ArrByRest) 
# dataframe$DepByRest <-factor(dataframe$DepByRest) 




### Delete null point rows and investigation
dataframe<-dataframe[!(dataframe$duration==0),] #remove dur = 0 data (originally '.' in pieces),they are generated as placeholders to '.'
#also including GRACE NOTE and rests
dataframe<-dataframe[!(dataframe$duration=='.'),] #rests and unknown notes with intervals are in [] form, either starting/end of piece
#so it does not affect intervals 
dataframe<-dataframe[!(dataframe$beat_pos=='.'),] #some rests
dataframe<-dataframe[!(dataframe$harm=='.'),] #delete other grace notes



#moving up approaching interval to get departure interval
dataframe<-dataframe[!(dataframe$scale_deg=='r'),] 
#placeholder
app<-c(dataframe$approaching)
a<- app[-1]
a<-append(a,'[cc]')
dataframe$depart <- a




romanrepre<-function(roman_num){
  roman_num<-stri_replace_first_regex(roman_num, "^[1-9][0-9]*\\.*", "")
  stri_replace_first_regex(roman_num, "[a-d]", "")
}

dataframe$harm<-romanrepre(dataframe$harm)

table(dataframe$harm)
table(dataframe$key)
table(dataframe$scale_deg)
filename

###prepare for identification in the next program
response <- c("Response") #an empty list record result
dataframe[,response] <- NA #this line can be placed at the end 

chordnotes <- c("Chordnotes") #an empty list record result
dataframe[,chordnotes] <- NA #this line can be placed at the end 

write.csv(dataframe,'HAYDN.csv')
write.table(dataframe, file = "HAYDN.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

