
library(stringi)
library(stringr)
rm(list = ls())


romanroot <- c(
  I = 1,
  C = 1,
  II = 3,
  III = 5,
  IV = 0,
  V = 2,
  VI = 4,
  VII= 6
)


keynames=c('f','c','g','d','a','e','b')


rn2root <-function(rn){
  
  rn <- toupper(rn)
  rn <- stri_extract_first(rn,regex = "[VICNGLF]{1,3}")
  root <-romanroot[rn]
  if (grepl("[C]", rn) == TRUE){
    root <-2
  }else if(grepl("[GLF]", rn) == TRUE){
    root <- -3
  }else if (grepl("[N]", rn) == TRUE){
    root <- -4
  }
  
  root
}


keyquality <- function(key){
  if(grepl("^[[:lower:]]+$",key)==TRUE){
    #minor key
    keyquality<-'m'}
  if(grepl("^[[:upper:]]+$",key)==TRUE){
    #major key
    keyquality<-'M'}
  keyquality
}



chordType <- function(rn){
  if(grepl("^[[:lower:]]+$", substring(rn,1,1)) == TRUE){
    thisChordType <-'MinorTriad'
    if(grepl("[o]", rn)==TRUE){
      thisChordType <-'DimTriad'
      if(grepl("[D]", rn)==TRUE){
        thisChordType <-'FullDim7th'
      }else if(grepl("[m]", rn)==TRUE){
        thisChordType <-'HalfDim7th'
      }
    }else if(grepl("[7^o]", rn)==TRUE){
      thisChordType <-'Minor7th'
    }
  }else if (grepl("^[[:upper:]]+$", stri_extract_first(rn, regex = "[VIvi]{1,3}"))==TRUE){
    thisChordType <-'MajorTriad'
    if(grepl("[7]", rn) == TRUE && grepl("[-]", stri_sub(rn, 1, 1)) == FALSE){
      thisChordType <-'Dom7th'
      if(grepl("[m]", rn) == TRUE){
        thisChordType <-'Minor9th'
      }else if(grepl("[M]", rn) == TRUE){
        thisChordType <-'Major9th'
      }else if(grepl("[+]", rn) == TRUE){
        thisChordType <-'Major7th'
      }
    }else if(grepl("[-]", stri_sub(rn, 1, 1)) == TRUE){
      thisChordType <-'Flat3rdTriad'
      if(grepl("[7]", rn) == TRUE){
        thisChordType <-'Flat3rd7th'
      }
    }else if(grepl("[+]", stri_sub(rn, -1))== TRUE){
      thisChordType <-'AugTriad'
    }
  }else if(grepl("[C]", rn) == TRUE){
    thisChordType <-'Cadential'
  }else if(grepl("[N]", rn) == TRUE){
    thisChordType <-'Neopolitan'
  }else if(grepl("[G]", rn) == TRUE){
    thisChordType <-'German'
  }else if(grepl("[F]", rn) == TRUE){
    thisChordType <-'French'
  }else if(grepl("[L]", rn) == TRUE){
    thisChordType <-'Italian'
  }else{
    thisChordType <-NaN
  }
  thisChordType
}
#----TEST--------
chordType('ii')
chordType('I')
chordType('iiom7')
chordType('Lt')
chordType('I+')
chordType('-I7')
chordType('I7')
chordType('-VI')
chordType('viio')
chordType('N')
chordType('V7M9')
chordType('-VI7')
chordType('V7m9')
chordType('ii7')
#-----END---------


scales=list(
  M=c(0,2,4,-1,1,3,5),
  m=c(0,2,-3,-1,1,-4,-2)
)

#Modulation process is referred from Nat's code
modulation.root=function(roman,keyquality='M'){
  
  roots=c('i','ii','iii','iv','v','vi','vii')
  
  scaledeg=match(tolower(gsub('[^IViv]','',roman)),roots)
  modroot=scales[[keyquality]][scaledeg]
  
  if(grepl('vii',roman) & keyquality=='m') modroot=5
  
  if(grepl('^[-b]',roman) & keyquality=='M') modroot=modroot-7
  if(grepl('^[#]',roman)) modroot=modroot+7
  
  modroot
}


fifths2notes <- function(fifth) {
  letternames <- c('0' = '4', '1' = '1', '2' = '5', '3' = '2', '4' = '6', '5' = '3', '6' = '7')
  
  lettername <- letternames[as.character(fifth %% 7)]
  alteration <- fifth %/% 7
  
  accidental <- ifelse(alteration > 0, "+", "-")
  
  accidental <- strrep(accidental, abs(alteration))
  
  paste0(lettername, accidental)
}


rn2notes <- function(rn, keyname) {
  keyname<- stri_extract_first(keyname,regex = "[A-Ga-g]")
  keyqual<-keyquality(keyname)
  #print(keyqual)
  keyname <- tolower(keyname)
  #key=match(keyname,keynames)
  
  modroot <-0
  original_rn<-rn
  #applied chord
  if(grepl("/",rn) == TRUE){
    locatevalue<-as.numeric(str_locate(rn, "/")[1,1])
    roman <- rn
    #extract chord part
    rn<-substr(roman, start = 0,stop=locatevalue-1) 
    #extract modulation part
    modulation<-substr(roman, start =locatevalue+1,stop =nchar(roman))
    if(grepl("[-]", modulation) == TRUE){
      modroot<-NaN
    }else if(str_count(original_rn, pattern = "/") > 1){
      modroot<-NaN
    }else{
      modroot <- modulation.root(modulation, keyqual)
    }
    
  }
  if(grepl("[[]", original_rn)){
    modroot<-NaN
  }
  
  thisChordType<-chordType(rn)
  if (is.nan(thisChordType)){
    chordtones<-NaN
  }
  root <- rn2root(rn)
  
  chordtype <- list(MajorTriad = c(0,1,4),MinorTriad = c(0,1,-3, 9), AugTriad=c(0,4,8,-4), 
                    Major7th=c(0,1,4,5), Dom7th=c(0,1,4,-2, 10), Minor7th=c(0,1,-3,-2,9,10), DimTriad = c(0,-3,-6,9,6),
                    HalfDim7th = c(0,-3,-6,-2,9,6,10), FullDim7th = c(0,-3,-6, 3,9,6),
                    Cadential=c(0,-1,3,11), Neopolitan = c(0,1,4,12,13), Major9th = c(0,1,4,5,2), Minor9th = c(0,1,-3,-2,9,10,2),
                    Italian = c(0,4,10,12,-2), German = c(0,4,10,12,-2,1,13), French = c(0,4,10,12,-2,6), Flat3rdTriad = c(-7,5,-3,-6,6), Flat3rd7th = c(-7,5,-3,-6,6,3,-9))
  
  if (is.nan(thisChordType)==TRUE){
    chordtones<-NaN
  }else{
    chordtones<-fifths2notes(root + modroot + chordtype[[thisChordType]])
  }
 
  #print(root)
  #print(modroot)
  #print(thisChordType)
  
  if(is.nan(modroot)==TRUE){
    chordtones<-NaN
  }
  
  chordtones
}


#------Testing!!!-------
rn2notes('I+','C')
rn2notes('i','C')
rn2notes('I','C')
rn2notes('iom7','C')
rn2notes('Lt','C')
rn2notes('Fr','C')
rn2notes('N','C')
rn2notes('Gn','C')
rn2notes('I+','C')
rn2notes('I7+','C')
rn2notes('I7','C')
rn2notes('-III7','C')
rn2notes('viio','C')
rn2notes('N','C')
rn2notes('I7M9','C')
rn2notes('-VI7','C')
rn2notes('I7m9','C')
rn2notes('ii7','C')
rn2notes('I/-v/ii','c')
rn2notes('I/iii','C')
rn2notes('C','c')
rn2notes('ii/V[vi]','c')
rn2notes('v/iii','c')
rn2notes('vii','c')
#------END--------

chordtones<-rn2notes('I+/-iii','C')
"5+" %in% chordtones
is.nan(chordtones)[1]



#--------------------------------LOAD DATAFRAME----------------------------
setwd('/Users/tianxuehu/Documents/MUSI7100/MusicDatasets/NCT/Data/TAVERN/chordtone_data')
#rm(list = ls())
dataframe<-read.csv('THEME.csv',header=TRUE, sep=",")
#dataframe<-read.csv('HAYDN.csv',header=TRUE, sep=",")




for (i in 1:nrow(dataframe)){
  this.harm <- as.character(dataframe[i,]$harm)
  this.deg<- as.character(dataframe[i,]$scale_deg)
  this.key<-as.character(dataframe[i,]$key)
  chordtonelist<-rn2notes(this.harm,this.key)
  if (is.nan(chordtonelist)[1]==TRUE){
    print(i)
    dataframe[i,]$Response<- 5
    dataframe[i,]$Chordnotes <- NaN
  }else{
    dataframe[i,]$Response <-this.deg %in% chordtonelist
    dataframe[i,]$Chordnotes <- paste(chordtonelist, sep = ' ', collapse=" ")
  }
}




table(dataframe$Response)

table(dataframe$harm)

#remove not counted data
dataframe<-dataframe[!(dataframe$Response==5),]
table(dataframe$Response)

write.csv(dataframe,'82%THEME.csv')
#write.csv(dataframe,'66%HAYDN.csv')

