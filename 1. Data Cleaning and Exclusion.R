################################################################################################################################
##############################################         EXCLUSION CRITERIA       ################################################
################################################################################################################################

rawdata <- read.csv("Data Study 4 3PPG Replication.csv",header=TRUE,sep=";",dec=".",na.strings=c("NA","","-9"),row.names=NULL)

which(duplicated(rawdata$P_CODE))

library(plyr)
rawdata.relabel <- rename(rawdata,c(
  "DQ01_01"="DQ01",    ## Data Quality ##
  "DQ01_02"="DQ02",    
  "JS03_11"="KO01_01", ## Attention Check ##
  ## 3PPG ##
  "DP03_01"="DP00_01", ## General Instructions Checks##
  "DP03_02"="DP00_02",
  "DP05_01"="DP40_01", ## Control Items ROUND 1 ##
  "DP05_02"="DP40_02",
  "DP16_01"="DP41_01", ## Control Items ROUND 2 ##
  "DP16_02"="DP41_02",
  "DP27_01"="DP42_01", ## Control Items ROUND 3 ##
  "DP27_02"="DP42_02",
  "DP38_01"="DP43_01", ## Control Items ROUND 4 ##
  "DP38_02"="DP43_02",
  "DP06_01"="DP01_01", ## ROUND 1 ##
  "DP08_01"="DP02_01",
  "DP08_02"="DP02_02",
  "DP09_01"="DP03_01",
  "DP09_02"="DP03_02",
  "DP10_01"="DP04_01",
  "DP10_02"="DP04_02",
  "DP11_01"="DP05_01",
  "DP11_02"="DP05_02",
  "DP12_01"="DP06_01",
  "DP12_02"="DP06_02",
  "DP13_01"="DP07_01",
  "DP13_02"="DP07_02",
  "DP14_01"="DP08_01",
  "DP14_02"="DP08_02",
  "DP17_01"="DP09_01", ## ROUND 2 ##
  "DP19_01"="DP10_01",
  "DP19_02"="DP10_02",
  "DP20_01"="DP11_01",
  "DP20_02"="DP11_02",
  "DP21_01"="DP12_01",
  "DP21_02"="DP12_02",
  "DP22_01"="DP13_01",
  "DP22_02"="DP13_02",
  "DP23_01"="DP14_01",
  "DP23_02"="DP14_02",
  "DP24_01"="DP15_01",
  "DP24_02"="DP15_02",
  "DP25_01"="DP16_01",
  "DP25_02"="DP16_02",
  "DP28_01"="DP17_01", ## ROUND 3 ##
  "DP30_01"="DP18_01",
  "DP30_02"="DP18_02",
  "DP31_01"="DP19_01",
  "DP31_02"="DP19_02",
  "DP32_01"="DP20_01",
  "DP32_02"="DP20_02",
  "DP33_01"="DP21_01",
  "DP33_02"="DP21_02",
  "DP34_01"="DP22_01",
  "DP34_02"="DP22_02",
  "DP35_01"="DP23_01",
  "DP35_02"="DP23_02",
  "DP36_01"="DP24_01",
  "DP36_02"="DP24_02",
  "DP39_01"="DP25_01", ## ROUND 4 ##
  "DP41_01"="DP26_01",
  "DP41_02"="DP26_02",
  "DP42_01"="DP27_01",
  "DP42_02"="DP27_02",
  "DP43_01"="DP28_01",
  "DP43_02"="DP28_02",
  "DP44_01"="DP29_01",
  "DP44_02"="DP29_02",
  "DP45_01"="DP30_01",
  "DP45_02"="DP30_02",
  "DP46_01"="DP31_01",
  "DP46_02"="DP31_02",
  "DP47_01"="DP32_01",
  "DP47_02"="DP32_02"
))

#If it is not specified that DECIMALS are indicated by ",", R will identify variables with commas as FACTORS or CHARACTERS.
#For transforming from FACTOR to NUMERIC:
for(i in c(ColumnA:ColumnX)) {                                
  rawdata.relabel[,i] <- as.numeric(as.character(rawdata.relabel[,i]))
}
#IMPORTANT:Sometimes R messes up the values during this transformation. Check whether the frequencies table change.


##Self-reported data quality.
#NOTE: In Exclusion Criteria doc, scale has 6 points. In Soscisurvey, scale goes from 1 to 5.
table(rawdata.relabel$DQ01)
table(rawdata.relabel$DQ02)
rawdata.relabel$Excl1<-ifelse(rawdata.relabel$DQ01&rawdata.relabel$DQ02<=2,1,0)  
table(rawdata.relabel$Excl1) #2 people are excluded based on self-report data quality.

##Attention Check.
rawdata.relabel$Excl2<-ifelse(rawdata.relabel$KO01_01!=5,1,0) 

table(rawdata.relabel$Excl2) #22 people are excluded based on Concentration Items.

##Delete Excluded cases.
library(dplyr)
rawdata.excl <- filter(rawdata.relabel, Excl1!=1 & !is.na(Excl1)) %>% filter(Excl2!=1 & !is.na(Excl2))

which(is.na(rawdata.relabel$Excl1))
which(is.na(rawdata.relabel$Excl2))


##3PPG Control items in each Round.
#Baseline. Correct Answers: R101 (7) - R102 (2)
R101 <- table(rawdata.excl$DP40_01)
R102 <- table(rawdata.excl$DP40_02)
#High Uncertainty/Low Ambiguity. Correct Answers: R201 (7) - R202 (10)
R201 <- table(rawdata.excl$DP41_01)
R202 <- table(rawdata.excl$DP41_02)
#Low Uncertainty/High Ambiguity. Correct Answers: R301 (10) - R302 (2)
R301 <- table(rawdata.excl$DP42_01)
R302 <- table(rawdata.excl$DP42_02)
#High Uncertainty/High Ambiguity. Correct Answers: R301 (10) - R302 (10)
R401 <- table(rawdata.excl$DP43_01)
R402 <- table(rawdata.excl$DP43_02)

R101
R102
R201
R202
R301
R302
R401
R402


#### ORDER VARIABLES ####
#Function for extracting numbers from string character.

Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

rawdata.excl$ORDER <- Numextract(rawdata.excl$IV02_01)


#In total AROUND 10 people failed 3PPG attention checks, but were redirected to instructions.

write.csv2(rawdata.excl, file = "Data Study 4 3PPG Replication (Analyses file).csv")


