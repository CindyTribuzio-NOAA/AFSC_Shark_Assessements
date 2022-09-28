# Compile ADFG survey data ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: 5_10_2020

# Setup ----
library(plyr)
library(reshape2)
AYR<-2020
LAYR<-2018
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
olddir<-paste(getwd(),"/Data/Cleaned/",LAYR,sep="")

# download full data set each year, don't worry about past years data
# Data cleanup ----
AFSCLL<-read.csv(paste(datadir,"/AFSCLL_Area Stratum Efforts",AYR,".csv",sep=""), header=T)
AFSCLL<-AFSCLL[AFSCLL$Survey.Country=="United States",]
colnames(AFSCLL)[1]<-"Year"
AFSCLL$FMP<-""
AFSCLL$FMP[AFSCLL$NPFMC.Sablefish.Mgmt.Area=="Aleutians"]<-"BSAI"
AFSCLL$FMP[AFSCLL$NPFMC.Sablefish.Mgmt.Area=="Bering Sea"]<-"BSAI"
AFSCLL$FMP[AFSCLL$NPFMC.Sablefish.Mgmt.Area=="Central Gulf of Alaska"]<-"GOA"
AFSCLL$FMP[AFSCLL$NPFMC.Sablefish.Mgmt.Area=="Western Gulf of Alaska"]<-"GOA"
AFSCLL$FMP[AFSCLL$NPFMC.Sablefish.Mgmt.Area=="West Yakutat"]<-"GOA"
AFSCLL$FMP[AFSCLL$NPFMC.Sablefish.Mgmt.Area=="East Yakutat/Southeast"]<-"GOA"
AFSC2<-ddply(AFSCLL,c("Year","Species.Code","FMP"),summarize,sumRPN=sum(RPN,na.rm=T))

write.csv(AFSC2,paste(outdir,"/AFSCLL_RPN",AYR,".csv",sep=""),row.names=F)
