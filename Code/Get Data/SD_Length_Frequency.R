# Retrieve and Prep Data ----
# for spiny dogfish length comps
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: 2_10_2020

# Setup ----
library(plyr)

AYR<-2020
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")

plus<-100 #sets uppper plus group,
minus<-40 #sets lower "minus" group

#IPHC data
#need to get new data each year from IPHC and add in FMP, this is not automated yet
#2019 data are not yet available, when/if this data stream continues, add to data retrieval/processing codes
# Lengths are PCL
IPHC_dat<-read.csv(paste(outdir,"/IPHC_dogfish_lengths",AYR,".csv",sep=""), header=T) 

levels(IPHC_dat$FMP)[levels(IPHC_dat$FMP)=="WA"]<-"WC"
levels(IPHC_dat$FMP)[levels(IPHC_dat$FMP)=="WAOR"]<-"WC"
levels(IPHC_dat$FMP)[levels(IPHC_dat$FMP)=="ORCA"]<-"WC"

IPHC_dat<-IPHC_dat[IPHC_dat$Sex!="U",] #takes out the unknown sex

#round all lengths down to nearest 5
IPHC_dat$binL<-floor(IPHC_dat$Length/5)*5 #rounds everything down to the next 5 (i.e. 40,45...)
IPHC_dat$binL[IPHC_dat$binL>plus]<-plus #turns everything over 90 to 90, 90 is the plus group
IPHC_dat$binL[IPHC_dat$binL<minus]<-minus

IPHC_size<-IPHC_dat[,c("Year","FMP","Length","Sex")]
IPHC_size$Source<-"IPHCLL"

IPHC_freq<-ddply(IPHC_dat,c("Year","FMP","binL","Sex"),
                 summarize,Frequency = length(Length))
IPHC_freq$Source<-"IPHCLL"

#AFSCLL survey data
#downloaded from AKFIN LL survey Length Summary View, begins in 2010
#add this and any data processing steps to data processing code
# Lengths are PCL
AFSCLL_dat<-read.csv(paste(datadir,"/AFSCLL_dogfish_length.csv",sep=""), header=T,skip=6) 
#note: this data is different format from IPHC, rows are not individual samples, but lengths with a frequency of occurence
levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)[levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)=="Western Gulf of Alaska"]<-"WGOA"
levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)[levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)=="Central Gulf of Alaska"]<-"CGOA"
levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)[levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)=="West Yakutat"]<-"EGOA"
levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)[levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)=="East Yakutat/Southeast "]<-"EGOA"
levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)[levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)=="Aleutians"]<-"AI"
levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)[levels(AFSCLL_dat$NPFMC.Sablefish.Mgmt.Area)=="Bering Sea"]<-"BS"
AFSCLL_dat$Sex[AFSCLL_dat$Sex==1]<-"M"
AFSCLL_dat$Sex[AFSCLL_dat$Sex==2]<-"F"
AFSCLL_dat$FMP<-as.character(ifelse(AFSCLL_dat$NMFS.Area.Code>600,"GOA","BSAI"))

AFSCLL<-AFSCLL_dat[,c("Year","FMP","Length","Sex","Frequencey")]
AFSCLL_size<-AFSCLL[rep(row.names(AFSCLL),AFSCLL$Frequencey),1:4]
AFSCLL_size$Source<-"AFSCLL"

AFSCLL_dat$binL<-floor(AFSCLL_dat$Length/5)*5 #rounds everything down to the next 5 (i.e. 40,45...)
AFSCLL_dat$binL[AFSCLL_dat$binL>plus]<-plus #turns everything over 90 to 90, 90 is the plus group
AFSCLL_dat$binL[AFSCLL_dat$binL<minus]<-minus

AFSCLL_freq<-AFSCLL_dat[,c("Year","FMP","binL","Sex","Frequencey")]
colnames(AFSCLL_freq)[5]<-"Frequency"
AFSCLL_freq$Source<-"AFSCLL"

#AFSCTWL data 
#downloaded from AKFIN RACE Survey Size comp by haul
#add this to data retrieval/processing codes
#Lengths are NOT PCL, must convert from FL and TL to PCL, but not sure yet 
#what the length type codes are
TWL_dat<-read.csv(paste(datadir,"/RACE_lengths_2019.csv",sep=""), header=T,skip=7)

TWL_dat$FMP<-0
BSAIsurv<-c("AI","EBS_SHELF","EBS_SLOPE")
TWL_dat[TWL_dat$Survey%in%BSAIsurv,]$FMP<-"BSAI"
TWL_dat[TWL_dat$Survey=="GOA",]$FMP<-"GOA"
TWL_dat$Length<-TWL_dat$Length..mm./10
TWL_dat$Sex<-lapply(TWL_dat$Sex,as.character)
TWL_dat$Sex[TWL_dat$Sex=="Male"]<-"M"
TWL_dat$Sex[TWL_dat$Sex=="Female"]<-"F"
TWL_dat<-TWL_dat[TWL_dat$Sex!="Unknown",] #takes out the unknown sex

#convert lengths assuming 1=FL and 5=TLnat
#Conversions based on Tribuzio and Kruse 2012
Lconv<-read.csv(paste(getwd(),"/Data/Static/Shark_Length_conversions.csv",sep=""),header=T)
TWL_dat$Length.Type[TWL_dat$Length.Type==1]<-"FL"
TWL_dat$Length.Type[TWL_dat$Length.Type==5]<-"TLnat"
TWL_dat$PCL<-0
if(TWL_dat$Length.Type=="FL"){
  TWL_dat$PCL<-Lconv[Lconv$Lfrom=="FL"&Lconv$Lto=="PCL",c("a")]+
    Lconv[Lconv$Lfrom=="FL"&Lconv$Lto=="PCL",c("b")]*TWL_dat$Length
}
if(TWL_dat$Length.Type=="TLnat"){
  TWL_dat$PCL<-Lconv[Lconv$Lfrom=="TLnat"&Lconv$Lto=="PCL",c("a")]+
    Lconv[Lconv$Lfrom=="FL"&Lconv$Lto=="PCL",c("b")]*TWL_dat$Length
}

AFSCTWL<-TWL_dat[,c("Year","FMP","Length","Sex","Frequency")]
AFSCTWL_size<-AFSCTWL[rep(row.names(AFSCTWL),AFSCTWL$Frequency),1:4]
AFSCTWL_size$Source<-"AFSCTWL"

TWL_dat$binL<-floor(TWL_dat$PCL/5)*5 #rounds everything down to the next 5 (i.e. 40,45...)
TWL_dat$binL[TWL_dat$binL>plus]<-plus #turns everything over 90 to 90, 90 is the plus group
TWL_dat$binL[TWL_dat$binL<minus]<-minus

AFSCTWL_freq<-TWL_dat[,c("Year","FMP","binL","Sex","Frequency")]
AFSCTWL_freq$Source<-"AFSCTWL"

SD_L_freq<-rbind(IPHC_freq,AFSCLL_freq,AFSCTWL_freq)
write.csv(SD_L_freq,paste(outdir,"/SD_Lfreq",AYR,".csv",sep=""),row.names = F)

SD_L_dat<-rbind(IPHC_size,AFSCLL_size,AFSCTWL_size)
write.csv(SD_L_dat,paste(outdir,"/SD_L_dat",AYR,".csv",sep=""),row.names = F)
