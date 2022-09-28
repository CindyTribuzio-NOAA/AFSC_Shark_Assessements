library(lubridate)


EMdat<-read.csv(paste(getwd(),"/Data/Annual_updates/",AYR,"/Tribuzio - COMPREHENSIVE_OBS_EM.csv",sep=""),header=T)

#add month to trip start date
EMdat$TRIP_START_DATE<-as.Date(EMdat$TRIP_START_DATE)
EMdat$Month<-month(EMdat$TRIP_START_DATE)

EMshort<-EMdat[EMdat$YEAR>2017&EMdat$AGENCY_GEAR_CODE=="HAL",]

length(unique(c(EMshort$ODDS_TRIP_NUMBER,EMshort$EM_HAUL_NUMBER)))
#6908 total unique trips/hauls

length(unique(c(EMshort[EMshort$SAMPLED_FLAG=="Y",c("ODDS_TRIP_NUMBER")],
                EMshort[EMshort$SAMPLED_FLAG=="Y",c("EM_HAUL_NUMBER")])))
#2877 reviewed hauls (~42% of hauls)

nonrev<-EMshort[EMshort$SAMPLED_FLAG=="N",]
nrow(nonrev) #4097 hauls to select from, target 10%, so ~400 hauls

#Criteris: 
#1) Sablefish in 650 in Sept
#2) halibut 620 May-Aug
#3) halibut 659 May/Sept

#Criteria 1
C1<-nonrev[nonrev$TRIP_TARGET_NAME=="Sablefish" & 
             nonrev$Month==9 & 
             nonrev$REPORTING_AREA_CODE==650,]
nrow(C1)
#59 hauls

#Criteria 2
C2<-nonrev[nonrev$TRIP_TARGET_NAME=="Halibut" & 
             (nonrev$Month>4 & nonrev$Month<10) & 
             nonrev$REPORTING_AREA_CODE==620,]
nrow(C2)
#271 hauls

#Criteria 3
C3<-nonrev[nonrev$TRIP_TARGET_NAME=="Halibut" & 
             (nonrev$Month==5 | nonrev$Month==9) & 
             nonrev$REPORTING_AREA_CODE==659,]
nrow(C3)
#88 hauls

revhauls<-rbind(C1,C2,C3)
nrow(revhauls)  

write.csv(revhauls,paste(getwd(),"/Outside_Analyses/EM/EM_sharks_review.csv",sep=""),row.names=F)

# Now to find the previously reviewed hauls with sharks
specs<-c(689,690,692)
prev<-EMshort[EMshort$SAMPLED_FLAG=="Y" & 
                EMshort$AGENCY_SPECIES_CODE %in% specs,]

#combine lists
rh2<-revhauls[,c("ODDS_TRIP_NUMBER","EM_HAUL_NUMBER","SAMPLED_FLAG",
                 "Month","YEAR","HAUL_TARGET_NAME","REPORTING_AREA_CODE")]
prev2<-prev[,c("ODDS_TRIP_NUMBER","EM_HAUL_NUMBER","SAMPLED_FLAG",
                   "Month","YEAR","HAUL_TARGET_NAME","REPORTING_AREA_CODE")]
shark_review<-rbind(rh2,prev2)
write.csv(shark_review,paste(getwd(),"/Outside_Analyses/EM/EM_sharks_review.csv",sep=""),row.names=F)
