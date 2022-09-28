# Shark Assessment Figs ----
# Updated 13_10_2020 by C. Tribuzio

#Setup ----
libs <- c("tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

#'%nin%'<-Negate('%in%')

AYR<-2020
cleandatdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")

# Data ----
CAS_dat<-read.csv(paste(cleandatdir,"/CAS_GFTBF_Sharks",AYR,".csv",sep=""),header=T)
CAS_dat<-CAS_dat[CAS_dat!="INSD",]

# FMP level summary ----
FMP_dat<-ddply(CAS_dat,c("Year","FMP.Area","Week.Number"),summarize,Tot_Catch=sum(Catch..mt.))
FMP_dat<-FMP_dat[!(is.na(FMP_dat$Year)),]
FMP_dat$Week.Number<-as.factor(FMP_dat$Week.Number)

#pull out 2020 data then  add it back in later
FMP_2020<-FMP_dat[FMP_dat$Year==2020,]
FMP_dat<-FMP_dat[FMP_dat$Year<2020,]
FMP_dat<-FMP_dat %>% complete(Year,FMP.Area,Week.Number,fill=list(Tot_Catch = 0))
FMP_dat<-rbind(FMP_dat,FMP_2020)

FMP2<-FMP_dat %>% group_by(FMP.Area,Year) %>% dplyr::mutate(Cum_Catch=cumsum(Tot_Catch),
                                                       Prop_Catch=(Cum_Catch/sum(Tot_Catch))*100)
colnames(FMP2)<-c("Year","FMP","Week","Total_Catch","Cumm_Catch","Prop_Catch")

write.csv(FMP2,paste(cleandatdir,"/FMP_cumm_catch",AYR,".csv",sep=""),row.names = F)

# Summary By Species at FMP level ----
spec_dat<-ddply(CAS_dat,c("Year","FMP.Area","Week.Number","Species"),summarize,Tot_Catch=sum(Catch..mt.))
spec_dat<-spec_dat[!(is.na(spec_dat$Year)),]
spec_dat$Week.Number<-as.factor(spec_dat$Week.Number)
#pull out 2020 data then  add it back in later
spec_2020<-spec_dat[spec_dat$Year==2020,]
spec_dat<-spec_dat[spec_dat$Year<2020,]
spec_dat<-spec_dat %>% complete(Species,Year,FMP.Area,Week.Number,fill=list(Tot_Catch = 0))
spec_dat<-rbind(spec_dat,spec_2020)

spec2<-spec_dat %>% group_by(FMP.Area,Year,Species) %>% dplyr::mutate(Cum_Catch=cumsum(Tot_Catch),
                                                       Prop_Catch=(Cum_Catch/sum(Tot_Catch))*100)
colnames(spec2)<-c("Species","Year","FMP","Week","Total_Catch","Cumm_Catch","Prop_Catch")

write.csv(spec2,paste(cleandatdir,"/Species_cumm_catch",AYR,".csv",sep=""),row.names = F)


# Summary ----
# questions: what proportion of catch remains after Oct 1?

# About what week is the first week of Oct?
mean(unique(CAS_dat[CAS_dat$Week.Number==40,]$WED..mmdd.),na.rm=T)

#so cut off about at week 40
#how much catch happens after week 40?
#add a pre/post flag to outputs
FMP2$period<-NA
FMP2[FMP2$Week<=40,]$period<-"Pre"
FMP2[FMP2$Week>40,]$period<-"Post"
FMP_summ<-ddply(FMP2,c("Year","FMP","period"),summarize,Tot_Catch=sum(Total_Catch))
FMP_summ2<-dcast(FMP_summ,Year+FMP~period)
FMP_summ2$pPost<-FMP_summ2$Post/(FMP_summ2$Post+FMP_summ2$Pre)*100

FMP_mean<-ddply(FMP_summ2,c("FMP"),summarize,Fmean=mean(pPost))

#what proportion by species catch is after week 40?
spec_dat$period<-NA
spec_dat[spec_dat$Week<=40,]$period<-"Pre"
spec_dat[spec_dat$Week>40,]$period<-"Post"
spec_dat_summ<-ddply(spec_dat,c("Year","FMP","Species","period"),summarize,Tot_Catch=sum(Total_Catch))
spec_dat_summ2<-dcast(spec_dat_summ,Year+FMP+Species~period)
spec_dat_summ2$pPost<-spec_dat_summ2$Post/(spec_dat_summ2$Post+spec_dat_summ2$Pre)*100

spec_dat_mean<-ddply(spec_dat_summ2,c("FMP","Species"),summarize,spec_mean=mean(pPost))
