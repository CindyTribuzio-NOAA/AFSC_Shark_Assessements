# Tier 5 and 6 ABC and OFL Calculations with Tier 6 alternatives ----

# Setup ----
library(plyr)
library(reshape2)

AYR<-2020 #assessment year

# Tier 5 Spiny Dogfish ----
#Updated 9/28/2020 by C. Tribuzio

# Directories ----
datadir<-paste(getwd(),"/Output/",AYR,"/RFX/",sep="")
outdir<-paste(getwd(),"/Output/",AYR,"/Harvest Specs/",sep="")
if(!dir.exists(outdir)){
  dir.create(paste(getwd(),"/Output/",AYR,"/Harvest Specs/",sep=""))
  print("created new directory")
}else{
  print("directory exists")
}

# Tier 5 Calcs ----
FOFL<-0.04 #from literature Tribuzio and Kruse 2012
FABC<-0.75*FOFL
q<-0.21

RFXdat<-read.csv(paste(datadir,"/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header=T)

cRFX<-RFXdat[RFXdat$YEAR==AYR & RFXdat$REGULATORY_AREA_NAME=="GOA",]$Biom_est
T5OFL<-(cRFX/q)*FOFL
T5ABC<-(cRFX/q)*FABC

T5specs<-as.data.frame(cbind(T5OFL,T5ABC,cRFX,FOFL,FABC))
T5specs$Metric<-"T5RFXq"
T5specs$FMP.Area<-"GOA"
T5specs<-T5specs[,c("Metric","FMP.Area","T5OFL","T5ABC","cRFX","FOFL","FABC")]
colnames(T5specs)[3:5]<-c("OFL","ABC","Biomass")

#Tier 6 Models for AFSC Sharks ----
#Updated 9/24/2020 by C. Tribuzio

# Data Info ----
#Data are queried from AKFIN Groundfish Total Catch by Fishery Table
#2003-current
#deselect the targets
#leave everything as null except species
#four species available so far: spiny dogfish, salmon shark, sleeper shark, other shark
#save file to "data" folder as .csv "Groundfish Total Catch By Fishery_SharkYYYY.csv"

# Directories ----
datadir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
outdir<-paste(getwd(),"/Output/",AYR,"/Harvest Specs/",sep="")
if(!dir.exists(outdir)){
  dir.create(paste(getwd(),"/Output/",AYR,"/Harvest Specs/",sep=""))
  print("created new directory")
}else{
  print("directory exists")
}

# Tier 6 calcs ----
# bring in data and combine/filter
CASdat<-read.csv(paste(datadir,"/CAS_GFTBF_Sharks.csv",sep=""),header=T)
Cd<-CASdat[,c("Year","FMP.Area","Species","Catch..mt.")]

olddat<-read.csv(paste(getwd(),"/Data/Static/pre2003_shark_cleaned.csv",sep=""),header=T)
od<-olddat[,c("Year","FMP.Area","Species","Catch..mt.")]

T6dat<-rbind(od,Cd)
#remove spiny dogfish in GOA because they are Tier 5
T6dat<-T6dat[!(T6dat$Species=="Spiny Dogfish"&T6dat$FMP.Area=="GOA"),]

#remove INSD waters info because not used in assesments
T6dat<-T6dat[!T6dat$FMP.Area=="INSD",]

#remove years that are not relevant
# GOA time frame is 1997-2007
# BSAI time frame is 2003-2015
T6dat<-T6dat[((T6dat$Year>2002&T6dat$Year<2016)&T6dat$FMP.Area=="BSAI")|
          ((T6dat$Year>1996&T6dat$Year<2008)&T6dat$FMP.Area=="GOA"),]

# This code includes all potential Tier 6 methods proposed to date. It's good to keep them handy for comparison.
# Current methods for GOA: mean historical catch
# Current method for BSAI: max historical, note that there is some discussion on summing the species or going with complex max, 
#     complex total max is what is currently used. This code looks at it both ways


# list of metrics to include: 
#     mean by species then summed, 
#     max by species then summed, 
#     max of total
#     5/95th percentile of data by species then summed
#     5/95% CI of mean by species then summed

T6metrics<-ddply(ddply(T6dat,c("Year","FMP.Area","Species"),summarize,Tot_Catch=sum(Catch..mt.)),
                 c("FMP.Area","Species"),summarize,
                 T6mean=mean(Tot_Catch),
                 T65pct=t.test(Tot_Catch)$conf.int[1],
                 T695pct=t.test(Tot_Catch)$conf.int[2],
                 T65ptl=quantile(Tot_Catch,0.05),
                 T625ptl=quantile(Tot_Catch,0.25),
                 T6med=median(Tot_Catch),
                 T695ptl=quantile(Tot_Catch,0.95),
                 T699ptl=quantile(Tot_Catch,0.99),
                 T6maxsp=max(Tot_Catch))
T6specs<-melt(dcast(melt(T6metrics),variable~FMP.Area,fun.aggregate = sum))
colnames(T6specs)<-c("Metric","FMP.Area","OFL")

T6maxc<-ddply(ddply(T6dat,c("Year","FMP.Area"),summarize,Tot_Catch=sum(Catch..mt.)),
              c("FMP.Area"),summarize,
              OFL=max(Tot_Catch))
T6maxc$Metric<-"T6maxc"
T6maxc<-T6maxc[,c("Metric","FMP.Area","OFL")]

T6specs<-rbind(T6specs,T6maxc)
T6specs$ABC<-T6specs$OFL*0.75

T6specs$Biomass<-NA
T6specs$FABC<-NA
T6specs$FOFL<-NA

#combine specs for output table
Shark_specs<-rbind(T5specs,T6specs)
write.csv(Shark_specs,paste(outdir,"Shark_specs",AYR,".csv",sep=""),row.names = F)

# Comp to last assess ----
LAYR<-2018 #Last Assessment YR

# How much did specs change?
#screen for changes in data
#first question: did the OFL/ABCs change everywhere?
#there is something wonky, because the 2018 doesn't match what's in the assessment
pspecs<-read.csv(paste(getwd(),"/Output/",LAYR,"/Harvest Specs/Shark_specs",LAYR,".csv",sep=""))
Shark_specs$pOFL<-pspecs$OFL
Shark_specs$pchange<-round(((Shark_specs$OFL-Shark_specs$pOFL)/Shark_specs$pOFL)*100,2)

#next identify the specific species/FMPs where change occurred
#haven't done this yet because we didn't need to in 2020

#next identify any specific sub-areas where change occurred

#then go to the data 

#brings in last years data file, noted "p" for previous
#pdatadir<-paste(getwd(),"/Data/Cleaned/",LAYR,sep="")
#pCASdat<-read.csv(paste(pdatadir,"/CAS_GFTBF_Sharks.csv",sep=""),header=T)
#pCd<-CASdat[,c("Year","FMP.Area","Species","Catch..mt.")]


