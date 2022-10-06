# Compile ADFG survey data ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: 5_10_2020

# Setup ----
datapath<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outpath<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
oldpath<-paste(getwd(),"/Data/Cleaned/",LAYR,sep="")

# SEAK LL Survey ----
# Contacted Rhea Eresmann Oct 4 2022

# Retrieve Data
# old data
ADFG_old<-read.csv(paste(olddir,"/ADFG_SEAK_LL",LAYR,".csv",sep=""),header=T)

ADFG_update<-read.csv(paste(datadir,"/ADFG NSEI and SSEI LL Survey Data",AYR,".csv",sep=""),header=T)
# turn all nas to zeros
ADFG_update[is.na(ADFG_update)]<-0
up<-ADFG_update[,c("Year","Project","Station.No","Trip.No","Set.No","Start.Latitude.Decimal.Degrees",
                   "Start.Longitude.Decimal.Degree","Avg.Depth.Fathoms","Hooks...Total.Number","Hooks...Number.with.Bait",
                   "Hooks...Number.Bare","Hooks...Number.Invalid","Sablefish","Dogfish","Sleeper.Shark")]

up3<-ddply(up,c("Year","Project","Station.No","Trip.No","Set.No","Start.Latitude.Decimal.Degrees",
                "Start.Longitude.Decimal.Degree","Avg.Depth.Fathoms"),summarize,
          HKS_tot=sum(Hooks...Total.Number),
          HKS_bait=sum(Hooks...Number.with.Bait),
          HKS_bare=sum(Hooks...Number.Bare),
          HKS_ineff=sum(Hooks...Number.Invalid),
          Sable_Tot=sum(Sablefish),
          DF_Tot=sum(Dogfish),
          PSS_Tot=sum(Sleeper.Shark))

colnames(up3)<-colnames(ADFG_old)

#combine for complete dataset
ADFGSEAKLL<-rbind(ADFG_old,up3)
write.csv(ADFGSEAKLL,paste(outdir,"/ADFG_SEAK_LL",AYR,".csv",sep=""),row.names = F)

#Kamishak Bay Trawl survey ----
#Data from M Byerly 13_10_2020
#As per email from M Byerly Oct 6 2022 no new data for these surveys

adfg_twl_dat<-read.csv(paste(datadir,"/ADFG LgMesh shark catch 1997to2019.csv",sep=""), header=T)

mdat<-melt(adfg_twl_dat,c("year","proj","n","species"))
mdat[grep("691", mdat$species), "species"] <- "Spiny Dogfish"
mdat[grep("692", mdat$species), "species"] <- "Pacific Sleeper Shark"
mdat[grep("690", mdat$species), "species"] <- "Salmon Shark"
mdat[grep("ct", mdat$variable), "Measure"] <- "Count"
mdat[grep("kg", mdat$variable), "Measure"] <- "Kilograms"
mdat[grep("cpue_ct", mdat$variable), "Measure"] <- "Count_CPUE"
mdat[grep("cpue_ct_s.e", mdat$variable), "Measure"] <- "Count_CPUE_var"
mdat[grep("den_ct_sqnmi", mdat$variable), "Measure"] <- "Count_density"
mdat[grep("cpue_kg_nmi", mdat$variable), "Measure"] <- "Kg_CPUE"
mdat[grep("cpue_kg_s.e", mdat$variable), "Measure"] <- "Kg_CPUE_var"
mdat[grep("den_kg_sqnmi", mdat$variable), "Measure"] <- "Kg_density"

mdat2<-dcast(mdat,year+proj+n+species~Measure,value.var="value",
             fun.aggregate = sum)
write.csv(mdat2,paste(outdir,"/ADFG_LRGTWL",AYR,".csv",sep=""),row.names=F)

#updated data
twl_old<-read.csv(paste(olddir,"/ADFG_LRGTWL",LAYR,".csv",sep=""),header=T)
twl_update<-read.csv(paste(datadir,"/largeMeshSharkCatch_89to15.csv",sep=""), header=T)

mdat<-melt(adfg_twl_dat,c("Year","PROJECT_CODE","n"))
mdat[grep("691", mdat$variable), "species"] <- "Spiny Dogfish"
mdat[grep("692", mdat$variable), "species"] <- "Pacific Sleeper Shark"
mdat[grep("690", mdat$variable), "species"] <- "Salmon Shark"
mdat[grep("Cnt_tot", mdat$variable), "Measure"] <- "Count"
mdat[grep("Kg_tot", mdat$variable), "Measure"] <- "Kilograms"
mdat[grep("Cnt_pNmi_mean", mdat$variable), "Measure"] <- "Count_CPUE"
mdat[grep("Cnt_pNmi_var", mdat$variable), "Measure"] <- "Count_CPUE_var"
mdat[grep("Kg_pNmi_mean", mdat$variable), "Measure"] <- "Kg_CPUE"
mdat[grep("Kg_pNmi_var", mdat$variable), "Measure"] <- "Kg_CPUE_var"

mdat2<-dcast(mdat,Year+PROJECT_CODE+n+species~Measure,value.var="value",fun.aggregate = sum)

TWL_new<-rbind(twl_old,mdat2)
write.csv(TWL_new,paste(outdir,"/ADFG_LRGTWL",AYR,".csv",sep=""),row.names=F)






# ADFG SWHS catch ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: 20_10_2020
# data provided by Sarah Webster
# updated data expected 10/14/2022 as per email Oct 4 2022

# Setup ----
datapath<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outpath<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
oldpath<-paste(getwd(),"/Data/Cleaned/",LAYR,sep="")

# Get Data ----
# NOTE: these data are formatted horribly when they come in, takes a lot of massaging to make it nice
ADFG<-read.csv(paste(datadir,"/ADFG_shark_harvest_",AYR,".csv",sep=""),header=T,skip=5)
sptyear<-AYR-1998
ssyear<-sptyear+7
sport<-ADFG[1:sptyear,]
Sshark<-ADFG[ssyear:(ssyear+nyear-1),]

#all sharks
harvest<-sport[,c("Year","Western","Central","Eastern")]
harvest$Central<-as.numeric(gsub(",","",harvest$Central))
harvest$Western<-as.numeric(gsub(",","",harvest$Western))
harvest$Eastern<-as.numeric(gsub(",","",harvest$Eastern))
h2<-melt(harvest,id.vars="Year")
h2$Type<-"Retained"
h2$Species<-"All Sharks"
colnames(h2)<-c("Year","NMFS_Area","Nfish","Type","Species")

discard<-sport[,c("Year","Western.3","Central.3","Eastern.3")]
colnames(discard)<-c("Year","Western","Central","Eastern")
discard$Central<-as.numeric(gsub(",","",discard$Central))
discard$Western<-as.numeric(gsub(",","",discard$Western))
discard$Eastern<-as.numeric(gsub(",","",discard$Eastern))
d2<-melt(discard,id.vars="Year")
d2$Type<-"Discarded"
d2$Species<-"All Sharks"
colnames(d2)<-c("Year","NMFS_Area","Nfish","Type","Species")

#salmon shark charter
sshark<-Sshark[,c("Year","Western","Central","Eastern")]
sshark[sshark$Year==1999,2:4]<-NA
sshark$Central<-as.numeric(gsub(",","",sshark$Central))
sshark$Western<-as.numeric(gsub(",","",sshark$Western))
sshark$Eastern<-as.numeric(gsub(",","",sshark$Eastern))
s2<-melt(sshark,id.vars="Year")
s2$Type<-"Retained"
s2$Species<-"Salmon Shark"
colnames(s2)<-c("Year","NMFS_Area","Nfish","Type","Species")

sport2<-rbind(h2,d2,s2)
write.csv(sport2,paste(outdir,"/ADFG_sportharvest",AYR,".csv",sep=""),row.names=F)
