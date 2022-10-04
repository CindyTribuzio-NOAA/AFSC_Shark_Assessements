#AKRO CAS Data ----
#Updated 9/24/2020 by C. Tribuzio
#This code will pull the data from AKFIN and clean it up for use in the assessment


# Still to do list ----
##1) auto query from AKFIN 

# Setup ----
datapath<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outpath<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")


# Clean old data ----
# should only need to run this once, then never again, here for records
# old_dat<-as.data.frame(read.csv(paste(getwd(),"/Data/Static","/pre2003_shark_catch.csv",sep=""), header=T))
# colnames(old_dat)
#[1] "Year"                      "Week.Ending.Date"          "Week.Number"               "WED..mmdd."                "Detail.Record.Count"       "Date.Received.from.AKR"   
#[7] "Date.Loaded.to.Repository" "Processor.ID"              "Processor.Vessel.Name"     "Processor.Name"            "Processor.Plant.Operation" "Vessel.ADFG"              
#[13] "Vessel.Name"               "Vessel.Length"             "Trip.Target.Group"         "Trip.Target.Name"          "Species.Group"             "Species"                  
#[19] "Gear"                      "FMP.Area"                  "FMP.Subarea"               "NMFS.Area"                 "Retained.Discarded"        "X..Distinct.Processors"   
#[25] "X..Distinct.Vessels"       "Conf.Flag"                 "Catch..mt."                "Weight.Posted..Sum." 

#fixes species names and target names
#levels(old_dat$Species)[levels(old_dat$Species)=="\"shark, spiny dogfish\""]<-"Spiny Dogfish"
#levels(old_dat$Species)[levels(old_dat$Species)=="\"Pacific sleeper shark\""]<-"Pacific Sleeper Shark"
#levels(old_dat$Species)[levels(old_dat$Species)=="\"shark, salmon\""]<-"Salmon Shark"
#levels(old_dat$Species)[levels(old_dat$Species)=="\"shark, other\""]<-"Other Sharks"

#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Pollock - midwater"]<-"Pollock"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Pollock - bottom"]<-"Pollock"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Rex Sole - GOA"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Rock Sole - BSAI"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Arrowtooth Flounder"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Flathead Sole"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Kamchatka Flounder - BSAI"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Shallow Water Flatfish - GOA"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Yellowfin Sole - BSAI"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Greenland Turbot - BSAI"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Other Flatfish - BSAI"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)=="Deep Water Flatfish - GOA"]<-"Flatfish"
#levels(old_dat$Trip.Target.Group)[levels(old_dat$Trip.Target.Group)==""]<-"Other"

#old_dat<-old_dat[old_dat$Species!="",]

#write.csv(old_dat,paste(getwd(),"/Data/Static/pre2003_shark_cleaned.csv",sep=""),row.names = F)

# CAS DATA ----
# SQL query







catchdat<-read.csv(paste(datadir,"/Groundfish Total Catch by Fishery_shark",AYR,".csv",sep=""),header = T)

#fixes species names and target names
levels(catchdat$Species)[levels(catchdat$Species)=="\"shark, spiny dogfish\""]<-"Spiny Dogfish"
levels(catchdat$Species)[levels(catchdat$Species)=="\"Pacific sleeper shark\""]<-"Pacific Sleeper Shark"
levels(catchdat$Species)[levels(catchdat$Species)=="\"shark, salmon\""]<-"Salmon Shark"
levels(catchdat$Species)[levels(catchdat$Species)=="\"shark, other\""]<-"Other Sharks"

levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Pollock - midwater"]<-"Pollock"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Pollock - bottom"]<-"Pollock"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Rex Sole - GOA"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Rock Sole - BSAI"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Alaska Plaice - BSAI"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Arrowtooth Flounder"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Flathead Sole"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Kamchatka Flounder - BSAI"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Shallow Water Flatfish - GOA"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Yellowfin Sole - BSAI"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Greenland Turbot - BSAI"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Other Flatfish - BSAI"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Deep Water Flatfish - GOA"]<-"Flatfish"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)==""]<-"Other"
levels(catchdat$Trip.Target.Name)[levels(catchdat$Trip.Target.Name)=="Other Species"]<-"Other"

colnames(catchdat)[1]<-"Year"

write.csv(catchdat,paste(outdir,"/CAS_GFTBF_Sharks",AYR,".csv",sep=""),row.names=F)

# Test for changes in data ----
LAYR<-2018
CAS_layr<-read.csv(paste(getwd(),"/Data/Cleaned/",LAYR,"/CAS_GFTBF_Sharks",LAYR,".csv",sep=""),header=T)
Cl<-ddply(CAS_layr,c("Year","FMP.Area","NMFS.Area","Trip.Target.Name","Gear","Species"),summarize,OLD_Catch=sum(Catch..mt.))

Ca<-ddply(catchdat,c("Year","FMP.Area","NMFS.Area","Trip.Target.Name","Gear","Species"),summarize,NEW_Catch=sum(Catch..mt.))

CAS_test<-merge(Cl,Ca)
CAS_test$diff<-CAS_test$NEW_Catch-CAS_test$OLD_Catch
CAS_test$pdiff<-round(((CAS_test$NEW_Catch-CAS_test$OLD_Catch)/CAS_test$OLD_Catch)*100,2)

CAS_diff<-CAS_test[CAS_test$diff>1&CAS_test$Year<2018,]
write.csv(CAS_diff,paste(outdir,"/SAFE_Sharks_CAS_diffs.csv",sep=""),row.names=F)

Clsimple<-ddply(CAS_layr,c("Year","FMP.Area"),summarize,OLD_Catch=sum(Catch..mt.))
Casimple<-ddply(catchdat,c("Year","FMP.Area"),summarize,NEW_Catch=sum(Catch..mt.))
CAS_simple<-merge(Clsimple,Casimple)
CAS_simple$diff<-CAS_simple$NEW_Catch-CAS_simple$OLD_Catch
CAS_simple$pdiff<-round(((CAS_simple$NEW_Catch-CAS_simple$OLD_Catch)/CAS_simple$OLD_Catch)*100,2)
CAS_sdiff<-CAS_simple[CAS_simple$diff>1&CAS_simple$Year<2018,]
write.csv(CAS_sdiff,paste(outdir,"/SAFE_Sharks_CAS_simplified_diffs.csv",sep=""),row.names=F)

#Lets see if what I pulled in Sept is diff from Oct
sept<-read.csv(paste(outdir,"/CAS_GFTBF_Sharks_Sept.csv",sep=""),header = T)
septa<-ddply(sept,c("Year","FMP.Area","NMFS.Area","Trip.Target.Name","Gear","Species"),summarize,Sept_Catch=sum(Catch..mt.))

Ca<-ddply(catchdat,c("Year","FMP.Area","NMFS.Area","Trip.Target.Name","Gear","Species"),summarize,NEW_Catch=sum(Catch..mt.))
sept_test<-merge(septa,Ca)
sept_test$diff<-sept_test$NEW_Catch-sept_test$Sept_Catch
sept_test$pdiff<-round((sept_test$diff/sept_test$Sept_Catch)*100,2)
sept_diff<-sept_test[sept_test$diff>1&sept_test$Year<2020,]

write.csv(sept_diff,paste(outdir,"/2020datapulls_sharks.csv",sep=""),row.names=F)

septsimple<-ddply(sept,c("Year","FMP.Area"),summarize,OLD_Catch=sum(Catch..mt.))
sept2simple<-ddply(catchdat,c("Year","FMP.Area"),summarize,NEW_Catch=sum(Catch..mt.))
sept_simple<-merge(septsimple,sept2simple)
sept_simple$diff<-sept_simple$NEW_Catch-sept_simple$OLD_Catch
sept_simple$pdiff<-round(((sept_simple$NEW_Catch-sept_simple$OLD_Catch)/sept_simple$OLD_Catch)*100,2)
sept_sdiff<-sept_simple[sept_simple$diff>1&sept_simple$Year<2018,]
write.csv(CAS_sdiff,paste(outdir,"/2020datapulls_sharkssimplified.csv",sep=""),row.names=F)

# Noncommercial Catch ----
old_nc<-read.csv(paste(getwd(),"/Data/Static/Non_comm_catchpre2010.csv",sep=""),header=T)
colnames(old_nc)<-c("Year","Source","AFSC_TWLt","AFSC_LLn","AFSC_LLt",
                    "IPHC_LLn","IPHC_LLt","ADFG_t","FMP")
old<-melt(old_nc,id.vars = c("Year","Source","FMP"))
colnames(old)[4:5]<-c("Survey","Volume")

newnc<-read.csv(paste(datadir,'/Noncommercial Fishery Catch_',AYR,".csv",sep=""),header=T)
colnames(newnc)[1]<-"Year"
#newnc[is.na(newnc)]<-""

#do ADFG separately, because these are combined in the table
ADFG<-newnc[newnc$Collection.Agency=="ADFG",]
ADFG2<-ddply(ADFG,c("Year","Collection.Agency","FMP.Area"),
             summarize,Totn=sum(Number.Animals),Volume=sum(Weight)/1000)
ADFG2$Survey<-"ADFG_t"
ADFG2$Source<-"AKRO"
ADFG2$FMP.Area<-revalue(ADFG2$FMP.Area, c("AI" = "BSAI","BS"="BSAI"))
colnames(ADFG2)[3]<-"FMP"
ADFG2<-ADFG2[,c("Year","Source","FMP","Survey","Volume")]
#add in zeros that are missing
ADFG3<-melt(dcast(ADFG2,Year+Source+Survey~FMP,fill=0),id.vars=c("Year","Source","Survey"))
ADFG3<-ADFG3[,c("Year","Source","variable","Survey","value")]
colnames(ADFG3)<-colnames(ADFG2)

#now all the rest
nonFG<-newnc[newnc$Collection.Agency!="ADFG",]
nonFG$OBS.Gear.Code<-revalue(as.factor(nonFG$OBS.Gear.Code), c("8" = "LL",
                                                           "9" = "LL",
                                                           "1" = "TWL",
                                                           "2" = "TWL"))
#make sure that only NMFS LL has an NA gear code
unique(nonFG[is.na(nonFG$OBS.Gear.Code),]$Collection.Agency)
nonFG[is.na(nonFG$OBS.Gear.Code),]$OBS.Gear.Code<-"LL"

#IPHC should not have zeros in the Number.Animals, these should all be blanks
#IPHC only reports weight, not numbers, there are no numbers data 2010+

#fix FMPs
nonFG$FMP.Area<-revalue(nonFG$FMP.Area, c("AI" = "BSAI","BS"="BSAI"))
nonFG$Collection.Agency<-revalue(nonFG$Collection.Agency, c("NMFS" = "AFSC"))

nnc<-ddply(nonFG,c("Year","Collection.Agency","OBS.Gear.Code","FMP.Area"),summarize,
           n=sum(Number.Animals),t=sum(Weight)/1000)


#create new survey names
nnc2<-melt(nnc,id.vars=c("Year","Collection.Agency","OBS.Gear.Code","FMP.Area"))
nnc2$Survey<-paste(nnc2$Collection.Agency,"_",nnc2$OBS.Gear.Code,nnc2$variable,sep="")
nnc2$Source<-"AKRO"
colnames(nnc2)[4]<-"FMP"
colnames(nnc2)[6]<-"Volume"
nnc3<-nnc2[,c("Year","Source","FMP","Survey","Volume")]

#add in zeros that are missing
nnc4<-melt(dcast(nnc3,Year+Source+FMP~Survey,fill=0),id.vars=c("Year","Source","FMP"))
colnames(nnc4)<-colnames(nnc3)

noncomm<-rbind(old,ADFG3,nnc4)
noncomm<-noncomm[!is.na(noncomm$Volume),]

write.csv(noncomm,paste(outdir,"/Noncommercial",AYR,".csv",sep=""),row.names = F)

















