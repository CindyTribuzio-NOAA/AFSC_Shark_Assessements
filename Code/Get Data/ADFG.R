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
# old data, anything prior to 2019 is no longer updated
ADFG_old<-read_csv(paste0(oldpath,"/ADFG_SEAK_LL",LAYR,".csv"))

ADFG_update<-read_csv(paste0(datapath,"/ADFG NSEI and SSEI LL Survey Data",AYR,".csv")) %>% 
  clean_names() %>% 
  select(year, project, station_no, trip_no, set_no, start_latitude_decimal_degrees, start_longitude_decimal_degree, 
         avg_depth_fathoms, hooks_total_number, hooks_number_with_bait, hooks_number_bare, hooks_number_invalid,
         sablefish, dogfish, sleeper_shark) %>% 
  group_by(year, project, station_no, trip_no, set_no, start_latitude_decimal_degrees, start_longitude_decimal_degree, 
           avg_depth_fathoms) %>% 
  summarise(hks_tot = sum(hooks_total_number),
            hks_bait = sum(hooks_number_with_bait),
            hks_bare = sum(hooks_number_bare),
            hks_ineff = sum(hooks_number_invalid),
            sable_tot = sum(sablefish),
            df_tot = sum(dogfish),
            pss_tot = sum(sleeper_shark))

colnames(ADFG_update)<-colnames(ADFG_old)

#filter out 2019 - present from "old" dataset
minyr <- min(ADFG_update$Year)
ADFG_old <- ADFG_old %>% filter(Year < minyr)

#combine for complete dataset
ADFGSEAKLL<-rbind(ADFG_old, ADFG_update)
write_csv(ADFGSEAKLL,paste(outpath,"/ADFG_SEAK_LL",AYR,".csv",sep=""))

#Kamishak Bay Trawl survey ----
#Data from M Byerly 13_10_2020
#As per email from M Byerly Oct 6 2022 no new data for these surveys, copied previous file to 2022 folder
# code still needs to be updated to dplyr

adfg_twl_dat<-read_csv(paste0(datapath,"/ADFG LgMesh shark catch 1997to2019.csv"))

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
# Last Updated: 11 Oct 2022
# data provided by Sarah Webster

# Setup ----
#datapath<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
#outpath<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
#oldpath<-paste(getwd(),"/Data/Cleaned/",LAYR,sep="")

# Get Data ----
# NOTE: these data are formatted horribly when they come in, takes a lot of massaging to make it nice.
# Comes in as an .xlsx, save as .csv first
ADFG_sport<-read_csv(paste0(datapath,"/ADFG_shark_harvest_",AYR-1,".csv"),skip=5) %>%  clean_names()
sptyear<-AYR-1998
ssyear<-sptyear+7
nyear <- AYR - 1998
sport<-ADFG_sport[1:sptyear,]
Sshark<-ADFG_sport[ssyear:(ssyear+nyear-1),]

#all sharks
harvest <- sport %>% 
  select(year_1, western_2, central_3, eastern_4) %>% 
  rename(year = year_1, 
         wgoa = western_2,
         cgoa = central_3,
         egoa = eastern_4) %>% 
  mutate(year = as.numeric(year),
         wgoa = as.numeric(gsub(",","", wgoa)),
         cgoa = as.numeric(gsub(",","", cgoa)),
         egoa = as.numeric(gsub(",","", egoa))) %>% 
  pivot_longer(!year, names_to = "NMFS_Area", values_to = "Nfish") %>% 
  mutate(Type = "Retained",
         Species = "All Sharks")

discard <- sport %>% 
  select(year_1, western_15, central_16, eastern_17) %>% 
  rename(year = year_1, 
         wgoa = western_15,
         cgoa = central_16,
         egoa = eastern_17) %>% 
  mutate(year = as.numeric(year),
         wgoa = as.numeric(gsub(",","", wgoa)),
         cgoa = as.numeric(gsub(",","", cgoa)),
         egoa = as.numeric(gsub(",","", egoa))) %>% 
  pivot_longer(!year, names_to = "NMFS_Area", values_to = "Nfish") %>% 
  mutate(Type = "Discarded",
         Species = "All Sharks")

#salmon shark charter
salshark <- Sshark %>% 
  select(year_1, western_2, central_3, eastern_4) %>% 
  rename(year = year_1, 
         wgoa = western_2,
         cgoa = central_3,
         egoa = eastern_4) %>% 
  mutate(year = as.numeric(year),
         wgoa = as.numeric(gsub(",","", wgoa)),
         cgoa = as.numeric(gsub(",","", cgoa)),
         egoa = as.numeric(gsub(",","", egoa))) %>% 
  pivot_longer(!year, names_to = "NMFS_Area", values_to = "Nfish") %>% 
  mutate(Type = "Retained",
         Species = "Salmon Shark")

sshark<-Sshark[,c("Year","Western","Central","Eastern")]
sshark[sshark$Year==1999,2:4]<-NA
sshark$Central<-as.numeric(gsub(",","",sshark$Central))
sshark$Western<-as.numeric(gsub(",","",sshark$Western))
sshark$Eastern<-as.numeric(gsub(",","",sshark$Eastern))
s2<-melt(sshark,id.vars="Year")
s2$Type<-"Retained"
s2$Species<-"Salmon Shark"
colnames(s2)<-c("Year","NMFS_Area","Nfish","Type","Species")

sport2<-rbind(harvest, discard, salshark)
write.csv(sport2,paste(outpath,"/ADFG_sportharvest",AYR,".csv",sep=""),row.names=F)
