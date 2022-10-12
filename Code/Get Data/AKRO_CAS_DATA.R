#AKRO CAS Data ----
#Updated 10/6/2022 by C. Tribuzio
#This code will pull the data from AKFIN and clean it up for use in the assessment


# Still to do list ----


# Setup ----
datapath<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outpath<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")

dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))


# Pre-2003 Catch Data ----
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
# SQL query with some data manipulation
CASdat <- sqlQuery(channel_akfin, query = ("
                select year, week_end_date, wed, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code,
                       trip_target_group, trip_target_name, species_group_name, species_name, processor_permit_id,
                       ito_company processor_name, ito_plant processor_plant, ves_akr_adfg, ves_akr_name,	ves_akr_length,
                       retained_or_discarded, weight_posted, gf_harvest_sector, deployment_trip_start_date,	deployment_trip_end_date,
                       deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category, sampling_strata,
                       sampling_strata_name, sampling_strata_selection_rate
                from council.comprehensive_blend_ca
                where year >= 2003
                and (fmp_area = 'GOA' or fmp_area = 'BSAI' or fmp_area = 'INSD')
                and (species_name = 'shark, spiny dogfish' or 
                     species_name = 'Pacific sleeper shark' or 
                     species_name = 'shark, salmon' or
                     species_name = 'shark, other')")) %>% 
  clean_names() %>% 
  as.data.frame() %>% 
  group_by(year, week_end_date, wed, fmp_area,	fmp_subarea, reporting_area_code, agency_gear_code,
           trip_target_group, trip_target_name, species_group_name, species_name, processor_permit_id,
           processor_name, processor_plant, ves_akr_adfg, ves_akr_name,	ves_akr_length,
           retained_or_discarded, gf_harvest_sector, deployment_trip_start_date,	deployment_trip_end_date,
           deployment_trip_pk, monitoring_status,	sampling_strata_deployment_category, sampling_strata,
           sampling_strata_name, sampling_strata_selection_rate) %>% 
  summarise(catch_mt = sum(weight_posted)) %>% 
  mutate(species = if_else(species_name == "shark, spiny dogfish", "Spiny Dogfish",
                           if_else(species_name == 'Pacific sleeper shark', "Pacific Sleeper Shark",
                                   if_else(species_name == "shark, salmon", "Salmon Shark", 'Other Sharks'))),
         trip_target = if_else(trip_target_name == "Pollock - midwater", "Pollock",
                               if_else(trip_target_name == "Pollock - bottom", "Pollock",
                                       if_else(trip_target_name == "Pacific Cod", "Pacific Cod",
                                               if_else(trip_target_name == "Atka Mackerel", "Atka Mackerel",
                                                       if_else(trip_target_name == "Other Species", "Other",
                                                               if_else(trip_target_name == "Sablefish", "Sablefish",
                                                                       if_else(trip_target_name == "Halibut", "Halibut",
                                                                               if_else(trip_target_name == "Rockfish", "Rockfish",
                                                                                       if_else(is.na(trip_target_name), "Other", "Flatfish")))))))))) %>% 
  select(-c(species_name))
write_csv(CASdat, paste0(datapath, "/confidential_CAS_sharks", AYR, ".csv"))


# Test for changes in data ----
# Does updated code data query match what AKFIN Answers creates?
# Should only need to do this for 2022, first year using new query. Keep for records in case of changes
#AKFIN_2022 <- read_csv(paste(getwd(),"/Data/Annual_updates/",AYR,"/confidential Groundfish Total Catch by Fishery_AKFIN2022.csv",sep="")) %>% 
#  clean_names() %>% 
#  group_by(year, fmp_area, nmfs_area, trip_target_name, gear, species) %>% 
#  summarise(AKFIN_catch = sum(catch_mt)) %>% 
#  mutate(species2 = if_else(species == "\"shark, spiny dogfish\"", "Spiny Dogfish",
#                           if_else(species == "\"Pacific sleeper shark\"", "Pacific Sleeper Shark",
#                                   if_else(species == "\"shark, salmon\"", "Salmon Shark", 'Other Sharks')))) %>% 
#  select(-species) %>% 
#  rename(species = species2)

#NEW <- CASdat %>% 
#  group_by(year, fmp_area, reporting_area_code, trip_target_name, agency_gear_code, species) %>% 
#  summarise(CODE_catch = sum(catch_mt)) %>% 
#  rename(nmfs_area = reporting_area_code,
#         gear = agency_gear_code) %>% 
#  filter(year > 2009 & year < 2021)

#testcode <- NEW %>% 
#  left_join(AKFIN_2022) %>% 
#  mutate(diff = CODE_catch - AKFIN_catch,
#         pdiff = abs(round(((CODE_catch - AKFIN_catch)/AKFIN_catch)*100,2)))
#max(testcode$pdiff)

#write_csv(testcode, paste(outpath,"/CODEvAKFIN_CAS_comparison.csv",sep=""))


# Examine if changes to CAS data, 
# Coded for 2022 because first year of code based query, future iterations will be automated

CAS_layr<-read_csv(paste(getwd(),"/Data/Annual_updates/",LAYR,"/confidential_Groundfish Total Catch by Fishery_shark2020_Sept.csv",sep="")) %>% 
  clean_names() %>% 
  group_by(year, fmp_area, nmfs_area, trip_target_name, gear, species) %>% 
  summarise(OLD_catch = sum(catch_mt)) %>% 
  filter(year > 2009)

CAS_2022 <- read_csv(paste(getwd(),"/Data/Annual_updates/",AYR,"/confidential Groundfish Total Catch by Fishery_AKFIN2022.csv",sep="")) %>% 
  clean_names() %>% 
  group_by(year, fmp_area, nmfs_area, trip_target_name, gear, species) %>% 
  summarise(NEW_catch = sum(catch_mt))
CAS_comp <- CAS_layr %>% 
  left_join(CAS_2022) %>% 
  mutate(diff = NEW_catch - OLD_catch,
         pdiff = abs(round(((NEW_catch - OLD_catch)/OLD_catch)*100,2))) %>% 
  filter(diff > 5 | is.na(diff))

write_csv(CAS_comp,paste(outpath,"/confidential_CAS_changes_sharks.csv",sep=""))

# check for differences between last assessment year and this assessment year.....has CAS changed anything?
CAS_layr<-read_csv(paste(getwd(),"/Data/Cleaned/",LAYR,"/confidential_CAS_GFTBF_Sharks",LAYR,".csv",sep="")) %>% 
  clean_names() %>% 
  group_by(year, fmp_area, nmfs_area, trip_target_name, gear, species) %>% 
  summarise(OLD_catch = sum(catch_mt))
CAS_ayr <- CASdat %>% 
  rename(nmfs_area = reporting_area_code,
         gear = agency_gear_code,
         trip_target_name = trip_target) %>% 
  group_by(year, fmp_area, nmfs_area, trip_target_name, gear, species) %>%
  summarise(NEW_catch = sum(catch_mt)) %>% 
  left_join(CAS_layr) %>% 
  mutate(diff = NEW_catch - OLD_catch,
         pdiff = abs(round(((NEW_catch - OLD_catch)/OLD_catch)*100,2))) %>% 
  filter(abs(diff) > 1 & year < AYR)

write_csv(CAS_ayr,paste(outpath,"/confidential_SAFE_Sharks_CAS_diffs.csv",sep=""))

Clsimple<-ddply(CAS_layr,c("Year","FMP.Area"),summarize,OLD_Catch=sum(Catch..mt.))
Casimple<-ddply(catchdat,c("Year","FMP.Area"),summarize,NEW_Catch=sum(Catch..mt.))
CAS_simple<-merge(Clsimple,Casimple)
CAS_simple$diff<-CAS_simple$NEW_Catch-CAS_simple$OLD_Catch
CAS_simple$pdiff<-round(((CAS_simple$NEW_Catch-CAS_simple$OLD_Catch)/CAS_simple$OLD_Catch)*100,2)
CAS_sdiff<-CAS_simple[CAS_simple$diff>1&CAS_simple$Year<2018,]
write.csv(CAS_sdiff,paste(outdir,"/SAFE_Sharks_CAS_simplified_diffs.csv",sep=""),row.names=F)

#Lets see if what I pulled in Sept is diff from Oct
#sept<-read.csv(paste(outdir,"/CAS_GFTBF_Sharks_Sept.csv",sep=""),header = T)
#septa<-ddply(sept,c("Year","FMP.Area","NMFS.Area","Trip.Target.Name","Gear","Species"),summarize,Sept_Catch=sum(Catch..mt.))

#Ca<-ddply(catchdat,c("Year","FMP.Area","NMFS.Area","Trip.Target.Name","Gear","Species"),summarize,NEW_Catch=sum(Catch..mt.))
#sept_test<-merge(septa,Ca)
#sept_test$diff<-sept_test$NEW_Catch-sept_test$Sept_Catch
#sept_test$pdiff<-round((sept_test$diff/sept_test$Sept_Catch)*100,2)
#sept_diff<-sept_test[sept_test$diff>1&sept_test$Year<2020,]

#write.csv(sept_diff,paste(outdir,"/2020datapulls_sharks.csv",sep=""),row.names=F)

#septsimple<-ddply(sept,c("Year","FMP.Area"),summarize,OLD_Catch=sum(Catch..mt.))
#sept2simple<-ddply(catchdat,c("Year","FMP.Area"),summarize,NEW_Catch=sum(Catch..mt.))
#sept_simple<-merge(septsimple,sept2simple)
#sept_simple$diff<-sept_simple$NEW_Catch-sept_simple$OLD_Catch
#sept_simple$pdiff<-round(((sept_simple$NEW_Catch-sept_simple$OLD_Catch)/sept_simple$OLD_Catch)*100,2)
#sept_sdiff<-sept_simple[sept_simple$diff>1&sept_simple$Year<2018,]
#write.csv(CAS_sdiff,paste(outdir,"/2020datapulls_sharkssimplified.csv",sep=""),row.names=F)

# Noncommercial Catch ----
# need to build the query for this still
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

















