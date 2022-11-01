# Tier 5 and 6 ABC and OFL Calculations with Tier 6 alternatives ----

# Tier 5 Spiny Dogfish ----
#Updated 10/13/2022 by C. Tribuzio

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

cRFX<-round(RFXdat[RFXdat$YEAR==SYR & RFXdat$REGULATORY_AREA_NAME=="GOA",]$Biom_est,0)
T5OFL<- round((cRFX/q)*FOFL, 0)
T5ABC<- round((cRFX/q)*FABC, 0)

T5specs<-as.data.frame(cbind(T5OFL,T5ABC,cRFX,FOFL,FABC))
T5specs$metric<-"T5RFXq"
T5specs$fmp_area<-"GOA"
T5specs<-T5specs[,c("metric","fmp_area","T5OFL","T5ABC","cRFX","FOFL","FABC")]
colnames(T5specs)[3:5]<-c("OFL","ABC","Biomass")

# model summary tables
T5sum<-RFXdat %>% 
  filter(YEAR == SYR,
         REGULATORY_AREA_NAME == "GOA") %>% 
  select(Biom_est, 0, Biom_LL, 0, Biom_UL, 0) %>% 
  mutate(Biom_est = round(Biom_est, 0), 
         Biom_LL = round(Biom_LL, 0), 
         Biom_UL = round(Biom_UL, 0)) %>% 
  pivot_longer(cols = c('Biom_est', 'Biom_LL', 'Biom_UL'), names_to = 'EST', values_to = "Biomass_t") %>% 
  mutate(Ba = round(Biomass_t / q, 0),
         OFL = round(Ba * FOFL, 0),
         ABC = round(Ba *FABC, 0))

write_csv(T5sum,paste0(outdir,"T5Summary_",AYR,".csv",sep=""))

#Tier 6 Models for AFSC Sharks ----
#Updated 10/13/2022 by C. Tribuzio

# Data Info ----
#Data are queried from AKFIN Groundfish Total Catch by Fishery Table
#2003-current
#deselect the targets
#leave everything as null except species
#four species available so far: spiny dogfish, salmon shark, sleeper shark, other shark
#save file to "data" folder as .csv "Groundfish Total Catch By Fishery_SharkYYYY.csv"

# Directories ----
datadir<-paste(getwd(),"/Data/Annual_Updates/",AYR,sep="")
outdir<-paste(getwd(),"/Output/",AYR,"/Harvest Specs/",sep="")
if(!dir.exists(outdir)){
  dir.create(paste(getwd(),"/Output/",AYR,"/Harvest Specs/",sep=""))
  print("created new directory")
}else{
  print("directory exists")
}

# Tier 6 calcs ----
# bring in data and combine/filter
CASdat<-read_csv(paste(datadir,"/confidential_CAS_sharks", AYR, ".csv",sep="")) %>% 
  select(year, fmp_area, species, catch_mt) %>% 
  group_by(year, fmp_area, species) %>% 
  summarise(tot_catch = sum(catch_mt)) %>% 
  rename(catch_mt = tot_catch)

olddat<-read_csv(paste(getwd(),"/Data/Static/confidential_pre2003_shark_cleaned.csv",sep="")) %>% 
  clean_names() %>% 
  select(year, fmp_area, species, catch_mt) %>% 
  group_by(year, fmp_area, species) %>% 
  summarise(tot_catch = sum(catch_mt)) %>% 
  rename(catch_mt = tot_catch)

# combine and remove GOA spinys, INSD and non-relevant years
# GOA time frame is 1997-2007
# BSAI time frame is 2003-2015
T6dat_BSAI<-rbind(olddat, CASdat) %>% 
  filter(((year > 2002 & year < 2016 ) & fmp_area == "BSAI"))
  
T6dat<-rbind(olddat, CASdat) %>% 
  filter(fmp_area == "GOA",
         species != "Spiny Dogfish",
         ((year > 1996 & year < 2008 ) & fmp_area == "GOA")) %>% 
  bind_rows(T6dat_BSAI)

# This code includes all potential Tier 6 methods proposed to date. It's good to keep them handy for comparison.
# Current methods for GOA: mean historical catch
# Current method for BSAI: max historical, note that there is some discussion on summing the species or going with complex max, 
#     complex total max is what is currently used. This code looks at it both ways


# list of metrics to include: 
#     mean by species then summed, 
#     max by species then summed, 
#     max of total
#     10/90th percentile of data by species then summed
#     5/95% CI of mean by species then summed

T6metrics <- T6dat %>% 
  group_by(fmp_area, species) %>% 
  summarise(T6mean=round(mean(catch_mt),),
            T65pct=round(t.test(catch_mt)$conf.int[1],),
            T695pct=round(t.test(catch_mt)$conf.int[2],),
            T65ptl=round(quantile(catch_mt,0.05),),
            T610ptl=round(quantile(catch_mt,0.10),),
            T6med=round(median(catch_mt),),
            T690ptl=round(quantile(catch_mt,0.10),),
            T699ptl=round(quantile(catch_mt,0.99),),
            T6maxsp=round(max(catch_mt),)) %>% 
  pivot_longer(!c(fmp_area, species), names_to = "metric", values_to = "OFL")

T6specs <- T6metrics %>% 
  group_by(fmp_area, metric) %>% 
  summarise(compOFL = round(sum(OFL))) %>% 
  rename(OFL = compOFL)

T6maxc<- T6dat %>% 
  group_by(year, fmp_area) %>% 
  summarise(Tot_Catch = sum(catch_mt)) %>% 
  group_by(fmp_area) %>% 
  summarise(OFL = round(max(Tot_Catch),0)) %>% 
  mutate(metric = "T6maxc")

T6specs<- bind_rows(T6specs, T6maxc) %>% 
  mutate(ABC = round(OFL*0.75, 0),
         Biomass = NA,
         FABC = NA, 
         FOFL = NA)

#Combined complex harvest specifications ----
Shark_specs<-bind_rows(T5specs,T6specs)
write_csv(Shark_specs,paste0(outdir,"Shark_specs",AYR,".csv",sep=""))

# Comp to last assess ----
# How much did specs change?
#screen for changes in data
#first question: did the OFL/ABCs change everywhere?
#there is something wonky, because the 2018 doesn't match what's in the assessment
pspecs<-read.csv(paste(getwd(),"/Output/",LAYR,"/Harvest Specs/Shark_specs",LAYR,".csv",sep="")) %>% 
  clean_names() %>% 
  select(metric, fmp_area, ofl) %>% 
  rename(pOFL = ofl)

specs_comp <- left_join(Shark_specs, pspecs) %>% 
  mutate(pchange = round(((OFL - pOFL)/pOFL) * 100, 2))

write_csv(specs_comp,paste0(outdir,"Shark_specs_compare",AYR,".csv",sep=""))

#next identify the specific species/FMPs where change occurred
#haven't done this yet because we didn't need to in 2020

#next identify any specific sub-areas where change occurred

#then go to the data 

#brings in last years data file, noted "p" for previous
#pdatadir<-paste(getwd(),"/Data/Cleaned/",LAYR,sep="")
#pCASdat<-read.csv(paste(pdatadir,"/CAS_GFTBF_Sharks.csv",sep=""),header=T)
#pCd<-CASdat[,c("Year","FMP.Area","Species","Catch..mt.")]


