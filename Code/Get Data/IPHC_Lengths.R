# Compile new IPHC length data ----
# for spiny dogfish length comps
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: 5_10_2020

# Setup ----
AYR<-2021
LAYR<-2020
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
olddir<-paste(getwd(),"/Data/Cleaned/",LAYR,sep="")

# Retrieve Data ----
olddat<-read_csv(paste(olddir,"/IPHC_dogfish_lengths",LAYR,".csv",sep="")) %>% 
  clean_names()

newdat<-read_csv(paste(datadir,"/IPHC Dogfish Data ", AYR, ".csv",sep="")) %>% 
  clean_names
  

# Clean and Combine data ----
alldat<-newdat %>% 
  select(year, station, length, sex, iphc_regulatory_area) %>% 
  rename(reg_area = iphc_regulatory_area) %>% 
  drop_na() %>% 
  filter(!grepl('NULL', length)) %>% 
  mutate(length = as.numeric(length)) %>% 
  bind_rows(olddat)

#many of the Reg Areas have extra spaces after characters, clean this out
#alldat[grep("2A",alldat$reg_area),]$reg_area<-"2A"
#alldat[grep("2B",alldat$Reg_Area),]$Reg_Area<-"2B"
#alldat[grep("2C",alldat$Reg_Area),]$Reg_Area<-"2C"
#alldat[grep("3A",alldat$Reg_Area),]$Reg_Area<-"3A"
#alldat[grep("3B",alldat$Reg_Area),]$Reg_Area<-"3B"



# Add in FMP ----
# this borrows station/FMP info from RPN work. Bring over RPN output to inform this
# "final_iphc_survey_1998_20XX.csv

Surveydat<-read_csv(paste(outdir,"/final_iphc_survey_1998_",AYR,".csv",sep="")) %>% 
  clean_names()
station<-unique(Surveydat[,c("year","station","fmp")])
rm(Surveydat)

lengthdat<-alldat %>% 
  left_join(station)

nrow(alldat)==nrow(lengthdat) #checks that alldat didn't change

# look for NAs in FMP
# these are probably stations that were not part of the normal station grid and not included in RPNs calcs
nadat<-lengthdat[is.na(lengthdat$fmp),]
nrow(nadat)

# anything with NA in 2A FMP="WC", 2B FMP = "CAN", 3A FMP = "GOA"
# this is more generalized than using the RPN assigned FMPs, but will be OK for this
lengthdat[is.na(lengthdat$fmp)&lengthdat$reg_area=="2A",]$fmp<-"WC"
lengthdat[is.na(lengthdat$fmp)&lengthdat$reg_area=="2B",]$fmp<-"CAN"
lengthdat[is.na(lengthdat$fmp)&lengthdat$reg_area=="2C",]$fmp<-"GOA"
lengthdat[is.na(lengthdat$fmp)&lengthdat$reg_area=="3A",]$fmp<-"GOA"
lengthdat[is.na(lengthdat$fmp)&lengthdat$reg_area=="3B",]$fmp<-"GOA"
lengthdat[is.na(lengthdat$fmp)&lengthdat$reg_area=="4B",]$fmp<-"BSAI"

# see if therea are any more
nadat<-lengthdat[is.na(lengthdat$fmp),]
nrow(nadat) #need to get this to zero

# Reg_Area 4A spans both FMPs, so sleuthed this one the hard way
# looked at where this one lands in all years and made an assignment
# station 6045 is ALWAYS in NMFS 518, so assign to BSAI
lengthdat[is.na(lengthdat$fmp)&lengthdat$station==6045,]$fmp<-"BSAI"

#search for 6026 and 6701
st_dat <- lengthdat %>% 
  filter(station == 6026 | station == 6701)
#6026 is always GOA
lengthdat[is.na(lengthdat$fmp)&lengthdat$station==6026,]$fmp<-"GOA"

#6701 is a weird one, not sure why it didn't match up, should be BSAI
lengthdat[is.na(lengthdat$fmp)&lengthdat$station==6701,]$fmp<-"BSAI"

# see if therea are any more
nadat<-lengthdat[is.na(lengthdat$fmp),]
nrow(nadat) #need to get this to zero

#export data
write.csv(lengthdat,paste(outdir,"/IPHC_dogfish_lengths",AYR,".csv",sep=""),row.names =F)
