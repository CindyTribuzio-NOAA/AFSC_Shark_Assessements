# Retrieve and Prep Data ----
# for spiny dogfish length comps
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: Sept 2022

# Setup ----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

#AYR <- 2022 #Assessment year

outpath <- paste0("Data/Cleaned/", AYR)
dir.create(outpath)
rawpath <- paste0("Data/Annual_updates/", AYR) 
dir.create(rawpath)

SDgroup <- c(310, 100, 40)
SSgroup <- c(320, 400, 50)
Lgroups <- as.data.frame(rbind(SDgroup, SSgroup))
colnames(Lgroups) <- c("species_code", 'plus', 'minus')

#AFSCLL survey data ----
# Get data
AFSCLL_dat <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.lls_length_summary_view
                where     species_code in (310)")) %>% 
  clean_names() 

AFSCLL_freq <- AFSCLL_dat %>% 
  filter(country == "United States") %>% 
  mutate(fmp = if_else(nmfs_area_code > 600, "GOA", "BSAI"),
         sex = if_else(sex == 1, "M", "F")) %>% 
  group_by(year, fmp, sex, length) %>% 
  summarise(tot_freq = sum(frequency)) %>% 
  mutate(species_code = 310) %>% 
  select(c(species_code, year, fmp, length, sex, tot_freq)) %>% 
  rename(Species = species_code, 
         Year = year,
         FMP = fmp,
         Length = length,
         Sex = sex,
         Frequency = tot_freq)

AFSCLL_size<-AFSCLL_freq[rep(row.names(AFSCLL_freq),AFSCLL_freq$Frequency),1:5]
AFSCLL_size$Source<-"AFSCLL"

AFSCLL_freq <- AFSCLL_freq %>% 
  mutate(Source = "AFSCTWL",
         binL = floor(Length/5)*5,
         binL = if_else(binL > Lgroups[Lgroups$species_code == 310, 'plus'], Lgroups[Lgroups$species_code == 310, 'plus'],
               if_else(binL < Lgroups[Lgroups$species_code == 310, 'minus'], Lgroups[Lgroups$species_code == 310, 'minus'], binL))) %>% 
  select(!Length)

#AFSCTWL data ----
RACE_haul <- read_csv(paste0(getwd(), "/", rawpath, "/RACE_HAUL", AYR,".csv")) %>% 
  select(c(cruisejoin, hauljoin, region, start_time)) %>% 
  mutate(year = year(start_time)) %>% 
  select(!start_time)
AFSCTWL_GOAAIdat <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_lengthaigoa
                where     species_code in (310, 320, 232)")) %>% 
  clean_names() %>% 
  left_join(RACE_haul) %>% 
  select(year, length, frequency, sex, region, species_code, length_type)

AFSCTWL_EBSshelfdat <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_length_ebsshelf
                where     species_code in (310, 320, 232)")) %>% 
  clean_names() %>% 
  left_join(RACE_haul) %>% 
  select(year, length, frequency, sex, region, species_code, length_type)

AFSCTWL_EBSslopedat <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_length_ebsslope
                where     species_code in (310, 320, 232)")) %>% 
  clean_names() %>% 
  left_join(RACE_haul) %>% 
  select(year, length, frequency, sex, region, species_code, length_type)

#convert lengths to PCL assuming 1=FL and 5=TLnat
#need to add more species to length conversions
Lconv_dat<-read_csv(paste0(getwd(),"/Data/Static/Shark_Length_conversions.csv")) %>% 
  filter(Lto == "PCL") %>% 
  select(species_code, Lfrom, a, b)

shark_TWLlength <- AFSCTWL_GOAAIdat %>% rbind(AFSCTWL_EBSshelfdat, AFSCTWL_EBSslopedat) %>% 
  mutate(length = length/10,
         sex2 = if_else(sex == 1, "M",
                        if_else(sex == 2, "F", "U")),
         Lfrom = if_else(length_type == 1, "FL", 
                            if_else(length_type == 5, "TLnat", "UNK"))) %>% 
  left_join(Lconv_dat) %>% 
  mutate(PCL = round(a + b*length,0)) %>% 
  select(species_code, year, region, PCL, sex2, frequency) %>% 
  rename(Species = species_code, Year = year, FMP = region, Length = PCL, Sex = sex2, Frequency = frequency)

AFSCTWL_size<-shark_TWLlength[rep(row.names(shark_TWLlength),shark_TWLlength$Frequency),1:5]
AFSCTWL_size$Source<-"AFSCTWL"

#easier to do species separately, note, skipped salmon sharks
SSTWL_dat <- shark_TWLlength %>% 
  filter(Species == 320) %>% 
  mutate(binL = floor(Length/10)*10,
         Source = "AFSCTWL")
SSTWL_dat$binL[SSTWL_dat$Species == 320 & SSTWL_dat$binL < Lgroups[Lgroups$species_code == 320, 'minus']] <- 
  Lgroups[Lgroups$species_code == 320, 'minus']
SSTWL_dat$binL[SSTWL_dat$Species == 320 & SSTWL_dat$binL > Lgroups[Lgroups$species_code == 320, 'plus']] <- 
  Lgroups[Lgroups$species_code == 320, 'plus']

SDTWL_dat <- shark_TWLlength %>% 
  filter(Species == 310) %>% 
  mutate(binL = floor(Length/5)*5,
         Source = "AFSCTWL")
SDTWL_dat$binL[SDTWL_dat$Species == 310 & SDTWL_dat$binL < Lgroups[Lgroups$species_code == 310, 'minus']] <- 
  Lgroups[Lgroups$species_code == 310, 'minus']
SDTWL_dat$binL[SDTWL_dat$Species == 310 & SDTWL_dat$binL > Lgroups[Lgroups$species_code == 310, 'plus']] <- 
  Lgroups[Lgroups$species_code == 310, 'plus']

AFSCTWL_freq <- SSTWL_dat %>%  bind_rows(SDTWL_dat) %>% select(!Length)


#IPHC data
# See IPHC_Lengths.R for data prep code
# Lengths are PCL
IPHC_size<-read.csv(paste(outpath,"/IPHC_dogfish_lengths", AYR,".csv",sep=""), header=T) %>%
  clean_names() %>% 
  filter(sex != "U") %>% #get rid of uknown sexes, not too many of them
  mutate(binL = floor(length/5)*5,
         binL = if_else(binL > Lgroups[Lgroups$species_code == 310, 'plus'], Lgroups[Lgroups$species_code == 310, 'plus'],
                        if_else(binL < Lgroups[Lgroups$species_code == 310, 'minus'], Lgroups[Lgroups$species_code == 310, 'minus'], binL)),
         Source = "IPHCLL",
         species = 310) %>% 
  select(year, fmp, length, sex, binL)



IPHC_freq<- IPHC_size %>% 
  group_by(year, fmp, binL, sex) %>% 
  summarise(Frequency = length(length)) %>% 
  mutate(Source = "IPHCLL",
         Species = 310)






# Combine all data ----
shark_L_freq<-bind_rows(IPHC_freq, AFSCLL_freq, AFSCTWL_freq)
write_csv(shark_L_freq,paste(outpath,"/SURVEY_shark_Lfreq",AYR,".csv",sep=""))

shark_L_dat<-bind_rows(IPHC_size, AFSCLL_size, AFSCTWL_size)
write_csv(shark_L_dat,paste(outpath,"/SURVEY_shark_L_dat",AYR,".csv",sep=""))
