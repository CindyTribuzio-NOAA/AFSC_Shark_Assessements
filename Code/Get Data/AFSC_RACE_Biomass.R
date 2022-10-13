# Query and clean AFSC RACE survey data ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: Sept 2022

# Setup ----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

outpath <- paste0("Data/Cleaned/", AYR)
dir.create(outpath)
rawpath <- paste0("Data/Annual_updates/", AYR) 
dir.create(rawpath)

# Get data ----
# Haul data, will need this to put length data into areas? Will not need it for RFX if we go with REMA
AFSCTWL_HAULGOA <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_haulaigoa")) %>% 
  clean_names()
AFSCTWL_HAULshelf <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_haul_ebsshelf")) %>% 
  clean_names()
AFSCTWL_HAULslope <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_haul_ebsslope")) %>% 
  clean_names()

AFSCTWL_HAUL <- AFSCTWL_HAULGOA %>% bind_rows(AFSCTWL_HAULshelf, AFSCTWL_HAULslope)
write_csv(AFSCTWL_HAUL, paste0(rawpath, "/RACE_HAUL", SYR, ".csv"))

# GOAAI survey biomass
# by FMP sub area
AFSCTWL_ABio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomassareaaigoa
                where     species_code in (310, 320, 232)")) %>% 
  clean_names() %>% 
  select(c("survey", 'year', 'regulatory_area_name', 'species_code', 'haul_count', 'catch_count', 'area_biomass', 'biomass_var'))
# Total GOA (this includes variance at the GOA level, otherwise, can sum biomass to get total GOA biomass)
AFSCTWL_FMPBio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomasstotalaigoa
                where     species_code in (310, 320, 232)")) %>% 
  clean_names() %>% 
  select(c("survey", 'year', 'species_code', 'haul_count', 'catch_count', 'total_biomass', 'biomass_var')) %>% 
  mutate(regulatory_area_name = survey) %>% 
  rename(area_biomass = total_biomass)

# EBS Shelf survey biomass
AFSCTWL_shelfBio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomass_ebsshelf
                where     species_code in (310, 320, 232)")) %>% 
  clean_names() %>% 
  filter(stratum == 999) %>% 
  select(c("survey", 'year', 'species_code', 'haul_count', 'catch_count', 'stratum_biomass', 'bio_var')) %>% 
  mutate(regulatory_area_name = "EBS") %>% 
  rename(biomass_var = bio_var,
         area_biomass = stratum_biomass)

# EBS Slope survey biomass
AFSCTWL_slopeBio <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_biomass_ebsslope
                where     species_code in (310, 320, 232)")) %>% 
  clean_names() %>% 
  filter(stratum == 999999) %>% 
  select(c("survey", 'year', 'species_code', 'haul_count', 'catch_count', 'stratum_biomass', 'bio_var')) %>% 
  mutate(regulatory_area_name = "EBS") %>% 
  rename(biomass_var = bio_var,
         area_biomass = stratum_biomass)

# Clean up and add CIs and CVs ----
AFSCTWL_BIOM <- AFSCTWL_ABio %>% bind_rows(AFSCTWL_FMPBio, AFSCTWL_shelfBio, AFSCTWL_slopeBio) %>% 
  mutate(cv = sqrt(biomass_var)/area_biomass,
         se = sqrt(biomass_var),
         bio_ll = area_biomass - 1.96*se,
         bio_ul = area_biomass + 1.96*se) %>% 
  replace(is.na(.), 0) %>% 
  rename(strata = regulatory_area_name,
         biomass = area_biomass)
write_csv(AFSCTWL_BIOM, paste0(outpath, "/RACE_biomass_sharks", SYR, ".csv")) 

# Stratum data ----
AFSCTWL_STRATGOA <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_goastrataaigoa")) %>% 
  clean_names()
AFSCTWL_STRATshelf <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_strata_ebsshelf")) %>% 
  clean_names()
AFSCTWL_STRATslope <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.race_strata_ebsslope")) %>% 
  clean_names()

AFSCTWL_STRATA <- AFSCTWL_STRATGOA %>% bind_rows(AFSCTWL_STRATshelf, AFSCTWL_STRATslope)
write_csv(AFSCTWL_STRATA, paste0(rawpath, "/RACE_STRATUM", SYR, ".csv"))
