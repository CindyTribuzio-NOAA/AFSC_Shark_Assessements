# Queries to pull IPHC FISS RPN and CPUE from AKFIN
# Updated August 2022
# Contact: cindy.tribuzio@noaa.gov

# Set up ----
dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

AYEAR <- 2022 #Assessment year

outpath <- paste0("Data/Cleaned/", AYEAR)
dir.create(outpath)
rawpath <- paste0("Data/Annual_updates/", AYEAR) 
dir.create(rawpath)

# Get data ----
sharkRPN <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc_host.fiss_rpn
                where     species in ('Spiny dogfish', 'Sleeper shark')")) %>% 
  clean_names() 

sharkCPUE <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc_host.fiss_cpue
                where     species in ('Spiny dogfish', 'Sleeper shark')")) %>% 
  clean_names() 

# Raw Survey Data (if needed)
IPHC_raw <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc_host.fiss_cleaned")) %>% 
  clean_names() 

# Save Survey Indices ----
write_csv(sharkRPN, paste0(outpath, "/IPHC_RPN_sharks_", AYR-1, ".csv"))
write_csv(sharkCPUE, paste0(outpath, "/IPHC_CPUE_sharks_", AYR-1, ".csv"))
write_csv(IPHC_raw, paste0(outpath, "/IPHC_FISS_survey_", AYR-1, "_trackoff.csv", sep = "")) #add "trackoff" so large file size isn't tracked
