# Queries to pull IPHC FISS RPN and CPUE from AKFIN
# Updated August 2022
# Contact: cindy.tribuzio@noaa.gov

# Set up ----

libs <- c("tidyverse", "RODBC", "lubridate", "janitor", "odbc", "DBI")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

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
#IPHC_raw <- sqlQuery(channel_akfin, query = ("
#                select    *
#                from      iphc.fiss_set_p_halb")) %>% 
#  clean_names() 

# Save Survey Indices ----
write_csv(sharkRPN, paste(outpath, "/IPHC_RPN_sharks_2021.csv", sep = ""))
write_csv(sharkCPUE, paste(outpath, "/IPHC_CPUE_sharks_2021.csv", sep = ""))
write_csv(IPHC_raw, paste(outpath, "/IPHC_FISS_raw_2021.csv", sep = ""))
