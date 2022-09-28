# Queries to pull IPHC FISS RPN and CPUE from AKFIN
# Updated August 2022
# Contact: cindy.tribuzio@noaa.gov

# Set up ----

libs <- c("tidyverse", "RODBC", "lubridate", "janitor")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

dbname <- "afsc"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))

AYEAR <- 2022 #Assessment year

outpath <- paste0("Data/Cleaned/", AYEAR)
dir.create(outpath)
rawpath <- paste0("Data/Annual_updates/", AYEAR) 
dir.create(rawpath)

# Get NORPAC data ----
NORPACspcomp <- sqlQuery(channel_afsc, query = ("
                select    *
                from      obsint.debriefed_spcomp
                where     species in (62, 64, 65, 66, 67, 68, 69, 76, 78, 156)")) %>% 
  clean_names() 
