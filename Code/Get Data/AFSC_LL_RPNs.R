# Query and clean AFSC LL survey data ----
# Contact: cindy.tribuzio@noaa.gov
# Last Updated: Sept 2022

# Setup ----
libs <- c("tidyverse", "RODBC", "lubridate", "janitor", "odbc", "DBI")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

dbname <- "akfin"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

channel_akfin <- odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

AYR <- 2022 #Assessment year

outpath <- paste0("Data/Cleaned/", AYEAR)
dir.create(outpath)
rawpath <- paste0("Data/Annual_updates/", AYEAR) 
dir.create(rawpath)

# Get data ----
sharkRPN_FMP <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.lls_fmp_all_strata
                where     species_code in (150, 222, 232, 310, 320,351)")) %>% 
  clean_names() 

sharkRPN_area <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.lls_fmp_subarea_all_strata
                where     species_code in (150, 222, 232, 310, 320,351)")) %>% 
  clean_names() 

# Data cleanup ----
RPN_fmp <- sharkRPN_FMP %>% 
  filter(country == "United States") %>% 
  select(c("year", "fmp_management_area", "species_code", "species", "cpue",
           "cpue_var", "rpn", "rpn_var", "rpw", "rpw_var")) %>% 
  rename(mgmt_area = fmp_management_area)

RPN_area <- sharkRPN_area %>% 
  filter(country == "United States") %>% 
  select(c("year", "council_management_area", "species_code", "species", 
           "cpue", "cpue_var", "rpn", "rpn_var", "rpw", "rpw_var")) %>% 
  rename(mgmt_area = council_management_area)

AFSCLL <- RPN_fmp %>% bind_rows(RPN_area)

write.csv(AFSCLL,paste(outpath,"/AFSCLL_RPN_sharks",AYR,".csv",sep=""),row.names=F)
