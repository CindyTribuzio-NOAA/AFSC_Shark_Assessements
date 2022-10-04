# Queries to pull IPHC FISS RPN and CPUE from AKFIN
# Updated August 2022
# Contact: cindy.tribuzio@noaa.gov

# Set up ----
dbname <- "afsc"
db <- read_csv('database.csv')
database_akfin=db %>% filter(database == dbname) %>% select(database) #need to add filter for AKFIN user/pass only
username_akfin=db %>% filter(database == dbname) %>% select(username)
password_akfin=db %>% filter(database == dbname) %>% select(password)

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE))

#AYR <- 2022 #Assessment year

outpath <- paste0("Data/Cleaned/", AYR)
dir.create(outpath)
rawpath <- paste0("Data/Annual_updates/", AYR) 
dir.create(rawpath)

# Get NORPAC data ----
# skip this section if data have already been queried
# species composition data
NORPACspcomp <- sqlQuery(channel_afsc, query = ("
                select    *
                from      obsint.debriefed_spcomp
                where     species in (62, 64, 65, 66, 67, 68, 69, 76, 78, 156)")) %>% 
  clean_names() %>% 
  select(!c(t_table, date_of_entry, sample_type, sample_size))
write_csv(NORPACspcomp, paste0(rawpath, "/confidential_NORPACspcomp", AYR, ".csv"))

# haul data
NORPAChaul <- sqlQuery(channel_afsc, query = ("
                select    *
                from      obsint.debriefed_haul")) %>% 
  clean_names()

NORPAChaul <- NORPAChaul %>% 
  select(cruise, permit, haul, haul_seq, haul_date, gear_type, fishing_depth_fathoms, bottom_depth_fathoms, 
         duration_in_min, nmfs_area, latdd_end, londd_end)
write_csv(NORPAChaul, paste0(rawpath, "/confidential_NORPAChaul", AYR, ".csv"))

# merge the haul info to the species comp

obs_dat <- NORPACspcomp %>% 
  left_join(NORPAChaul)
write_csv(obs_dat, paste0(outpath, "/confidential_NORPACsharks", AYR, ".csv"))

rm(c(NORPAChaul, NORPACspcomp, obs_dat))

