# Title: Shark Stock Structure ----
# Updated: Aug 16 2022
# Recent Author: Cindy Tribuzio

# To do ----
#1) Figure out how to automate the channel name in the sqlQuery

# Setup ----
libs <- c("tidyverse", "janitor", "lubridate", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

#datadir<-paste(getwd(),"/Obj1_obs_size_method/Data/Raw_Data/",sep="")
outdir<-paste(getwd(),"/Outside_Analyses/Stock Structure 2022/",sep="")
#shapedir <- paste(getwd(),"/Obj1_obs_size_method/Data/Shapefile/",sep="")

# establish database connection
dbname <- "afsc"
db <- read_csv('database.csv')
assign(paste0("username_", dbname), db %>% filter(database == dbname) %>% select(username))
assign(paste0("password_", dbname), db %>% filter(database == dbname) %>% select(password))

assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_afsc, pwd = password_afsc, believeNRows=FALSE))

# Get NORPAC Data ----
# NORPAC haul data, queried from AFSC by year
# skip this section if data have already been queried
NORPAC_haul <- sqlQuery(channel_afsc, query = ("
                select    CRUISE, PERMIT, HAUL, YEAR, NMFS_AREA
                from      obsint.debriefed_haul
                where     year >= 2003")) %>% 
  clean_names() %>% 
  mutate(fmp_subarea = if_else(nmfs_area < 541, "EBS",
                               if_else(between(nmfs_area, 540, 544), "AI",
                                       if_else(nmfs_area == 610, "WGOA", 
                                               if_else(between(nmfs_area, 610, 640), "CGOA",
                                                       if_else(nmfs_area == 649 | nmfs_area == 659, "INSD",
                                                               if_else(nmfs_area == 640 | nmfs_area == 650, "EGOA", "OUT" )))))),
         fmp = if_else(nmfs_area < 541, "EBS",
                       if_else(between(nmfs_area, 540, 544), "AI", "GOA")))

NORPAC_spcom <- sqlQuery(channel_afsc, query = ("
                select    *
                from      obsint.debriefed_spcomp
                where     species = 62 and
                          year >= 2003")) %>% 
  clean_names() 

# Write data out so it can be read back in without having to run queries
write_csv(NORPAC_haul, paste0(outdir, "confidential_NORPAChaul_03_22.csv"))
rm(NORPAC_haul)
write_csv(NORPAC_spcom, paste0(outdir, "confidential_NORPACspcom_03_22.csv"))
rm(NORPAC_spcom)

norpach <- read_csv(paste0(outdir, "confidential_NORPAChaul_03_22.csv"))
norpacsp <- read_csv(paste0(outdir, "confidential_NORPACspcom_03_22.csv"))

# Summarize  ----
#By year and NMFS area
haul_ct <- norpach %>% 
  group_by(year, nmfs_area, fmp) %>% 
  summarize(n_hauls = length(haul))

sp_ct <- norpacsp %>% 
  left_join(norpach, by = c("cruise", "permit", "haul", "year")) %>% 
  group_by(year, nmfs_area, fmp) %>% 
  summarize(shark_hauls = length(haul_join))

shark_obshauls <- sp_ct %>% 
  left_join(haul_ct) %>% 
  mutate(haul_prop = shark_hauls/n_hauls)

ggplot(shark_obshauls, aes(x = year, y = haul_prop))+
  geom_point()+
  geom_line()+
  facet_grid(nmfs_area~.)

outdat <- shark_obshauls %>% filter(nmfs_area > 659)
ggplot(outdat, aes(x = year, y = haul_prop))+
  geom_point()+
  geom_line()+
  facet_grid(nmfs_area~., scales = "free")

# Distribution of obs between areas
# stacked bar plot of obs hauls with PSS catch
indat <- shark_obshauls %>% filter(nmfs_area < 659, !nmfs_area == 649)
ggplot(indat, aes(x = year, y = shark_hauls, fill = as.factor(nmfs_area)))+
  geom_bar(position = "fill", stat="identity") +
  #scale_fill_brewer(palette = "Earth")+
  facet_grid(fmp~.)


# By FMP Subarea
haul_ct <- norpach %>% 
  group_by(year, fmp_subarea) %>% 
  summarize(n_hauls = length(haul))

sp_ct <- norpacsp %>% 
  left_join(norpach, by = c("cruise", "permit", "haul", "year")) %>% 
  group_by(year, fmp_subarea) %>% 
  summarize(shark_hauls = length(haul_join))

shark_obshauls <- sp_ct %>% 
  left_join(haul_ct) %>% 
  mutate(haul_prop = shark_hauls/n_hauls)

ggplot(shark_obshauls, aes(x = year, y = haul_prop))+
  geom_point()+
  geom_line()+
  facet_grid(fmp_subarea~., scales = "free")





