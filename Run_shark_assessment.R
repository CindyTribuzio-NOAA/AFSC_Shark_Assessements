# Shark Assessment  ----
# Updated Sept 30 2022 by C. Tribuzio
#use this code to work through each step and run all of the other codes

# To Do ----
# 1) integrate all of the codes to run from this aka do it all
# 2) fix all outputs to append year to end of file name

# Setup ----
libs <- c("tidyverse", "janitor", "Hmisc", "RColorBrewer", "gridExtra", "gtable", 
          "grid", "flextable", "officer", "lubridate", "RODBC", "DBI")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

# Create Directories ----
SYR <- 2021 #survey year
AYR <- 2022 #assessment year
endyr <- 2021 #for RFX model end year
LAYR <- 2020 #last assessment year

dir.create(paste0(getwd(),"/Data/Annual_updates/",AYR), showWarnings = T)
dir.create(paste0(getwd(),"/Output/",AYR), showWarnings = T)
dir.create(paste0(getwd(),"/Documents/",AYR), showWarnings = T)

# Get Data ----
# RACE Survey Biomass
source(paste(getwd(),"/Code/Get Data/AFSC_RACE_Biomass.R",sep=""))

# AFSC LL Survey RPNs
source(paste(getwd(),"/Code/Get Data/AFSC_LL_RPNs.R",sep=""))

# IPHC LL Survey RPNs
source(paste(getwd(),"/Code/Get Data/IPHC_FISS_Indices.R",sep=""))

# IPHC Spiny Dogfish Lengths
# this has to be done before all the survey lengths can be done
source(paste(getwd(),"/Code/Get Data/IPHC_Lengths.R",sep=""))

# Survey length data
source(paste(getwd(),"/Code/Get Data/Survey_Length_Frequency.R",sep=""))

# AKRO CAS
source(paste(getwd(),"/Code/Get Data/AKRO_CAS_DATA.R",sep=""))

# ADFG
# contact ADFG for data files, see notes in code for last contact
# this code will likely need annual attention
source(paste(getwd(),"/Code/Get Data/ADFG.R",sep=""))

# RACE Biomass ----
# this is redundant and archaic, only needed for 2022 for bridging, Future will use rema package


# RFX ----
# this is redundant and archaic, only needed for 2022 for bridging, Future will use rema package

# Harvest Specs ----

# Assessment Figures ----

# Assessment Tables ----






