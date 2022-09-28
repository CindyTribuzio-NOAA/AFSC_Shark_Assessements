# Compute RACE Trawl Survey Biomass ----
#Updated 9/1/2020 by C. Tribuzio

# Still to do list ----
##1)download data directly from AKFIN

#Setup ----
codedir<-datadir<-paste(getwd(),"/Code/RACE_Biomass/",sep="")
source(paste(codedir,"RACE_biomass_functions.R",sep=""))

SYR<-2019 #survey year
AYR<-2020 #assessment year
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
outdir<-paste(getwd(),"/Output/",AYR,"/RACE_Biomass/",sep="")

# Set up biomass groups ----
#makes a nested list that the biomass for loop will run through
bgroups<-list("species"=list(spec1=310,
                             spec2=320,
                             spec3=232,
                             spec4=list(310,320,232),
                             spec5=list(310,320)),
     "outname"=list(out1="Spiny Dogfish",
                    out2="Pacific Sleeper Shark",
                    out3="Salmon Shark",
                    out4="Sharks",
                    out5="PSS_SD"))

RACE_BIOMASS(Species=bgroups,outname="Sharks",SYR=SYR,datadir=datadir,outdir=outdir)

# Biomass by species ----
#when run separately like this, it can take a lot of time because each run reloads the data
RACE_BIOMASS(Species=310,outname="Spiny_Dogfish",SYR=SYR,datadir=datadir,outdir=outdir)

