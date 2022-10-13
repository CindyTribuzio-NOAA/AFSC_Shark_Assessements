# Setup ----
datadir<-paste(getwd(),"/Output/",AYR,"/RACE_Biomass/",sep="")
outdir<-paste(getwd(),"/Output/",AYR,"/RFX/",sep="")
dir.create(outdir)
codedir<-paste(getwd(),"/Code/RFX",sep="")
figdir<-paste(getwd(),"/Output/",AYR,"/Figures/",sep="")

source(paste(codedir,"/RFX_functions.R",sep=""))

# run code for sharks ----
RFX_fx(outname="Spiny_Dogfish",AYR,endyr,datadir,outdir,regional=T)

RFX_fx(outname="Sharks",AYR,endyr,datadir,outdir,regional=T)

# RFX fig Quick Check ----
library(ggplot2)
library(gridExtra)

# Data collection ----
# Current year RFX
RFX<-read.csv(paste(outdir,"/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header=T)
areas<-c("GOA", "WESTERN GOA", "EASTERN GOA", "CENTRAL GOA")
SDRFX<-RFX[RFX$Group=="Spiny_Dogfish" & RFX$REGULATORY_AREA_NAME %in% areas,]

# Last assessment RFX
pRFX<-read.csv(paste(getwd(),"/Output/",LAYR,"/RFX/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header = T)
pSDRFX<-pRFX[pRFX$Group=="Spiny_Dogfish" & pRFX$REGULATORY_AREA_NAME %in% areas,]

# RACE Biomass
SD_RACE<-read.csv(paste(datadir,"RACE_Biomass_Spiny_Dogfish.csv",sep=""),header=T)
SD_GOA<-SD_RACE[SD_RACE$REGULATORY_AREA_NAME %in% areas,]
SD_GOA$RACEUL<-SD_GOA$Biomass+1.96*SD_GOA$SE
SD_GOA$RACELL<-SD_GOA$Biomass-1.96*SD_GOA$SE

#merge data sets together for easier graphing
SD_RFX <- SDRFX %>% left_join(SD_GOA)
#get rid of outrageous values for the sake of graphing
#turns all LL values below zero into zero for graph
SD_RFX[!is.na(SD_RFX$RACELL) & SD_RFX$RACELL<0,]$RACELL<-0 

#need to put data into dorder so it projects correctly
#SD_RFX$REGULATORY_AREA_NAME<-factor(SD_RFX$REGULATORY_AREA_NAME,levels=c("GOA","WESTERN GOA","CENTRAL GOA","EASTERN GOA"))

# Figures for document
WGOA_doc<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="WESTERN GOA",],aes(x=YEAR,y=Biomass/1000))+
  geom_ribbon(aes(ymin=Biom_LL/1000,ymax=Biom_UL/1000),fill="grey85")+
  geom_errorbar(aes(ymax=RACEUL/1000,ymin=RACELL/1000),colour="black",width=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  geom_line(aes(x=YEAR,y=Biom_est/1000))+
  geom_line(data=pSDRFX[pSDRFX$REGULATORY_AREA_NAME=="WESTERN GOA",],aes(x=YEAR,y=Biom_est/1000),linetype="dashed",color="black",size=1)+
  coord_cartesian(y=c(0,2),x=c(1983,2022))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x="",y="",title="Western GOA")

CGOA_doc<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="CENTRAL GOA",],aes(x=YEAR,y=Biomass/1000))+
  geom_ribbon(aes(ymin=Biom_LL/1000,ymax=Biom_UL/1000),fill="grey85")+
  geom_errorbar(aes(ymax=RACEUL/1000,ymin=RACELL/1000),colour="black",width=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  geom_line(aes(x=YEAR,y=Biom_est/1000))+
  geom_line(data=pSDRFX[pSDRFX$REGULATORY_AREA_NAME=="CENTRAL GOA",],aes(x=YEAR,y=Biom_est/1000),linetype="dashed",color="black",size=1)+
  coord_cartesian(y=c(0,100),x=c(1983,2022))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x="",y="",title="Central GOA")

EGOA_doc<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="EASTERN GOA",],aes(x=YEAR,y=Biomass/1000))+
  geom_ribbon(aes(ymin=Biom_LL/1000,ymax=Biom_UL/1000),fill="grey85")+
  geom_errorbar(aes(ymax=RACEUL/1000,ymin=RACELL/1000),colour="black",width=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  geom_line(aes(x=YEAR,y=Biom_est/1000))+
  geom_line(data=pSDRFX[pSDRFX$REGULATORY_AREA_NAME=="EASTERN GOA",],aes(x=YEAR,y=Biom_est/1000),linetype="dashed",color="black",size=1)+
  coord_cartesian(y=c(0,275),x=c(1983,2022))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x="",y="",title="Eastern GOA")

GOA_doc<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="GOA",],aes(x=YEAR,y=Biomass/1000))+
  geom_ribbon(aes(ymin=Biom_LL/1000,ymax=Biom_UL/1000),fill="grey85")+
  geom_errorbar(aes(ymax=RACEUL/1000,ymin=RACELL/1000),colour="black",width=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  geom_point(data=SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="GOA"&SD_RFX$YEAR==2001,],aes(x=YEAR,y=Biomass/1000),color="blue",size=4)+
  geom_point(data=SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="GOA"&SD_RFX$YEAR==2001,],aes(x=YEAR,y=Biomass/1000),color="light blue",size=3)+
  geom_line(aes(x=YEAR,y=Biom_est/1000))+
  geom_line(data=pSDRFX[pSDRFX$REGULATORY_AREA_NAME=="GOA",],aes(x=YEAR,y=Biom_est/1000),linetype="dashed",color="black",size=1)+
  coord_cartesian(y=c(0,300),x=c(1983,2022))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x="",y="",title="Gulf of Alaska")

RFX_fig_doc<-grid.arrange(arrangeGrob(WGOA_doc,CGOA_doc,EGOA_doc,GOA_doc,layout_matrix=rbind(c(1,2,3),c(4,4,4)),
                                      left = textGrob("Biomass (1000s t)", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=15)),
                                      bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=15))))


