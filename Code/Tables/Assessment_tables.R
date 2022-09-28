# Shark Assessment Tables ----
# Updated 7_10_2020 by C. Tribuzio

# To DO ----
#1) combine all into one doc
#2) do it in markdown (but need to figure out heading numbering etc)

# Setup ----
libs <- c("tidyverse", "flextable","officer","reshape2","plyr")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

'%nin%'<-Negate('%in%')

AYR<-2020
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
cleandatdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
tabledir<-paste(getwd(),"/Output/",AYR,"/Tables/",sep="")

#adapted from theme_booktabs
theme_SAFE<-function(x){
  big_border <- fp_border(width = 2)
  std_border <- fp_border(width = 1)
  x<-fontsize(x,size=10,part="all")
  x<-font(x,fontname="Times New Roman",part="all")
  x<-bold(x,part="header")
  x <- hline_top(x, border = big_border, part = "header")
  x <- hline(x, border = std_border, part = "header")
  x <- hline_bottom(x, border = big_border, part = "body")
  x <- align(x,align="center",part="header")
}

# Table 20.1 done by hand

# GOA table 20.2 also done by hand

# TAC/ABC GOA 20.3 BSAI 20.2 done by hand as well

# GOA 20.4 sport catch ----
ADFG<-read.csv(paste(cleandatdir,"/ADFG_sportharvest",AYR,".csv",sep=""),header=T)

tot_sharks<-ddply(ADFG,c("Year","Species"),summarize,Tot_N=sum(Nfish))

A2<-dcast(ADFG,Year+NMFS_Area+Species~Type,value.var = "Nfish")
A2[is.na(A2)]<-0
A2$TotN<-(A2$Discarded+A2$Retained)
A2$pdisc<-round((A2$Discarded/A2$TotN)*100,0)
A2$TotN<-formatC(A2$TotN,format="d",big.mark=",")
A2$colvals<-paste(A2$TotN,"(",A2$pdisc,"%)",sep="")

A3<-dcast(A2,Year+Species~NMFS_Area,value.var = "colvals")
A4<-merge(A3,tot_sharks)
ADFGtab<-flextable(A4[A4$Species=="All Sharks",],
          col_keys = c("Year","Western","Central","Eastern","Tot_N"),
          theme_fun = theme_SAFE) %>%
  align(j=c("Year"),align = "Center",part="body") %>%
  colformat_num(j=c("Tot_N"),big.mark = ",",digits=0) %>%
  set_header_labels(Tot_N="Total") %>%
  add_header(Year="Sport Catch of All Sharks",Western="Sport Catch of All Sharks",
             Central="Sport Catch of All Sharks",Eastern="Sport Catch of All Sharks",
             Tot_N="Sport Catch of All Sharks",) %>%
  merge_h(part="header") %>%
  align(align="Center",part="header") %>%
  font(fontname="Times New Roman",part="header") %>%
  bold(part="header") %>%
  fontsize(size=10,part="all") %>%
  hline_top(border=fp_border(width = 2), part = "header")
save_as_html(ADFGtab,path=paste(tabledir,"/GOA20_4_ADFGsport.html",sep=""))

ADFGtab2<-flextable(A4[A4$Species=="Salmon Shark",],
                    col_keys = c("Year","Western","Central","Eastern","Tot_N"),
                    theme_fun = theme_SAFE) %>%
  align(j=c("Year"),align = "Center",part="body") %>%
  colformat_num(j=c("Tot_N"),big.mark = ",",digits=0) %>%
  set_header_labels(Tot_N="Total") %>%
  add_header(Year="Charter Catch of Salmon Shark",Western="Charter Catch of Salmon Shark",
             Central="Charter Catch of Salmon Shark",Eastern="Charter Catch of Salmon Shark",
             Tot_N="Charter Catch of Salmon Shark",) %>%
  merge_h(part="header") %>%
  align(align="Center",part="header") %>%
  font(fontname="Times New Roman",part="header") %>%
  bold(part="header") %>%
  fontsize(size=10,part="all") %>%
  hline_top(border=fp_border(width = 2), part = "header")
save_as_html(ADFGtab2,path=paste(tabledir,"/GOA20_4_ADFGcharter.html",sep=""))

# GOA 20.5 Discard rates ----
CAS_dat<-read.csv(paste(cleandatdir,"/CAS_GFTBF_Sharks",AYR,".csv",sep=""),header=T)
CAS2<-dcast(CAS_dat,Year+FMP.Area+Species~Retained.Discarded,fun.aggregate = sum,value.var = "Catch..mt.")
CAS2$Drate<-round(CAS2$Discarded/(CAS2$Discarded+CAS2$Retained)*100,0)
CAS3<-dcast(CAS2,FMP.Area+Year~Species,fun.aggregate = sum, value.var = "Drate")
disc_mean<-ddply(CAS2,c("FMP.Area","Species"),summarize,Average=round(mean(Drate),0))
dm2<-dcast(disc_mean,FMP.Area~Species,fun.aggregate = mean,value.var = "Average")
dm3<-cbind("Mean",dm2)
dm3<-dm3[,c("FMP.Area","\"Mean\"","Other Sharks","Pacific Sleeper Shark","Salmon Shark","Spiny Dogfish")]
colnames(dm3)[2]<-"Year"

CAS4<-rbind(CAS3,dm3)
CAS5<-cbind(CAS4[,c("FMP.Area","Year")],
  paste(CAS4[,c("Spiny Dogfish")],"%",sep=""),
            paste(CAS4[,c("Pacific Sleeper Shark")],"%",sep=""),
            paste(CAS4[,c("Salmon Shark")],"%",sep=""),
            paste(CAS4[,c("Other Sharks")],"%",sep=""))
GOAspec_order<-c("Spiny Dogfish","Pacific Sleeper Shark","Salmon Shark","Other Sharks")
colnames(CAS5)<-c("FMP.Area","Year",GOAspec_order)

GOAdiscards<-flextable(CAS5[CAS5$FMP.Area=="GOA",], col_keys = c("Year",GOAspec_order), theme_fun=theme_SAFE)
GOAdiscards<-align(GOAdiscards,j="Year",align = "Center",part="body")
#GOAdiscards<-set_header_labels(GOAdiscards,Other Sharks="Other/Unidentified Sharks")
save_as_html(GOAdiscards,path=paste(tabledir,"/GOA20_5_discards.html",sep=""))

# BSAI 20.3 Discard rates ----
CAS2<-dcast(CAS_dat,Year+FMP.Subarea+Species~Retained.Discarded,fun.aggregate = sum,value.var = "Catch..mt.")
CAS2$Drate<-round(CAS2$Discarded/(CAS2$Discarded+CAS2$Retained)*100,0)
CAS3<-dcast(CAS2,FMP.Subarea+Year~Species,fun.aggregate = sum, value.var = "Drate",fill=999)
CAS3[CAS3==999]<-""
disc_mean<-ddply(CAS2,c("FMP.Subarea","Species"),summarize,Average=round(mean(Drate),0))
dm2<-dcast(disc_mean,FMP.Subarea~Species,fun.aggregate = mean,value.var = "Average")
dm3<-cbind("Mean",dm2)
dm3<-dm3[,c("FMP.Subarea","\"Mean\"","Other Sharks","Pacific Sleeper Shark","Salmon Shark","Spiny Dogfish")]
colnames(dm3)[2]<-"Year"

CAS4<-rbind(CAS3,dm3)
CAS5<-cbind(CAS4[,c("FMP.Subarea","Year")],
            paste(CAS4[,c("Spiny Dogfish")],"%",sep=""),
            paste(CAS4[,c("Pacific Sleeper Shark")],"%",sep=""),
            paste(CAS4[,c("Salmon Shark")],"%",sep=""),
            paste(CAS4[,c("Other Sharks")],"%",sep=""))
colnames(CAS5)<-c("FMP.Subarea","Year",GOAspec_order)

#this is clunky, but need to get the Means with their FMPs
BS_dat<-CAS5[CAS5$FMP.Subarea=="BS",]
BS_dat$FMP_Sub<-"Bering Sea" #this will look nicer than "BS"
AI_dat<-CAS5[CAS5$FMP.Subarea=="AI",]
AI_dat$FMP_Sub<-"Aleutian Islands" #this will look nicer than "BS"

BSAI_dat<-rbind(AI_dat,BS_dat)

#need to figure out how to make NAs blanks
BSAIspec_order<-c("Pacific Sleeper Shark","Salmon Shark","Spiny Dogfish","Other Sharks")
BSAIdiscards<-flextable(BSAI_dat[BSAI_dat$FMP.Subarea=="BS"|BSAI_dat$FMP.Subarea=="AI",],
                        col_keys = c("FMP_Sub","Year",BSAIspec_order), theme_fun=theme_SAFE) %>%
  merge_v(j="FMP_Sub") %>%
  set_header_labels(FMP_Sub="FMP Subarea") %>%
  align(j=c("FMP_Sub","Year"),align = "Center",part="body") %>%
  hline(i=nrow(AI_dat),border=fp_border(width = 1),part="body")
save_as_html(BSAIdiscards,path=paste(tabledir,"/BSAI20_3_discards.html",sep=""))
                
# GOA 20.6 total catch ----
#bring in old data
catch_old<-read.csv(paste(getwd(),"/Data/Static/pre2003_shark_cleaned.csv",sep=""),header=T)
catch_old<-catch_old[!(catch_old$Year<2003&catch_old$FMP.Area=="BSAI"),] #in BSAI we aren't worried about anything prior to 2003, won't be in the table
catch_old<-catch_old[!(catch_old$Year<1997&catch_old$FMP.Area=="GOA"),] #in GOA nothing before 1997
c_old<-ddply(catch_old,c("Year","FMP.Area","Species"),summarize,Tot_Catch=round(sum(Catch..mt.),0))
c_old2<-dcast(c_old,FMP.Area+Year~Species)
c_tot<-ddply(c_old,c("Year","FMP.Area"),summarize,Total=round(sum(Tot_Catch),0))
c_old3<-merge(c_old2,c_tot)

#test that the total equals the sum of the others, should result in no rows
c_old3[rowSums(c_old3[,3:6],na.rm=T)!=c_old3$Total,]

#format updated data
CAS2<-ddply(CAS_dat,c("Year","FMP.Area","Species"),summarize,Tot_Catch=round(sum(Catch..mt.),0))
CAS3<-dcast(CAS2,FMP.Area+Year~Species)
CAS_tot<-ddply(CAS2,c("Year","FMP.Area"),summarize,Total=round(sum(Tot_Catch),0))
CAS4<-merge(CAS3,CAS_tot)

#test that the total equals the sum of the others, should result in no rows
CAS4[rowSums(CAS4[,3:6],na.rm=T)!=CAS4$Total,]

#add a fake row to trick flextable to show a break between 2002 and 2003
frow<-c("GOA","-","-","-","-","-","-")

shark_catch<-rbind(c_old3,frow,CAS4)

GOAcatch<-flextable(shark_catch[shark_catch$FMP.Area=="GOA",], 
                    col_keys = c("Year",GOAspec_order,"Total"), theme_fun=theme_SAFE) %>%
  align(j="Year",align = "Center",part="body") %>%
  colformat_num(j=c(GOAspec_order,"Total"),big.mark = ",",digits=0)

GOAcatch<-colformat_num(GOAcatch,j=c(GOAspec_order,"Total"),big.mark = ",",digits=0)

save_as_html(GOAcatch,path=paste(tabledir,"/GOA20_6_catch.html",sep=""))

#BSAI 20.4 total catch ----
BSAIcatch<-flextable(shark_catch[shark_catch$FMP.Area=="BSAI",], 
                     col_keys = c("Year",BSAIspec_order,"Total"), theme_fun=theme_SAFE) %>%
  align(j="Year",align = "Center",part="body") %>%
  colformat_num(j=c(BSAIspec_order,"Total"),big.mark = ",",digits=0)
save_as_html(BSAIcatch,path=paste(tabledir,"/BSAI20_5_catch.html",sep=""))

#Alternative with AI/BS breakout
CAS2<-ddply(CAS_dat,c("Year","FMP.Subarea","Species"),summarize,Tot_Catch=round(sum(Catch..mt.),0))
CAS3<-dcast(CAS2,FMP.Subarea+Year~Species)

#verify that all zeros are in fact <1 mt and not true zeros
td<-ddply(CAS_dat,c("Year","FMP.Subarea","Species"),summarize,Tot_Catch_dec=sum(Catch..mt.))
td3<-merge(CAS2,td)
td3[td3$Tot_Catch-td3$Tot_Catch_dec==0,] #kicks out two in GOA, so we don't care

#now we know all zeros are really <1 mt and all NA's are zeros
CAS4[CAS4==0]<-"<1"
CAS4[is.na(CAS4)]<-0

#now combine BS and AI catches into single cell value
cat_mat<-matrix(paste(as.matrix(CAS4[CAS4$FMP.Subarea=="AI",c(BSAIspec_order)]),
      as.matrix(CAS4[CAS4$FMP.Subarea=="BS",c(BSAIspec_order)]),sep="/"),18,4)
CAS5<-cbind(unique(CAS4[,c("Year")]),cat_mat)
colnames(CAS5)<-c("Year",BSAIspec_order)
CAS6<-merge(CAS5,CAS_tot[CAS_tot$FMP.Area=="BSAI",],by="Year")

altBSAIcatch<-flextable(CAS6,col_keys = c("Year",BSAIspec_order,"Total"), theme_fun=theme_SAFE)
altBSAIcatch<-align(altBSAIcatch,j="Year",align = "Center",part="body")

save_as_html(altBSAIcatch,path=paste(tabledir,"/BSAIalt20_5_catch.html",sep=""))

# GOA 20.7 INSD catches ----
INSDdat<-CAS_dat[CAS_dat$FMP.Area=="INSD",]
Id<-ddply(INSDdat,c("Species","Year","Trip.Target.Name"),summarize,
          Tot_Catch=round(sum(Catch..mt.),1))
Id<-Id[Id$Species %in% c("Pacific Sleeper Shark","Spiny Dogfish"),]
Id<-Id[!is.na(Id$Year),]
Id2<-dcast(Id,Species+Year~Trip.Target.Name)

Idtot<-ddply(INSDdat,c("Species","Year"),summarize,Tot_Catch=round(sum(Catch..mt.),1))
Id3<-merge(Id2,Idtot)

Id3[Id3==0]<-"<0.1"
Id3[is.na(Id3)]<-""

#need to identify confidential cells
INSDconf<-ddply(INSDdat,c("Species","Year","Trip.Target.Name"),summarize,
                Nvessels=length(unique(Vessel.Name)))
INSDconf<-INSDconf[!is.na(INSDconf$Year),]
Ic<-dcast(INSDconf,Species+Year~Trip.Target.Name)
Ictot<-data.frame(rowSums(Ic<3,na.rm=T))
Ictot[Ictot==0]<-"NA"
colnames(Ictot)<-"Conf_ves"
Ic3<-cbind(Ic,Ictot)

Ic3<-Ic3[Ic3$Species %in% c("Pacific Sleeper Shark","Spiny Dogfish"),]
colnames(Ic3)<-c("Species","Year","vesHal","vesPC","vesplk","vesrk","vessab","vestot")

Id4<-merge(Id3,Ic3)

colnames(Id4)[4]<-"Pcod"

#make table
INSDcatch<-flextable(Id4, col_keys = colnames(Id4[1:ncol(Id3)]), theme_fun=theme_SAFE) %>%
  merge_v(j="Species") %>%
  set_header_labels(Tot_Catch="Total") %>%
  set_header_labels(Pcod="Pacific Cod") %>%
  align(j=c("Species","Year"),align = "Center",part="body") %>%
  hline(i=nrow(Id3[Id3$Species=="Pacific Sleeper Shark",]),
        border=fp_border(width = 1),part="body") %>%
  bg(i=~vesHal<3,j=~Halibut,bg="dark grey") %>%
  color(i=~vesHal<3,j=~Halibut,color="dark grey") %>%
  bg(i=~vesPC<3,j=~Pcod,bg="dark grey") %>%
  color(i=~vesPC<3,j=~Pcod,color="dark grey") %>%
  bg(i=~vesplk<3,j=~Pollock,bg="dark grey") %>%
  color(i=~vesplk<3,j=~Pollock,color="dark grey") %>%
  bg(i=~vesrk<3,j=~Rockfish,bg="dark grey") %>%
  color(i=~vesrk<3,j=~Rockfish,color="dark grey") %>%
  bg(i=~vessab<3,j=~Sablefish,bg="dark grey") %>%
  color(i=~vessab<3,j=~Sablefish,color="dark grey") %>%
  bg(i=~vestot<3,j=~Tot_Catch,bg="dark grey") %>%
  color(i=~vestot<3,j=~Tot_Catch,color="dark grey") 


save_as_html(INSDcatch,path=paste(tabledir,"/GOA20_7_INSD.html",sep=""))

# RACE tables ----
# GOA 20.8 RACE TWL ----
shark_biomass<-read.csv(paste(getwd(),"/Output/",AYR,"/RACE_Biomass/RACE_Biomass_Sharks.csv",sep=""),header=T)
GOA<-shark_biomass[shark_biomass$SURVEY=="GOA"&
                     shark_biomass$REGULATORY_AREA_NAME=="GOA"&
                     shark_biomass$Group %in% c("Pacific Sleeper Shark","Spiny Dogfish","Salmon Shark"),]
GOA2<-melt(GOA,id.vars = c("SURVEY","REGULATORY_AREA_NAME","YEAR","Group"))
GOA2<-GOA2[!c(GOA2$variable=="SE"|
             GOA2$variable=="Variance"),]
GOA3<-dcast(GOA2,Group+YEAR~variable,value.var = "value")
GOA3<-GOA3[,c("Group","YEAR",'HAUL_COUNT',"CATCH_COUNT","Biomass","CV")]
GOAtot<-round(ddply(GOA3,c('YEAR'),summarize,Tot_Biom=sum(Biomass)),0)
GOA3$Biomass<-round(GOA3$Biomass,0)
GOA3$CV<-round(GOA3$CV,2)
test<-dcast(GOA3,YEAR+HAUL_COUNT~Group,value.var=c("CATCH_COUNT")) 
GOA4<-cbind(GOA3[GOA3$Group=="Spiny Dogfish",c("YEAR","HAUL_COUNT")],
            GOA3[GOA3$Group=="Spiny Dogfish",c("CATCH_COUNT","Biomass","CV")],
            GOA3[GOA3$Group=="Pacific Sleeper Shark",c("CATCH_COUNT","Biomass","CV")],
            GOA3[GOA3$Group=="Salmon Shark",c("CATCH_COUNT","Biomass","CV")])
colnames(GOA4)<-c("YEAR","Haul","SDCatch","SDBiom","SDCV","PSScatch","PSSBiom","PSSCV",
                  "SScatch","SSBiom","SSCV")
GOA5<-merge(GOA4,GOAtot)

GOABTS<-flextable(GOA5,col_keys = colnames(GOA5),theme_fun=theme_SAFE) %>%
  align(j="YEAR", align = "Center", part="body") %>%
  colformat_num(j=c("SDBiom","PSSBiom","SSBiom","Tot_Biom"),big.mark = ",",digits=0) %>%
  set_header_labels(YEAR="Year") %>%
  set_header_labels(Haul="Survey Hauls") %>%
  set_header_labels(SDCatch="Hauls w/Catch") %>%
  set_header_labels(SDBiom="Biomass") %>%
  set_header_labels(SDCV="CV") %>%
  set_header_labels(PSScatch="Hauls w/Catch") %>%
  set_header_labels(PSSBiom="Biomass") %>%
  set_header_labels(PSSCV="CV") %>%
  set_header_labels(SScatch="Hauls w/Catch") %>%
  set_header_labels(SSBiom="Biomass") %>%
  set_header_labels(SSCV="CV") %>%
  set_header_labels(Tot_Biom="Total Shark Biomass") %>%
  add_header(SDCatch="Spiny Dogfish",SDBiom="Spiny Dogfish",SDCV="Spiny Dogfish",
             PSScatch="Pacific Sleeper Shark",PSSBiom="Pacific Sleeper Shark",PSSCV="Pacific Sleeper Shark",
             SScatch="Salmon Shark",SSBiom="Salmon Shark",SSCV="Salmon Shark") %>%
  merge_h(part="header") %>%
  align(align="Center",part="header") %>%
  font(fontname="Times New Roman",part="header") %>%
  bold(part="header") %>%
  fontsize(size=10,part="all") %>%
  hline_top(border=fp_border(width = 2), part = "header")

save_as_html(GOABTS,path=paste(tabledir,"/GOA20_8_RACE.html",sep=""))

# BSAI 20.5 Slope ----
slope<-shark_biomass[shark_biomass$SURVEY=="EBS_SLOPE"&
                     shark_biomass$Group %in% c("Pacific Sleeper Shark","Spiny Dogfish"),]
slope2<-melt(slope,id.vars = c("SURVEY","REGULATORY_AREA_NAME","YEAR","Group"))
slope2<-slope2[!c(slope2$variable=="SE"|
                slope2$variable=="Variance"),]
slope3<-dcast(slope2,Group+YEAR~variable,value.var = "value")
slope3<-slope3[,c("Group","YEAR",'HAUL_COUNT',"CATCH_COUNT","Biomass","CV")]
slope3$Biomass<-round(slope3$Biomass,0)
slope3$CV<-round(slope3$CV,2)
slope4<-cbind(slope3[slope3$Group=="Spiny Dogfish",c("YEAR","HAUL_COUNT")],
            slope3[slope3$Group=="Spiny Dogfish",c("CATCH_COUNT","Biomass","CV")],
            slope3[slope3$Group=="Pacific Sleeper Shark",c("CATCH_COUNT","Biomass","CV")])
colnames(slope4)<-c("YEAR","Haul","SDCatch","SDBiom","SDCV","PSScatch","PSSBiom","PSSCV")

slopeBTS<-flextable(slope4,col_keys = colnames(slope4),theme_fun=theme_SAFE) %>%
  align(j="YEAR", align = "Center", part="body") %>%
  colformat_num(j=c("SDBiom","PSSBiom"),big.mark = ",",digits=0) %>%
  set_header_labels(YEAR="Year") %>%
  set_header_labels(Haul="Survey Hauls") %>%
  set_header_labels(SDCatch="Hauls w/Catch") %>%
  set_header_labels(SDBiom="Biomass") %>%
  set_header_labels(SDCV="CV") %>%
  set_header_labels(PSScatch="Hauls w/Catch") %>%
  set_header_labels(PSSBiom="Biomass") %>%
  set_header_labels(PSSCV="CV") %>%
  add_header(SDCatch="Spiny Dogfish",SDBiom="Spiny Dogfish",SDCV="Spiny Dogfish",
             PSScatch="Pacific Sleeper Shark",PSSBiom="Pacific Sleeper Shark",PSSCV="Pacific Sleeper Shark") %>%
  merge_h(part="header") %>%
  align(align="Center",part="header") %>%
  font(fontname="Times New Roman",part="header") %>%
  bold(part="header") %>%
  fontsize(size=10,part="all") %>%
  hline_top(border=fp_border(width = 2), part = "header")

save_as_html(slopeBTS,path=paste(tabledir,"/BSAI20_5_slope.html",sep=""))

# BSAI 20.6 Aleutians ----
AI<-shark_biomass[shark_biomass$REGULATORY_AREA_NAME=="AI"&
                       shark_biomass$Group %in% c("Pacific Sleeper Shark","Spiny Dogfish"),]
AI2<-melt(AI[,!names(AI) %in% c("REGULATORY_AREA_NAME")],id.vars = c("SURVEY","YEAR","Group"))
AI2<-AI2[!c(AI2$variable=="SE"|
                    AI2$variable=="Variance"),]
AI3<-dcast(AI2,Group+YEAR~variable,value.var = "value")
AI3<-AI3[,c("Group","YEAR",'HAUL_COUNT',"CATCH_COUNT","Biomass","CV")]
AI3$Biomass<-round(AI3$Biomass,0)
AI3$CV<-round(AI3$CV,2)
AI4<-cbind(AI3[AI3$Group=="Spiny Dogfish",c("YEAR","HAUL_COUNT")],
              AI3[AI3$Group=="Spiny Dogfish",c("CATCH_COUNT","Biomass","CV")],
              AI3[AI3$Group=="Pacific Sleeper Shark",c("CATCH_COUNT","Biomass","CV")])
colnames(AI4)<-c("YEAR","Haul","SDCatch","SDBiom","SDCV","PSScatch","PSSBiom","PSSCV")

AIBTS<-flextable(AI4,col_keys = colnames(AI4),theme_fun=theme_SAFE) %>%
  align(j="YEAR", align = "Center", part="body") %>%
  colformat_num(j=c("SDBiom","PSSBiom"),big.mark = ",",digits=0) %>%
  set_header_labels(YEAR="Year") %>%
  set_header_labels(Haul="Survey Hauls") %>%
  set_header_labels(SDCatch="Hauls w/Catch") %>%
  set_header_labels(SDBiom="Biomass") %>%
  set_header_labels(SDCV="CV") %>%
  set_header_labels(PSScatch="Hauls w/Catch") %>%
  set_header_labels(PSSBiom="Biomass") %>%
  set_header_labels(PSSCV="CV") %>%
  add_header(SDCatch="Spiny Dogfish",SDBiom="Spiny Dogfish",SDCV="Spiny Dogfish",
             PSScatch="Pacific Sleeper Shark",PSSBiom="Pacific Sleeper Shark",PSSCV="Pacific Sleeper Shark") %>%
  merge_h(part="header") %>%
  align(align="Center",part="header") %>%
  font(fontname="Times New Roman",part="header") %>%
  bold(part="header") %>%
  fontsize(size=10,part="all") %>%
  hline_top(border=fp_border(width = 2), part = "header")

save_as_html(AIBTS,path=paste(tabledir,"/BSAI20_6_AI.html",sep=""))

# BSAI 20.7 Shelf ----
shelf<-shark_biomass[shark_biomass$SURVEY=="EBS_SHELF"&
                    shark_biomass$Group %in% c("Pacific Sleeper Shark","Spiny Dogfish"),]
shelf2<-melt(shelf[,!names(shelf) %in% c("REGULATORY_AREA_NAME")],id.vars = c("SURVEY","YEAR","Group"))
shelf2<-shelf2[!c(shelf2$variable=="SE"|
              shelf2$variable=="Variance"),]
shelf3<-dcast(shelf2,Group+YEAR~variable,value.var = "value")
shelf3<-shelf3[,c("Group","YEAR",'HAUL_COUNT',"CATCH_COUNT","Biomass","CV")]
shelf3$Biomass<-round(shelf3$Biomass,0)
shelf3$CV<-round(shelf3$CV,2)
shelf4<-cbind(shelf3[shelf3$Group=="Spiny Dogfish",c("YEAR","HAUL_COUNT")],
           shelf3[shelf3$Group=="Spiny Dogfish",c("CATCH_COUNT","Biomass","CV")],
           shelf3[shelf3$Group=="Pacific Sleeper Shark",c("CATCH_COUNT","Biomass","CV")])
colnames(shelf4)<-c("YEAR","Haul","SDCatch","SDBiom","SDCV","PSScatch","PSSBiom","PSSCV")

shelfBTS<-flextable(shelf4,col_keys = colnames(shelf4),theme_fun=theme_SAFE) %>%
  align(j="YEAR", align = "Center", part="body") %>%
  colformat_num(j=c("SDBiom","PSSBiom"),big.mark = ",",digits=0) %>%
  set_header_labels(YEAR="Year") %>%
  set_header_labels(Haul="Survey Hauls") %>%
  set_header_labels(SDCatch="Hauls w/Catch") %>%
  set_header_labels(SDBiom="Biomass") %>%
  set_header_labels(SDCV="CV") %>%
  set_header_labels(PSScatch="Hauls w/Catch") %>%
  set_header_labels(PSSBiom="Biomass") %>%
  set_header_labels(PSSCV="CV") %>%
  add_header(SDCatch="Spiny Dogfish",SDBiom="Spiny Dogfish",SDCV="Spiny Dogfish",
             PSScatch="Pacific Sleeper Shark",PSSBiom="Pacific Sleeper Shark",PSSCV="Pacific Sleeper Shark") %>%
  merge_h(part="header") %>%
  align(align="Center",part="header") %>%
  font(fontname="Times New Roman",part="header") %>%
  bold(part="header") %>%
  fontsize(size=10,part="all") %>%
  hline_top(border=fp_border(width = 2), part = "header")

save_as_html(shelfBTS,path=paste(tabledir,"/BSAI20_7_shelf.html",sep=""))
#####Note that there are no values for 2019, there were no sharks caught in 2019
####Need to fix this in RACE Biomass code

# GOA 20.9 research catch ----
redat<-read.csv(paste(cleandatdir,"/Noncommercial",AYR,".csv",sep=""),header=T)
redat<-redat[!is.na(redat$Volume),]

re2<-dcast(redat,FMP+Source+Year~Survey,value.var = "Volume")
re2$rvar<-round(re2$ADFG_t,2)
re2[is.na(re2$ADFG_t),]$ADFG_t<-""
re2[re2$ADFG_t<0.01&re2$ADFG_t>0,]$rvar<-"<0.01"
re2$ADFG_t<-re2$rvar

re2$rvar<-round(re2$AFSC_TWLt,2)
re2[is.na(re2$AFSC_TWLt),]$AFSC_TWLt<-""
re2[re2$AFSC_TWLt<0.01&re2$AFSC_TWLt>0,]$rvar<-"<0.01"
re2$AFSC_TWLt<-re2$rvar

re2$rvar<-round(re2$AFSC_LLt,2)
re2[is.na(re2$AFSC_LLt),]$AFSC_LLt<-""
re2[re2$AFSC_LLt<0.01&re2$AFSC_LLt>0,]$rvar<-"<0.01"
re2$AFSC_LLt<-re2$rvar

re2$rvar<-round(re2$IPHC_LLt,2)
re2[is.na(re2$IPHC_LLt),]$IPHC_LLt<-""
re2[re2$IPHC_LLt<0.01&re2$IPHC_LLt>0,]$rvar<-"<0.01" #kicks an error, it's ok, just means there aren't any
re2$IPHC_LLt<-re2$rvar

re2<-re2[order(re2$FMP,re2$Year),]


#need to kill the IPHC numbers 2010+, they don't regularly report #s
#this is a last second thing to do because it screws up the "type"
re2[re2$Source=="AKRO",]$IPHC_LLn<-""


re2[is.na(re2)]<-""

GOAres<-flextable(re2[re2$FMP=="GOA",],col_keys = c("Year","AFSC_TWLt","AFSC_LLn",
                                                     "AFSC_LLt","IPHC_LLn","IPHC_LLt",
                                                     "ADFG_t")) %>%
  colformat_num(j=c("AFSC_LLn","IPHC_LLn"),big.mark = ",",digits=0) %>%
  align(j="Year", align = "Center", part="body") %>%
  align(align = "Center", part="header") %>%
  align(j=c("AFSC_TWLt","AFSC_LLn","AFSC_LLt","IPHC_LLn","IPHC_LLt","ADFG_t"),
        align="right",part="body") %>%
  set_header_labels(AFSC_TWLt="AFSC Trawl Surveys (t)") %>%
  set_header_labels(AFSC_LLn="AFSC LL Survey (#s)") %>%
  set_header_labels(AFSC_LLt="AFSC LL Survey (t)") %>%
  set_header_labels(IPHC_LLn="IPHC LL Survey (#s)") %>%
  set_header_labels(IPHC_LLt="IPHC LL Survey (t)") %>%
  set_header_labels(ADFG_t="ADF&G (t) (includes sport and research)") %>%
  hline(i=2009-1977,border=fp_border(width = 1),part="body")
save_as_html(GOAres,path=paste(tabledir,"/GOA20_9_noncomm.html",sep=""))

# BSAI 20.8 Research catch ----
BSAIres<-flextable(re2[re2$FMP=="BSAI",],col_keys = c("Year","AFSC_TWLt","AFSC_LLn",
                                                    "AFSC_LLt","IPHC_LLn","IPHC_LLt",
                                                    "ADFG_t")) %>%
  colformat_num(j=c("AFSC_LLn","IPHC_LLn"),big.mark = ",",digits=0) %>%
  #above doesn't work becuase they aren't actually numbers, problem with NAs to deal with next time
  align(j="Year", align = "Center", part="body") %>%
  align(align = "Center", part="header") %>%
  align(j=c("AFSC_TWLt","AFSC_LLn","AFSC_LLt","IPHC_LLn","IPHC_LLt","ADFG_t"),
        align="right",part="body") %>%
  set_header_labels(AFSC_TWLt="AFSC Trawl Surveys (t)") %>%
  set_header_labels(AFSC_LLn="AFSC LL Survey (#s)") %>%
  set_header_labels(AFSC_LLt="AFSC LL Survey (t)") %>%
  set_header_labels(IPHC_LLn="IPHC LL Survey (#s)") %>%
  set_header_labels(IPHC_LLt="IPHC LL Survey (t)") %>%
  set_header_labels(ADFG_t="ADF&G (t) (includes sport and research)") %>%
  hline(i=2009-1977,border=fp_border(width = 1),part="body")
save_as_html(BSAIres,path=paste(tabledir,"/BSAI20_8_noncomm.html",sep=""))

# GOA 20.10 RFX ----
RFX<-read.csv(paste(getwd(),"/Output/",AYR,"/RFX/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header=T)
keeps<-c("GOA","CENTRAL GOA","WESTERN GOA","EASTERN GOA")
GOARFX<-RFX[RFX$REGULATORY_AREA_NAME %in% keeps,]
GOARFX$Biom_est<-round(GOARFX$Biom_est,0)
GOARFX$Biom_LL<-round(GOARFX$Biom_LL,0)
GOARFX$Biom_UL<-round(GOARFX$Biom_UL,0)

GR<-dcast(GOARFX,YEAR~REGULATORY_AREA_NAME,value.var = "Biom_est")
GR<-GR[,c("YEAR","WESTERN GOA","CENTRAL GOA","EASTERN GOA","GOA")]


GR2<-cbind(GR,GOARFX[GOARFX$REGULATORY_AREA_NAME=="GOA",c("Biom_LL","Biom_UL")])
colnames(GR2)<-c("Year","Western","Central","Eastern","GOA","GOA_LL","GOA_UL")

RFXtab<-flextable(GR2,col_keys = colnames(GR2)) %>%
  colformat_num(j=c("Western","Central","Eastern","GOA","GOA_LL","GOA_UL"),
                big.mark = ",",digits=0) %>%
  align(j="Year", align = "Center", part="body") %>%
  set_header_labels(GOA="Total GOA") %>%
  set_header_labels(GOA_LL="Lower 95% CI") %>%
  set_header_labels(GOA_UL="Upper 95% CI")
save_as_html(RFXtab,path=paste(tabledir,"/GOA20_10_RFX.html",sep=""))

           