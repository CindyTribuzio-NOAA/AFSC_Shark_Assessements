# Shark Assessment Figs ----
# Updated 9/30/2020 by C. Tribuzio

# TO DO ----
# clean up PSS length data files, too many files floating around
# standardize figure style and codes, these codes are grabbed from mulitple 
# historical versions

# Setup ----
#library(ggplot2)
#library(plyr)
#library(gridExtra)
#library(gtable)
#library(grid)
#library(reshape2)
#library(forcats)
#library(lubridate)
#library(readxl)
#library(ggridges)

libs <- c("tidyverse", "ggridges","gridExtra","grid","gtable")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

'%nin%'<-Negate('%in%')

AYR<-2020
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
cleandatdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
figdir<-paste(getwd(),"/Output/",AYR,"/Figures/",sep="")

# ggplot themes
theme_doc<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      plot.title=element_text(size=20,colour='black',hjust = 0.5),
      plot.background=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_line(color="grey90"),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(colour='black'),
      axis.line.y = element_line(colour='black'),
      axis.text=element_text(size=12,colour='black'),
      axis.ticks=element_line(colour='black'),
      axis.title.y=element_text(colour='black',angle=90),
      axis.title.x=element_text(colour='black'),
      legend.background=element_blank(),
      legend.text=element_text(colour='black',size=12),
      legend.title=element_text(colour='black',size=12),
      strip.background=element_blank(),
      strip.text=element_text(size=20,colour='black')
    )
}

theme_pres<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      axis.line.x=element_line(size=1,color = '#FFFFCC'),
      axis.line.y=element_line(size=1,color = '#FFFFCC'),
      axis.text=element_text(size=rel(1),colour='#FFFFCC',face="bold"),
      axis.ticks=element_line(colour='#FFFFCC'),
      axis.title.y=element_text(colour='#FFFFCC',face="bold",angle=90),
      axis.title.x=element_text(colour='#FFFFCC',face="bold"),
      plot.title=element_text(size=rel(2),colour='#FFFFCC',face="bold",hjust = 0.5),
      plot.background=element_rect(fill="transparent",color=NA),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.background=element_blank(),
      legend.text=element_text(colour='#FFFFCC',size=12),
      legend.title=element_text(colour='#FFFFCC',size=12),
      strip.background=element_blank(),
      strip.text=element_text(size=12,colour='#FFFFCC',face="bold")
    )
}

# Figures ----
# Figs 20.2 PSS size ---- 
# both assessments - PSS Size data
#coast wide sleeper length data from sleeper shark length data summary.xlsx
#this needs to be updated the hard way each year, IF new data are available
allSP<-read.csv(paste(datadir,"/Sleeper_lengths_coast",AYR,".csv",sep=""), header=T)
plus<-400 #sets uppper plus group,
minus<-50 #sets lower "minus" group

allSP$binL<-floor(allSP$TLcm/10)*10 #rounds everything down to the next 10 (i.e. 50,60...)
allSP$binL[allSP$binL>plus]<-plus #turns everything over 400 to 400, 400 is the plus group
allSP$binL[allSP$binL<minus]<-minus
SP2<-allSP[rep(1:nrow(allSP), allSP[["Frequency"]]), ]#expands the frequencies out

SP2$FMP<-factor(SP2$FMP,levels=c("BS","AI","GOA","BC","WC"))
slregion<-ggplot(SP2,aes(x=factor(Year),y=TLcm,color=FMP))+
  geom_boxplot()+
  geom_jitter()+
  facet_grid(FMP~.)+
  labs(x="Year",y="Total Length (cm)")+
  theme_doc()+theme(legend.position="none",
                    axis.line.x = element_line(),
                    axis.line.y = element_line(),
                    axis.title = element_text(size=20),
                    strip.background = element_rect(fill="grey",color=NA))
ggsave(path=figdir,"GOABSAI_shark20_2PSSlength.png",plot=slregion,dpi=600,width = 11, height = 7)

# Figs 20.3 Total Catch by species/complex ----
CASdat<-read.csv(paste(getwd(),"/Data/Cleaned/",AYR,"/CAS_GFTBF_Sharks",AYR,".csv",sep=""),header=T)
Cd<-CASdat[,c("Year","FMP.Area","Species","Catch..mt.")]

olddat<-read.csv(paste(getwd(),"/Data/Static/pre2003_shark_cleaned.csv",sep=""),header=T)
od<-olddat[,c("Year","FMP.Area","Species","Catch..mt.")]

catchdat<-rbind(od,Cd)
cd2<-ddply(catchdat,c("Year","FMP.Area","Species"),summarize,Catch=sum(Catch..mt.))

sp_complex_catch<-function(DATA,DOC,FMP){
  if(DOC==T){
    fun_theme<-theme_doc
  } else {
    fun_theme<-theme_pres
  }
  if(FMP=="GOA"){
    DATA$Species<-factor(DATA$Species,levels = c("Spiny Dogfish",
                                         "Pacific Sleeper Shark",
                                         "Salmon Shark",
                                         "Other Sharks"))
  } else {
    DATA$Species<-factor(DATA$Species,levels = c("Pacific Sleeper Shark",
                                         "Salmon Shark",
                                         "Other Sharks",
                                         "Spiny Dogfish"))
  }
  ggplot(DATA[DATA$FMP.Area==FMP,], aes(x=Year, y=Catch,fill=Species,group=Species)) + 
    geom_bar(data=DATA[DATA$Species=="Spiny Dogfish"&DATA$FMP.Area==FMP,],
             stat="identity",aes(x=Year,y=Catch),color="orange",fill="yellow",size=1)+
    geom_bar(data=DATA[DATA$Species=="Pacific Sleeper Shark" &DATA$FMP.Area==FMP,],
             stat="identity",aes(x=Year,y=Catch),color="dark green",fill="light green",size=1)+
    geom_bar(data=DATA[DATA$Species=="Salmon Shark"&DATA$FMP.Area==FMP,],
             stat="identity",aes(x=Year,y=Catch),color="dark blue",fill="light blue",size=1)+
    geom_bar(data=DATA[DATA$Species=="Other Sharks"&DATA$FMP.Area==FMP,],
             stat="identity",aes(x=Year,y=Catch),color="magenta",fill="pink",size=1)+
    facet_wrap(~Species,ncol=1)+
    coord_cartesian(ylim=c(0,round(max(DATA[,c("Catch")]),digits=-2)))+
    scale_y_continuous(expand=c(0,0))+
    labs(y="",x="")+
    fun_theme()
}
GOAspecdoc<-sp_complex_catch(cd2,FMP="GOA",DOC=T)
GOAspecpres<-sp_complex_catch(cd2,FMP="GOA",DOC=F)
BSAIspecdoc<-sp_complex_catch(cd2,FMP="BSAI",DOC=T)
BSAIspecpres<-sp_complex_catch(cd2,FMP="BSAI",DOC=F)

#all species together
tot_complex_catch<-function(DATA,FMP,DOC){
  if(DOC==T){
    fun_theme<-theme_doc
  } else {
    fun_theme<-theme_pres
  }
  fun_max<-round(max(ddply(DATA,"Year",summarize,yrtot=sum(Catch))[2]),digits=-2)
  if(FMP=="GOA"){
    DATA$Species<-factor(DATA$Species,levels = c("Spiny Dogfish",
                                                 "Pacific Sleeper Shark",
                                                 "Salmon Shark",
                                                 "Other Sharks"))
  } else {
    DATA$Species<-factor(DATA$Species,levels = c("Pacific Sleeper Shark",
                                                 "Salmon Shark",
                                                 "Other Sharks",
                                                 "Spiny Dogfish"))
  }
  ggplot(DATA[DATA$FMP.Area==FMP,], aes(x=Year, y=Catch,fill=Species,group=Species,color=Species)) + 
    geom_bar(stat="identity",size=1,show.legend=F)+
    scale_fill_manual(values=c("Pacific Sleeper Shark"="light green",
                               "Spiny Dogfish"="yellow",
                               "Salmon Shark"="light blue",
                               "Other Sharks"="pink"))+
    scale_color_manual(values=c("Pacific Sleeper Shark"="dark green",
                                "Spiny Dogfish"="orange",
                                "Salmon Shark"="dark blue",
                                "Other Sharks"="magenta"))+
    guides(colour="none",fill="none")+
    geom_bar(stat="identity")+
    coord_cartesian(ylim=c(0,fun_max))+
    scale_y_continuous(expand=c(0,0))+
    labs(y="",x="")+
    fun_theme()
}
GOAtotdoc<-tot_complex_catch(cd2,FMP="GOA",DOC=T)
GOAtotpres<-tot_complex_catch(cd2,FMP="GOA",DOC=F)
BSAItotdoc<-tot_complex_catch(cd2,FMP="BSAI",DOC=T)
BSAItotpres<-tot_complex_catch(cd2,FMP="BSAI",DOC=F)

catchfig<-function(FMP,TYPE){
  figx1<-paste(FMP,"spec",TYPE,sep="")
  figx2<-paste(FMP,"tot",TYPE,sep="")
  if(TYPE=='doc'){
    textcol<-'black'
  } else {
    textcol<-'#FFFFCC'
  }
  fun_fig<-grid.arrange(arrangeGrob(get(figx1),get(figx2), ncol = 2,
                                    left = textGrob("Catch (t)", rot = 90, vjust = 1.5,gp=gpar(col=textcol, fontsize=20)),
                                    bottom = textGrob("Year", vjust=0,gp=gpar(col=textcol, fontsize=20))))
  ggsave(path = figdir,paste(FMP,"20.3catch",TYPE,".png",sep=""),plot=fun_fig,dpi=600,width = 10, height = 11)
}
catchfig(FMP="GOA",TYPE="doc")
catchfig(FMP="GOA",TYPE="pres")
catchfig(FMP="BSAI",TYPE="doc")
catchfig(FMP="BSAI",TYPE="pres")

# Fig 20.4 Catch by area (new to BSAI doc this year) ----
GOA<-dcast(CASdat[CASdat$FMP.Area=="GOA"|CASdat$FMP.Area=="INSD",],
           Year+Species~NMFS.Area,fun.aggregate =  sum, value.var="Catch..mt.")
GOA3<-melt(GOA,id.vars = c("Year","Species"))
colnames(GOA3)[3:4]<-c("NMFS.Area","Catch")
GOA3$NMFS.Area<-as.factor(GOA3$NMFS.Area)
GOA3$NMFS.Area<-factor(GOA3$NMFS.Area,c("610","620","630","640","650","649","659"))

#need to create a fake factor to order the areas the way I want them for the figures
AOrder<-matrix(NA,ncol=1,nrow=nrow(GOA3))
unqkey<-unique(GOA3$NMFS.Area)
for (i in 1:length(AOrder)){
  ifelse(GOA3$NMFS.Area[i]==610,AOrder[i]<-c("A"),
         ifelse(GOA3$NMFS.Area[i]==620,AOrder[i]<-c("B"),
                ifelse(GOA3$NMFS.Area[i]==630,AOrder[i]<-c("C"),
                       ifelse(GOA3$NMFS.Area[i]==640,AOrder[i]<-c("D"),
                              ifelse(GOA3$NMFS.Area[i]==650,AOrder[i]<-c("E"),
                                     ifelse(GOA3$NMFS.Area[i]==649,AOrder[i]<-c("F"),AOrder[i]<-c("G")))))))
}
GOA4<-cbind(GOA3,AOrder)
GOA5<-GOA4[order(GOA4$AOrder,GOA4$Year,decreasing=F),]
GOA5$AOrder<-factor(GOA5$AOrder,levels=c("A","B","C","D","E","F","G"))

rup <- function(x,to=10){ #rounds up to the nearest 100 to make graphics look nice
  to*(x%/%to + as.logical(x%%to))
}

catch_area<-function (DATA,TYPE){
  if(TYPE=="doc"){
    fun_theme<-theme_doc
    fun_color<-"black"
  } else {
    fun_theme<-theme_pres
    fun_color<-"cornsilk"
  }
  print(ggplot(DATA, aes(x=Year,y=Catch,fill=fct_rev(AOrder))) +
          geom_bar(stat="identity",show.legend=F)+
          geom_bar(stat="identity",color="black",show.legend=F)+
          scale_fill_manual(values=c("G"="#4575b4","F"="#91bfdb","E"="#fdf5e6","D"="#ffffbf","C"="#fee090","B"="#fc8d59","A"="#d73027"),
                            breaks=c("A","B","C","D","E","F","G"),labels=c("659","649","650","640","630","620","610"),name="NMFS\nArea")+
          #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
          #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
          scale_x_continuous(breaks=seq(2003,max(DATA$Year),4))+
          coord_cartesian(ylim=c(0,rup(max(ddply(DATA,"Year",summarize,yrtot=sum(Catch))[2]))))+
          scale_y_continuous(expand=c(0,0))+
          labs(y="",x="",title=DATA$Species)+
          fun_theme()+theme(
            plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            axis.line.x = element_line(colour=fun_color),
            axis.line.y = element_line(colour=fun_color),
            panel.grid.major=element_blank(),
            plot.title=element_text(size=20,colour=fun_color,hjust = 0.5)))
}

unq_key<-unique(GOA5$Species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-GOA5[GOA5$Species==unq_key[i,1],]
  d<-catch_area(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

#make legend plot, then grid arrange them all together, see below code for example
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

carea_legdoc<-ggplot(GOA5[GOA5$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=fct_rev(AOrder))) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("G"="#4575b4","F"="#91bfdb","E"="#fdf5e6","D"="#ffffbf","C"="#fee090","B"="#fc8d59","A"="#d73027"),
                    breaks=c("G","F","E","D","C","B","A"),labels=c("659","649","650","640","630","620","610"),name="NMFS\nArea")+
  #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
  #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
  scale_x_continuous(breaks=seq(2003,max(GOA5$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(GOA5[GOA5$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=GOA5[GOA5$Species=="Spiny Dogfish",]$Species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(carea_legdoc)), "guide-box") 
grid.draw(legend_doc)

carea_legpres<-ggplot(GOA5[GOA5$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=fct_rev(AOrder))) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("G"="#4575b4","F"="#91bfdb","E"="#fdf5e6","D"="#ffffbf","C"="#fee090","B"="#fc8d59","A"="#d73027"),
                    breaks=c("G","F","E","D","C","B","A"),labels=c("659","649","650","640","630","620","610"),name="NMFS\nArea")+
  #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
  #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
  scale_x_continuous(breaks=seq(2003,max(GOA5$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(GOA5[GOA5$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="Catch (t)",title=GOA5[GOA5$Species=="Spiny Dogfish",]$Species)+
  theme_pres()
legend_pres = gtable_filter(ggplot_gtable(ggplot_build(carea_legpres)), "guide-box") 
grid.draw(legend_pres)

carea_doc<-grid.arrange(arrangeGrob(SDdoc, OSdoc,SSdoc,PSSdoc,nrow = 2,
                                    left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                    bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20))), 
                        legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                        nrow=1)
ggsave(path = figdir, "GOA_20_4catch_area.png",plot=carea_doc,dpi=600,width = 11, height = 7)

carea_pres<-grid.arrange(arrangeGrob(SDpres, OSpres,SSpres,PSSpres,nrow = 2,
                                     left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="cornsilk", fontsize=20)),
                                     bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="cornsilk", fontsize=20))), 
                         legend_pres, widths=unit.c(unit(1, "npc") - legend_pres$width, legend_pres$width), 
                         nrow=1)
ggsave(path = figdir, "GOA_20_4catch_area_pres.png",plot=carea_pres,dpi=600,width = 11, height = 7)  

#BSAI
BSAI<-dcast(CASdat[CASdat$FMP.Area=="BSAI",],
           Year+Species~FMP.Subarea,fun.aggregate =  sum, 
           value.var="Catch..mt.")
BSAI3<-melt(BSAI,id.vars = c("Year","Species"))
colnames(BSAI3)[3:4]<-c("FMP.Subarea","Catch")
BSAI3$FMP.Subarea<-as.factor(BSAI3$FMP.Subarea)

#need to create a fake factor to match color schemes in function
AOrder<-matrix(NA,ncol=1,nrow=nrow(BSAI3))
unqkey<-unique(BSAI3$FMP.Subarea)
for (i in 1:length(AOrder)){
  ifelse(BSAI3$FMP.Subarea[i]=="AI",AOrder[i]<-c("A"),AOrder[i]<-c("G"))
}
BSAI4<-cbind(BSAI3,AOrder)
BSAI5<-BSAI4[order(BSAI4$AOrder,BSAI4$Year,decreasing=F),]
BSAI5$AOrder<-factor(BSAI5$AOrder,levels=c("A","G"))

unq_key<-unique(BSAI5$Species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-BSAI5[BSAI5$Species==unq_key[i,1],]
  d<-catch_area(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

#make legend plot, then grid arrange them all together
carea_legdoc<-ggplot(BSAI5[BSAI5$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=fct_rev(AOrder))) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("G"="#4575b4","F"="#91bfdb","E"="#fdf5e6","D"="#ffffbf","C"="#fee090","B"="#fc8d59","A"="#d73027"),
                    breaks=c("G","F","E","D","C","B","A"),labels=c("BS","649","650","640","630","620","AI"),name="FMP\nSubarea")+
  #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
  #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
  scale_x_continuous(breaks=seq(2003,max(BSAI5$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(BSAI5[BSAI5$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=BSAI5[BSAI5$Species=="Spiny Dogfish",]$Species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(carea_legdoc)), "guide-box") 
grid.draw(legend_doc)

carea_legpres<-ggplot(GOA5[GOA5$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=fct_rev(AOrder))) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("G"="#4575b4","F"="#91bfdb","E"="#fdf5e6","D"="#ffffbf","C"="#fee090","B"="#fc8d59","A"="#d73027"),
                    breaks=c("G","F","E","D","C","B","A"),labels=c("BS","649","650","640","630","620","AI"),name="FMP\nSubarea")+
  #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
  #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
  scale_x_continuous(breaks=seq(2003,max(BSAI5$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(GOA5[GOA5$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="Catch (t)",title=GOA5[GOA5$Species=="Spiny Dogfish",]$Species)+
  theme_pres()
legend_pres = gtable_filter(ggplot_gtable(ggplot_build(carea_legpres)), "guide-box") 
grid.draw(legend_pres)

carea_doc<-grid.arrange(arrangeGrob(OSdoc,SSdoc,PSSdoc,SDdoc,nrow = 2,
                                    left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                    bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20))), 
                        legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                        nrow=1)
ggsave(path = figdir, "BSAI_20_4catch_area.png",plot=carea_doc,dpi=600,width = 11, height = 7)

carea_pres<-grid.arrange(arrangeGrob(OSpres,SSpres,PSSpres,SDpres,nrow = 2,
                                     left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="cornsilk", fontsize=20)),
                                     bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="cornsilk", fontsize=20))), 
                         legend_pres, widths=unit.c(unit(1, "npc") - legend_pres$width, legend_pres$width), 
                         nrow=1)
ggsave(path = figdir, "BSAI_20_4catch_area_pres.png",plot=carea_pres,dpi=600,width = 11, height = 7)  

# Fig 20.6 GOA catch by fishery ----
GOA<-dcast(CASdat[CASdat$FMP.Area=="GOA",],
           Year+Species~Trip.Target.Name,fun.aggregate =  sum, value.var="Catch..mt.")
GOA3<-melt(GOA,id.vars = c("Year","Species"))
colnames(GOA3)[3:4]<-c("Target","Catch")

catch_fishery<-function (DATA,TYPE){
  if(TYPE=="doc"){
    fun_theme<-theme_doc
    fun_color<-"black"
  } else {
    fun_theme<-theme_pres
    fun_color<-"cornsilk"
  }
  print(ggplot(DATA, aes(x=Year,y=Catch,fill=Target)) +
          geom_bar(stat="identity",show.legend=F)+
          geom_bar(stat="identity",color="black",show.legend=F)+
          scale_fill_manual(values=c("Atka Mackerel"="#4575b4",
                                     "Flatfish"="#91bfdb",
                                     "Halibut"="#fdf5e6",
                                     "Other"="#ffffbf",
                                     "Pacific Cod"="#fee090",
                                     "Pollock"="#fc8d59",
                                     "Rockfish"="#d73027",
                                     "Sablefish"="#0033FF"),
                            breaks=c("Atka Mackerel","Flatfish","Halibut",
                                     "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                            labels=c("Atka Mackerel","Flatfish","Halibut",
                                     "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                            name="Target\nFishery")+
          scale_x_continuous(breaks=seq(2003,max(DATA$Year),4))+
          coord_cartesian(ylim=c(0,rup(max(ddply(DATA,"Year",summarize,yrtot=sum(Catch))[2]))))+
          scale_y_continuous(expand=c(0,0))+
          labs(y="",x="",title=DATA$Species)+
          fun_theme()+theme(
            plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            axis.line.x = element_line(colour=fun_color),
            axis.line.y = element_line(colour=fun_color),
            panel.grid.major=element_blank(),
            plot.title=element_text(size=20,colour=fun_color,hjust = 0.5)))
}

unq_key<-unique(GOA3$Species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-GOA3[GOA3$Species==unq_key[i,1],]
  d<-catch_fishery(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

cfishery_legdoc<-ggplot(GOA3[GOA3$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=Target)) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("Atka Mackerel"="#4575b4",
                             "Flatfish"="#91bfdb",
                             "Halibut"="#fdf5e6",
                             "Other"="#ffffbf",
                             "Pacific Cod"="#fee090",
                             "Pollock"="#fc8d59",
                             "Rockfish"="#d73027",
                             "Sablefish"="#0033FF"),
                    breaks=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    labels=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    name="Target\nFishery")+
  scale_x_continuous(breaks=seq(2003,max(GOA3$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(GOA3[GOA3$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=GOA3[GOA3$Species=="Spiny Dogfish",]$Species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(cfishery_legdoc)), "guide-box") 
grid.draw(legend_doc)

cfishery_legpres<-ggplot(GOA3[GOA3$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=Target)) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("Atka Mackerel"="#4575b4",
                             "Flatfish"="#91bfdb",
                             "Halibut"="#fdf5e6",
                             "Other"="#ffffbf",
                             "Pacific Cod"="#fee090",
                             "Pollock"="#fc8d59",
                             "Rockfish"="#d73027",
                             "Sablefish"="#0033FF"),
                    breaks=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    labels=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    name="Target\nFishery")+
  scale_x_continuous(breaks=seq(2003,max(GOA3$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(GOA3[GOA3$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="Catch (t)",title=GOA3[GOA3$Species=="Spiny Dogfish",]$Species)+
  theme_pres()
legend_pres = gtable_filter(ggplot_gtable(ggplot_build(cfishery_legpres)), "guide-box") 
grid.draw(legend_pres)

cfishery_doc<-grid.arrange(arrangeGrob(SDdoc, OSdoc,SSdoc,PSSdoc,nrow = 2,
                                    left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                    bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20))), 
                        legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                        nrow=1)
ggsave(path = figdir, "GOA_20_6catch_fishery_doc.png",plot=cfishery_doc,dpi=600,width = 11, height = 7)

cfishery_pres<-grid.arrange(arrangeGrob(SDpres, OSpres,SSpres,PSSpres,nrow = 2,
                                     left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="cornsilk", fontsize=20)),
                                     bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="cornsilk", fontsize=20))), 
                         legend_pres, widths=unit.c(unit(1, "npc") - legend_pres$width, legend_pres$width), 
                         nrow=1)
ggsave(path = figdir, "GOA_20_6catch_fishery_pres.png",plot=cfishery_pres,dpi=600,width = 11, height = 7)  

# Fig 20.6 BSAI catch by fishery ----
BSAI<-dcast(CASdat[CASdat$FMP.Area=="BSAI",],
           Year+Species~Trip.Target.Name,fun.aggregate =  sum, value.var="Catch..mt.")
BSAI3<-melt(BSAI,id.vars = c("Year","Species"))
colnames(BSAI3)[3:4]<-c("Target","Catch")

unq_key<-unique(BSAI3$Species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-BSAI3[BSAI3$Species==unq_key[i,1],]
  d<-catch_fishery(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

cfishery_legdoc<-ggplot(BSAI3[BSAI3$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=Target)) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("Atka Mackerel"="#4575b4",
                             "Flatfish"="#91bfdb",
                             "Halibut"="#fdf5e6",
                             "Other"="#ffffbf",
                             "Pacific Cod"="#fee090",
                             "Pollock"="#fc8d59",
                             "Rockfish"="#d73027",
                             "Sablefish"="#0033FF"),
                    breaks=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    labels=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    name="Target\nFishery")+
  scale_x_continuous(breaks=seq(2003,max(BSAI3$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(GOA3[GOA3$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=BSAI3[BSAI3$Species=="Spiny Dogfish",]$Species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(cfishery_legdoc)), "guide-box") 
grid.draw(legend_doc)

cfishery_legpres<-ggplot(BSAI3[BSAI3$Species=="Spiny Dogfish",], aes(x=Year,y=Catch,fill=Target)) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("Atka Mackerel"="#4575b4",
                             "Flatfish"="#91bfdb",
                             "Halibut"="#fdf5e6",
                             "Other"="#ffffbf",
                             "Pacific Cod"="#fee090",
                             "Pollock"="#fc8d59",
                             "Rockfish"="#d73027",
                             "Sablefish"="#0033FF"),
                    breaks=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    labels=c("Atka Mackerel","Flatfish","Halibut",
                             "Other","Pacific Cod","Pollock","Rockfish","Sablefish"),
                    name="Target\nFishery")+
  scale_x_continuous(breaks=seq(2003,max(GOA3$Year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(max(ddply(BSAI3[BSAI3$Species=="Spiny Dogfish",],"Year",summarize,yrtot=sum(Catch))[2]))))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="Catch (t)",title=BSAI3[GOA3$Species=="Spiny Dogfish",]$Species)+
  theme_pres()
legend_pres = gtable_filter(ggplot_gtable(ggplot_build(cfishery_legpres)), "guide-box") 
grid.draw(legend_pres)

cfishery_doc<-grid.arrange(arrangeGrob(OSdoc,SSdoc,PSSdoc,SDdoc,nrow = 2,
                                       left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                       bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20))), 
                           legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                           nrow=1)
ggsave(path = figdir, "BSAI_20_6catch_fishery_doc.png",plot=cfishery_doc,dpi=600,width = 11, height = 7)

cfishery_pres<-grid.arrange(arrangeGrob(OSpres,SSpres,PSSpres,SDpres,nrow = 2,
                                        left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="cornsilk", fontsize=20)),
                                        bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="cornsilk", fontsize=20))), 
                            legend_pres, widths=unit.c(unit(1, "npc") - legend_pres$width, legend_pres$width), 
                            nrow=1)
ggsave(path = figdir, "BSAI_20_6catch_fishery_pres.png",plot=cfishery_pres,dpi=600,width = 11, height = 7)  

# Fig 20.7 GOA Cumulative catches ----
FMP_dat<-read.csv(paste(cleandatdir,"/FMP_cumm_catch",AYR,".csv",sep=""),header=T)
#only show the last 10 years of data
FMP2<-FMP_dat[FMP_dat$Year>max(FMP_dat$Year,na.rm=T)-10,]
FMP2$Year<-as.factor(FMP2$Year)

GOA_doc<-ggplot(FMP2[FMP2$FMP=="GOA",],aes(x=Week,y=Cumm_Catch,fill=Year,
                                  color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",title="GOA Sharks Complex",x="")+
  theme_doc()

spec_dat<-read.csv(paste(cleandatdir,"/Species_cumm_catch",AYR,".csv",sep=""),header=T)
#only show the last 10 years of data
spec2<-spec_dat[spec_dat$Year>max(spec_dat$Year,na.rm=T)-10,]
spec2$Year<-as.factor(spec2$Year)
SD_doc<-ggplot(spec2[spec2$FMP=="GOA"&spec2$Species=="Spiny Dogfish",],
               aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="GOA"&spec2$Species=="Spiny Dogfish",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")
PSS_doc<-ggplot(spec2[spec2$FMP=="GOA"&spec2$Species=="Pacific Sleeper Shark",],
               aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="GOA"&spec2$Species=="Pacific Sleeper Shark",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")
SS_doc<-ggplot(spec2[spec2$FMP=="GOA"&spec2$Species=="Salmon Shark",],
                aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="GOA"&spec2$Species=="Salmon Shark",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")
OS_doc<-ggplot(spec2[spec2$FMP=="GOA"&spec2$Species=="Other Sharks",],
               aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="GOA"&spec2$Species=="Other Sharks",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")

lay<-rbind(c(1,1),c(1,1),c(2,3),c(4,5))
GOA_cum_doc<-grid.arrange(grobs=list(GOA_doc,SD_doc,PSS_doc,SS_doc,OS_doc),nrow=4,layout_matrix = lay,
             left = textGrob("Cummulative Catch", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=20)),
             bottom = textGrob("Week Number", vjust=0,gp=gpar(col="black", fontsize=20)))

# Fig 20.7 BSAI Cumulative catches ----
BSAI_doc<-ggplot(FMP2[FMP2$FMP=="BSAI",],aes(x=Week,y=Cumm_Catch,fill=Year,
                                           color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",title="BSAI Sharks Complex",x="")+
  theme_doc()

SD_doc<-ggplot(spec2[spec2$FMP=="BSAI"&spec2$Species=="Spiny Dogfish",],
               aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="BSAI"&spec2$Species=="Spiny Dogfish",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")
PSS_doc<-ggplot(spec2[spec2$FMP=="BSAI"&spec2$Species=="Pacific Sleeper Shark",],
                aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="BSAI"&spec2$Species=="Pacific Sleeper Shark",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")
SS_doc<-ggplot(spec2[spec2$FMP=="BSAI"&spec2$Species=="Salmon Shark",],
               aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="BSAI"&spec2$Species=="Salmon Shark",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")
OS_doc<-ggplot(spec2[spec2$FMP=="BSAI"&spec2$Species=="Other Sharks",],
               aes(x=Week,y=Cumm_Catch,fill=Year,color=Year))+
  geom_point(aes(size=Year,alpha=Year))+
  geom_line(aes(alpha=Year))+
  labs(y="",
       title=spec2[spec2$FMP=="BSAI"&spec2$Species=="Other Sharks",]$Species,
       x="")+
  theme_doc()+theme(legend.position = "none")

lay<-rbind(c(1,1),c(1,1),c(2,3),c(4,5))
BSAI_cum_doc<-grid.arrange(grobs=list(BSAI_doc,PSS_doc,SS_doc,OS_doc,SD_doc),nrow=4,layout_matrix = lay,
                          left = textGrob("Proportion Total Catch", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=20)),
                          bottom = textGrob("Week Number", vjust=0,gp=gpar(col="black", fontsize=20)))




# Fig 20.7 & 20.8 GOA SD length ----
l_freqs <- read.csv(paste(cleandatdir,"/SD_Lfreq", AYR, ".csv",sep=""),header=T)

# Get Comps the hard way, Jane has nice piping code, but I can't figure it out
Ntots<-ddply(l_freqs,c("FMP","Source","Sex","Year"),summarize,ntot=sum(Frequency))
L2<-merge(l_freqs,Ntots)
Ltots<-ddply(L2,c("FMP","Source","Sex","Year","binL"),summarize,
             n=sum(Frequency),Prop=n/mean(ntot),N=paste0("N = ",n))

# Comp test: should all be 1!
Ltots %>% group_by(Source, Year) %>% summarize(tst = sum(Prop)) %>% pull(tst) 

#summary stats
SD_L_dat<-read.csv(paste(cleandatdir,"/SD_L_dat2020.csv",sep=""),header=T)

#there is one erroneous male dogfish that can't be real, take it out
SD_L_dat<-SD_L_dat[SD_L_dat$Length<130,]

Ldat_means<-ddply(SD_L_dat,c("Source","Year","FMP","Sex"),summarize,
                  nsamp=length(Length),
                  meanL=mean(Length),
                  medianL=median(Length),
                  stdev=sd(Length),
                  UL=1.96*sd(Length)+meanL,
                  LL=meanL-1.96*sd(Length))

#Figures
fdata.cor <- ddply(Ltots[Ltots$FMP=="GOA"&Ltots$Sex=="F",], c("Source","Year"), 
                   summarize, n=paste("n =", sum(n)))
mdata.cor <- ddply(Ltots[Ltots$FMP=="GOA"&Ltots$Sex=="M",], c("Source","Year"), 
                   summarize, n=paste("n =", sum(n)))

#females
SDF_length_doc<-ggplot(Ltots[Ltots$FMP=="GOA"&Ltots$Sex=="F",], 
                       aes(x=binL, y=Prop,group=Source)) + 
  geom_bar(position="stack",stat="identity",fill="yellow",colour="orange",size=1)+
  facet_grid(Year~Source)+
  labs(title="",x="PCL bin (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  geom_text(data=fdata.cor, aes(x=85, y=0.25, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_doc()+theme(axis.text.y=element_text(size=8),
                    strip.text.y = element_text(size=10),
                    strip.text.x = element_text(size=10),
                    plot.margin=margin(t = -10, r = 0, b = 0, l = 10, unit = "pt"))

SDF_all<-ggplot(SD_L_dat[SD_L_dat$FMP=="GOA"&SD_L_dat$Sex=="F"&SD_L_dat$Year>2009,], 
       aes(x=Length,group=Source,fill=Source)) + 
  geom_boxplot()+
  labs(title="",x="Length (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  #geom_text(data=fdata.cor, aes(x=85, y=0.25, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_doc()+theme(axis.text.y=element_blank(),
                    #legend.position = "none",
                    #strip.text.y = element_text(size=10),
                    #strip.text.x = element_blank(),
                    plot.margin=margin(t = 0, r = 10, b = 0, l = 30, unit = "pt"))

lay<-rbind(c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(1,1,1,1),c(2,2,2,2))
SDF_lengths_doc<-grid.arrange(grobs=list(SDF_length_doc,SDF_all),nrow=6,layout_matrix = lay,
                              top = textGrob("Female Spiny Dogfish",  vjust = 1,gp=gpar(col="black", fontsize=20)))

ggsave(path=figdir,"GOA_SDF20_7_doc.png",plot=SDF_lengths_doc,dpi=600,width=12,height=10)


SDF_length_pres<-ggplot(Ltots[Ltots$FMP=="GOA"&Ltots$Sex=="F",], 
                       aes(x=binL, y=Prop,group=Source)) + 
  geom_bar(position="stack",stat="identity",fill="yellow",colour="orange",size=1)+
  facet_grid(Year~Source)+
  labs(title="",x="PCL bin (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  geom_text(data=fdata.cor, aes(x=85, y=0.25, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_pres()+theme(axis.text.y=element_text(size=8),
                    strip.text.y = element_text(size=10),
                    strip.text.x = element_text(size=10),
                    plot.margin=margin(t = -10, r = 0, b = 0, l = 10, unit = "pt"))

SDF_all_pres<-ggplot(SD_L_dat[SD_L_dat$FMP=="GOA"&SD_L_dat$Sex=="F"&SD_L_dat$Year>2009,], 
                aes(x=Length,group=Source,fill=Source)) + 
  geom_boxplot()+
  labs(title="",x="Length (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  #geom_text(data=fdata.cor, aes(x=85, y=0.25, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_pres()+theme(axis.text.y=element_blank(),
                    #legend.position = "none",
                    #strip.text.y = element_text(size=10),
                    #strip.text.x = element_blank(),
                    plot.margin=margin(t = 0, r = 10, b = 0, l = 30, unit = "pt"))

SDF_lengths_pres<-grid.arrange(grobs=list(SDF_length_pres,SDF_all_pres),nrow=6,layout_matrix = lay,
                              top = textGrob("Female Spiny Dogfish",  vjust = 1,gp=gpar(col="black", fontsize=20)))

ggsave(path=figdir,"GOA_SDF20_7_pres.png",plot=SDF_lengths_pres,dpi=600,width=12,height=10)

#males
SDM_length_doc<-ggplot(Ltots[Ltots$FMP=="GOA"&Ltots$Sex=="M",], 
                       aes(x=binL, y=Prop,group=Source)) + 
  geom_bar(position="stack",stat="identity",fill="yellow",colour="orange",size=1)+
  facet_grid(Year~Source)+
  labs(title="",x="PCL bin (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  geom_text(data=mdata.cor, aes(x=85, y=0.25, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_doc()+theme(axis.text.y=element_text(size=8),
                    strip.text.y = element_text(size=10),
                    strip.text.x = element_text(size=10),
                    plot.margin=margin(t = -10, r = 0, b = 0, l = 10, unit = "pt"))

SDM_all<-ggplot(SD_L_dat[SD_L_dat$FMP=="GOA"&SD_L_dat$Sex=="M"&SD_L_dat$Year>2009,], 
                aes(x=Length,group=Source,fill=Source)) + 
  geom_boxplot()+
  labs(title="",x="Length (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  theme_doc()+theme(axis.text.y=element_blank(),
                    plot.margin=margin(t = 0, r = 10, b = 0, l = 30, unit = "pt"))

SDM_lengths_doc<-grid.arrange(grobs=list(SDM_length_doc,SDM_all),nrow=6,layout_matrix = lay,
                              top = textGrob("Male Spiny Dogfish",  vjust = 1,gp=gpar(col="black", fontsize=20)))

ggsave(path=figdir,"GOA_SDM20_7_doc.png",plot=SDM_lengths_doc,dpi=600,width=12,height=10)


SDM_length_pres<-ggplot(Ltots[Ltots$FMP=="GOA"&Ltots$Sex=="M",], 
                        aes(x=binL, y=Prop,group=Source)) + 
  geom_bar(position="stack",stat="identity",fill="yellow",colour="orange",size=1)+
  facet_grid(Year~Source)+
  labs(title="",x="PCL bin (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  geom_text(data=mdata.cor, aes(x=85, y=0.25, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_pres()+theme(axis.text.y=element_text(size=8),
                     strip.text.y = element_text(size=10),
                     strip.text.x = element_text(size=10),
                     plot.margin=margin(t = -10, r = 0, b = 0, l = 10, unit = "pt"))

SDM_all_pres<-ggplot(SD_L_dat[SD_L_dat$FMP=="GOA"&SD_L_dat$Sex=="M"&SD_L_dat$Year>2009,], 
                     aes(x=Length,group=Source,fill=Source)) + 
  geom_boxplot()+
  labs(title="",x="Length (cm)")+
  scale_y_continuous(breaks=c(0,0.2))+
  #geom_text(data=fdata.cor, aes(x=85, y=0.25, label=n), size=3,colour="black", inherit.aes=FALSE, parse=FALSE)+
  theme_pres()+theme(axis.text.y=element_blank(),
                    #legend.position = "none",
                    #strip.text.y = element_text(size=10),
                    #strip.text.x = element_blank(),
                    plot.margin=margin(t = 0, r = 10, b = 0, l = 30, unit = "pt"))

SDM_lengths_pres<-grid.arrange(grobs=list(SDM_length_pres,SDM_all_pres),nrow=6,layout_matrix = lay,
                               top = textGrob("Male Spiny Dogfish",  vjust = 1,gp=gpar(col="cornsilk", fontsize=20)))

ggsave(path=figdir,"GOA_SDM20_7_pres.png",plot=SDM_lengths_pres,dpi=600,width=12,height=10)

# Fig 20.9 GOA regional dogfish length ----
Idat<-Ltots[Ltots$Source=="IPHCLL",]
Idat$FMP<-factor(Idat$FMP,levels=c("BSAI","GOA","CAN","WC"))
Idat2<-SD_L_dat[!is.na(SD_L_dat$FMP)&SD_L_dat$Source=="IPHCLL",]
Idat2$FMP<-factor(Idat2$FMP,levels=c("BSAI","GOA","CAN","WC"))

f2data.cor <- ddply(Idat[Idat$Sex=="F",], c("FMP","Year"), summarize, n=paste("n =", sum(n)))
m2data.cor <- ddply(Idat[Idat$Sex=="M",], c("FMP","Year"), summarize, n=paste("n =", sum(n)))

#doc version
IPHCF_doc<-ggplot(Idat[Idat$Sex=="F",], aes(x=binL, y=Prop,group=FMP,color=FMP,fill=FMP)) + 
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="BSAI",],stat="identity",aes(x=binL, y=Prop),color="cadetblue1",fill="cadetblue4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="GOA",],stat="identity",aes(x=binL, y=Prop),color="darkseagreen1",fill="darkseagreen4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="CAN",],stat="identity",aes(x=binL, y=Prop),color="burlywood1",fill="burlywood4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="WC",],stat="identity",aes(x=binL, y=Prop),color="mediumorchid1",fill="mediumorchid4",size=1)+
  facet_grid(Year~FMP)+
  labs(title="Female",x="",y="Proportion")+
  scale_y_continuous(breaks=c(0.25,0.5,0.75,1),expand=c(0,0))+
  #scale_x_discrete(breaks=c())
  geom_text(data=f2data.cor, aes(x=80, y=0.40, label=n), size=5,colour="#666666", inherit.aes=FALSE, parse=FALSE)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text=element_text(size=rel(1),colour='black'),
        axis.ticks=element_line(colour='black'),
        axis.title.y=element_text(colour='black',angle=90,margin=margin(0,15,0,0),size=rel(1.5),vjust=-2),
        axis.title.x=element_text(colour='black',margin=margin(10,0,0,0),size=rel(1.2)),
        plot.title=element_text(size=rel(2),colour='black',margin=margin(0,0,0,0)),
        plot.background=element_blank(),
        plot.margin=margin(t = -1, r = -5, b = -40, l = 0, unit = "pt"),
        panel.grid=element_line("light grey"),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.85),
        legend.background=element_blank(),
        legend.text=element_text(colour='black',size=12),
        legend.title=element_text(colour='black',size=12),
        strip.text.x=element_text(size=12,colour='black',face="bold"),
        strip.text.y=element_blank())

IPHCM_doc<-ggplot(Idat[Idat$Sex=="M",], aes(x=binL, y=Prop,group=FMP,color=FMP,fill=FMP)) + 
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="BSAI",],stat="identity",aes(x=binL, y=Prop),color="cadetblue1",fill="cadetblue4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="GOA",],stat="identity",aes(x=binL, y=Prop),color="darkseagreen1",fill="darkseagreen4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="CAN",],stat="identity",aes(x=binL, y=Prop),color="burlywood1",fill="burlywood4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="WC",],stat="identity",aes(x=binL, y=Prop),color="mediumorchid1",fill="mediumorchid4",size=1)+
  facet_grid(Year~FMP)+
  labs(title="Male",x="",y="")+
  scale_y_continuous(breaks=c(0.5,1),expand=c(0,0))+
  geom_text(data=m2data.cor, aes(x=80, y=0.75, label=n), size=5,colour="#666666", inherit.aes=FALSE, parse=FALSE)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text=element_text(size=rel(1),colour='black'),
        axis.ticks=element_line(colour='black'),
        axis.title.y=element_text(colour='black',angle=90,margin=margin(0,15,0,0),size=20),
        axis.title.x=element_text(colour='black',margin=margin(10,0,0,0),size=rel(1.2)),
        plot.title=element_text(size=rel(2),colour='black',margin=margin(0,0,0,0)),
        plot.background=element_blank(),
        plot.margin=margin(t = -1, r = 5, b = -40, l = -20, unit = "pt"),
        panel.grid=element_line("light grey"),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.85),
        legend.background=element_blank(),
        legend.text=element_text(colour='black',size=12),
        legend.title=element_text(colour='black',size=12),
        strip.text=element_text(size=12,colour='black',face="bold"))

IPHCF_all<-ggplot(Idat2[Idat2$Sex=="F",], aes(x=Length,group=FMP,fill=FMP,color=FMP)) + 
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="BSAI",],aes(x=Length),color="cadetblue1",fill="cadetblue4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="GOA",],aes(x=Length),color="darkseagreen1",fill="darkseagreen4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="CAN",],aes(x=Length),color="burlywood1",fill="burlywood4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="WC",],aes(x=Length),color="mediumorchid1",fill="mediumorchid4")+
  facet_grid(FMP~.)+
  labs(title="",x="")+
  theme_doc() + theme(
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major =element_blank(),
    strip.text=element_blank(),
    plot.margin=margin(t = -1, r = 0, b = -40, l = 58, unit = "pt")
  )

IPHCM_all<-ggplot(Idat2[Idat2$Sex=="M",], aes(x=Length,group=FMP,fill=FMP,color=FMP)) + 
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="BSAI",],aes(x=Length),color="cadetblue1",fill="cadetblue4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="GOA",],aes(x=Length),color="darkseagreen1",fill="darkseagreen4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="CAN",],aes(x=Length),color="burlywood1",fill="burlywood4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="WC",],aes(x=Length),color="mediumorchid1",fill="mediumorchid4")+
  facet_grid(FMP~.)+
  labs(title="",x="")+
  theme_doc() + theme(
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major =element_blank(),
    strip.text=element_text(size=10,colour='black',face="bold"),
    plot.margin=margin(t = -1, r = 10, b = -40, l = 35, unit = "pt")
  )
lay<-rbind(c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(3,3),c(4,5),c(4,5),c(4,5),c(6,6))
PCLgrob<-textGrob("PCL bin (cm)", vjust = 1, gp=gpar(col="black", fontsize=15))
Lgrob<-textGrob("Length (cm)", vjust = 1, gp=gpar(col="black", fontsize=15))

IPHC_SD_Length_doc<-grid.arrange(arrangeGrob(IPHCF_doc,
                                             IPHCM_doc,
                                             PCLgrob,
                                             IPHCF_all,
                                             IPHCM_all,
                                             Lgrob,
                                             nrow = 13,
                                             layout_matrix = lay))

ggsave(path=figdir,"GOA20_9_doc.png",plot=IPHC_SD_Length_doc,dpi=600,width = 11,height = 7) 

#presentation version
IPHCF_pres<-ggplot(Idat[Idat$Sex=="F",], aes(x=binL, y=Prop,group=FMP,color=FMP,fill=FMP)) + 
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="BSAI",],stat="identity",aes(x=binL, y=Prop),color="cadetblue1",fill="cadetblue4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="GOA",],stat="identity",aes(x=binL, y=Prop),color="darkseagreen1",fill="darkseagreen4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="CAN",],stat="identity",aes(x=binL, y=Prop),color="burlywood1",fill="burlywood4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="F"&Idat$FMP=="WC",],stat="identity",aes(x=binL, y=Prop),color="mediumorchid1",fill="mediumorchid4",size=1)+
  facet_grid(Year~FMP)+
  labs(title="Female",x="",y="Proportion")+
  scale_y_continuous(breaks=c(0.25,0.5,0.75,1),expand=c(0,0))+
  #scale_x_discrete(breaks=c())
  geom_text(data=f2data.cor, aes(x=80, y=0.40, label=n), size=5,colour="#666666", inherit.aes=FALSE, parse=FALSE)+
  theme(axis.line.x = element_line(color="cornsilk"),
        axis.line.y = element_line(color="cornsilk"),
        axis.text=element_text(size=rel(1),colour='cornsilk'),
        axis.ticks=element_line(colour='cornsilk'),
        axis.title.y=element_text(colour='cornsilk',angle=90,margin=margin(0,15,0,0),size=rel(1.5),vjust=-2),
        axis.title.x=element_text(colour='cornsilk',margin=margin(10,0,0,0),size=rel(1.2)),
        plot.title=element_text(size=rel(2),colour='cornsilk',margin=margin(0,0,0,0)),
        plot.background=element_blank(),
        plot.margin=margin(t = -1, r = -5, b = -40, l = 0, unit = "pt"),
        panel.grid=element_line("light grey"),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.85),
        legend.background=element_blank(),
        legend.text=element_text(colour='cornsilk',size=12),
        legend.title=element_text(colour='cornsilk',size=12),
        strip.text.x=element_text(size=12,colour='cornsilk',face="bold"),
        strip.text.y=element_blank())

IPHCM_pres<-ggplot(Idat[Idat$Sex=="M",], aes(x=binL, y=Prop,group=FMP,color=FMP,fill=FMP)) + 
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="BSAI",],stat="identity",aes(x=binL, y=Prop),color="cadetblue1",fill="cadetblue4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="GOA",],stat="identity",aes(x=binL, y=Prop),color="darkseagreen1",fill="darkseagreen4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="CAN",],stat="identity",aes(x=binL, y=Prop),color="burlywood1",fill="burlywood4",size=1)+
  geom_bar(data=Idat[Idat$Sex=="M"&Idat$FMP=="WC",],stat="identity",aes(x=binL, y=Prop),color="mediumorchid1",fill="mediumorchid4",size=1)+
  facet_grid(Year~FMP)+
  labs(title="Male",x="",y="")+
  scale_y_continuous(breaks=c(0.5,1),expand=c(0,0))+
  geom_text(data=m2data.cor, aes(x=80, y=0.75, label=n), size=5,colour="#666666", inherit.aes=FALSE, parse=FALSE)+
  theme(axis.line.x = element_line(colour="cornsilk"),
        axis.line.y = element_line(colour="cornsilk"),
        axis.text=element_text(size=rel(1),colour='cornsilk'),
        axis.ticks=element_line(colour='cornsilk'),
        axis.title.y=element_text(colour='cornsilk',angle=90,margin=margin(0,15,0,0),size=20),
        axis.title.x=element_text(colour='cornsilk',margin=margin(10,0,0,0),size=rel(1.2)),
        plot.title=element_text(size=rel(2),colour='cornsilk',margin=margin(0,0,0,0)),
        plot.background=element_blank(),
        plot.margin=margin(t = -1, r = 5, b = -40, l = -20, unit = "pt"),
        panel.grid=element_line("light grey"),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.85),
        legend.background=element_blank(),
        legend.text=element_text(colour='cornsilk',size=12),
        legend.title=element_text(colour='cornsilk',size=12),
        strip.text=element_text(size=12,colour='cornsilk',face="bold"))


IPHCF_allpres<-ggplot(Idat2[Idat2$Sex=="F",], aes(x=Length,group=FMP,fill=FMP,color=FMP)) + 
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="BSAI",],aes(x=Length),color="cadetblue1",fill="cadetblue4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="GOA",],aes(x=Length),color="darkseagreen1",fill="darkseagreen4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="CAN",],aes(x=Length),color="burlywood1",fill="burlywood4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="F"&Idat2$FMP=="WC",],aes(x=Length),color="mediumorchid1",fill="mediumorchid4")+
  facet_grid(FMP~.)+
  labs(title="",x="")+
  theme_doc() + theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color="cornsilk"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color="cornsilk"),
    panel.grid.major =element_blank(),
    strip.text=element_blank(),
    plot.margin=margin(t = -1, r = 0, b = -40, l = 58, unit = "pt")
  )

IPHCM_allpres<-ggplot(Idat2[Idat2$Sex=="M",], aes(x=Length,group=FMP,fill=FMP,color=FMP)) + 
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="BSAI",],aes(x=Length),color="cadetblue1",fill="cadetblue4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="GOA",],aes(x=Length),color="darkseagreen1",fill="darkseagreen4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="CAN",],aes(x=Length),color="burlywood1",fill="burlywood4")+
  geom_boxplot(data=Idat2[Idat2$Sex=="M"&Idat2$FMP=="WC",],aes(x=Length),color="mediumorchid1",fill="mediumorchid4")+
  facet_grid(FMP~.)+
  labs(title="",x="")+
  theme_doc() + theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color="cornsilk"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color="cornsilk"),
    panel.grid.major =element_blank(),
    strip.text=element_text(size=10,colour='cornsilk',face="bold"),
    plot.margin=margin(t = -1, r = 10, b = -40, l = 35, unit = "pt")
  )
lay<-rbind(c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(1,2),c(3,3),c(4,5),c(4,5),c(4,5),c(6,6))
PCLgrob<-textGrob("PCL bin (cm)", vjust = 1, gp=gpar(col="cornsilk", fontsize=15))
Lgrob<-textGrob("Length (cm)", vjust = 1, gp=gpar(col="cornsilk", fontsize=15))

IPHC_SD_Length_pres<-grid.arrange(arrangeGrob(IPHCF_pres,
                                             IPHCM_pres,
                                             PCLgrob,
                                             IPHCF_allpres,
                                             IPHCM_allpres,
                                             Lgrob,
                                             nrow = 13,
                                             layout_matrix = lay))

ggsave(path=figdir,"GOA20_9_pres.png",plot=IPHC_SD_Length_pres,dpi=600,width = 11,height = 7) 

# Figs 20.11 and 20.9 Biomass (GOA/BSAI) ----
shark_biomass<-read.csv(paste(getwd(),"/Output/",AYR,"/RACE_Biomass/RACE_Biomass_Sharks.csv",sep=""),header=T)
shark_biomass$FMP<-0
BSAIsurv<-c("AI","EBS_SHELF","EBS_SLOPE")
shark_biomass[shark_biomass$SURVEY%in%BSAIsurv,]$FMP<-"BSAI"
shark_biomass[shark_biomass$SURVEY=="GOA",]$FMP<-"GOA"

#need to calculate CI first
shark_biomass$UL<-shark_biomass$Biomass+sqrt(shark_biomass$Variance)*1.96
shark_biomass$LL<-shark_biomass$Biomass-sqrt(shark_biomass$Variance)*1.96

#get rid of extra groups
keeps<-c("Pacific Sleeper Shark","Spiny Dogfish","Salmon Shark")
shark_biomass<-shark_biomass[shark_biomass$Group %in% keeps,]

#all surveys and species in one plot
bsaibiom_doc<-ggplot(shark_biomass[shark_biomass$FMP=="BSAI",], aes(x=YEAR, y=Biomass,group=Group,fill=Group)) + 
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Spiny Dogfish"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="orange",fill="yellow",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Pacific Sleeper Shark"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark green",fill="light green",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Salmon Shark"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark blue",fill="light blue",size=1)+
  facet_grid(Group~SURVEY,scales="free_y")+
  geom_linerange(aes(ymax=UL,ymin=Biomass),colour="black")+
  #scale_y_log10()+
  labs(y="Biomass (t)",x="Year")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015))+
  theme(
    plot.title=element_text(size=rel(2),colour='black',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_line(color="grey90"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='black'),
    axis.line.y = element_line(colour='black'),
    axis.text=element_text(size=15,colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',angle=90,size=20),
    axis.title.x=element_text(colour='black',size=20),
    legend.background=element_blank(),
    legend.text=element_text(colour='black',size=12),
    legend.title=element_text(colour='black',size=12),
    strip.text=element_text(size=12,colour='black',face="bold")
  )
ggsave(path=figdir,"BSAI_20_9biom_doc.png",plot=bsaibiom_doc,dpi=600,height=7,width=8)

goabiom_doc<-ggplot(shark_biomass[shark_biomass$FMP=="GOA"&shark_biomass$REGULATORY_AREA_NAME=="GOA",],
                    aes(x=YEAR, y=Biomass,group=Group,fill=Group)) + 
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Spiny Dogfish"&shark_biomass$FMP=="GOA")
                              &shark_biomass$REGULATORY_AREA_NAME=="GOA",],
           stat="identity",aes(x=YEAR,y=Biomass),color="orange",fill="yellow",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Pacific Sleeper Shark"&shark_biomass$FMP=="GOA"
                               &shark_biomass$REGULATORY_AREA_NAME=="GOA"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark green",fill="light green",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Salmon Shark"&shark_biomass$FMP=="GOA")
                              &shark_biomass$REGULATORY_AREA_NAME=="GOA",],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark blue",fill="light blue",size=1)+
  facet_grid(Group~SURVEY,scales="free_y")+
  geom_linerange(aes(ymax=UL,ymin=Biomass),colour="black")+
  #scale_y_log10()+
  labs(y="Biomass (t)",x="Year")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    plot.title=element_text(size=rel(2),colour='black',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_line(color="grey90"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='black'),
    axis.line.y = element_line(colour='black'),
    axis.text=element_text(size=rel(1),colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',face="bold",angle=90),
    axis.title.x=element_text(colour='black',face="bold"),
    legend.background=element_blank(),
    legend.text=element_text(colour='black',size=12),
    legend.title=element_text(colour='black',size=12),
    strip.text=element_text(size=12,colour='black',face="bold")
  )
ggsave(path=figdir,"GOA_20_11biom_doc.png",plot=goabiom_doc,dpi=600,height=8,width=8)

bsaibiom_pres<-ggplot(shark_biomass[shark_biomass$FMP=="BSAI",], aes(x=YEAR, y=Biomass,group=Group,fill=Group)) + 
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Spiny Dogfish"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="orange",fill="yellow",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Pacific Sleeper Shark"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark green",fill="light green",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Salmon Shark"&shark_biomass$FMP=="BSAI"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark blue",fill="light blue",size=1)+
  facet_grid(Group~SURVEY,scales="free_y")+
  geom_linerange(aes(ymax=UL,ymin=Biomass),colour="cornsilk")+
  #scale_y_log10()+
  labs(y="Biomass (t)",x="Year")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(breaks=c(1985,1995,2005,2015))+
  theme(
    plot.title=element_text(size=rel(2),colour='cornsilk',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_line(color="grey90"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='cornsilk'),
    axis.line.y = element_line(colour='cornsilk'),
    axis.text=element_text(size=15,colour='cornsilk'),
    axis.ticks=element_line(colour='cornsilk'),
    axis.title.y=element_text(colour='cornsilk',angle=90,size=20),
    axis.title.x=element_text(colour='cornsilk',size=20),
    legend.background=element_blank(),
    legend.text=element_text(colour='cornsilk',size=12),
    legend.title=element_text(colour='cornsilk',size=12),
    strip.text=element_text(size=12,colour='cornsilk',face="bold")
  )
ggsave(path=figdir,"BSAI_20_9biom_pres.png",plot=bsaibiom_pres,dpi=600,height=7,width=8)

goabiom_pres<-ggplot(shark_biomass[shark_biomass$FMP=="GOA"&shark_biomass$REGULATORY_AREA_NAME=="GOA",],
                    aes(x=YEAR, y=Biomass,group=Group,fill=Group)) + 
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Spiny Dogfish"&shark_biomass$FMP=="GOA")
                              &shark_biomass$REGULATORY_AREA_NAME=="GOA",],
           stat="identity",aes(x=YEAR,y=Biomass),color="orange",fill="yellow",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Pacific Sleeper Shark"&shark_biomass$FMP=="GOA"
                               &shark_biomass$REGULATORY_AREA_NAME=="GOA"),],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark green",fill="light green",size=1)+
  geom_bar(data=shark_biomass[(shark_biomass$Group=="Salmon Shark"&shark_biomass$FMP=="GOA")
                              &shark_biomass$REGULATORY_AREA_NAME=="GOA",],
           stat="identity",aes(x=YEAR,y=Biomass),color="dark blue",fill="light blue",size=1)+
  facet_grid(Group~SURVEY,scales="free_y")+
  geom_linerange(aes(ymax=UL,ymin=Biomass),colour="cornsilk")+
  #scale_y_log10()+
  labs(y="Biomass (t)",x="Year")+
  scale_y_continuous(expand=c(0,0))+
  theme(
    plot.title=element_text(size=rel(2),colour='cornsilk',face="bold",hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_line(color="grey90"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='cornsilk'),
    axis.line.y = element_line(colour='cornsilk'),
    axis.text=element_text(size=rel(1),colour='cornsilk'),
    axis.ticks=element_line(colour='cornsilk'),
    axis.title.y=element_text(colour='cornsilk',face="bold",angle=90),
    axis.title.x=element_text(colour='cornsilk',face="bold"),
    legend.background=element_blank(),
    legend.text=element_text(colour='cornsilk',size=12),
    legend.title=element_text(colour='cornsilk',size=12),
    strip.text=element_text(size=12,colour='cornsilk',face="bold")
  )
ggsave(path=figdir,"GOA_20_11biom_pres.png",plot=goabiom_pres,dpi=600,height=8,width=8)

# Fig 20.10 BSAI RPNs ----
IPHC_SDdat<-read.csv(paste(cleandatdir,"/IPHC_RPN_Spiny dogfish_",AYR-1,"_EXTRAP.csv",sep=""), header=T)
IPHC_PSSdat<-read.csv(paste(cleandatdir,"/IPHC_RPN_Sleeper shark_",AYR-1,"_EXTRAP.csv",sep=""), header=T)
IPHC_dat<-rbind(IPHC_SDdat,IPHC_PSSdat)

levels(IPHC_dat$species)[levels(IPHC_dat$species)=="Spiny dogfish"]<-"Spiny Dogfish"
levels(IPHC_dat$species)[levels(IPHC_dat$species)=="Sleeper shark"]<-"Pacific Sleeper Shark"

IPHC2<-ddply(IPHC_dat,c("year","FMP_sub_area","species"),summarize,
             totRPN=sum(strata_rpn),totLL=sum(boot_lci),totUL=sum(boot_uci))
colnames(IPHC2)<-c("Year","FMP","Species","RPN","LL","UL")

BIPHC<-IPHC2[IPHC2$FMP=="BS"|IPHC2$FMP=="AI",]
BIPHC$Species<-factor(BIPHC$Species,levels=c("Pacific Sleeper Shark","Spiny Dogfish"))

BS_IPHC_doc<-ggplot(BIPHC[(BIPHC$FMP=="BS"),], aes(x=Year,y=RPN,group=Species,fill=Species)) + 
  geom_bar(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="BS"),],aes(x=Year, y=RPN),stat="identity",color="dark green",fill="light green",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="BS"),],aes(ymax=UL,ymin=LL),colour="black",width=1)+
  geom_bar(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="BS"),],aes(x=Year, y=RPN),stat="identity",color="orange",fill="yellow",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="BS"),],aes(ymax=UL,ymin=LL),colour="black",width=1)+
  facet_grid(Species~FMP,scale="free_y")+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="")+
  theme_doc()+theme(legend.position="none",
                    axis.line.x=element_line(color="black"),
                    axis.line.y=element_line(color="black"))
AI_IPHC_doc<-ggplot(BIPHC[(BIPHC$FMP=="AI"),], aes(x=Year,y=RPN,group=Species,fill=Species)) + 
  geom_bar(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="AI"),],aes(x=Year, y=RPN),stat="identity",color="dark green",fill="light green",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="AI"),],aes(ymax=UL,ymin=LL),colour="black",width=1)+
  geom_bar(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="AI"),],aes(x=Year, y=RPN),stat="identity",color="orange",fill="yellow",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="AI"),],aes(ymax=UL,ymin=LL),colour="black",width=1)+
  facet_grid(Species~FMP,scale="free_y")+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="")+
  theme_doc()+theme(legend.position="none",
                    axis.line.x=element_line(color="black"),
                    axis.line.y=element_line(color="black"),
                    strip.text.y = element_blank())


IPHC_RPN_doc<-grid.arrange(arrangeGrob(AI_IPHC_doc, BS_IPHC_doc, nrow = 1,
                                       left = textGrob("Relative Population Numbers", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=20)),
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=20))))
ggsave(path=figdir,"BSAI_20_10IPHC_RPN_doc.png",plot=IPHC_RPN_doc,dpi=600,width=15,height=15)

BS_IPHC_pres<-ggplot(BIPHC[(BIPHC$FMP=="BS"),], aes(x=Year,y=RPN,group=Species,fill=Species)) + 
  geom_bar(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="BS"),],aes(x=Year, y=RPN),stat="identity",color="dark green",fill="light green",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="BS"),],aes(ymax=UL,ymin=LL),colour="cornsilk",width=1)+
  geom_bar(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="BS"),],aes(x=Year, y=RPN),stat="identity",color="orange",fill="yellow",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="BS"),],aes(ymax=UL,ymin=LL),colour="cornsilk",width=1)+
  facet_grid(Species~FMP,scale="free_y")+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="")+
  theme_pres()+theme(legend.position="none",
                    axis.line.x=element_line(color="cornsilk"),
                    axis.line.y=element_line(color="cornsilk"))
AI_IPHC_pres<-ggplot(BIPHC[(BIPHC$FMP=="AI"),], aes(x=Year,y=RPN,group=Species,fill=Species)) + 
  geom_bar(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="AI"),],aes(x=Year, y=RPN),stat="identity",color="dark green",fill="light green",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Pacific Sleeper Shark"&BIPHC$FMP=="AI"),],aes(ymax=UL,ymin=LL),colour="cornsilk",width=1)+
  geom_bar(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="AI"),],aes(x=Year, y=RPN),stat="identity",color="orange",fill="yellow",size=1)+
  geom_linerange(data=BIPHC[(BIPHC$Species=="Spiny Dogfish"&BIPHC$FMP=="AI"),],aes(ymax=UL,ymin=LL),colour="cornsilk",width=1)+
  facet_grid(Species~FMP,scale="free_y")+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="")+
  theme_pres()+theme(legend.position="none",
                    axis.line.x=element_line(color="cornsilk"),
                    axis.line.y=element_line(color="cornsilk"),
                    strip.text.y = element_blank())


IPHC_RPN_pres<-grid.arrange(arrangeGrob(AI_IPHC_pres, BS_IPHC_pres, nrow = 1,
                                       left = textGrob("Relative Population Numbers", rot = 90, vjust = 1.5,gp=gpar(col="cornsilk", fontsize=20)),
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="cornsilk", fontsize=20))))
ggsave(path=figdir,"BSAI_20_10IPHC_RPN_pres.png",plot=IPHC_RPN_pres,dpi=600,width=15,height=15)

# Fig 20.13 DF GOA all survey indices ----

#ADFG SEAK LL
#read in data
ADFGSEAKLL<-read.csv(paste(cleandatdir,"/ADFG_SEAK_LL",AYR,".csv",sep=""),header=T)
#Calculate annual CPUE
SEAK<-ddply(ADFGSEAKLL,c("Year"),summarize,Tot_hks=sum(hooks_tot),ineff_hks=sum(hooks_inval),toteffhks=Tot_hks-ineff_hks,
            dogfish=sum(dogf_no),PSS=sum(PSS_no),CPUE_dog=dogfish/toteffhks,CPUE_PSS=PSS/toteffhks)
SEAKm<-melt(SEAK[,c("Year","CPUE_dog","CPUE_PSS")],id.vars=c("Year"))

#Plot
ADFG_SEAK_doc<-ggplot(SEAKm[SEAKm$variable=="CPUE_dog",],aes(x=Year,y=value,color=variable,shape=variable))+
  geom_line(color="orange",size=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  labs(y="CPUE (#/efhks)",x="",title="ADF&G SEAK Longline Survey")+
  #coord_cartesian(x=c(1982,2016))+
  scale_x_continuous(limits=c(1982,AYR))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=0.5),axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2)),legend.position="none") +
  theme(axis.line = element_line(color = 'black'))

#PWS Trawl Bay survey
ADFG_LRGTWL<-read.csv(paste(cleandatdir,"/ADFG_LRGTWL",AYR,".csv",sep=""),header=T)
ADFG_Kshak_doc<-ggplot(ADFG_LRGTWL[ADFG_LRGTWL$year>1997&
                                     ADFG_LRGTWL$species=="Spiny Dogfish"&
                                     ADFG_LRGTWL$proj=="T06",],
                       aes(x=year,y=Kg_CPUE,color=species,shape=species))+
  geom_line(color="orange",size=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  labs(y="CPUE (kg/nmi)",x="",title="ADF&G PWS Trawl Survey")+
  #coord_cartesian(x=c(1982,2016))+
  scale_x_continuous(limits=c(1982,AYR))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust = 0.5),axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2)),legend.position="none") +
  theme(axis.line = element_line(color = 'black'))

#GOA AFSC Trawl Survey
GOASD<-shark_biomass[shark_biomass$REGULATORY_AREA_NAME=="GOA"&shark_biomass$Group=="Spiny Dogfish",]
AFSC_TWLSD_doc<-ggplot(GOASD, aes(x=YEAR, y=Biomass/1000,group=Group,fill=Group)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1)+
  geom_errorbar(aes(ymax=UL/1000,ymin=LL/1000),colour="black",width=1)+
  labs(y="Biomass (1000s t)",x="",title="AFSC Trawl Survey")+
  coord_cartesian(y=c(0,300))+
  scale_x_continuous(limits=c(1982,AYR))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust = 0.5),legend.position="none",axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2))) +
  theme(axis.line = element_line(color = 'black'))

#ADFG PWS LL
PWSLL<-read.csv(paste(getwd(),"/Data/Static/ADFG_PWS_LLSurvey_CPUE.csv",sep=""), header=T)
PWSLL<-PWSLL[PWSLL$Survey=="Prince William Sound Longline Survey",]
mPWS<-melt(PWSLL[,c("Year","pkEvent_ID","Salmon_Shark","Spiny_Dogfish","Sleeper_Shark")],id.vars=c("Year","pkEvent_ID"))
meanPWS<-ddply(mPWS,c("Year","variable"),summarize,avgCPUE=mean(value))
ADFG_PWSSD_doc<-ggplot(meanPWS[meanPWS$variable=="Spiny_Dogfish",],aes(x=Year,y=avgCPUE,color=variable,shape=variable))+
  geom_line(color="orange",size=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  labs(y="mean CPUE (#/100hks)",x="",title="ADF&G PWS Longline Survey")+
  #coord_cartesian(x=c(1982,2016))+
  scale_x_continuous(limits=c(1982,2018))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=(0.5)),axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2)),legend.position="none") +
  theme(axis.line = element_line(color = 'black'))

#AFSC RPNs
AFSCLL<-read.csv(paste(cleandatdir,"/AFSCLL_RPN",AYR,".csv",sep=""), header=T)

#curiosity figure
ggplot(AFSCLL[AFSCLL$FMP=="GOA",], aes(x=Year, y=sumRPN/1000)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1)+
  labs(y="RPNs",x="",title="AFSC Longline Survey")+
  #coord_cartesian(y=c(0,max(110)))+
  scale_x_continuous(limits=c(1982,2018))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(Species.Code~FMP,scales="free")
ggplot(AFSCLL[AFSCLL$FMP=="BSAI",], aes(x=Year, y=sumRPN/1000)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1)+
  labs(y="RPNs",x="",title="AFSC Longline Survey")+
  #coord_cartesian(y=c(0,max(110)))+
  scale_x_continuous(limits=c(1982,2018))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(Species.Code~FMP,scales="free")

AFSCLL_doc<-ggplot(AFSCLL[AFSCLL$Species.Code==310&AFSCLL$FMP=="GOA",], aes(x=Year, y=sumRPN/1000)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1)+
  labs(y="RPNs",x="",title="AFSC Longline Survey")+
  coord_cartesian(y=c(0,max(110)))+
  scale_x_continuous(limits=c(1982,AYR))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=0.5),legend.position="none",axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2))) +
  theme(axis.line = element_line(color = 'black'))

#IPHC RPNs
drops<-c("AI","BS")
IPHC_GOA<-IPHC2[IPHC2$FMP%nin%drops,]
IPHC_GOA2<-ddply(IPHC_GOA,c("Year","Species"),summarize,FMPRPN=sum(RPN),FMPLL=sum(LL),FMPUL=sum(UL))
IPHC_dog_doc<-ggplot(IPHC_GOA2[IPHC_GOA2$Species=="Spiny Dogfish",], aes(x=Year, y=FMPRPN/1000)) + 
  geom_bar(stat="identity",color="orange",fill="yellow",size=1)+
  geom_errorbar(aes(ymax=FMPUL/1000,ymin=FMPLL/1000),colour="black",width=1)+
  labs(y="RPNs (1000s)",x="",title="IPHC Longline Survey")+
  coord_cartesian(y=c(0,max(IPHC_GOA2[IPHC_GOA2$Species=="Spiny Dogfish",]$FMPUL)/1000))+
  scale_x_continuous(limits=c(1982,2020))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=0.5),legend.position="none",axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2))) +
  theme(axis.line = element_line(color = 'black'))

#Dogfish plot
Dog_combined_indices_doc<-grid.arrange(arrangeGrob(AFSC_TWLSD_doc,
                                                   ADFG_Kshak_doc,
                                                   IPHC_dog_doc, 
                                                   ADFG_SEAK_doc,
                                                   AFSCLL_doc,
                                                   ADFG_PWSSD_doc,ncol = 2))
ggsave(path=figdir,"GOA20.13_DF.png",Dog_combined_indices_doc,dpi=300)

# Fig 20.14 PSS GOA all survey indices ----

#GOA trawl survey
GOAPSS<-shark_biomass[shark_biomass$REGULATORY_AREA_NAME=="GOA"&shark_biomass$Group=="Pacific Sleeper Shark",]
AFSC_TWLPSS_doc<-ggplot(GOAPSS, aes(x=YEAR, y=Biomass/1000,group=Group,fill=Group)) + 
  geom_bar(stat="identity",color="dark green",fill="light green",size=1)+
  geom_errorbar(aes(ymax=UL/1000,ymin=LL/1000),colour="black",width=1)+
  labs(y="Biomass (1000s t)",x="",title="AFSC Trawl Survey")+
  coord_cartesian(y=c(0,160))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(1982,AYR))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=0.5),legend.position="none",axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2))) +
  theme(axis.line = element_line(color = 'black'))

#ADFG PWS LL
sl_PWSLL_doc<-ggplot(meanPWS[meanPWS$variable=="Sleeper_Shark",],aes(x=Year,y=avgCPUE,color=variable,shape=variable))+
  geom_line(color="dark green",size=1,show.legend = F)+
  geom_point(color="dark green",size=4,show.legend = F)+
  geom_point(color="light green",size=3,show.legend = F)+
  labs(y="mean CPUE (#/100hks)",x="",title="ADF&G PWS Longline Survey")+
  #coord_cartesian(x=c(1982,2016))+
  scale_x_continuous(limits=c(1982,AYR))+
  scale_y_continuous(expand=c(0.01,0.01))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=0.5),legend.position="none",axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2))) +
  theme(axis.line = element_line(color = 'black'))

#ADFG SEAK LL
sl_SEAKLL_doc<-ggplot(SEAKm[SEAKm$variable=="CPUE_PSS",],aes(x=Year,y=value,color=variable,shape=variable))+
  geom_line(color="dark green",size=1,show.legend = F)+
  geom_point(color="dark green",size=4,show.legend = F)+
  geom_point(color="light green",size=3,show.legend = F)+
  labs(y="CPUE (#/efhks)",x="",title="ADF&G SEAK Longline Survey")+
  #coord_cartesian(x=c(1982,2016))+
  scale_x_continuous(limits=c(1982,AYR))+
  #scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=0.5),legend.position="none",axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2))) +
  theme(axis.line = element_line(color = 'black'))

#IPHC RPNs
sl_IPHC_doc<-ggplot(IPHC_GOA2[IPHC_GOA2$Species=="Pacific Sleeper Shark",], aes(x=Year, y=FMPRPN/1000)) + 
  geom_bar(stat="identity",color="dark green",fill="light green",size=1)+
  geom_errorbar(aes(ymax=FMPUL/1000,ymin=FMPLL/1000),colour="black",width=1)+
  labs(y="RPNs (1000s)",x="",title="IPHC Longline Survey")+
  coord_cartesian(y=c(0,max(IPHC_GOA2[IPHC_GOA2$Species=="Pacific Sleeper Shark",]$FMPUL/1000)))+
  scale_x_continuous(limits=c(1982,AYR))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()
        ,panel.border = element_blank(),panel.background = element_blank(),axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),axis.text.y=element_text(colour='black',size=10),axis.text.x=element_text(colour='black',size=10),
        plot.title=element_text(size=rel(1),colour='black',hjust=0.5),legend.position="none",axis.ticks=element_line(colour='black'),axis.line=element_line(size=1),
        axis.text=element_text(size=rel(1.2))) +
  theme(axis.line = element_line(color = 'black'))
sl_survindices_doc<-grid.arrange(arrangeGrob(AFSC_TWLPSS_doc, sl_PWSLL_doc, sl_IPHC_doc,sl_SEAKLL_doc, nrow = 2))
ggsave(path=figdir,"GOA20_14PSS_doc.png",plot=sl_survindices_doc,dpi=300)

# Fig 20.15 IPHC CPUEs coastwide ----
PSSCPUE<-read.csv(paste(cleandatdir,"/IPHC_CPUE_Sleeper shark_",AYR-1,"_EXTRAP.csv",sep=""),header=T)
SDCPUE<-read.csv(paste(cleandatdir,"/IPHC_CPUE_Spiny dogfish_",AYR-1,"_EXTRAP.csv",sep=""),header=T)
sharkCPUE<-rbind(PSSCPUE,SDCPUE)

levels(sharkCPUE$species)[levels(sharkCPUE$species)=="Spiny dogfish"]<-"Spiny Dogfish"
levels(sharkCPUE$species)[levels(sharkCPUE$species)=="Sleeper shark"]<-"Pacific Sleeper Shark"

sC2<-sharkCPUE[sharkCPUE$area_combo=="FMP (without Inside waters)",]

sC2$area<-factor(sC2$area,c("BSAI","GOA","CAN","WC"))
PSS_CPUE_doc<-ggplot(sC2[sC2$species=="Pacific Sleeper Shark",],
                     aes(x=year,y=area_cpue,color=area,fill=area))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  geom_errorbar(aes(ymax=boot_lci,ymin=boot_uci),width=1,show.legend = F)+
  labs(title=sC2[sC2$species=="Pacific Sleeper Shark",]$species[1],x="",y="")+
  facet_grid(area~.,scales="free_y")+
  theme_doc()+
  theme(panel.grid = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        strip.text.y = element_blank())
SD_CPUE_doc<-ggplot(sC2[sC2$species=="Spiny Dogfish",],
                    aes(x=year,y=area_cpue,color=area,fill=area))+
  geom_point(show.legend = F)+
  geom_line(show.legend = F)+
  geom_errorbar(aes(ymax=boot_lci,ymin=boot_uci),width=1,show.legend = F)+
  labs(title=sC2[sC2$species=="Spiny Dogfish",]$species[1],x="",y="")+
  facet_grid(area~.,scales="free_y")+
  theme_doc()+
  theme(panel.grid = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))
IPHC_CPUE_doc<-grid.arrange(arrangeGrob(PSS_CPUE_doc, SD_CPUE_doc, nrow = 1,
                                        left = textGrob("CPUE (#/effhks)", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=20)),
                                        bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=20))))
ggsave(path=figdir,"GOA20_15_IPHC_CPUE_doc.png",IPHC_CPUE_doc,dpi=600)


# Figs 20.24 RFX ----
# Setup Themes ----
theme_pres<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      axis.line.x = element_line(color="cornsilk"),
      axis.line.y = element_line(color="cornsilk"),
      axis.text=element_text(size=rel(1),colour='cornsilk'),
      axis.ticks=element_line(colour='cornsilk'),
      axis.title.y=element_text(colour='cornsilk',face="bold",angle=90,size=15),
      axis.title.x=element_text(colour='cornsilk',face="bold"),
      plot.title=element_text(size=15,colour='cornsilk',face="bold",hjust = 0.5),
      plot.background=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.background=element_blank(),
      legend.text=element_text(colour='cornsilk',size=12),
      legend.title=element_text(colour='cornsilk',size=12),
      strip.background=element_blank(),
      strip.text=element_text(colour='cornsilk',size=12),
      plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )
}

theme_doc<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.text=element_text(size=rel(1),colour='black'),
      axis.ticks=element_line(colour='black'),
      axis.title.y=element_text(colour='black',face="bold",angle=90,size=15),
      axis.title.x=element_text(colour='black',face="bold"),
      plot.title=element_text(size=15,colour='black',face="bold",hjust = 0.5),
      plot.background=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.background=element_blank(),
      legend.text=element_text(colour='black',size=12),
      legend.title=element_text(colour='black',size=12),
      strip.background=element_blank(),
      #strip.text=element_blank(),
      plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )
}


# Data collection ----
# Current year RFX
RFX<-read.csv(paste(outdir,"/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header=T)
areas<-c("GOA", "WESTERN GOA", "EASTERN GOA", "CENTRAL GOA")
SDRFX<-RFX[RFX$Group=="Spiny_Dogfish" & RFX$REGULATORY_AREA_NAME %in% areas,]

# Last assessment RFX
LAYR <- 2018
pRFX<-read.csv(paste(getwd(),"/Output/",LAYR,"/RFX/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header = T)
pSDRFX<-pRFX[pRFX$Group=="Spiny_Dogfish" & pRFX$REGULATORY_AREA_NAME %in% areas,]

# RACE Biomass
SD_RACE<-read.csv(paste(datadir,"RACE_Biomass_Spiny_Dogfish.csv",sep=""),header=T)
SD_GOA<-SD_RACE[SD_RACE$REGULATORY_AREA_NAME %in% areas,]
SD_GOA$RACEUL<-SD_GOA$Biomass+1.96*SD_GOA$SE
SD_GOA$RACELL<-SD_GOA$Biomass-1.96*SD_GOA$SE

#merge data sets together for easier graphing
SD_RFX<-merge(SDRFX,SD_GOA,by=c("YEAR","REGULATORY_AREA_NAME","Group"),all=T)
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
  labs(x="",y="",title="Western GOA")+
  theme_doc()

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
  labs(x="",y="",title="Central GOA")+
  theme_doc()

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
  labs(x="",y="",title="Eastern GOA")+
  theme_doc()

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
  labs(x="",y="",title="Gulf of Alaska")+
  theme_doc()

RFX_fig_doc<-grid.arrange(arrangeGrob(WGOA_doc,CGOA_doc,EGOA_doc,GOA_doc,layout_matrix=rbind(c(1,2,3),c(4,4,4)),
                                      left = textGrob("Biomass (1000s t)", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=15)),
                                      bottom = textGrob("Year", vjust=0,gp=gpar(col="black", fontsize=15))))
ggsave(path = figdir,
       plot=RFX_fig_doc,
       "GOA_shark20_23RFX_doc.png",dpi=600,width = 8,height = 5)

#Figures for presentations
WGOA_pres<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="WESTERN GOA",],aes(x=YEAR,y=Biomass/1000))+
  geom_ribbon(aes(ymin=Biom_LL/1000,ymax=Biom_UL/1000),fill="grey85")+
  geom_errorbar(aes(ymax=RACEUL/1000,ymin=RACELL/1000),colour="black",width=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  geom_line(aes(x=YEAR,y=Biom_est/1000))+
  geom_line(data=pSDRFX[pSDRFX$REGULATORY_AREA_NAME=="WESTERN GOA",],aes(x=YEAR,y=Biom_est/1000),linetype="dashed",color="black",size=1)+
  coord_cartesian(y=c(0,2),x=c(1983,2022))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x="",y="",title="Western GOA")+
  theme_pres()

CGOA_pres<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="CENTRAL GOA",],aes(x=YEAR,y=Biomass/1000))+
  geom_ribbon(aes(ymin=Biom_LL/1000,ymax=Biom_UL/1000),fill="grey85")+
  geom_errorbar(aes(ymax=RACEUL/1000,ymin=RACELL/1000),colour="black",width=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  geom_line(aes(x=YEAR,y=Biom_est/1000))+
  geom_line(data=pSDRFX[pSDRFX$REGULATORY_AREA_NAME=="CENTRAL GOA",],aes(x=YEAR,y=Biom_est/1000),linetype="dashed",color="black",size=1)+
  coord_cartesian(y=c(0,100),x=c(1983,2022))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x="",y="",title="Central GOA")+
  theme_pres()

EGOA_pres<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="EASTERN GOA",],aes(x=YEAR,y=Biomass/1000))+
  geom_ribbon(aes(ymin=Biom_LL/1000,ymax=Biom_UL/1000),fill="grey85")+
  geom_errorbar(aes(ymax=RACEUL/1000,ymin=RACELL/1000),colour="black",width=1)+
  geom_point(color="orange",size=4)+
  geom_point(color="yellow",size=3)+
  geom_line(aes(x=YEAR,y=Biom_est/1000))+
  geom_line(data=pSDRFX[pSDRFX$REGULATORY_AREA_NAME=="EASTERN GOA",],aes(x=YEAR,y=Biom_est/1000),linetype="dashed",color="black",size=1)+
  coord_cartesian(y=c(0,275),x=c(1983,2022))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x="",y="",title="Eastern GOA")+
  theme_pres()

GOA_pres<-ggplot(SD_RFX[SD_RFX$REGULATORY_AREA_NAME=="GOA",],aes(x=YEAR,y=Biomass/1000))+
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
  labs(x="",y="",title="Gulf of Alaska")+
  theme_pres()

RFX_fig_pres<-grid.arrange(arrangeGrob(WGOA_pres,CGOA_pres,EGOA_pres,GOA_pres,layout_matrix=rbind(c(1,2,3),c(4,4,4)),
                                       left = textGrob("Biomass (1000s t)", rot = 90, vjust = 1.5,gp=gpar(col="cornsilk", fontsize=15)),
                                       bottom = textGrob("Year", vjust=0,gp=gpar(col="cornsilk", fontsize=15))))
ggsave(path = figdir,
       plot=RFX_fig_pres,
       "GOA_shark20_23RFX_pres.png",dpi=600,width = 8,height = 5)



###############################
#additional figs to work through
###################################
#complex catch w/ABC, OFL lines

#make df of ABC,OFL and TACs
limit<-c("ABC","OFL","TAC")
AREA<-c("GOA","BSAI")

Values<-c(4514,6020,4514,517,689,180) #these are what was specified for LAST year
Hspecs<-merge(limit,AREA)
Hspecs<-cbind(Hspecs,Values)
colnames(Hspecs)[1:2]<-c("Metric","FMP")

data4$Source<-c("CAS")
olddat4$Source<-c("Psuedoblend")
allyr<-rbind(data4,olddat4)

specs_catch<-function(DATA,FMP,TYPE){
  if(TYPE=="doc"){
    fun_theme<-theme_doc
    fun_color<-"black"
  } else {
    fun_theme<-theme_pres
    fun_color<-"cornsilk"
  }
  if(FMP=="GOA"){
    D2<-DATA[DATA$FMP==FMP,]
    fun_fig<-ggplot(D2, aes(x=Year, y=Catch)) + 
      geom_line(data=D2[D2$Source=="CAS",],stat="identity",size=2,colour="blue")+geom_point(size=6,shape=18,colour="blue")+geom_point(size=3,shape=18,color="light blue")+
      geom_line(data=D2[D2$Source=="Psuedoblend",],aes(x=Year,y=Catch),stat="identity",size=2,colour="purple")+geom_point(data=olddat4,aes(x=Year,y=Catch),size=6,shape=18,colour="purple")+
      geom_point(data=D2[D2$Source=="Psuedoblend",],aes(x=Year,y=Catch),size=3,shape=18,colour="magenta")+
      geom_hline(yintercept=Hspecs[Hspecs$FMP=="GOA"&Hspecs$Metric=="ABC",c("Values")],linetype="dashed",colour=fun_color,size=1)+
      geom_hline(yintercept=Hspecs[Hspecs$FMP=="GOA"&Hspecs$Metric=="OFL",c("Values")],linetype="solid",colour=fun_color,size=1)+  
      coord_cartesian(ylim=c(0,Hspecs[Hspecs$FMP=="GOA"&Hspecs$Metric=="OFL",c("Values")]*1.1),xlim=c(min(1997),max(as.numeric(D2$Year))+1))+
      labs(title="Shark Complex Catch",y="Catch (t)")+
      annotate("text",x=2012,y=Hspecs[Hspecs$FMP=="GOA"&Hspecs$Metric=="OFL",c("Values")]*1.05,label="OFL",colour=fun_color)+
      annotate("text",x=2012,y=Hspecs[Hspecs$FMP=="GOA"&Hspecs$Metric=="ABC",c("Values")]*1.05,label="ABC & TAC",colour=fun_color)+
      fun_theme()
  } else {
    D2<-DATA[DATA$FMP==FMP,]
    fun_fig<-ggplot(D2, aes(x=Year, y=Catch)) + 
      geom_line(data=D2[D2$Source=="CAS",],stat="identity",size=2,colour="blue")+geom_point(size=6,shape=18,colour="blue")+geom_point(size=3,shape=18,color="light blue")+
      geom_hline(yintercept=Hspecs[Hspecs$FMP=="BSAI"&Hspecs$Metric=="ABC",c("Values")],linetype="dashed",colour=fun_color,size=1)+
      geom_hline(yintercept=Hspecs[Hspecs$FMP=="BSAI"&Hspecs$Metric=="OFL",c("Values")],linetype="solid",colour=fun_color,size=1)+
      geom_hline(yintercept=Hspecs[Hspecs$FMP=="BSAI"&Hspecs$Metric=="TAC",c("Values")],linetype="dotted",colour=fun_color,size=1)+
      coord_cartesian(ylim=c(0,Hspecs[Hspecs$FMP=="BSAI"&Hspecs$Metric=="OFL",c("Values")]*1.1),xlim=c(min(2003),max(as.numeric(D2$Year))+1))+
      labs(title="Shark Complex Catch",y="Catch (t)",colour=fun_color)+
      annotate("text",x=2012,y=Hspecs[Hspecs$FMP=="BSAI"&Hspecs$Metric=="OFL",c("Values")]*1.05,label="OFL",colour=fun_color)+
      annotate("text",x=2012,y=Hspecs[Hspecs$FMP=="BSAI"&Hspecs$Metric=="ABC",c("Values")]*1.05,label="ABC",colour=fun_color)+
      annotate("text",x=2012,y=Hspecs[Hspecs$FMP=="BSAI"&Hspecs$Metric=="TAC",c("Values")]*1.2,label="TAC",colour=fun_color)+
      fun_theme()
  }
  ggsave(paste(FMP,"catch_specs",TYPE,".png",sep=""),plot=fun_fig,dpi=600)
}

specs_catch(allyr,FMP="GOA",TYPE="doc")
specs_catch(allyr,FMP="GOA",TYPE="pres")
specs_catch(allyr,FMP="BSAI",TYPE="doc")
specs_catch(allyr,FMP="BSAI",TYPE="pres")
