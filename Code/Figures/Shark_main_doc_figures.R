# Shark Assessment Maine Document Figures ----
# Updated 10/24/2022 by C. Tribuzio

# Setup ----
datadir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
cleandatdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")
figdir<-paste(getwd(),"/Output/",AYR,"/Figures/",sep="")
dir.create(figdir, showWarnings = T)
outdir <- paste0(getwd(),"/Output/",AYR)

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

# Figs 19.2 Total Catch by species/complex ----
CASdat<-read_csv(paste0(getwd(),"/Data/Annual_updates/",AYR,"/confidential_CAS_sharks",AYR,".csv",sep="")) %>% 
  clean_names()

Cd <- CASdat %>% 
  select(year, fmp_area, species, catch_mt)

olddat<-read_csv(paste0(getwd(),"/Data/Static/confidential_pre2003_shark_cleaned.csv",sep="")) %>% 
  clean_names() %>% 
  filter((year > 2002 & fmp_area == "BSAI")|
           (year > 1996 & fmp_area == "GOA"))
od<-olddat %>% 
  select(year, fmp_area, species, catch_mt)

catchdat<-rbind(od,Cd)
cd2 <- catchdat %>% 
  group_by(year, fmp_area, species) %>% 
  summarise(Catch = sum(catch_mt))

sp_complex_catch<-function(DATA,DOC,FMP){
  if(DOC==T){
    fun_theme<-theme_doc
  } else {
    fun_theme<-theme_pres
  }
  max_dat <- DATA %>% filter(fmp_area == FMP)
  fun_max <- max_dat %>% 
    group_by(year) %>% 
    summarise(yrtot = sum(Catch))
  fun_max <- max(fun_max$yrtot)
  DATA$species<-factor(DATA$species,levels = c("Spiny Dogfish",
                                                 "Pacific Sleeper Shark",
                                                 "Salmon Shark",
                                                 "Other Sharks"))
  ggplot(DATA[DATA$fmp_area==FMP,], aes(x=year, y=Catch,fill=species,group=species)) + 
    geom_bar(data=DATA[DATA$species=="Spiny Dogfish"&DATA$fmp_area==FMP,],
             stat="identity",aes(x=year,y=Catch),color="orange",fill="yellow",size=1)+
    geom_bar(data=DATA[DATA$species=="Pacific Sleeper Shark" &DATA$fmp_area==FMP,],
             stat="identity",aes(x=year,y=Catch),color="dark green",fill="light green",size=1)+
    geom_bar(data=DATA[DATA$species=="Salmon Shark"&DATA$fmp_area==FMP,],
             stat="identity",aes(x=year,y=Catch),color="dark blue",fill="light blue",size=1)+
    geom_bar(data=DATA[DATA$species=="Other Sharks"&DATA$fmp_area==FMP,],
             stat="identity",aes(x=year,y=Catch),color="magenta",fill="pink",size=1)+
    facet_wrap(~species,ncol=1)+
    coord_cartesian(ylim=c(0,round(fun_max,digits=-2)))+
    scale_y_continuous(expand=c(0,0))+
    labs(y="",x="", title = FMP)+
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
  max_dat <- DATA %>% filter(fmp_area == FMP)
  fun_max <- max_dat %>% 
    group_by(year) %>% 
    summarise(yrtot = sum(Catch))
  fun_max <- max(fun_max$yrtot)
  if(FMP=="GOA"){
    DATA$species<-factor(DATA$species,levels = c("Spiny Dogfish",
                                                 "Pacific Sleeper Shark",
                                                 "Salmon Shark",
                                                 "Other Sharks"))
  } else {
    DATA$species<-factor(DATA$species,levels = c("Pacific Sleeper Shark",
                                                 "Salmon Shark",
                                                 "Other Sharks",
                                                 "Spiny Dogfish"))
  }
  ggplot(DATA[DATA$fmp_area==FMP,], aes(x=year, y=Catch,fill=species,group=species,color=species)) + 
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
    labs(y="",x="", title = "Complex Catch")+
    fun_theme()
}
GOAtotdoc<-tot_complex_catch(cd2,FMP="GOA",DOC=T)
GOAtotpres<-tot_complex_catch(cd2,FMP="GOA",DOC=F)
BSAItotdoc<-tot_complex_catch(cd2,FMP="BSAI",DOC=T)
BSAItotpres<-tot_complex_catch(cd2,FMP="BSAI",DOC=F)

FMP_catch<-grid.arrange(arrangeGrob(BSAIspecdoc, GOAspecdoc, BSAItotdoc, GOAtotdoc, ncol = 2, nrow = 2, heights = c(4,1),
                                  left = textGrob("Catch (t)", rot = 90, vjust = 1.5,gp=gpar(col=textcol, fontsize=20)),
                                  bottom = textGrob("Year", vjust=0,gp=gpar(col=textcol, fontsize=20))))
ggsave(path = figdir,"Main_19.2catch_doc.png",plot=FMP_catch,dpi=600,width = 10, height = 11)

# Fig 19.3 Total Catch by fmp and sub area ----
GOA <- CASdat %>% 
  filter(fmp_area == "GOA" | fmp_area == "INSD") %>% 
  group_by(year, species, reporting_area_code) %>% 
  summarise(tot_catch = sum(catch_mt)) %>% 
  mutate(nmfs_area = as.factor(reporting_area_code))

GOA$nmfs_area <- factor(GOA$nmfs_area,c("610","620","630","640","650","649","659"))

#need to create a fake factor to order the areas the way I want them for the figures
AOrder<-matrix(NA,ncol=1,nrow=nrow(GOA))
unqkey<-unique(GOA$nmfs_area)
for (i in 1:length(AOrder)){
  ifelse(GOA$nmfs_area[i]==610,AOrder[i]<-c("A"),
         ifelse(GOA$nmfs_area[i]==620,AOrder[i]<-c("B"),
                ifelse(GOA$nmfs_area[i]==630,AOrder[i]<-c("C"),
                       ifelse(GOA$nmfs_area[i]==640,AOrder[i]<-c("D"),
                              ifelse(GOA$nmfs_area[i]==650,AOrder[i]<-c("E"),
                                     ifelse(GOA$nmfs_area[i]==649,AOrder[i]<-c("F"),AOrder[i]<-c("G")))))))
}
colnames(AOrder) <- "AOrder"
GOA4<-cbind(GOA,AOrder)
GOA5<-GOA4[order(GOA4$AOrder,GOA4$year,decreasing=F),]
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
  fun_max <- DATA %>% 
    group_by(year) %>% 
    summarise(yrtot = sum(tot_catch))
  fun_max <- max(fun_max$yrtot)
  print(ggplot(DATA, aes(x=year,y=tot_catch,fill=fct_rev(AOrder))) +
          geom_bar(stat="identity",show.legend=F)+
          geom_bar(stat="identity",color="black",show.legend=F)+
          scale_fill_manual(values=c("G"="#4575b4","F"="#91bfdb","E"="#fdf5e6","D"="#ffffbf","C"="#fee090","B"="#fc8d59","A"="#d73027"),
                            breaks=c("A","B","C","D","E","F","G"),labels=c("659","649","650","640","630","620","610"),name="NMFS\nArea")+
          #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
          #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
          scale_x_continuous(breaks=seq(2003,max(DATA$year),4))+
          coord_cartesian(ylim=c(0,rup(fun_max)))+
          scale_y_continuous(expand=c(0,0))+
          labs(y="",x="",title=DATA$species)+
          fun_theme()+theme(
            plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            axis.line.x = element_line(colour=fun_color),
            axis.line.y = element_line(colour=fun_color),
            panel.grid.major=element_blank(),
            plot.title=element_text(size=20,colour=fun_color,hjust = 0.5)))
}

unq_key<-unique(GOA5$species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-GOA5[GOA5$species==unq_key[i,1],]
  d<-catch_area(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

#make legend plot, then grid arrange them all together, see below code for example
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

leg_max <- GOA5 %>% 
  filter(species == "Spiny Dogfish") %>% 
  group_by(year) %>% 
  summarise(yrtot = sum(tot_catch))
leg_max <- max(leg_max$yrtot)

carea_legdoc<-ggplot(GOA5[GOA5$species=="Spiny Dogfish",], aes(x=year,y=tot_catch,fill=fct_rev(AOrder))) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("G"="#4575b4","F"="#91bfdb","E"="#fdf5e6","D"="#ffffbf","C"="#fee090","B"="#fc8d59","A"="#d73027"),
                    breaks=c("G","F","E","D","C","B","A"),labels=c("659","649","650","640","630","620","610"),name="NMFS\nArea")+
  #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
  #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
  scale_x_continuous(breaks=seq(2003,max(GOA5$year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(leg_max)))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=GOA5[GOA5$species=="Spiny Dogfish",]$species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(carea_legdoc)), "guide-box") 
grid.draw(legend_doc)

GOAcarea_doc<-grid.arrange(arrangeGrob(SDdoc, OSdoc,SSdoc,PSSdoc,nrow = 4,
                                    bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20)),
                                    top = textGrob("GOA", vjust = 0.5,gp=gpar(col="black", fontsize=20))), 
                        legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                        nrow=1)

#BSAI
BSAI <- CASdat %>% 
  filter(fmp_area == "BSAI") %>% 
  group_by(year, species, fmp_subarea) %>% 
  summarise(tot_catch = sum(catch_mt))

#need to create a fake factor to match color schemes in function
AOrder<-matrix(NA,ncol=1,nrow=nrow(BSAI))
unqkey<-unique(BSAI$fmp_subarea)
for (i in 1:length(AOrder)){
  ifelse(BSAI$fmp_subarea[i]=="AI",AOrder[i]<-c("A"),AOrder[i]<-c("G"))
}
colnames(AOrder) <- "AOrder"
BSAI4<-cbind(BSAI,AOrder)
BSAI5<-BSAI4[order(BSAI4$AOrder,BSAI4$year,decreasing=F),]
BSAI5$AOrder<-factor(BSAI5$AOrder,levels=c("A","G"))

unq_key<-unique(BSAI5$species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-BSAI5[BSAI5$species==unq_key[i,1],]
  d<-catch_area(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

#make legend plot, then grid arrange them all together
leg_max <- BSAI5 %>% 
  filter(species == "Pacific Sleeper Shark") %>% 
  group_by(year) %>% 
  summarise(yrtot = sum(tot_catch))
leg_max <- max(leg_max$yrtot)

carea_legdoc<-ggplot(BSAI5[BSAI5$species=="Spiny Dogfish",], aes(x=year,y=tot_catch,fill=fct_rev(AOrder))) +
  geom_bar(stat="identity")+
  geom_bar(stat="identity",color="black")+
  scale_fill_manual(values=c("G"="#4575b4","A"="#d73027"),
                    breaks=c("G","A"),labels=c("BS","AI"),name="FMP\nSubarea")+
  #scale_fill_brewer(palette="YlOrRd",labels=c("659","649","650","640","630","620","610"),
  #                 breaks=c("G","F","E","D","C","B","A"),name="NMFS\nArea")+
  scale_x_continuous(breaks=seq(2003,max(BSAI5$year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(leg_max)))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=BSAI5[BSAI5$species=="Spiny Dogfish",]$species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(carea_legdoc)), "guide-box") 
grid.draw(legend_doc)

BSAI_carea_doc<-grid.arrange(arrangeGrob(SDdoc, OSdoc,SSdoc,PSSdoc,nrow = 4,
                                    left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                    bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20)),
                                    top = textGrob("BSAI", vjust = 0.5,gp=gpar(col="black", fontsize=20))), 
                        legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                        nrow=1)
carea_fig <- grid.arrange(arrangeGrob(BSAI_carea_doc, GOAcarea_doc, nrow = 1))

ggsave(path = figdir, "Main_19.3catch_area.png",plot=carea_fig,dpi=600,width = 10, height = 11)

# Fig 19.5 Catch by target fishery ----
GOA <- CASdat %>% 
  filter(fmp_area == "GOA") %>% 
  group_by(year, species, trip_target) %>% 
  summarise(tot_catch = sum(catch_mt))

catch_fishery<-function (DATA,TYPE){
  if(TYPE=="doc"){
    fun_theme<-theme_doc
    fun_color<-"black"
  } else {
    fun_theme<-theme_pres
    fun_color<-"cornsilk"
  }
  fun_max <- DATA %>% 
    group_by(year) %>% 
    summarise(yrtot = sum(tot_catch))
  fun_max <- max(fun_max$yrtot)
  print(ggplot(DATA, aes(x=year,y=tot_catch,fill=trip_target)) +
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
          scale_x_continuous(breaks=seq(2003,max(DATA$year),4))+
          coord_cartesian(ylim=c(0,rup(fun_max)))+
          scale_y_continuous(expand=c(0,0))+
          labs(y="",x="",title=DATA$species)+
          fun_theme()+theme(
            plot.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            axis.line.x = element_line(colour=fun_color),
            axis.line.y = element_line(colour=fun_color),
            panel.grid.major=element_blank(),
            plot.title=element_text(size=20,colour=fun_color,hjust = 0.5)))
}

unq_key<-unique(GOA$species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-GOA[GOA$species==unq_key[i,1],]
  d<-catch_fishery(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

leg_max <- GOA %>% 
  filter(species == "Spiny Dogfish") %>% 
  group_by(year) %>% 
  summarise(yrtot = sum(tot_catch))
leg_max <- max(leg_max$yrtot)

cfishery_legdoc<-ggplot(GOA[GOA$species=="Spiny Dogfish",], aes(x=year,y=tot_catch,fill=trip_target)) +
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
  scale_x_continuous(breaks=seq(2003,max(GOA$year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(leg_max)))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=GOA[GOA$species=="Spiny Dogfish",]$species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(cfishery_legdoc)), "guide-box") 
grid.draw(legend_doc)

GOAcfishery_doc<-grid.arrange(arrangeGrob(SDdoc, OSdoc,SSdoc,PSSdoc,nrow = 4,
                                       top = textGrob("GOA",vjust = 0.5, gp=gpar(col="black", fontsize=20)),
                                       bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20))), 
                           legend_doc, widths=unit.c(unit(1, "npc") - legend_doc$width, legend_doc$width), 
                           nrow=1)

# BSAI catch by fishery ----
BSAI <- CASdat %>% 
  filter(fmp_area == "BSAI") %>% 
  group_by(year, species, trip_target) %>% 
  summarise(tot_catch = sum(catch_mt))

unq_key<-unique(BSAI$species)
unq_key<-merge(unq_key,c("doc","pres"))
unq_key$short<-c("PSS","OS","SS","SD")

for (i in 1:nrow(unq_key)){ #this kicks out 8 grobs
  loop_dat<-BSAI[BSAI$species==unq_key[i,1],]
  d<-catch_fishery(loop_dat,TYPE=unq_key[i,2])
  assign(paste(unq_key[i,3],unq_key[i,2],sep=""),d)
}

leg_max <- BSAI %>% 
  filter(species == "Pacific Sleeper Shark") %>% 
  group_by(year) %>% 
  summarise(yrtot = sum(tot_catch))
leg_max <- max(leg_max$yrtot)

cfishery_legdoc<-ggplot(BSAI[BSAI$species=="Spiny Dogfish",], aes(x=year,y=tot_catch,fill=trip_target)) +
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
  scale_x_continuous(breaks=seq(2003,max(BSAI$year),2))+
  coord_cartesian(ylim=c(0,roundUpNice(leg_max)))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="",title=BSAI[BSAI$species=="Spiny Dogfish",]$species)+
  theme_doc()
legend_doc = gtable_filter(ggplot_gtable(ggplot_build(cfishery_legdoc)), "guide-box") 
grid.draw(legend_doc)

BSAIcfishery_doc<-grid.arrange(arrangeGrob(SDdoc, OSdoc,SSdoc,PSSdoc,nrow = 4,
                                       left = textGrob("Catch (t)", rot = 90, vjust = 1,gp=gpar(col="black", fontsize=20)),
                                       bottom = textGrob("Year", vjust = -0.5,gp=gpar(col="black", fontsize=20)),
                                       top = textGrob("BSAI",vjust = 0.5, gp=gpar(col="black", fontsize=20))))

ctarget_fig <- grid.arrange(arrangeGrob(BSAIcfishery_doc, GOAcfishery_doc, nrow = 1, widths = c(1, 1.3)))
ggsave(path = figdir, "Main_19.4catch_target.png",plot=ctarget_fig,dpi=600,width = 10, height = 11)

# Fig 19.4 Cumulative catches ----
AYRdat <- read.csv(paste(cleandatdir,"/confidential_shark_FMP_cumm_catch",AYR,".csv",sep=""),header=T) %>% 
  filter(year == 2022 & week_num <41) %>%  #only need the last 10 years
  mutate(year = as.factor(year))
FMP_dat<-read.csv(paste(cleandatdir,"/confidential_shark_FMP_cumm_catch",AYR,".csv",sep=""),header=T) %>% 
  filter(year > AYR-10 & year != 2022) %>%  #only need the last 10 years
  mutate(year = as.factor(year)) %>% 
  bind_rows(AYRdat)

GOA_doc<-ggplot(FMP_dat[FMP_dat$fmp_area=="GOA",],aes(x=week_num,y=cum_catch,fill=year,
                                           color=year, size = year, alpha = year))+
  geom_line(aes(alpha=year, size = year))+
  geom_vline(xintercept = 40, linetype = "dashed")+
  labs(y="",title="GOA",x="")+
  theme_doc()
BSAI_doc<-ggplot(FMP_dat[FMP_dat$fmp_area=="BSAI",],aes(x=week_num,y=cum_catch,fill=year,
                                                      color=year, size = year))+
  geom_line(aes(alpha=year, size = year))+
  geom_vline(xintercept = 40, linetype = "dashed")+
  labs(y="",title="BSAI",x="")+
  theme_doc()
cum_catch_fig<-grid.arrange(BSAI_doc, GOA_doc,nrow=2,
                          left = textGrob("Cummulative Catch", rot = 90, vjust = 1.5,gp=gpar(col="black", fontsize=20)),
                          bottom = textGrob("Week Number", vjust=0,gp=gpar(col="black", fontsize=20)))
ggsave(path = figdir, "Main_19.5cumm_catch.png",plot=cum_catch_fig,dpi=600,width = 10, height = 11)

# Figs 19.6 RFX ----
# Setup Themes ----
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


# Data collection
# Current year RFX
RFX<-read.csv(paste(outdir,"/RFX/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header=T)
areas<-c("GOA", "WESTERN GOA", "EASTERN GOA", "CENTRAL GOA")
SDRFX<-RFX[RFX$Group=="Spiny_Dogfish" & RFX$REGULATORY_AREA_NAME %in% areas,]

# Last assessment RFX
pRFX<-read.csv(paste(getwd(),"/Output/",LAYR,"/RFX/RFX_Biomass_Spiny_Dogfish.csv",sep=""),header = T)
pSDRFX<-pRFX[pRFX$Group=="Spiny_Dogfish" & pRFX$REGULATORY_AREA_NAME %in% areas,]

# RACE Biomass
SD_RACE<-read.csv(paste(outdir,"/RACE_Biomass/RACE_Biomass_Spiny_Dogfish", AYR, ".csv",sep=""),header=T)
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
       "Main_19.6RFX.png",dpi=600,width = 8,height = 5)