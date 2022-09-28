###PACIFIC SLEEPER SHARK STOCK STRUCTURE DOCUMENT SEPTEMBER 2022


# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rgdal)
library(patchwork)


# SET UP MAP LAYERS -------------------------------------------------------

#get land
world <- ne_countries(scale = "medium", returnclass = "sp")
#usa
usa <- subset(world, admin == "United States of America")
usa <- fortify(usa)
usa$long <- ifelse(usa$long > 0, usa$long - 360, usa$long)
#russia
russia <- subset(world, admin == "Russia")
russia <- fortify(russia)
russia$long = ifelse(russia$long > 0, russia$long - 360, russia$long)
#canada
canada <- subset(world, admin == "Canada")
canada <- fortify(canada)
#mexico
mexico <-subset(world, admin == "Mexico")
mexico <- fortify(mexico)
#bathymetry (from downloaded OFIS bathymetry shapefile)
race_bathy <- readOGR("Data/race_bathy_to_200_NAD1983_HARN")
race_bathy_df <- fortify(race_bathy)
race_bathy_df$long <- ifelse(race_bathy_df$long > 0, race_bathy_df$long - 360, race_bathy_df$long)

#get states
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))


# Fig.1 LENGTH DISTRIBUTIONS ----------------------------------------------------

#Load and prep data
#same as size summary datafile but includes FMP (note that WC records are listed as GOA)
LW.survey <- read.csv("Data/PSS_size_joins.csv")
#remove outliers
#LW.survey <- LW.survey[-c(3,599),]
LW.survey <- LW.survey[LW.survey$TLcm != 0 & LW.survey$Weight !=27.398,]

#fix coding for West Coast
LW.survey$FMP <- ifelse(LW.survey$NMFS_Sub_A == "WC", "WC", LW.survey$FMP)

#Read in additional file from NORPAC (Cindy sent 7/29/22 and updated 8/16/22)
#Coordinates have been converted to non-confidential points
LW.NORPAC <- read.csv("Data/NCF_Obj1_obsSPdat_updated081622.csv")
LW.NORPAC <- LW.NORPAC %>% 
  select(year, haul_date, sex_obs, wt, pcl, ncf_lat, ncf_lon, nmfs_area) %>% 
  rename("Year" = "year",
         "Sex" = "sex_obs",
         "Weight" = "wt",
         "PCL" = "pcl",
         "LAT" = "ncf_lat",
         "LONG" = "ncf_lon",
         "NMFS_AREA" = "nmfs_area") %>% 
  filter(PCL > 0) %>% 
  mutate(TLcm = (17.78 + 1.1*PCL),
         NMFS_Sub_A = ifelse(NMFS_AREA < 541, "BS",
                             ifelse(between(NMFS_AREA, 540, 544), "AI",
                                    ifelse(NMFS_AREA == 610, "WGOA", 
                                           ifelse(between(NMFS_AREA, 610, 640), "CGOA",
                                                  ifelse(NMFS_AREA == 649 | NMFS_AREA == 659, "INSD",
                                                         ifelse(NMFS_AREA == 640 | NMFS_AREA == 650, "EGOA", "OUT" )))))),
         FMP = ifelse(NMFS_AREA < 600, "BSAI",
                      ifelse(between(NMFS_AREA, 600, 659), "GOA", 
                             "OTHER")))

#merge with survey data
LW.all <- full_join(LW.NORPAC, LW.survey)

#how many in each FMP/Subarea
table(LW.all$FMP)
table(LW.all$NMFS_Sub_A)

length.dists.fmp <- ggplot(LW.all[!LW.all$FMP=="",], aes(TLcm, color=FMP)) +
  geom_density(size=1, trim=TRUE) +
  geom_vline(xintercept=370, linetype="dashed", color="gray50", size=1)+
  labs(x="\nTotal length (cm)", y="Density\n") +
  scale_color_manual(labels = c(paste0("BSAI (n=", nrow(LW.all[LW.all$FMP=="BSAI",]),")"), 
                                c(paste0("GOA (n=", nrow(LW.all[LW.all$FMP=="GOA",]),")")),
                                c(paste0("West Coast (n=", nrow(LW.all[LW.all$FMP=="WC",]),")"))),
                     values = c("deeppink", "purple", "lightblue")) +
  coord_cartesian(xlim=c(0,470)) +
  theme_pubr(legend = "right") +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.grid.major.y = element_line())

ggsave(path = "Figures", filename = "Length frequencies PSS.png", 
       plot = length.dists.fmp, dpi = 300, width = 6, height = 3)


# Fig.2 CAPTURE LOCATIONS SMALL SHARKS ------------------------------------------

#number of sharks under 100 cm TL
nrow(LW.all[LW.all$TLcm < 100,])
#number of sharks under 75 cm TL
nrow(LW.all[LW.all$TLcm < 75,])

#adjust data points for 180 line
LW.all$LONG <- ifelse(LW.all$LONG > 0, LW.all$LONG - 360, LW.all$LONG)

#colored by year
sub.75.map <- ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#d6d6d6", color = "#7f8182", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#7f8182", size = 0.2) +
  geom_polygon(data = mexico, aes(long, lat, group=group), fill= "#d6d6d6", color = "#7f8182", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#3874d6", size = 0.2, alpha=0.4) +
  geom_sf(data = states, fill = NA, color = "#7f8182", size = 0.2) + 
  geom_count(data = LW.all[LW.all$TLcm<75,], 
             aes(x = LONG, y = LAT, fill=factor(Year)), 
             shape = 21, size = 2) +
  labs(fill = "Year") +
  scale_fill_viridis_d() +
  coord_sf(crs = st_crs(4269), xlim = c(-180,-115), ylim = c(30, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.1, "lines"),
        panel.grid = element_blank())

ggsave(path = "Figures", filename = "PSS sub 75TL map.png", 
       plot = sub.75.map, dpi = 300, width = 6, height = 5)


#FISHERY CATCHES ------------------------------------------------------

#..Load/prep data (Cindy updated 8/5/22)-------------------
fisherycatch <- read.csv("Data/confidential_CAS_GFTBF_Sharks2022.csv")
#omit nonPSS
fisherycatch <- filter(fisherycatch, Species == "\"Pacific sleeper shark\"")

fisherycatch <- fisherycatch %>% 
  rename(Year = ï..Year)

#rename gear levels
fisherycatch$Gear <- as.factor(fisherycatch$Gear)
levels(fisherycatch$Gear)
levels(fisherycatch$Gear) <- list(Longline="HAL", "Non-pelagic trawl"="NPT", Pot="POT", "Pelagic trawl" = "PTR")

#change target to factors
fisherycatch$Trip.Target.Group <- as.factor(fisherycatch$Trip.Target.Group)
levels(fisherycatch$Trip.Target.Group)
#group Kamchatka with flatfish
fisherycatch$Trip.Target.Group <- recode_factor(fisherycatch$Trip.Target.Group, 
                                                "Kamchatka Flounder - BSAI" = "Flatfish")
#rename areas
fisherycatch$FMP.Area <- as.factor(fisherycatch$FMP.Area)
levels(fisherycatch$FMP.Area) <- 
  list("BSAI"="BSAI", "GOA"="GOA", "Inside"="INSD")
#split into two datasets
BSAI.fishery <- fisherycatch %>% 
  filter(FMP.Area == "BSAI")
GOA.fishery <- fisherycatch %>% 
  filter(FMP.Area == "GOA")


#..Catch by target (text in fishery section)------------
(GOA.targets <- GOA.fishery %>% 
   filter(Year < 2022) %>% 
   group_by(Trip.Target.Group) %>% 
   summarise(total.catch = round(sum(Catch..mt.),0)) %>% 
   mutate(percent = round(total.catch/sum(total.catch)*100,0), #percentage of total catch
          average = round(total.catch/length(unique(GOA.fishery$Year)),0)) %>% #average tons per year
   arrange(desc(percent)))

(BSAI.targets <- BSAI.fishery %>% 
    filter(Year < 2022) %>% 
    group_by(Trip.Target.Group) %>% 
    summarise(total.catch = round(sum(Catch..mt.),0)) %>% 
    mutate(percent = round(total.catch/sum(total.catch)*100,0), #percentage of total catch
           average = round(total.catch/length(unique(BSAI.fishery$Year)),0)) %>% #average tons per year
    arrange(desc(percent)))



#..Fig.3a Catch in weight timeseries by FMP area ------------------------
catch.ts.wt <- fisherycatch %>% 
  filter(Year!=2022) %>% #2022 is partial
  ggplot(aes(factor(Year), Catch..mt., fill=FMP.Area)) +
  geom_bar(stat = "identity") +
  facet_wrap(~FMP.Area, ncol=1, scales = "free_y") +
  labs(y = "Catch (metric tons)\n", fill = "FMP Area") +
  #scale_x_continuous(breaks = seq(2000,2020, by = 5)) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult=c(0,0.1))) +
  scale_fill_manual(values = c("#374E55FF", "#DF8F44FF", "#B24745FF")) +
  theme_pubr(border = TRUE, legend = "none", base_size=9) +
  theme(panel.grid.major.y = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_blank())

# ..Fig.3b Catch in numbers timeseries by FMP  --------------------------------------------

#catch in numbers obtained from CAS by Cindy on 8/30/22
catch.nos <- read.csv("Data/confidential_CAS_sharknumbers.csv")
range(catch.nos$YEAR)
range(fisherycatch$Year)
#add dummy NAs for years not in catch.nos for comparison with catch weight
dummy.yr <- data.frame("YEAR" = rep(seq(2003, 2010, by=1),3),
                       "COUNT" = rep(NA, 3),
                       "FMP" = c(rep("BSAI", 8), rep("GOA", 8), rep("INSD", 8)))
catch.nos <- full_join(catch.nos, dummy.yr)
catch.nos$FMP <- as.factor(catch.nos$FMP)
levels(catch.nos$FMP) <- 
  list("BSAI"="BSAI", "GOA"="GOA", "Inside"="INSD")

catch.ts.nos <- catch.nos %>% 
  filter(YEAR!=2022) %>% #2022 is partial
  ggplot(aes(factor(YEAR), COUNT, fill=FMP)) +
  geom_bar(stat="identity") +
  facet_wrap(~FMP, ncol=1, scales = "free_y") +
  labs(y="Number of sharks\n", fill="FMP") +
  scale_fill_manual(values = c("#374E55FF", "#DF8F44FF", "#B24745FF")) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult=c(0,0.05))) +
  theme_pubr(border = TRUE, legend = "none", base_size=9) +
  theme(panel.grid.major.y = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_blank())

#Join catch weight and catch numbers into one figure (Figure 3 both panels)
catch.timeseries <- catch.ts.wt + catch.ts.nos

ggsave(path = "Figures", filename = "PSS catch area.png", 
       plot = catch.timeseries, dpi = 300, width = 9, height = 5)


#Text in fishery section (range of annual catch in weight and numbers by area)
(BSAI.year <- BSAI.fishery %>% 
    group_by(Year) %>% 
    summarise(total.catch = round(sum(Catch..mt.),0)))
range(BSAI.year[BSAI.year$Year<2022,]$total.catch)

(GOA.year <- GOA.fishery %>% 
    group_by(Year) %>% 
    summarise(total.catch = round(sum(Catch..mt.),0)))
range(GOA.year[GOA.year$Year<2022,]$total.catch)

(Inside.year <- fisherycatch %>% 
    filter(FMP.Area == "Inside") %>% 
    group_by(Year) %>% 
    summarise(total.catch = round(sum(Catch..mt.),0)))
range(Inside.year[Inside.year$Year<2022,]$total.catch)


(BSAI.year.nos <- catch.nos %>% 
    filter(FMP == "BSAI") %>% 
    group_by(YEAR) %>% 
    summarise(total.catch = round(sum(COUNT),0)))
range(BSAI.year.nos[BSAI.year.nos$YEAR<2022,]$total.catch, na.rm=TRUE)

(GOA.year.nos <- catch.nos %>% 
    filter(FMP == "GOA") %>% 
    group_by(YEAR) %>% 
    summarise(total.catch = round(sum(COUNT),0)))
range(GOA.year.nos[GOA.year.nos$YEAR<2022,]$total.catch, na.rm=TRUE)

(Inside.year.nos <- catch.nos %>% 
    filter(FMP == "Inside") %>% 
    group_by(YEAR) %>% 
    summarise(total.catch = round(sum(COUNT),0)))
range(Inside.year.nos[Inside.year.nos$YEAR<2022,]$total.catch, na.rm=TRUE)


#Percent declines between start of timeseries (2003) and now (2021 last full year of data)
#GOA
round((GOA.year$total.catch[GOA.year$Year == 2003] - GOA.year$total.catch[GOA.year$Year == 2021]) /
  GOA.year$total.catch[GOA.year$Year == 2003] * 100 , 0)

#Percent declines between start of timeseries (2003) and now (2021 last full year of data)
#BSAI
round((BSAI.year$total.catch[BSAI.year$Year == 2003] - BSAI.year$total.catch[BSAI.year$Year == 2021]) /
  BSAI.year$total.catch[BSAI.year$Year == 2003] * 100 , 0)

#Difference between first 5 years and last five years of timeseries
#GOA
round((mean(GOA.year$total.catch[between(GOA.year$Year, 2003, 2003+4)]) - 
  mean(GOA.year$total.catch[between(GOA.year$Year, 2021-4, 2021)])) /
  mean(GOA.year$total.catch[between(GOA.year$Year, 2003, 2003+4)]) * 100 , 0)

#BSAI
round((mean(BSAI.year$total.catch[between(BSAI.year$Year, 2003, 2003+4)]) - 
         mean(BSAI.year$total.catch[between(BSAI.year$Year, 2021-4, 2021)])) /
        mean(BSAI.year$total.catch[between(BSAI.year$Year, 2003, 2003+4)]) * 100 , 0)


# ..Fig.4 Compare catch in weight with catch in numbers -------------------------
#join dataframes
cw <- fisherycatch %>% 
  select(Year, 
         FMP = FMP.Area,
         NMFS.Area,
         FMP.Subarea,
         catch.weight = Catch..mt.) %>% 
  group_by(Year, FMP) %>% 
  summarise(catch.weight = sum(catch.weight))
cn <- catch.nos %>% 
  select(Year = YEAR,
         FMP,
         NMFS.Area = REPORTING_AREA, 
         FMP.Subarea = FMP_subarea,
         catch.count = COUNT) %>% 
  group_by(Year, FMP) %>% 
  summarize(catch.numbers = round(sum(catch.count),0))
all.catch <- merge(cw, cn, by = c("Year", "FMP"), all = TRUE)

#Scattergraph of catch weight vs catch numbers
nos.vs.wt <- ggplot(all.catch[!is.na(all.catch$catch.numbers) & all.catch$Year !=2022,], 
                    aes(catch.weight, catch.numbers, color=FMP)) +
  geom_point() +
  facet_wrap(~FMP, ncol=1, scales="free_y") +
  labs(x="\nCatch in weight (mt)", y="Catch in numbers\n") + 
  scale_color_manual(values = c("#374E55FF", "#DF8F44FF", "#B24745FF")) +
  scale_y_continuous(labels = scales::comma) +
  theme_pubr(border=TRUE, legend="none") +
  theme(panel.grid.major = element_line())

ggsave(path = "Figures", filename = "catch numbers vs weight.png", 
       plot = nos.vs.wt, dpi = 300, height = 7, width = 6)

ggplot(all.catch[!is.na(all.catch$catch.numbers) & all.catch$Year !=2022,], 
       aes(catch.weight, catch.numbers, color=FMP, shape=FMP)) +
  geom_point(size=3) +
  labs(x="\nCatch in weight (mt)", y="Catch in numbers\n") + 
  scale_color_manual(values = c("#374E55FF", "#DF8F44FF", "#B24745FF")) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult=c(0.05,0.05))) +
  theme_pubr(border=TRUE, legend="right") +
  theme(panel.grid.major = element_line())

# ..Fig.5 Catch by week ---------------------------------------------------------

#histogram (years color-coded) top panel
wkly.catch.yr <- fisherycatch %>% 
  filter(FMP.Area != "Inside" & between(Year, 2013, 2021)) %>% 
  ggplot(aes(Week.Number, Catch..mt., fill=factor(Year))) +
  facet_wrap(~FMP.Area, ncol = 1, scales = "free_y") +
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks=seq(5,50,5)) +
  labs(fill="Year") +
  theme_pubr(border=TRUE, legend = "right") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(),
        axis.title = element_blank(),
        axis.text.x = element_blank())

#histogram (target color-coded) bottom panel
wkly.catch.target <- fisherycatch %>% 
  filter(FMP.Area != "Inside" & between(Year, 2013, 2021)) %>% 
  ggplot(aes(Week.Number, Catch..mt., fill=Trip.Target.Group)) +
  facet_wrap(~FMP.Area, ncol = 1, scales = "free_y") +
  geom_bar(stat="identity") +
  scale_fill_viridis_d(option="magma") +
  scale_x_continuous(breaks=seq(5,50,5)) +
  labs(x = "\nWeek number", fill="Target") +
  theme_pubr(border=TRUE, legend = "right") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line(),
        axis.title.y = element_blank())

weekly.catches <- gridExtra::grid.arrange(patchworkGrob(wkly.catch.yr/wkly.catch.target), left="Catch (mt)")

ggsave(path = "Figures", filename = "weekly catches.png", 
       plot = weekly.catches, dpi = 300, height = 7.5, width = 6.5)


# ..Fishery Maps Nonconfidential -------------------------------------------
#Downloaded most fish file on 8/8/22 from
#https://www.fisheries.noaa.gov/resource/map/spatial-data-collected-groundfish-observers-alaska

fishery.map <- read.csv("Data/MOST_FISH.csv")
fishery.map <- fishery.map %>% 
  filter(SPECN == 62)

#adjust data points for 180 line
fishery.map$LON400SQKM <- ifelse(fishery.map$LON400SQKM > 0, 
                                 fishery.map$LON400SQKM - 360, fishery.map$LON400SQKM)

#add pre and post restructuring of observer program cutoff
fishery.map$restructure <- ifelse(fishery.map$YEAR < 2013, "pre", "post")

# ...Fig.11a Fishery map pre and post-2013 restructuring -------------------
#Calculate average catch pre and post-observer program restructuring
avg.fishery.restruc <- fishery.map %>% 
  filter(YEAR>=1998) %>%  #to be same time period as IPHC data
  group_by(LAT400SQKM, LON400SQKM, restructure) %>%
  summarise(avg.catch = (mean(KG)/1000))
avg.fishery.restruc$restructure <- factor(avg.fishery.restruc$restructure,
                                          levels = c("pre", "post"),
                                          labels = c("pre-2013", "post-2013"))

fishery.map.average.2013 <- ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = avg.fishery.restruc, 
             aes(x = LON400SQKM, y = LAT400SQKM, 
                 color=avg.catch), 
             shape = 16, size = 1) +
  facet_wrap(~restructure, ncol=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.9, end = 0, guide="colourbar") +
  guides(color=guide_legend()) +
  labs(color="Average weight (t)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,-30,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))


#...Fig.6 Fishery map by year -----------------
fishery.yr <- fishery.map %>% 
  group_by(LAT400SQKM, LON400SQKM, YEAR) %>%
  summarise(tot.catch = (sum(KG)/1000))

fishery.yr <- ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = fishery.yr[fishery.yr$YEAR >= 1998,], 
             aes(x = LON400SQKM, y = LAT400SQKM, 
                 color=tot.catch), 
             shape = 16, alpha = 0.7, size = 1) +
  facet_wrap(~YEAR, ncol=4) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar") +
  guides(color=guide_legend()) +
  labs(color="Total weight (t)") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.1, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))

ggsave(path = "Figures", filename = "Fishery maps by year.png", 
       plot = fishery.yr, dpi = 300, width=6.5)



# IPHC LONGLINE SURVEY --------------------------------------------------------
# ..Fig.7 IPHC Timeseries  ----------------------------------------------------

#Cindy sent updated data 8/5/22
#Survey trends
IPHC <- read.csv("Data/IPHC_FISS_CPUE98_21.csv")
unique(IPHC$SPECIES)
IPHC <- IPHC %>% 
  filter(SPECIES == "Sleeper shark")

IPHC <- IPHC[IPHC$AREA_COMBO=="FMP (without Inside waters)",]   #same data as in assessment
IPHC$AREA <- factor(IPHC$AREA, levels=c("BSAI","GOA","CAN","WC"))

#Add column to distinguish zero catches
IPHC <- IPHC %>% 
  mutate(zero.catch = ifelse(AREA_CPUE==0, "zero", "nonzero"),
         percent.positive = N_POS_CATCH/N_STATIONS)

(IPHC.surveys <- 
  IPHC %>%  
    ggplot(aes(SURVEY_YEAR, AREA_CPUE, shape=zero.catch)) +
    geom_point() +
    geom_errorbar(aes(ymin = BOOT_LCI, ymax = BOOT_UCI), width=0.2) +
    facet_wrap(~AREA, ncol=1, scales = "free_y") +
    scale_shape_manual(values = c(16,4)) +
    labs(x = "\nYear", y = "CPUE (number of sharks/effective hooks)\n") +
    theme_pubr(border=TRUE, legend = "none") +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
          panel.grid.major.y = element_line()))

ggsave(path = "Figures", filename = "IPHC survey indices.png", 
       plot = IPHC.surveys, dpi = 300, height = 7, width = 6)


# ..Fig.9 IPHC Proportion of positive hauls  -----------------------------

IPHC.per.pos <- IPHC %>% 
  filter(AREA == "GOA"|AREA == "BSAI") %>% 
ggplot(aes(SURVEY_YEAR, percent.positive)) +
  geom_line() +
  geom_point() +
  facet_wrap(~AREA, ncol=1) +
  labs(x="\nYear", y="Proportion of stations with catch\n") +
  theme_pubr(border=TRUE) +
  theme(panel.grid.major.y = element_line())

ggsave(path = "Figures", filename = "IPHC proportion positive FMPs.png", 
       plot = IPHC.per.pos, dpi = 300, height = 5, width = 6)


#..IPHC Maps-----------
#Load and prep data
IPHC.map.data <- read.csv("Data/clean_iphc_survey_1998_2021.csv")

IPHC.map.data <- IPHC.map.data %>% 
  filter(SPECIES_COMMON_NAME == "Sleeper Shark") %>% 
  filter(FMP_AREA == "BSAI" | FMP_AREA == "GOA") %>% 
  filter(OBS_CATCH > 0)

#adjust data points for 180 line
IPHC.map.data$START_LON <- ifelse(IPHC.map.data$START_LON > 0, 
                                  IPHC.map.data$START_LON - 360, IPHC.map.data$START_LON)

#...Fig.8 map by year --------------
IPHC.maps.year <- ggplot() +
  facet_wrap(~SURVEY_YEAR, ncol=4) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = IPHC.map.data, 
             aes(x = START_LON, y = START_LAT, color=OBS_CATCH), 
             shape = 16, alpha = 0.7, size = 1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar", 
                        breaks = c(1,5,10,15,20,25,30)) +
  labs(color = "Number of sharks") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.1, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))

ggsave(path = "Figures", filename = "IPHC maps by year.png", 
       plot = IPHC.maps.year, dpi = 300, width=6.5)


#...Fig.11b map averaged according to observer program restructuring periods -------
#add pre and post restructuring of observer program cutoff
IPHC.map.data$restructure <- ifelse(IPHC.map.data$SURVEY_YEAR < 2013, "pre", "post")
IPHC.map.data$restructure <- factor(IPHC.map.data$restructure,
                                          levels = c("pre", "post"),
                                          labels = c("pre-2013", "post-2013"))

avg.IPHC.2013 <- IPHC.map.data %>% 
  group_by(STATION, restructure) %>%
  summarise(avg.catch = mean(OBS_CATCH),
            startlon = START_LON,
            startlat = START_LAT)

IPHC.map.2013 <- ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = avg.IPHC.2013, 
             aes(x = startlon, y = startlat, color=avg.catch), 
             shape = 16, alpha = 0.7, size = 1) +
  facet_wrap(~restructure, ncol=1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.9, end = 0, guide="colourbar") +
  guides(color=guide_legend()) +
  labs(color = "Average number of sharks") +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10), #change legend text font size
        legend.box.margin = margin(-10,-10,-30,-10),
        legend.box.spacing = unit(0,'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(-0.5,-0.5,-0.5,-0.5, "pt"))

#.Fig.11 both panels - Join IPHC and fishery maps --------------
(combo.2013.restructure.map <- fishery.map.average.2013 + IPHC.map.2013 +
    theme(plot.margin = margin(0,0,-10,0,"pt")))

ggsave(path = "Figures", filename = "Fishery and IPHC maps restructuring.png", 
       plot = combo.2013.restructure.map, dpi = 300)

# RACE BOTTOM TRAWL -----------------------------------------------------

RACE.trawl <- read.csv("Data/RACE_Biomass_Sharks.csv")
RACE.trawl <- RACE.trawl[RACE.trawl$Group == "Pacific Sleeper Shark",]
RACE.trawl <- RACE.trawl[RACE.trawl$SURVEY == RACE.trawl$REGULATORY_AREA_NAME,] #don't double-count
RACE.trawl$Per_pos_haul <- RACE.trawl$CATCH_COUNT/RACE.trawl$HAUL_COUNT*100
#rename survey areas for prettier facets
RACE.trawl$SURVEY <- factor(RACE.trawl$SURVEY,
                            levels = c("EBS_SHELF", 
                                       "EBS_SLOPE", 
                                       "AI",
                                       "GOA"),
                            labels = c("EBS Shelf", 
                                       "EBS Slope", 
                                       "AI", 
                                       "GOA"))
#Add column to distinguish zero catches
RACE.trawl <- RACE.trawl %>% 
  mutate(zero.catch = ifelse(CATCH_COUNT==0, "zero", "nonzero"))


# ..Fig.10 RACE survey timeseries -------------------------------------------
(RACE.surveys <- ggplot(RACE.trawl, aes(YEAR, Biomass, shape=zero.catch)) +
    geom_point() +
    geom_errorbar(aes(ymin = Biomass - SE, ymax = Biomass + SE), width=0.2) +
    facet_wrap(~SURVEY, scales = "free_y", ncol=1) +
    scale_y_continuous(labels=scales::comma_format()) +
    scale_shape_manual(values = c(16,4)) +
    labs(x = "\nYear", y = "Biomass (t)\n") +
    theme_pubr(border=TRUE, legend = "none") +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
          panel.grid.major.y = element_line()))

ggsave(path = "Figures", filename = "RACE survey indices.png", 
       plot = RACE.surveys, dpi = 300, height = 7, width = 6)

survey.totals <- RACE.trawl %>% 
  group_by(SURVEY, YEAR) %>% 
  summarize(total = sum(Biomass))

#Min and Max Biomass by survey
survey.totals %>% 
  group_by(SURVEY) %>% 
  summarize(min = min(total),
            max = max(total))

#Percentage of hauls with PSS catch by survey
RACE.trawl %>% 
  group_by(SURVEY) %>% 
  summarise(min = min(Per_pos_haul),
            max = max(Per_pos_haul))
