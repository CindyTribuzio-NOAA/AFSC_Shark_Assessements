library(ggplot2)
library(dplyr)
library(ggpubr)
library(sf)
library(rnaturalearth)
library(rgdal)

#PREPARE data----------------------------------------------------------------------------------------------------------
#RACE bottom trawl survey catch data
Data <- read.csv("~/Stock Assessments/Shark Assessments/Analysis/2020/final_iphc_survey_1998_2019_sharks.csv")


#rename columns
names(Data)[names(Data) == "spp_common"] <- "Common_Name"
names(Data)[names(Data) == "year"] <- "Year"
names(Data)[names(Data) == "startlat"] <- "Lat"
names(Data)[names(Data) == "startlon"] <- "Long"
names(Data)[names(Data) == "obs_catch"] <- "Number_of_Fish"

levels(Data$Common_Name)
levels(Data$FMP)

#remove zero catch values
Data <- filter(Data, Number_of_Fish > 0)

#adjust data points for 180 line
Data$Long = ifelse(Data$Long > 0, Data$Long - 360, Data$Long)

GOA <- subset(Data, FMP == "GOA")
BSAI <- subset(Data, FMP == "BSAI")


###set up map---------------------------------------------------------------------------------------------------------
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

#bathymetry (from downloaded OFIS bathymetry shapefile)
race_bathy = readOGR("C:\\Users\\Beth.Matta\\Work\\Age and Growth\\Yeung Flatfish EFH\\Adults survey data growth index\\race_bathy_to_200_NAD1983_HARN")
race_bathy_df = fortify(race_bathy)
race_bathy_df$long = ifelse(race_bathy_df$long > 0, race_bathy_df$long - 360, race_bathy_df$long)

#set place to save maps
setwd("C:\\Users\\Beth.Matta\\Work\\Stock Assessments\\Shark Assessments\\Analysis\\2020\\maps\\IPHC maps")


#data exploration---------------------------------------------------------------------------------------------------------
PSS <- subset(Data, Common_Name == "Sleeper Shark")
sum(PSS$Number_of_Fish)
max(PSS$Long)
min(PSS$Long)
max(PSS$Lat)
min(PSS$Lat)

SD <- subset(Data, Common_Name == "Spiny Dogfish")
sum(SD$Number_of_Fish)
max(SD$Long)
min(SD$Long)
max(SD$Lat)
min(SD$Lat)

SS <- subset(Data, Common_Name == "Salmon Shark")
sum(SS$Number_of_Fish)
max(SS$Long)
min(SS$Long)
max(SS$Lat)
min(SS$Lat)


ggplot(data = subset(Data, Common_Name == "Sleeper Shark"), aes(Year, Number_of_Fish)) +
  facet_wrap(~FMP, scales = "free") +
     geom_bar(stat = "identity") +
     labs(title="Sleeper Shark catches - IPHC surveys")
ggplot(data = subset(Data, Common_Name == "Spiny Dogfish"), aes(Year, Number_of_Fish)) +
  facet_wrap(~FMP, scales = "free") +
     geom_bar(stat = "identity") +
     labs(title="Spiny Dogfish catches - IPHC surveys")
ggplot(data = subset(Data, Common_Name == "Salmon Shark"), aes(Year, Number_of_Fish)) +
  facet_wrap(~FMP, nrow=2) +
  geom_bar(stat = "identity") +
  labs(title="Salmon shark catches - IPHC surveys")



##GOA# Number of fish per station----------------------------------------------------------------------------------------------
#Sleeper sharks
GOA_IPHC_PSS_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(GOA, Common_Name == "Sleeper Shark"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size = 1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar", breaks = c(1,5,10,15,20,25)) +
  scale_x_continuous(breaks = seq(-170,-130, by = 10)) +
  scale_y_continuous(breaks = seq(52,62, by = 4)) +
  labs(title="Sleeper Shark catches - IPHC surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-171,-130), ylim = c(52, 62), expand = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid = element_blank())

#Spiny Dogfish
GOA_IPHC_SD_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(GOA, Common_Name == "Spiny Dogfish"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size = 1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar") +
  scale_x_continuous(breaks = seq(-170,-130, by = 10)) +
  scale_y_continuous(breaks = seq(52,62, by = 4)) +
  labs(title="Spiny Dogfish catches - IPHC surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-171,-130), ylim = c(52, 62), expand = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid = element_blank())

#Salmon Shark
GOA_IPHC_SS_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(GOA, Common_Name == "Salmon Shark"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size=2) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar") +
  scale_x_continuous(breaks = seq(-170,-130, by = 10)) +
  scale_y_continuous(breaks = seq(52,62, by = 4)) +
  labs(title="Salmon Shark catches - IPHC surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-171,-130), ylim = c(52, 62), expand = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid = element_blank())

#Blue Shark
GOA_IPHC_BS_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(GOA, Common_Name == "Blue Shark"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size=2) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar") +
  scale_x_continuous(breaks = seq(-170,-130, by = 10)) +
  scale_y_continuous(breaks = seq(52,62, by = 4)) +
  labs(title="Blue Shark catches - IPHC surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-171,-130), ylim = c(52, 62), expand = FALSE) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid = element_blank())

ggsave("GOA_IPHC_PSS_number_per_year.png", plot=GOA_IPHC_PSS_number_year, width = 11, height = 7, dpi=600)
ggsave("GOA_IPHC_SD_number_per_year.png", plot=GOA_IPHC_SD_number_year, width = 11, height = 7, dpi=600)
ggsave("GOA_IPHC_SS_number_per_year.png", plot=GOA_IPHC_SS_number_year, width = 11, height = 7, dpi=600)
ggsave("GOA_IPHC_BS_number_per_year.png", plot=GOA_IPHC_BS_number_year, width = 11, height = 7, dpi=600)


##BSAI# Number of fish per station----------------------------------------------------------------------------------------------
#Sleeper sharks
BSAI_IPHC_PSS_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(BSAI, Common_Name == "Sleeper Shark"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size = 2) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar", breaks = c(2,4,6,8)) +
  labs(title="Sleeper Shark catches - IPHC surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4326), xlim = c(-190,-156), ylim = c(51, 62), expand = FALSE, datum = NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid = element_blank())

#Spiny Dogfish
BSAI_IPHC_SD_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(BSAI, Common_Name == "Spiny Dogfish"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size = 2) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar") +
  scale_x_continuous(breaks = seq(-170,-130, by = 10)) +
  scale_y_continuous(breaks = seq(52,62, by = 4)) +
  labs(title="Spiny Dogfish catches - IPHC surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4326), xlim = c(-190,-156), ylim = c(51, 62), expand = FALSE, datum = NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid = element_blank())

#Salmon Shark
BSAI_IPHC_SS_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(BSAI, Common_Name == "Salmon Shark"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size = 2) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar", breaks = c(1,2)) +
  scale_x_continuous(breaks = seq(-170,-130, by = 10)) +
  scale_y_continuous(breaks = seq(52,62, by = 4)) +
  labs(title="Salmon Shark catches - IPHC surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4326), xlim = c(-190,-156), ylim = c(51, 62), expand = FALSE, datum = NA) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.5, "lines"),
        panel.grid = element_blank())


ggsave("BSAI_IPHC_PSS_number_per_year.png", plot=BSAI_IPHC_PSS_number_year, width = 11, height = 7, dpi=600)
ggsave("BSAI_IPHC_SD_number_per_year.png", plot=BSAI_IPHC_SD_number_year, width = 11, height = 7, dpi=600)
ggsave("BSAI_IPHC_SS_number_per_year.png", plot=BSAI_IPHC_SS_number_year, width = 11, height = 7, dpi=600)

