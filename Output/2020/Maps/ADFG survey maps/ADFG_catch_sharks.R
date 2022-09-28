library(ggplot2)
library(dplyr)
library(ggpubr)
library(sf)
library(rnaturalearth)
library(rgdal)

#PREPARE data----------------------------------------------------------------------------------------------------------
#RACE bottom trawl survey catch data
Data <- read.csv("~/Stock Assessments/Shark Assessments/Analysis/2020/ADFG_SEAK_LL2020.csv")

Data$fYear<-factor(Data$Year)

#rearrange species from columns to rows
Data <- Data %>% tidyr::gather(Common_Name, Number_of_Fish, dogf_no:PSS_no)
Data$Common_Name <- factor(Data$Common_Name)
levels(Data$Common_Name)
levels(Data$Common_Name) <- c("spiny dogfish", "Pacific sleeper shark")

#remove zero catch values
Data <- filter(Data, Number_of_Fish > 0)


###set up map---------------------------------------------------------------------------------------------------------
#get land
world <- ne_countries(scale = "medium", returnclass = "sp")

#usa
usa <- subset(world, admin == "United States of America")
usa <- fortify(usa)
usa$long <- ifelse(usa$long > 0, usa$long - 360, usa$long)

#canada
canada <- subset(world, admin == "Canada")
canada <- fortify(canada)

#bathymetry (from downloaded OFIS bathymetry shapefile)
race_bathy = readOGR("C:\\Users\\Beth.Matta\\Work\\Age and Growth\\Yeung Flatfish EFH\\Adults survey data growth index\\race_bathy_to_200_NAD1983_HARN")
race_bathy_df = fortify(race_bathy)
race_bathy_df$long = ifelse(race_bathy_df$long > 0, race_bathy_df$long - 360, race_bathy_df$long)

#set place to save maps
setwd("C:\\Users\\Beth.Matta\\Work\\Stock Assessments\\Shark Assessments\\Analysis\\2020\\maps\\ADFG maps")


#data exploration---------------------------------------------------------------------------------------------------------
PSS <- subset(Data, Common_Name == "Pacific sleeper shark")
sum(PSS$Number_of_Fish)
max(PSS$Long)
min(PSS$Long)
max(PSS$Lat)
min(PSS$Lat)


SD <- subset(Data, Common_Name == "spiny dogfish")
sum(SD$Number_of_Fish)
max(SD$Long)
min(SD$Long)
max(SD$Lat)
min(SD$Lat)



ggplot(data = subset(Data, Common_Name == "Pacific sleeper shark"), aes(Year, Number_of_Fish)) +
     geom_bar(stat = "identity") +
     labs(title="Pacific sleeper shark catches - ADFG surveys")
ggplot(data = subset(Data, Common_Name == "spiny dogfish"), aes(Year, Number_of_Fish)) +
     geom_bar(stat = "identity") +
     labs(title="Spiny dogfish catches - ADFG surveys")




##GOA# Number of fish per station----------------------------------------------------------------------------------------------
#Sleeper sharks
GOA_ADFG_PSS_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(Data, Common_Name == "Pacific sleeper shark"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size=2) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar", breaks = c(2,4,6,8,10)) +
  scale_x_continuous(breaks = seq(-136,-131, by = 2)) +
  scale_y_continuous(breaks = seq(54,58, by = 1)) +
  labs(title="Pacific sleeper shark catches - ADFG surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-136,-131), ylim = c(54, 58.2), expand = FALSE) +
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

#spiny dogfish
GOA_ADFG_SD_number_year <- 
  ggplot() +
  facet_wrap(~Year) +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = subset(Data, Common_Name == "spiny dogfish"), 
             aes(x = Long, y = Lat, color=Number_of_Fish), 
             shape = 16, alpha = 0.7, size=2) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.8, end = 0, guide="colourbar") +
  scale_x_continuous(breaks = seq(-136,-131, by = 2)) +
  scale_y_continuous(breaks = seq(54,58, by = 1)) +
  labs(title="Spiny dogfish catches - ADFG surveys", color = "Number of sharks") +
  coord_sf(crs = st_crs(4274), xlim = c(-136,-131), ylim = c(54, 58.2), expand = FALSE) +
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


ggsave("GOA_ADFG_PSS_number_per_year.png", plot=GOA_ADFG_PSS_number_year, width = 11, height = 7, dpi=600)
ggsave("GOA_ADFG_SD_number_per_year.png", plot=GOA_ADFG_SD_number_year, width = 11, height = 7, dpi=600)


