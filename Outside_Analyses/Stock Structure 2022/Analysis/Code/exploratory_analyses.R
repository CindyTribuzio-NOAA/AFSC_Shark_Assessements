### SUPPLEMENTARY/EXPLORATORY ANALYSES FOR THE 2022 PACIFIC SLEEPER SHARK STOCK STRUCTURE DOCUMENT



# LENGTH DISTRIBUTIONS ----------------------------------------------------

ggplot(LW.all[!LW.all$FMP=="",], aes(TLcm, color=NMFS_Sub_A)) +
  facet_wrap(~FMP, ncol=1) +
  geom_density(size=1, trim=TRUE) +
  geom_vline(xintercept=370, linetype="dashed", color="gray50", size=1)+
  labs(x="\nTotal length (cm)", y="Density\n") +
  #scale_color_manual(labels = c("BSAI", "GOA", "West Coast"),
  #                   values = c("deeppink", "purple", "lightblue")) +
  coord_cartesian(xlim=c(0,470)) +
  theme_pubr(legend = "right") +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.grid.major.y = element_line())

#Old Figure 1 (used RACE and NORPAC queries)

#..RACE surveys-------------

RACE.lf <- read.csv("Data/race_length_by_haul.csv", skip=7)
RACE.lf <- RACE.lf[rep(1:nrow(RACE.lf), RACE.lf[["Frequency"]]), ]#expands the frequencies out
RACE.lf$TLcm <- RACE.lf$Length..mm./10

table(RACE.lf$Survey)

RACE.lf.plot <- ggplot(RACE.lf, aes(TLcm, color=Survey, linetype=Survey)) +
  geom_density(size=1, trim=TRUE) +
  geom_vline(xintercept=370, linetype="dashed", color="gray50", size=1)+
  labs(x="\nTotal length (cm)", y="Density\n") +
  scale_color_manual(labels = c("AI", "EBS Shelf", "EBS Slope", "GOA"),
                     values = c("deeppink", "pink", "brown", "purple")) +
  scale_linetype_manual(labels = c("AI", "EBS Shelf", "EBS Slope", "GOA"),
                        values = c("solid", "dashed", "dotted", "solid")) +
  coord_cartesian(xlim=c(0,450)) +
  theme_pubr(legend = "right") +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.grid.major.y = element_line())


#..NORPAC-----
NORPAC.lf <- read.csv("Data/NORPAC_PSS_lengths.csv", skip=6)
NORPAC.lf <- NORPAC.lf[rep(1:nrow(NORPAC.lf), NORPAC.lf[["Frequency"]]), ]#expands the frequencies out

table(NORPAC.lf$FMP.Area)

NORPAC.lf.plot <- ggplot(NORPAC.lf, aes(Length..cm., color=FMP.Area)) +
  geom_density(size=1, trim=TRUE) +
  geom_vline(xintercept=370, linetype="dashed", color="gray50", size=1)+
  labs(x="\nTotal length (cm)", y="Density\n", color="FMP Area") +
  scale_color_manual(labels = c("BSAI", "GOA", "West Coast"),
                     values = c("firebrick1", "purple", "lightblue")) +
  coord_cartesian(xlim=c(0,450)) +
  theme_pubr(legend = "right") +
  theme(plot.margin = margin(0,0,0,0, "cm"),
        panel.grid.major.y = element_line())

lf.combo <- RACE.lf.plot/NORPAC.lf.plot
lf.combo[[1]] <- lf.combo[[1]] + theme(axis.text.x = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.title.x = element_blank())

ggsave(path = "Figures", filename = "Length frequencies PSS.png", 
       plot = lf.combo, dpi = 300, width = 6, height = 5)


# REGIONAL GROWTH DIFFERENCES ------------------------------------------------------
#same as size summary datafile but includes FMP (note that WC records are listed as GOA)
LW <- read.csv("Data/PSS_size_joins.csv")
#subset to measured weights only and get rid of TL NA values
LW.meas <- subset(LW, Weight_typ == "measure")
LW.meas <- LW.meas %>% tidyr::drop_na(TLcm)
#remove West Coast records
LW.meas <- LW.meas %>% 
  filter(NMFS_AREA < 670)

ggplot(LW.meas, aes(TLcm, Weight, color=FMP)) +
  geom_point() +
  geom_smooth()

#Remove obvious outliers
#One individual with a zero length recorded
#40 cm individual on RACE trawl survey recorded as weighing 27.398 kg
#93 cm individual on RACE trawl survey recorded as weight 62 kg
#264 cm weighing 601 kg
#216 cm weighing 420 kg
LW.meas <- LW.meas[-c(3,347,112,292,360),]

ggplot(LW.meas, aes(TLcm, fill=FMP)) +
  geom_histogram(color="black") +
  facet_wrap(~FMP, nrow=2, scales = "free_y") +
  labs(x="\nTotal length (cm)", y="Number of sharks\n") +
  theme_pubr(legend = "none")

table(LW.meas$FMP)

ggplot(LW.meas, aes(log(TLcm), log(Weight), color=factor(FMP))) +
  geom_point(alpha=0.3, size=2) +
  geom_smooth(method="lm",se=FALSE) +
  labs(x="ln Total length (cm)", y="ln Weight (kg)", color = "") +
  theme_pubr(base_size = 11) +
  theme(legend.position = c(0.2,0.9),
        legend.background = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

lnLW.fit.area <- lm(log(Weight)~log(TLcm)*FMP, data=LW.meas)
summary(lnLW.fit.area)


# CHANGES IN LENGTH OVER TIME ------------------------------------------------------

#NORPAC, BSAI
NORPAC.lf %>% 
  filter(FMP.Area == "BSAI") %>% 
  group_by(Year) %>% 
  summarize(mean.length = mean(Length..cm.)) %>% 
  ggplot() +
  geom_line(aes(Year, mean.length)) +
  geom_point(aes(Year, mean.length))

#RACE, BSAI
RACE.lf %>% 
  filter(Survey == "EBS_SLOPE") %>% 
  group_by(Year) %>% 
  summarize(mean.length = mean(Length..mm.)) %>% 
  ggplot() +
  geom_line(aes(Year, mean.length)) +
  geom_point(aes(Year, mean.length))

#USE SIZE JOINS FILE
LW <- LW[-c(3,347,112,292,360),]
table(LW$FMP)
LW %>% 
  filter(FMP == "BSAI"|FMP=="GOA") %>% 
  filter(!is.na(TLcm)) %>% 
  group_by(Year, FMP) %>% 
  summarize(mean.length = mean(TLcm),
            se.length = plotrix::std.error(TLcm)) %>% 
  ggplot() +
  facet_wrap(~FMP, ncol=1, scales = "free_y") +
  geom_line(aes(Year, mean.length)) +
  geom_point(aes(Year, mean.length)) +
  theme_pubr()



# FISHERY -----------------------------------------------------------------

# ..Summarize  ----
#By year and NMFS area
haul_ct <- norpac.haul %>% 
  group_by(year, nmfs_area, fmp) %>% 
  summarize(n_hauls = length(haul))

sp_ct <- norpac.spcomp %>% 
  left_join(norpac.haul, by = c("cruise", "permit", "haul", "year")) %>% 
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

#..Map ----
#map averaged across all years (station hot spots)
avg.fishery <- fishery.map %>% 
  group_by(LAT400SQKM, LON400SQKM) %>%
  summarise(avg.catch = (mean(KG)/1000))

fishery.map.average <- ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = avg.fishery, 
             aes(x = LON400SQKM, y = LAT400SQKM, 
                 color=avg.catch), 
             shape = 16, size = 1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.9, end = 0, guide="colourbar") +
  guides(color=guide_legend()) +
  labs(color="Average weight (t)", size="Average weight (t)") +
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

ggsave(path = "Figures", filename = "Fishery map average catch.png", 
       plot = fishery.map.average, dpi = 300, width=6.5)

#..Catch by week-----
#GOA
#recently not a lot of catch in the winter but no other clear pattern (variable year to year)
#(artifact of target fisheries main fishing periods?)
ggplot(GOA.fishery[between(GOA.fishery$Year, 2013, 2021),], aes(Week.Number, Catch..mt.)) +
  facet_wrap(~Year, scales = "free_y") +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(0,52,5)) +
  labs(x="Week", y="Catch (mt)", title="GOA weekly catch since 2013") +
  theme_pubr(border=TRUE, base_size = 10)

#BSAI
#most of catch between weeks 24 (mid-June) and 40 (early October)
ggplot(BSAI.fishery[between(BSAI.fishery$Year, 2013, 2021),], aes(Week.Number, Catch..mt.)) +
  facet_wrap(~Year) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(0,52,5)) +
  labs(x="Week", y="Catch (mt)", title="BSAI weekly catch since 2013") +
  theme_pubr(border=TRUE, base_size = 10)

fisherycatch %>% 
  filter(FMP.Area != "Inside" & between(Year, 2013, 2021)) %>% 
  ggplot(aes(Week.Number, Catch..mt., fill=factor(Year))) +
  facet_wrap(~FMP.Subarea, ncol = 1, scales = "free_y") +
  geom_bar(stat="identity") +
  scale_fill_viridis_d() +
  labs(x = "\nWeek number", y = "Catch (mt)\n", fill="Year") +
  theme_pubr(border=TRUE, legend = "right") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line())

#Both FMPs line graph cumulative catch
fisherycatch %>% 
  filter(FMP.Area != "Inside") %>% 
  group_by(FMP.Area, Year, Week.Number) %>% 
  summarize(catch.tots = sum(Catch..mt.)) %>% 
  mutate(csum = cumsum(catch.tots)) %>% 
  filter(Year >= 2011) %>% 
  ggplot(aes(Week.Number, csum, color=factor(Year))) +
  facet_wrap(~FMP.Area, ncol = 1, scales = "free_y") +
  geom_line(size=1) +
  scale_color_viridis_d() +
  labs(x = "\nWeek number", y = "Cumulative catch (mt)\n", color = "Year") +
  theme_pubr(border=TRUE, legend = "right") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_line())


# ..Target by year --------------------------------------------------------

ggplot(fisherycatch, aes(factor(Year), Catch..mt., fill=Trip.Target.Group)) +
  geom_bar(stat="identity") +
  facet_wrap(~FMP.Area, ncol=1, scales="free_y") +
  labs(x="\nYear", y="Catch (mt)\n", fill="Target") +
  scale_fill_viridis_d(option="magma") +
  theme_pubr(border=TRUE, legend="right") +
  theme(panel.grid.major.y = element_line(),
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_blank())

# ..Proportion of positive hauls (Fig 9)------------------------------------------

#From Cindy 08/18/22
norpac.haul <- read.csv("Data/confidential_NORPAChaul_03_22.csv")
norpac.spcomp <- read.csv("Data/confidential_NORPACspcom_03_22.csv")

# By FMP Subarea
haul_ct <- norpac.haul %>% 
  group_by(year, fmp_subarea) %>% 
  summarize(n_hauls = length(haul))

sp_ct <- norpac.spcomp %>% 
  left_join(norpac.haul, by = c("cruise", "permit", "haul", "year")) %>% 
  group_by(year, fmp_subarea) %>% 
  summarize(shark_hauls = length(haul_join))

shark_obshauls <- sp_ct %>% 
  left_join(haul_ct) %>% 
  mutate(haul_prop = shark_hauls/n_hauls)

#reorder and rename FMP subareas for prettier facets
shark_obshauls$fmp_subarea <- factor(shark_obshauls$fmp_subarea,
                                     levels = c("EBS",
                                                "AI",
                                                "WGOA",
                                                "CGOA",
                                                "EGOA",
                                                "INSD",
                                                "OUT"),
                                     labels = c("EBS",
                                                "AI",
                                                "WGOA",
                                                "CGOA",
                                                "EGOA",
                                                "INSIDE",
                                                "NON-AK"))

fishery.prop.pos <- shark_obshauls %>% 
  filter(fmp_subarea != "NON-AK" & fmp_subarea != "INSIDE") %>% 
  ggplot(aes(x = year, y = haul_prop))+
  geom_point()+
  geom_line()+
  facet_grid(fmp_subarea~., scales = "free") +
  labs(x="Year", y="Proportion of hauls with catch\n") +
  theme_pubr(border=TRUE) +
  theme(panel.grid.major.y = element_line())

ggsave(path = "Figures", filename = "fishery proportion positive subareas.png", 
       plot = fishery.prop.pos, dpi = 300, height = 7, width = 6)






# IPHC --------------------------------------------------------------------

#map totaled and averaged across all years (station hot spots)
summedIPHC <- IPHC.map.data %>% 
  group_by(STATION) %>%
  summarise(tot.catch = sum(OBS_CATCH),
            avg.catch = mean(OBS_CATCH),
            startlon = START_LON,
            startlat = START_LAT)

IPHC.map.average <- ggplot() +
  geom_polygon(data = usa, aes(long, lat, group=group), fill= "#8c8c8c", color = "#c9c9c9", size = 0.2) +
  geom_polygon(data = canada, aes(long, lat, group=group), fill= "#d6d6d6", color = "#c9c9c9", size = 0.2) +
  geom_path(data = race_bathy_df, aes(x = long, y= lat, group = group), color = "#73ab6a", size = 0.2, alpha=0.4) +
  geom_count(data = summedIPHC, 
             aes(x = startlon, y = startlat, color=avg.catch), 
             shape = 16, alpha = 0.7, size = 1) +
  scale_color_viridis_c(option="plasma", space="Lab", begin = 0.9, end = 0, guide="colourbar") +
  labs(color = paste0("Average number of sharks caught ", 
                      min(IPHC.map.data$SURVEY_YEAR), " to ", max(IPHC.map.data$SURVEY_YEAR))) +
  coord_sf(crs = st_crs(4326), xlim = c(-186,-130), ylim = c(50, 65), expand = FALSE) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.key.width = unit(2.5, "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        strip.background = element_rect(fill="#b8eafc"),
        strip.text = element_text(face = "bold.italic"),
        panel.spacing = unit(0.1, "lines"),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0, "cm"))

ggsave(path = "Figures", filename = "IPHC map average catch.png", 
       plot = IPHC.map.average, dpi = 300, width=6.5)
  