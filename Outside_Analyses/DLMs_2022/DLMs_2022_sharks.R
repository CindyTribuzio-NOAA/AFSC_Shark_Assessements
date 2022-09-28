# Title: shark DLMs 2022 ----
# Updated: Sept 7 2022
# Recent Author: Cindy Tribuzio

# To do ----
#1) Automate catch data query

# Setup ----
libs <- c("tidyverse", "janitor", "lubridate", "RODBC", "patchwork")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)
'%nin%'<-Negate('%in%') #this is a handy function

datadir<-paste(getwd(),"/Outside_Analyses/DLMs_2022/",sep="")
outdir<-paste(getwd(),"/Outside_Analyses/DLMs_2022/",sep="")
#shapedir <- paste(getwd(),"/Obj1_obs_size_method/Data/Shapefile/",sep="")

AYR <- year(Sys.time())

fun_ORCScatch<-function(DATA, Status){
  if(Status == "Underexploited"){
    ORCS_out <- quantile(DATA$tot_catch, 0.9)
  }
  if(Status == "Fully Exploited"){
    fun_dat <- DATA %>% filter(year >= AYR-10)
    ORCS_out <- quantile(fun_dat$tot_catch, 0.25)
  }
  if(Status == "Overexploited"){
    ORCS_out <- quantile(DATA$tot_catch, 0.1)
  }
  ORCS_out
}

theme_doc<- function(base_size = 12, base_family = "Helvetica") { #this function sets the theme for the whole figure
  theme_bw(base_size = base_size, base_family = base_family) %+replace% #also note that this creates a bunch of font warnings that are not a real problem, I just haven't dealt with it yet
    theme(
      plot.title=element_text(size=20,colour='black',hjust = 0.5),
      plot.background=element_blank(),
      panel.grid.minor=element_blank(),
      panel.grid.major=element_blank(),
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
      strip.text.x=element_text(size=12,colour='black'),
      strip.text.y=element_text(size=12,colour='black'),
    )
}


# establish database connection
#dbname <- "akfin"
#db <- read_csv('database.csv')
#assign(paste0("username_", dbname), db %>% filter(database == dbname) %>% select(username))
#assign(paste0("password_", dbname), db %>% filter(database == dbname) %>% select(password))

#assign(paste0("channel_", dbname), odbcConnect(dbname, uid = username_afsc, pwd = password_afsc, believeNRows=FALSE))

# Bring in data ----
catchdat <- read_csv(paste0(datadir, "confidential_GFTCBF.csv")) %>% 
  clean_names() %>% 
  filter(year < AYR & year >= 2003) %>% 
  mutate(species)

old_catch <- read_csv(paste0(datadir, "pre2003_shark_cleaned.csv")) %>% 
  clean_names() %>% 
  filter(fmp_area == "GOA" & year >= 1997) %>% 
  mutate(species)

# Clean up catch data ----
# makes naming conventions consistent
catchdat[catchdat$species=="\"shark, spiny dogfish\"", c("species")]<-"Spiny dogfish"
catchdat[catchdat$species=="\"Pacific sleeper shark\"", c("species")]<-"Pacific sleeper shark"
catchdat[catchdat$species=="\"shark, salmon\"", c("species")]<-"Salmon shark"
catchdat[catchdat$species=="\"shark, other\"", c("species")]<-"Other sharks"

old_catch[old_catch$species=="Spiny Dogfish", c("species")]<-"Spiny dogfish"
old_catch[old_catch$species=="Pacific Sleeper Shark", c("species")]<-"Pacific sleeper shark"
old_catch[old_catch$species=="Salmon Shark", c("species")]<-"Salmon shark"
old_catch[old_catch$species=="Other Sharks", c("species")]<-"Other sharks"

# making ORCS data sets
BSAI <- catchdat %>% 
  group_by(fmp_area, species, year) %>% 
  summarize(tot_catch = sum(catch_mt)) %>% 
  filter(year >= 2003 & fmp_area == "BSAI")

GOA <- catchdat %>% 
  group_by(fmp_area, species, year) %>% 
  summarize(tot_catch = sum(catch_mt)) %>% 
  filter(species != "Spiny dogfish" & fmp_area == "GOA")

old_GOA <- old_catch %>% 
  group_by(fmp_area, species, year) %>% 
  summarize(tot_catch = sum(catch_mt)) %>% 
  filter(species != "Spiny dogfish" & fmp_area == "GOA")

catch2 <- BSAI %>% bind_rows(GOA, old_GOA)

# Catch trends in recent 5 years ----
catch5yr <- catch2 %>% filter(year >= AYR-5)
catch5yr$species <- factor(catch5yr$species,levels=c("Pacific sleeper shark","Salmon shark",
                                                     "Other sharks","Spiny dogfish"))
catch5fig <- ggplot(catch5yr, aes(x = year, y = tot_catch)) +
  geom_point()+
  geom_line()+
  geom_smooth(method='lm')+
  labs(x = "Year", y = "Catch (t)")+
  scale_x_continuous(breaks=seq(2018,2021,2))+
  facet_grid(fmp_area~species)+
  theme_doc()

ggsave(path = outdir, "5yrCatchLM.png",plot=catch5fig,dpi=600)

unqkey <- unique(catch5yr[c("fmp_area", "species")])

lm_out <- data.frame(matrix(nrow = nrow(unqkey), ncol = 4))
colnames(lm_out) <- c('fmp_area', 'species', 'slope', 'slope_P')

for (i in 1:nrow(unqkey)){
  loopdat <- catch5yr %>% filter(fmp_area == unqkey[i,1] &
                                 species == unqkey[i,2])
  loop_lm <- loopdat %>% lm(tot_catch ~ year, .) %>% summary
  loop_out <- bind_cols(unqkey[i,1], unqkey[i,2], 
                        data.frame(loop_lm$coefficients[2,1]), 
                        data.frame(loop_lm$coefficients[2,4]))
  lm_out[i,] <- loop_out
}

# identify which have sig slopes ----
lm_sig <- lm_out %>%  filter(slope_P < 0.05)

# ORCS ----
ORCS_metrics <- read_csv(paste0(datadir, "ORCS_est_scalars.csv"))

ORCS_metrics_short <- unique(ORCS_metrics[c("status", "statistic")])
  

shark_ORCS <- read_csv(paste0(datadir, "ORCS_sharks_attributes.csv"))

ORCS_scored <- shark_ORCS %>% 
  group_by(species, fmp_area, score_type) %>% 
  summarise(mean_score = mean(score)) %>% 
  mutate(status = if_else(mean_score <1.5, "Underexploited",
                          if_else(mean_score > 2.5, "Overexploited", "Fully Exploited"))) %>% 
  left_join(ORCS_metrics_short)

catch_stat <- data.frame(matrix(nrow = nrow(ORCS_scored), ncol = 4))
colnames(catch_stat) <- c("fmp_area", "species", "score_type", "stat_value")

for (i in 1:nrow(ORCS_scored)){
  loopdat <- catch2 %>% filter(fmp_area == ORCS_scored[i,2] &
                                 species == ORCS_scored[i,1])
  loopstat <- ORCS_scored[i,5]
  stat1 <- fun_ORCScatch(loopdat, loopstat)
  loop_out <- data.frame(c(ORCS_scored[i,2], ORCS_scored[i,1], ORCS_scored[i,3]), (stat1[1]))
  catch_stat[i,] <- loop_out
}

ORCS_scored <- ORCS_scored %>% 
  left_join(catch_stat) %>% 
  mutate(scalar_type = "S_50th") %>% 
  left_join(ORCS_metrics) %>% 
  select(!criteria) %>% 
  mutate(OFL = round(stat_value*scalar_value, 0),
         ABC = round(OFL*0.75, 0))

write_csv(ORCS_scored, paste0(outdir, "/ORCS_sharks_OFLABC.csv", sep = ""))

# Constant Catch Methods ----
CC_output <- catch5yr %>% 
  group_by(species, fmp_area) %>% 
  summarize(mean_catch = mean(tot_catch),
            CC1 = round(mean(tot_catch), 0),
            CC2 = round(mean(tot_catch)*0.9, 0),
            CC3 = round(mean(tot_catch)*0.8, 0),
            CC4 = round(mean(tot_catch)*0.7, 0),
            CC5 = round(mean(tot_catch)*0.6, 0)) %>% 
  pivot_longer(!c(species, fmp_area, mean_catch), names_to = "CC_method", values_to = "CC_OFL") %>% 
  mutate(CC_ABC = round(0.75*CC_OFL, 0))

write_csv(CC_output, paste0(outdir, "/CC_sharksOFL.csv", sep = ""))

# Rare species Percentiles ----
rare_catch <- catch2 %>% 
  filter(species == "Other sharks" | 
         species == "Spiny dogfish" & fmp_area == "BSAI") %>% 
  filter(fmp_area == "BSAI" & year <=2015 |
           fmp_area == "GOA" & year <= 2007) %>% 
  group_by(fmp_area, species) %>% 
  summarize(OFL = round(quantile(tot_catch, 0.9), 0),
            ABC = round(quantile(tot_catch, 0.9)*0.75, 0))

# Harvest Recommendations Figs ----
# make catch time series for full complex and species
old_GOA2 <- old_catch %>% 
  group_by(fmp_area, species, year) %>% 
  summarize(tot_catch = sum(catch_mt)) %>% 
  filter(fmp_area == "GOA")

spec_catch <- catchdat %>% 
  group_by(fmp_area, species, year) %>% 
  summarize(tot_catch = sum(catch_mt)) %>% 
  bind_rows(old_GOA2)

comp_catch <- spec_catch %>% 
  group_by(fmp_area, year) %>% 
  summarize(t_catch = sum(tot_catch)) %>% 
  rename(tot_catch = t_catch) %>% 
  mutate(species = "Shark complex")

catch3 <- spec_catch %>% 
  bind_rows(comp_catch) %>% 
  filter(fmp_area != "INSD")

#make catch for proposed ABC subdivisions
GOA_SD <- catch3 %>% 
  filter(species == "Spiny dogfish" & fmp_area == "GOA")
GOA_T6 <- catch3 %>% 
  filter(fmp_area == "GOA" & species != "Spiny dogfish") %>%
  filter(species != "Shark complex") %>% 
  group_by(year) %>% 
  summarize(catch_t = sum(tot_catch)) %>% 
  rename(tot_catch = catch_t) %>% 
  mutate(species = "Tier 6",
         fmp_area = "GOA")
BSAI_PSS <- catch3 %>% 
  filter(species == "Pacific sleeper shark" & fmp_area == "BSAI")
BSAI_OT6 <- catch3 %>% 
  filter(fmp_area == "BSAI" & species != "Pacific sleeper shark") %>% 
  filter(species != "Shark complex") %>% 
  group_by(year) %>% 
  summarize(catch_t = sum(tot_catch)) %>% 
  rename(tot_catch = catch_t) %>% 
  mutate(species = "Tier 6",
         fmp_area = "BSAI")

catch4 <- GOA_SD %>% 
  bind_rows(GOA_T6, BSAI_PSS, BSAI_OT6, comp_catch) %>% 
  filter(fmp_area != "INSD")
catch4$species <- factor(catch4$species,levels=c("Pacific sleeper shark","Spiny dogfish",
                                                 "Tier 6", "Shark complex"))
OFLs <- read_csv(paste0(datadir, "Har_rec_compare.csv"))
OFLs$species <- factor(OFLs$species,levels=c("Pacific sleeper shark","Spiny dogfish",
                                             "Tier 6", "Shark complex"))
catchABCfig_GOA <- ggplot(catch4 %>% filter(fmp_area == "GOA" & species != "Shark complex"), 
                          aes(x = year, y = tot_catch, color = species)) +
  geom_point(size = 3, shape = 18)+
  geom_line()+
  geom_hline(data = OFLs %>%  filter(fmp_area == "GOA" & species != "Shark complex"), 
             aes(yintercept = ABC), linetype = "dashed")+
  labs(x = "Year", y = "Catch (t)")+
  facet_grid(species~., scales = "free")+
  theme(
    plot.title=element_text(size=20,colour='black',hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='black'),
    axis.line.y = element_line(colour='black'),
    axis.text=element_text(size=12,colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',angle=90),
    axis.title.x=element_text(colour='black'),
    legend.position = "none",
    strip.background=element_blank(),
    strip.text.x=element_text(size=12,colour='black'),
    strip.text.y = element_text(size=12,colour='black')
  )

catchCOMPLEXfig_GOA <- ggplot(catch4 %>% filter(fmp_area == "GOA" & species == "Shark complex"), 
                          aes(x = year, y = tot_catch, color = species)) +
  geom_point(size = 3, shape = 18, color = "blue")+
  geom_line(color = "blue")+
  geom_hline(data = OFLs %>%  filter(fmp_area == "GOA" & species == "Shark complex"), 
             aes(yintercept = ABC), linetype = "dashed")+
  geom_hline(data = OFLs %>%  filter(fmp_area == "GOA" & species == "Shark complex"), 
             aes(yintercept = OFL))+
  labs(x = "Year", y = "Catch (t)")+
  facet_grid(species~., scales = "free")+
  theme(
    plot.title=element_text(size=20,colour='black',hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='black'),
    axis.line.y = element_line(colour='black'),
    axis.text=element_text(size=12,colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',angle=90),
    axis.title.x=element_text(colour='black'),
    legend.position = "none",
    strip.background=element_blank(),
    strip.text.x=element_text(size=12,colour='black'),
    strip.text.y = element_text(size=12,colour='black')
  )

catchABCOFL_fig <- catchABCfig_GOA + catchCOMPLEXfig_GOA

ggsave(path = outdir, "GOA_alternatives.png",plot=catchABCOFL_fig,dpi=600)

# catch figs
catch3$species <- factor(catch3$species,levels=c("Pacific sleeper shark","Salmon shark",
                                                 "Other sharks","Spiny dogfish", "Shark complex"),
                         labels = c("Pacific sleeper shark","Salmon shark",
                                    "Other sharks","Spiny dogfish", "Shark complex"))
catchfig_BSAI <- ggplot(catch3 %>% filter(fmp_area == "BSAI"), aes(x = year, y = tot_catch,
                                                                   color = species, shape = fmp_area)) +
  geom_point(size = 3)+
  geom_line()+
  #geom_hline(data = OFLs %>%  filter(fmp_area == "BSAI"), aes(yintercept = OFL, linetype = model))+
  labs(x = "Year", y = "Catch (t)")+
  facet_grid(species~fmp_area, scales = "free")+
 # scale_x_continuous(breaks = seq(1997, 2021))+
  theme(
    plot.title=element_text(size=20,colour='black',hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='black'),
    axis.line.y = element_line(colour='black'),
    axis.text=element_text(size=12,colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',angle=90),
    axis.title.x=element_text(colour='black'),
    legend.position = "none",
    strip.background=element_blank(),
    strip.text.x=element_text(size=12,colour='black'),
    strip.text.y = element_blank()
  )

catchfig_GOA <- ggplot(catch3 %>% filter(fmp_area == "GOA"), aes(x = year, y = tot_catch,
                                                                   color = species)) +
  geom_point(size = 3, shape = 18)+
  geom_line()+
  #geom_hline(data = OFLs %>%  filter(fmp_area == "GOA"), aes(yintercept = OFL, linetype = model))+
  labs(x = "Year", y = "")+
  facet_grid(species~fmp_area, scales = "free", labeller = label_wrap_gen(10))+
  theme(
    plot.title=element_text(size=20,colour='black',hjust = 0.5),
    plot.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(colour='black'),
    axis.line.y = element_line(colour='black'),
    axis.text=element_text(size=12,colour='black'),
    axis.ticks=element_line(colour='black'),
    axis.title.y=element_text(colour='black',angle=90),
    axis.title.x=element_text(colour='black'),
    legend.position = "none",
    strip.background=element_blank(),
    strip.text.x=element_text(size=12,colour='black'),
    strip.text.y = element_text(size=12,colour='black')
  )

tot_catch_fig <- catchfig_BSAI + catchfig_GOA

ggsave(path = outdir, "shark_catch.png",plot=tot_catch_fig,dpi=600)
