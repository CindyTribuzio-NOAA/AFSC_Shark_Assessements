# Cummulative Catch file ----
# Updated 10_25_2022 by C. Tribuzio

#Setup ----
datdir<-paste(getwd(),"/Data/Annual_updates/",AYR,sep="")
cleandatdir<-paste(getwd(),"/Data/Cleaned/",AYR,sep="")

# Data ----
CAS_dat<-read.csv(paste(datdir,"/confidential_CAS_sharks",AYR,".csv",sep=""),header=T) %>% 
  filter(fmp_area!= "INSD") %>% 
  mutate(week_num = week(week_end_date))

# FMP level summary ----
FMP_dat <- CAS_dat %>% 
  group_by(year, fmp_area, week_num) %>% 
  summarise(tot_catch = sum(catch_mt)) %>% 
  mutate(week_num = as.factor(week_num))

#pull out current year data then  add it back in later
#FMP_dat2<-FMP_dat[FMP_dat$year<AYR,]
FMP_dat3<-FMP_dat %>% 
  pivot_wider(names_from = week_num, values_from = tot_catch) %>% 
  replace(is.na(.), 0) %>% 
  pivot_longer(cols = !year&!fmp_area, names_to = "week_num", values_to = "catch_mt") 

FMP_cum <- FMP_dat3 %>% 
  group_by(fmp_area, year) %>%
  summarise(cum_catch = cumsum(catch_mt), weekn = week_num) %>% 
  rename(week_num = weekn) %>% 
  left_join(FMP_dat3)


#FMP_AYR<-FMP_dat %>% 
#  filter(year == AYR) %>% 
#  pivot_wider(names_from = week_num, values_from = tot_catch) %>% 
#  replace(is.na(.), 0) %>% 
#  pivot_longer(cols = !year&!fmp_area, names_to = "week_num", values_to = "catch_mt")
#FMPAYR_cum <- FMP_AYR %>% 
#  group_by(fmp_area, year) %>%
#  summarise(cum_catch = cumsum(catch_mt), .groups = "drop") %>% 
#  select(cum_catch) %>% 
#  bind_cols(FMP_AYR)

#FMP_dat<-rbind(FMP_cum,FMPAYR_cum)

write.csv(FMP_cum, paste(cleandatdir,"/confidential_shark_FMP_cumm_catch",AYR,".csv",sep=""),row.names = F)

# Summary By Species at FMP level ----
FMP_dat <- CAS_dat %>% 
  group_by(year, fmp_area, week_num, species) %>% 
  summarise(tot_catch = sum(catch_mt)) %>% 
  mutate(week_num = as.factor(week_num))

FMP_dat3<-FMP_dat %>% 
  pivot_wider(names_from = week_num, values_from = tot_catch) %>% 
  replace(is.na(.), 0) %>% 
  pivot_longer(cols = !year&!fmp_area&!species, names_to = "week_num", values_to = "catch_mt") 

FMP_cum <- FMP_dat3 %>% 
  group_by(fmp_area, year, species) %>%
  summarise(cum_catch = cumsum(catch_mt), weekn = week_num) %>% 
  rename(week_num = weekn) %>% 
  left_join(FMP_dat3)

write.csv(FMP_cum, paste(cleandatdir,"/confidential_sharkspecies_FMP_cumm_catch",AYR,".csv",sep=""),row.names = F)


# Summary ----
# questions: what proportion of catch remains after Oct 1?

# About what week is the first week of Oct?
postweek <- CAS_dat %>% 
  filter(week_num == 40) %>% 
  group_by(year) %>% 
  summarise(mean_wk = unique(wed))
# shows week 40 is approximately Oct 1, or shortly after

#so cut off about at week 40
#how much catch happens after week 40?
#add a pre/post flag to outputs
catch_total <- CAS_dat %>% 
  group_by(year, fmp_area) %>% 
  summarise(tot_catch = sum(catch_mt))

CAS_week <- CAS_dat %>% 
  mutate(period = if_else(week_num <= 40, "Pre", "Post")) %>% 
  group_by(year, fmp_area, period) %>% 
  summarise(per_catch = sum(catch_mt)) %>% 
  left_join(catch_total) %>% 
  mutate(pcatch = per_catch/tot_catch)

mean_per_catch <- CAS_week %>% 
  filter(year > AYR-10,
         period == "Post") %>% 
  group_by(fmp_area) %>% 
  summarise(mean_prop = mean(pcatch))






FMP_summ2<-dcast(FMP_summ,Year+FMP~period)
FMP_summ2$pPost<-FMP_summ2$Post/(FMP_summ2$Post+FMP_summ2$Pre)*100

FMP_mean<-ddply(FMP_summ2,c("FMP"),summarize,Fmean=mean(pPost))

#what proportion by species catch is after week 40?
spec_dat$period<-NA
spec_dat[spec_dat$Week<=40,]$period<-"Pre"
spec_dat[spec_dat$Week>40,]$period<-"Post"
spec_dat_summ<-ddply(spec_dat,c("Year","FMP","Species","period"),summarize,Tot_Catch=sum(Total_Catch))
spec_dat_summ2<-dcast(spec_dat_summ,Year+FMP+Species~period)
spec_dat_summ2$pPost<-spec_dat_summ2$Post/(spec_dat_summ2$Post+spec_dat_summ2$Pre)*100

spec_dat_mean<-ddply(spec_dat_summ2,c("FMP","Species"),summarize,spec_mean=mean(pPost))
