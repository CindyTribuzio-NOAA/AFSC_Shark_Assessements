# Compute RACE Trawl Survey Biomass ----
#Updated 8/31/2020 by C. Tribuzio
#This function will estimate biomass for all surveys and sub-regions
#I have not yet built in the flexibility to do separate depths

library(plyr)
# Biomass Function ----
#Biomass estimation function, includes formatting data from AKFIN
#Estimates biomass for each of the species/species groups provided
RACE_BIOMASS<-function(Species,outname,SYR,datapath,outpath){
  
  # Data prep ----
  print("Importing and formating data")
  print("Have patience, you must. Large this data file is.")
  
  #RACE haul data
  HAUL<-read.csv(paste(datapath,"/RACE_HAUL",SYR,".csv",sep=""),header=T)
  HAUL<-HAUL[,-c(1:2)]
  colnames(HAUL)<-c("SURVEY","YEAR","VESSEL","CRUISE","HAUL","HAULJOIN","STRATUM","INPFC","REG_AREA","STRATUM_DESC","HAUL_TYPE","PERFORMANCE",
                    "START_TIME","DURATION","DISTANCE_FISHED","NET_WIDTH","NET_MEASURED","NET_HEIGHT","START_LATITUDE","START_LONGITUDE",
                    "END_LATITUDE","END_LONGITUDE","STATIONID","GEAR_DEPTH","BOTTOM_DEPTH","BOTTOM_TYPE","SURFACE_TEMPERATURE","GEAR_TEMPERATURE",
                    "WIRE_LENGTH","GEAR","ACCESSORIES","SUBSAMPLE","CRUISEJOIN","AUDITJOIN","SAT_PERF","NOTE")
  haul_col<-colnames(HAUL)
  HAUL<-HAUL[!is.na(HAUL$STRATUM),] #gets rid of hauls with no stratum info
  HAUL<-HAUL[HAUL$PERFORMANCE>-0.01,] #gets rid of poor performance hauls
  HAUL<-HAUL[HAUL$HAUL_TYPE==3,] #gets rid of experimental hauls
  HAUL<-HAUL[HAUL$STRATUM %nin% c(82,90),] #these are only in EBS_SHELF and not used in biomass calcs
  HAUL<-HAUL[HAUL$GEAR %nin% c(706,707),] #these are only in the AI in 1980
  HAUL<-HAUL[!(HAUL$SURVEY=="GOA" & HAUL$YEAR==1999 & is.na(HAUL$NET_WIDTH)),] #there are hauls with NET_WIDTH=0/NA that
  #are included in the haul counts. This only removes the GOA 1999 ones that are not part of haul count,
  #see emails with Wayne Palsson 8/26/2020 title RACE Haul Counts
  
  #RACE stratum information
  STRATUM<-read.csv(paste(datapath,"/RACE_STRATUM",SYR,".csv",sep=""),header=T)
  STRATUM<-STRATUM[,-c(1:2)]
  colnames(STRATUM)<-c("SURVEY","STRATUM","AREA","PERIMETER","INPFC_AREA","MIN_DEPTH","MAX_DEPTH","DESCRIPTION","SUMMARY_AREA","SUMMARY_DEPTH",
                       "SUMMARY_AREA_DEPTH","REGULATORY_AREA_NAME","STRATUM_TYPE")  
  
  #RACE catch
  CATCH<-read.csv(paste(datapath,"/RACE_CATCH",SYR,".csv",sep=""),header=T,skip=6)
  CATCH<-CATCH[,-ncol(CATCH)]
  colnames(CATCH)<-c("SURVEY","YEAR","CRUISEJOIN","HAULJOIN","CATCHJOIN","CRUISE","VESSEL","HAUL","START_LATITUDE","START_LONGITUDE",
                     "END_LATITUDE","END_LONGITUDE","STRATUM","INPFC","MIN_DEPTH","MAX_DEPTH","DESCRIPTION","REGULATORY_AREA_NAME",
                     "STRATUM_TYPE","DOMAIN","DENSITY","PERFORMANCE","SAT_PERF","PERF_NOTE","GEAR_DEPTH","BOTTOM_DEPTH",
                     "SPECIES_CODE","COMM_NAME","SCI_NAME","WEIGHT","NUMBER_FISH","SUBSAMPLE_CODE","VOUCHER")
    #CATCH has some weird rows in 2016 due to someone inputting a "," in the common name
  CATCH[grep("Stone et al",CATCH$SCI_NAME),] #4 rows from the Aleutian Islands
  nrow(CATCH)
  CATCH<-CATCH[-grep("Stone et al",CATCH$SCI_NAME),]
  nrow(CATCH)#should be 4 less rows
  CATCH$WEIGHT<-as.numeric(as.character(CATCH$WEIGHT))
  
  #RACE surveys
  SURVEYS<-read.csv(paste(datapath,"/RACE_SURVEYS",SYR,".csv",sep=""),header=T)
  SURVEYS<-SURVEYS[,-c(1:2)]
  colnames(SURVEYS)<-c("SURVEY","VESSEL","CRUISE","START_DATE","END_DATE","MIN_LATITUDE","MAX_LATITUDE", 
                       "MIN_LONGITUDE","MAX_LONGITUDE","AGENCY_NAME","SURVEY_NAME","YEAR","CRUISEJOIN")  
  survey_list<-unique(SURVEYS$SURVEY_NAME)
  survey<-SURVEYS[SURVEYS$SURVEY_NAME%in%survey_list,] #creates a short list of all region surveys used to estimate biomass
  
  #combining data into useful dataframes
  catch<-CATCH[CATCH$CRUISEJOIN%in%survey$CRUISEJOIN,]
  haul<-HAUL[HAUL$CRUISEJOIN%in%survey$CRUISEJOIN,]
  year<-as.numeric(names(table(haul$YEAR))) #is this used anywhere?
  #get rid of excess columns, this will clean stuff up later
  haul<-haul[,c("SURVEY","CRUISEJOIN","HAULJOIN","HAUL","STRATUM","DISTANCE_FISHED","NET_WIDTH","YEAR")]
  haul<-merge(haul,STRATUM[,c("SURVEY","STRATUM","REGULATORY_AREA_NAME")],by=c("SURVEY","STRATUM"),all.x=T)
  haul$REGULATORY_AREA_NAME<-ifelse(haul$REGULATORY_AREA_NAME=="",as.character(haul$SURVEY),as.character(haul$REGULATORY_AREA_NAME))
  
  catch<-catch[,c("YEAR","SURVEY","CRUISEJOIN","HAULJOIN","SPECIES_CODE","WEIGHT")]
  catch<-droplevels(catch)
  #clean out bad hauls based on unique haul joins
  haul_list<-haul$HAULJOIN
  #match catch hauls to good hauls in haul_list
  catch<-catch[catch$HAULJOIN%in%haul_list,]
  
  # Analysis Section ----
  # Get haul data and catch data in one object
  print("OK, done with that, now estimating biomass with variance")
  catch2<-merge(catch,haul,by=c("SURVEY","CRUISEJOIN","HAULJOIN","YEAR"),all.x=TRUE) # outer join
  
  loopmat<-matrix(ncol=7,nrow=0)
  for (i in 1:ifelse(length(Species)>1,
                     length(Species$species),
                     length(Species))) {
    ### Extract catch in survey years and get rid of unwanted species
    ifelse(length(Species)==1, 
           spec<-Species,
           spec<-as.data.frame(Species$species[i]))
    ifelse(length(Species)==1, 
           outn<-outname,
           outn<-Species$outname[i][[1]])
    temp<-catch2[catch2$YEAR%in%year,]
    temp<-temp[temp$SPECIES_CODE%in%spec,] 
    if(nrow(temp)==0){
      print(paste(Species$outname[i][[1]],'Species has no biomass'))
      next
    }
    ### Get an object with all hauljoins, but no duplicates
    temp2<-temp[!duplicated(temp$HAULJOIN),] #should this just be a list of unique hauljoins then?  temp2<-unique(temp$HAULJOIN)
    ### Calculated combined catch for multiple species
    cpue<-catch2[catch2$SPECIES_CODE %in% spec,] #these steps create a dummy species that can be used to sum over
    cpue<-cpue[complete.cases(cpue),]
    ### sum up catch across all species in a group for each haul
    sumcatch<-as.matrix(tapply(cpue$WEIGHT,cpue$HAULJOIN,sum)) 
    ### sloppy matrix work
    x<-as.numeric(rownames(sumcatch))
    sumcatch<-cbind(x,as.numeric(sumcatch[,1]))
    colnames(sumcatch)<-c("HAULJOIN","sumcatch")
    ### name all the species codes to one...
    cpue$SPECIES_CODE<-spec[[1]]
    #### add new column onto table with combined catch
    cpue<-merge(cpue,sumcatch,by="HAULJOIN")
    ### calculate CPUE (kg/km^2)
    cpue$cpue<-cpue$sumcatch/(cpue$DISTANCE_FISHED*cpue$NET_WIDTH/1000)
    ### again getting rid of extra records
    cpue<-cpue[!duplicated(cpue$HAULJOIN),] #this is ok because all species get given the same group code and are combined by this point
    cpue<-cpue[cpue$SPECIES_CODE%in%spec[[1]],]
    cpue<-cpue[!is.na(cpue$SPECIES_CODE),]
    cpue<-cpue[cpue$YEAR%in%year,]
    temp2$cpue<-0
    temp2[match(cpue$HAULJOIN,temp2$HAULJOIN),]$cpue<-cpue$cpue
    if(nrow(temp2[is.infinite(temp2$cpue),])!=0) temp2[is.infinite(temp2$cpue),]$cpue<-0
    chaul_reg<-ddply(temp2,c("SURVEY","YEAR","REGULATORY_AREA_NAME"),summarize,CATCH_COUNT=length(HAULJOIN)) #for catch_count
    chaul_surv<-ddply(temp2,c("SURVEY","YEAR"),summarize,CATCH_COUNT=length(HAULJOIN)) #for catch_count
    #### need to add in hauls with no catch for variance
    hdat<-haul[,c("SURVEY","HAULJOIN","CRUISEJOIN","YEAR","STRATUM","HAUL","DISTANCE_FISHED","NET_WIDTH","REGULATORY_AREA_NAME")]
    temp2<-merge(temp2,hdat,by=c("SURVEY","HAULJOIN","CRUISEJOIN","YEAR","STRATUM","HAUL","DISTANCE_FISHED","NET_WIDTH","REGULATORY_AREA_NAME"),all.y=T)
    temp2[is.na(temp2)]<-0 
    
    #for whole survey estimates
    cstrat<-ddply(temp2,c("SURVEY","YEAR","STRATUM"),summarize,CPUE=sum(cpue,na.rm=T),CPUEvar=var(cpue,na.rm=T))
    cstrat[is.na(cstrat)]<-0
    hstrat<-ddply(haul,c("SURVEY","YEAR","STRATUM"),summarize,n_sta=length(unique(HAULJOIN)))
    biomvar<-merge(cstrat,hstrat,by.x=c("SURVEY","YEAR","STRATUM"),by.y=c("SURVEY","YEAR","STRATUM"),all.x=T)
    colnames(biomvar)<-c("SURVEY","YEAR","STRATUM","CPUE","VAR","n_stations")
    stratarea<-STRATUM[,c("SURVEY","STRATUM","AREA")]
    biomvar<-merge(biomvar,stratarea,by=c("SURVEY","STRATUM"),all.x=T)
    biomvar$BIOMASS<-(biomvar$CPUE/biomvar$n_stations)*biomvar$AREA
    biomvar$VAR2<-biomvar$AREA^2*(biomvar$VAR/biomvar$n_stations)
    Biomass_SURVEY<-ddply(biomvar,c("SURVEY","YEAR"),summarize,Biomass=sum(BIOMASS,na.rm=T)/1000,Variance=sum(VAR2,na.rm=T)/1000000,
                      SE=sqrt(sum(VAR2,na.rm=T))/1000,CV=SE/(sum(BIOMASS,na.rm=T)/1000))
    Biomass_SURVEY$REGULATORY_AREA_NAME<-Biomass_SURVEY$SURVEY
    Biomass_SURVEY<-Biomass_SURVEY[,c("SURVEY","YEAR","REGULATORY_AREA_NAME","Biomass","Variance","SE","CV")]
    shaul<-ddply(hstrat,c("SURVEY","YEAR"),summarize,HAUL_COUNT=sum(n_sta))
    counts<-merge(shaul,chaul_surv,all.x=T)
    Biomass_SURVEY<-merge(Biomass_SURVEY,counts,by=c("SURVEY","YEAR"))
    
    #by GOA FMP sub area
    cstrat_area<-ddply(temp2[temp2$SURVEY=="GOA",],c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME"),summarize,CPUE=sum(cpue,na.rm=T),CPUEvar=var(cpue,na.rm=T))
    cstrat_area[is.na(cstrat_area)]<-0
    colnames(cstrat_area)[4]<-"REGULATORY_AREA_NAME"
    hstrat_area<-ddply(haul[haul$SURVEY=="GOA",],c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME"),summarize,n_sta=length(unique(HAULJOIN)))
    biomvar_area<-merge(cstrat_area,hstrat_area,by=c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME"),all.x=T)
    colnames(biomvar_area)<-c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME","CPUE","VAR","n_stations")
    biomvar_area<-merge(biomvar_area,stratarea[stratarea$SURVEY=="GOA",],by=c("SURVEY","STRATUM"),all.x=T)
    biomvar_area$BIOMASS<-(biomvar_area$CPUE/biomvar_area$n_stations)*biomvar_area$AREA
    biomvar_area$VAR2<-biomvar_area$AREA^2*(biomvar_area$VAR/biomvar_area$n_stations)
    BiomassGOA_area<-ddply(biomvar_area,c("SURVEY","YEAR","REGULATORY_AREA_NAME"),summarize,Biomass=sum(BIOMASS,na.rm=T)/1000,Variance=sum(VAR2,na.rm=T)/1000000,
                           SE=sqrt(sum(VAR2,na.rm=T))/1000,CV=SE/(sum(BIOMASS,na.rm=T)/1000))
    shaul_GOA<-ddply(hstrat_area,c("SURVEY","YEAR","REGULATORY_AREA_NAME"),summarize,HAUL_COUNT=sum(n_sta))
    counts_GOA<-merge(shaul_GOA,chaul_reg,all.x=T)
    BiomassGOA_area<-merge(BiomassGOA_area,counts_GOA,by=c("SURVEY","YEAR","REGULATORY_AREA_NAME"))
    
    #by AI FMP sub area
    cstratAI_area<-ddply(temp2[temp2$SURVEY=="AI",],c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME"),summarize,CPUE=sum(cpue,na.rm=T),CPUEvar=var(cpue,na.rm=T))
    cstratAI_area[is.na(cstratAI_area)]<-0
    colnames(cstratAI_area)[4]<-"REGULATORY_AREA_NAME"
    hstratAI_area<-ddply(haul[haul$SURVEY=="AI",],c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME"),summarize,n_sta=length(unique(HAULJOIN)))
    biomvarAI_area<-merge(cstratAI_area,hstratAI_area,by=c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME"),all.x=T)
    colnames(biomvarAI_area)<-c("SURVEY","YEAR","STRATUM","REGULATORY_AREA_NAME","CPUE","VAR","n_stations")
    biomvarAI_area<-merge(biomvarAI_area,stratarea[stratarea$SURVEY=="AI",],by=c("SURVEY","STRATUM"),all.x=T)
    biomvarAI_area$BIOMASS<-(biomvarAI_area$CPUE/biomvarAI_area$n_stations)*biomvarAI_area$AREA
    biomvarAI_area$VAR2<-biomvarAI_area$AREA^2*(biomvarAI_area$VAR/biomvarAI_area$n_stations)
    BiomassAI_area<-ddply(biomvarAI_area,c("SURVEY","YEAR","REGULATORY_AREA_NAME"),summarize,Biomass=sum(BIOMASS,na.rm=T)/1000,Variance=sum(VAR2,na.rm=T)/1000000,
                           SE=sqrt(sum(VAR2,na.rm=T))/1000,CV=SE/(sum(BIOMASS,na.rm=T)/1000))
    shaul_AI<-ddply(hstratAI_area,c("SURVEY","YEAR","REGULATORY_AREA_NAME"),summarize,HAUL_COUNT=sum(n_sta))
    counts_AI<-merge(shaul_AI,chaul_reg,all.x=T)
    BiomassAI_area<-merge(BiomassAI_area,counts_AI,by=c("SURVEY","YEAR","REGULATORY_AREA_NAME"))
    
    #combine for output
    Biom_lout<-rbind(Biomass_SURVEY,BiomassGOA_area,BiomassAI_area)
    #add in NA's for skipped areas/strata, e.g., EGOA in 2001
    #this only adds in full surveys at this point, if doing depth, need to fix
    #this is probably super clunky, but because there are NO data for EGOA in 2001, I have to create a "spot" for it
    #do this BEFORE adding to loopmat, otherwise, it only adds in one group and is a problem for RFX
    area_list<-unique(haul[,c("SURVEY","REGULATORY_AREA_NAME")])
    srv_yr<-unique(haul[,c("SURVEY","YEAR")])
    srvarea<-merge(srv_yr,area_list,all=T)
    Biom_lout<-merge(srvarea,Biom_lout,by=c("SURVEY","YEAR","REGULATORY_AREA_NAME"),all=T)
    Biom_lout$Group<-outn
    
    loopmat<-rbind(loopmat,Biom_lout)
  }
 
  # Output ----
  #these lines replace NA with zeros for strata with zero biomass but leaves NA for strata with no survey
  #e.g., EGOA in 2001
  loopmat[!(is.na(loopmat$Biomass))& loopmat$Biomass==0,]$CV <- 0
  loopmat[!(is.na(loopmat$Biomass))& loopmat$Biomass==0,]$CATCH_COUNT <- 0
  write.csv(loopmat,paste(outpath,"RACE_Biomass_",outname, AYR".csv",sep=""),row.names=F)
}
