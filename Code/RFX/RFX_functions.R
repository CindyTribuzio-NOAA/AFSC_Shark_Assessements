# Random Effects Models ----
#Updated 9/1/2020 by C. Tribuzio
#adapted from Dana's ranef.r code, double checked by Pete

# Still to do list ----
##1) add in option to turn off the subregions
##2) make start year adaptable, currently set at first year of survey
##3) make end year adaptable
##4) why does regional==F return repeats?


# Packages ----
library(plyr)
library(reshape2)
library(stringr)

# Function ----
RFX_fx<-function(outname,AYR,endyr,datadir,outdir,regional=T){ #note: outname needs to match the RACE biomass file
  # Data Prep ----
  RFX_data<-read.csv(paste(datadir,"/RACE_Biomass_",outname,".csv",sep=""),header=T)
  
  #RFX_data$SE[RFX_data$SE==0]<-0.1 #model can't have zero for SE or variance
  #RFX_data$Variance[RFX_data$Variance==0]<-0.1
  #RFX_data$CV[RFX_data$CV==0]<-999

  unqkey<-unique(RFX_data[,c("SURVEY","Group")]) #list of all of the RFX models to run
  #runs models by each reg area as well as whole surveys
  #does not deal with separate depths yet
  
  outmat<-matrix(nrow=0,ncol=7)
  colnames(outmat)<-c("Biom_est","Biom_LL","Biom_UL","Biom_CV","YEAR","REG_AREA","Group")  
  
  ### loop through each group and survey/area to be modeled ----
  for (i in 1:nrow(unqkey)){
    loopdat<-RFX_data[RFX_data$SURVEY==unqkey[i,1] & 
                        RFX_data$Group==unqkey[i,2],]
    styr <-min(loopdat$YEAR) #first year to be run through the RFX model
    
    ### .dat build for ADMB ----
    yrs_srv<-unique(loopdat$YEAR) #list of years which have data
    nobs<-length(yrs_srv) #number of years with data
    yrs<-c(styr,endyr)
    
    #loopdat has full survey and regional estimates, need to drop full survey for GOA and AI surveys
    if(unqkey[i,1]=="GOA") ld2<-loopdat[loopdat$SURVEY=="GOA" & loopdat$REGULATORY_AREA_NAME!="GOA",]
    if(unqkey[i,1]=="AI") ld2<-loopdat[loopdat$SURVEY=="AI" & loopdat$REGULATORY_AREA_NAME!="AI",]
    
    #wanted to run it by full survey ONLY turn on regional
    if(regional==F) ld2<-loopdat[loopdat$SURVEY==unqkey[i,1] & 
                                   loopdat$REGULATORY_AREA_NAME==as.character(unqkey[i,1]),]
    
    #there are no sub regions for either EBS survey in this code, so loopdat is the same as ld2
    if(str_detect(unqkey[i,1], "^(EBS_)")) ld2<-loopdat
    
    regnames<-unique(ld2$REGULATORY_AREA_NAME)
    nregs<-length(regnames)
    PEI<-rep(1,nregs)
    
    tempB<-dcast(ld2,YEAR~REGULATORY_AREA_NAME,value.var="Biomass",fun.aggregate = mean)
    srv_est<-tempB[, names(tempB) %in% regnames]
    unname(srv_est) # gets rid of column names
    srv_est[is.na(srv_est)] <- "-9" # ADMB flag 
    tempSE<-dcast(loopdat,YEAR~REGULATORY_AREA_NAME,value.var="SE",fun.aggregate = mean)
    srv_SE<-tempSE[, names(tempSE) %in% regnames]
    unname(srv_SE)
    srv_SE[is.na(srv_SE)] <- "-9"

    #this creates the dat file for ADMB
    cat("# Model start and end years","\n",yrs,"\n",
      "# Number of survey indices fit (i.e., regions/depth strata)","\n",nregs,"\n",
      "# Number or process error parameters","\n",1,"\n",
      "# Process error index","\n",PEI,"\n",
      "# Number of surveys","\n",nobs,"\n",
      "# Survey years","\n",yrs_srv,"\n",
      "# Survey biomass","\n",
      sep=" ",file=paste(codedir,"/re.dat",sep=""))
    write.table(srv_est, file = paste(codedir,"/re.dat",sep=""), sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(paste0("# Survey biomass SE"), file = paste(codedir,"/re.dat",sep=""), sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    write.table(srv_SE, file = paste(codedir,"/re.dat",sep=""), sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    ### ADMB compiled model ----
    #change the working directory so ADMB puts output in the right place
    projdir<-getwd()
    setwd(codedir)
    system(paste(codedir,"/re.exe",sep=""))
    #try(system("re.exe"),silent=T)
    #if ('try-error' %in% class(fit)) next
    setwd(projdir)
    
    ### Summary ---- 
    #nlines is the number of lines of data to be read, or the total number of years of the model
    #skip is the number of lines to skip, # cooresponds the line with "biomA"
    #these set up the skips for each data summary
    totyr<-endyr-styr+1
    modyrs<-seq(styr,endyr)
    LLst<-17+(nobs+1)*2+2
    bst<-LLst+totyr+1
    ULst<-bst+totyr+1
    CVst<-(ULst+totyr+1)+totyr+1
    
    #Biomass
    if(!str_detect(unqkey[i,1], "^(EBS_)")){
      re_biom<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=totyr,skip=bst),ncol=nregs,byrow=T) 
      if(nrow(re_biom)==0) re_biom<-matrix(nrow=totyr,ncol=nregs,0)
      re_biom<-cbind(modyrs,re_biom)
      colnames(re_biom)<-c("YEAR", as.character(regnames))
      re_b2<-try(melt(as.data.frame(re_biom),id=c("YEAR")),silent=T)
      if ('try-error' %in% class(re_b2)) next
      #re_b2<-melt(as.data.frame(re_biom),id=c("YEAR"))
      colnames(re_b2)<-c("YEAR","REGULATORY_AREA_NAME","Biomass")
    }

    #CV
    if(!str_detect(unqkey[i,1], "^(EBS_)")){
      re_biomCV<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=totyr,skip=CVst),ncol=nregs,byrow=T)
      if(nrow(re_biomCV)==0) re_biomCV<-matrix(nrow=totyr,ncol=nregs,0)
      re_biomCV<-cbind(modyrs,re_biomCV)
      colnames(re_biomCV)<-c("YEAR",as.character(regnames))
      re_CV2<-melt(as.data.frame(re_biomCV),id=c("YEAR"))
      colnames(re_CV2)<-c("YEAR","REGULATORY_AREA_NAME","CV")
    }
    
    #Biomass LL
    if(!str_detect(unqkey[i,1], "^(EBS_)")){
      re_biomLL<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=totyr,skip=LLst),ncol=nregs,byrow=T)
      if(nrow(re_biomLL)==0) re_biomLL<-matrix(nrow=totyr,ncol=nregs,0)
      re_biomLL<-cbind(modyrs,re_biomLL)
      colnames(re_biomLL)<-c("YEAR",as.character(regnames))
      re_bLL2<-melt(as.data.frame(re_biomLL),id=c("YEAR"))
      colnames(re_bLL2)<-c("YEAR","REGULATORY_AREA_NAME","LL")
    }
    
    #Biomass UL
    if(!str_detect(unqkey[i,1], "^(EBS_)")){
      re_biomUL<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=totyr,skip=ULst),ncol=nregs,byrow=T) 
      if(nrow(re_biomUL)==0) re_biomUL<-matrix(nrow=totyr,ncol=nregs,0)
      re_biomUL<-cbind(modyrs,re_biomUL)
      colnames(re_biomUL)<-c("YEAR",as.character(regnames))
      re_bUL2<-melt(as.data.frame(re_biomUL),id=c("YEAR"))
      colnames(re_bUL2)<-c("YEAR","REGULATORY_AREA_NAME","UL")
    }
    
    #Total Survey Area
    re_biomSURVEY<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=1,skip=7),ncol=1,byrow=T)
    re_biomSURVEY<-as.data.frame(cbind(seq(styr,endyr),as.character(unqkey[i,1]),re_biomSURVEY))
    if(nrow(re_biomSURVEY)==0) re_biomSURVEY<-cbind(modyrs,as.data.frame(as.character(unqkey[i,1])),0)
    colnames(re_biomSURVEY)<-c("YEAR","REGULATORY_AREA_NAME","Biomass")

    re_biomSURVEYCV<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=1,skip=9),ncol=1,byrow=T)
    re_biomSURVEYCV<-as.data.frame(cbind(seq(styr,endyr),as.character(unqkey[i,1]),re_biomSURVEYCV))
    if(nrow(re_biomSURVEYCV)==0) re_biomSURVEYCV<-cbind(modyrs,as.data.frame(as.character(unqkey[i,1])),0)
    colnames(re_biomSURVEYCV)<-c("YEAR","REGULATORY_AREA_NAME","CV")
    
    re_biomSURVEYLL<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=1,skip=13),ncol=1,byrow=T)
    re_biomSURVEYLL<-as.data.frame(cbind(seq(styr,endyr),as.character(unqkey[i,1]),re_biomSURVEYLL))
    if(nrow(re_biomSURVEYLL)==0) re_biomSURVEYLL<-cbind(modyrs,as.data.frame(as.character(unqkey[i,1])),0)
    colnames(re_biomSURVEYLL)<-c("YEAR","REGULATORY_AREA_NAME","LL")
    
    re_biomSURVEYUL<-matrix(scan(file=paste(codedir,"/rwout.rep",sep=""),nlines=1,skip=11),ncol=1,byrow=T)
    re_biomSURVEYUL<-as.data.frame(cbind(seq(styr,endyr),as.character(unqkey[i,1]),re_biomSURVEYUL))
    if(nrow(re_biomSURVEYUL)==0) re_biomSURVEYUL<-cbind(modyrs,as.data.frame(as.character(unqkey[i,1])),0)
    colnames(re_biomSURVEYUL)<-c("YEAR","REGULATORY_AREA_NAME","UL")
    
    ## make a data frame of results
    ifelse(!str_detect(unqkey[i,1], "^(EBS_)"),rB<-data.frame(rbind(re_b2,re_biomSURVEY)),rB<-re_biomSURVEY)
    ifelse(!str_detect(unqkey[i,1], "^(EBS_)"),rCV<-data.frame(rbind(re_CV2,re_biomSURVEYCV)),rCV<-re_biomSURVEYCV)
    ifelse(!str_detect(unqkey[i,1], "^(EBS_)"),rLL<-data.frame(rbind(re_bLL2,re_biomSURVEYLL)),rLL<-re_biomSURVEYLL)
    ifelse(!str_detect(unqkey[i,1], "^(EBS_)"),rUL<-data.frame(rbind(re_bUL2,re_biomSURVEYUL)),rUL<-re_biomSURVEYUL)
    rout<-merge(rB,rCV,by=c("YEAR","REGULATORY_AREA_NAME"))
    rout<-merge(rout,rLL,by=c("YEAR","REGULATORY_AREA_NAME"))
    rout<-merge(rout,rUL,by=c("YEAR","REGULATORY_AREA_NAME"))
    colnames(rout)<-c("YEAR","REGULATORY_AREA_NAME","Biom_est","Biom_CV","Biom_LL","Biom_UL")
    rout$Group<-unqkey[i,2]
    outmat<-rbind(outmat,rout)
  }
  
  ## write to output directory
  write.csv(outmat, paste(outdir,"RFX_Biomass_",outname,".csv",sep=""),row.names=F)
  
  }





