# setwd("D:\\Dropbox\\CAProject\\ShinyDemo")
# setwd("E:/Dropbox/CAProject/ShinyDemo") # Path to save output files
# Function to get MSE object given stock and fleet number
# Stock and Fleet number from dropdown menus with descriptive labels
ChooseSFOb <- function(Stock, Fleet) {
  Name <- paste0("Stock", Stock, "_Fleet", Fleet)
  MSE <- get(Name)
  return(MSE)
}

# Function to create feasiblity object depending on what checkboxes are selected
ModFease <- function(AvailData, FeaseObj) {
  FeaseObj@Catch <- ifelse (AvailData[1] == 1, 1, 0)
  FeaseObj@Index <- ifelse (AvailData[2] == 1, 1, 0)
  FeaseObj@Natural_mortality_rate <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Maturity_at_length <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Growth <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Length_weight_conversion <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Fleet_selectivity <- ifelse (AvailData[4] == 1, 1, 0)
  FeaseObj@Catch_at_length <- ifelse (AvailData[6] == 1, 1, 0)
  FeaseObj@Catch_at_age <- ifelse (AvailData[5] == 1, 1, 0)
  FeaseObj@Stock_recruitment_relationship <- ifelse (AvailData[3] == 1, 1, 0)
  # May need to add data types for below - otherwise just leave them for Demo
  # FeaseObj@Recruitment_index <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Target_catch <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Target_biomass <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Target_index <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Abundance <- ifelse (AvailData[1] == 1, 1, 0)
  return(FeaseObj)
}

# Function to get list of 'feasible' MPs given checked/unchecked data types 
FeaseMPs <- function(AvailData, MSEObj, AllFeaseObj) {
  FeaseObj <- ModFease(AvailData, AllFeaseObj)
  chkFease <- Fease(FeaseObj)
  allfeaseMPs <- rownames(chkFease)[which(chkFease[,1]=="Yes")]
  feaseMPs <- MSEObj@MPs[which(MSEObj@MPs %in% allfeaseMPs)]
  return(feaseMPs)
} 

################################################
# TradePlot function - generic trade-off plots #
################################################


## Calculate Performance Statistics ##
PerfStats <- function(MSEobj, maxVar=15, BmsyRef=0.5, B0Ref=0.2) {
 
  BMSYref<-rep(NA,MSEobj@nMPs)
  B0ref<-rep(NA,MSEobj@nMPs)
  PNOF<-rep(NA,MSEobj@nMPs)
  LTY<-rep(NA,MSEobj@nMPs)
  STY<-rep(NA,MSEobj@nMPs)
  VY<-rep(NA,MSEobj@nMPs)

  y1 <- 1:(MSEobj@proyears-1)
  y2 <- 2:MSEobj@proyears
  
  ystart<-1:5
  yend<-max(MSEobj@proyears-4,1):MSEobj@proyears
  
  RefYd<-MSEobj@OM$RefY
  if (maxVar < 1) maxVar <- maxVar * 100
  
  for(mm in 1:MSEobj@nMPs){  
    PNOF[mm]<-round(sum(MSEobj@F_FMSY[,mm,]<1,na.rm=T)/prod(dim(MSEobj@F_FMSY[,mm,]),na.rm=T)*100,1)
    BMSYref[mm]<-round(sum(MSEobj@B_BMSY[,mm,]>BmsyRef,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
	B0ref[mm]<-round(sum(MSEobj@B_BMSY[,mm,] * MSEobj@OM$BMSY_B0 > B0Ref,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
    # LTY[mm]<-round(sum(MSEobj@C[,mm,yend]/RefYd>0.5,na.rm=T)/(MSEobj@nsim*length(yend)),3)*100
	# STY[mm]<-round(sum(MSEobj@C[,mm,ystart]/RefYd>0.5,na.rm=T)/(MSEobj@nsim*length(ystart)),3)*100
	LTY[mm]<-round(mean(apply(MSEobj@C[,mm,yend],1,mean,na.rm=T)/RefYd,na.rm=T)*100,1)
	STY[mm]<-round(mean(apply(MSEobj@C[,mm,ystart],1,mean,na.rm=T)/RefYd,na.rm=T)*100,1)
    AAVY<-apply((((MSEobj@C[,mm,y1]-MSEobj@C[,mm,y2])/MSEobj@C[,mm,y2])^2)^0.5,1,mean,na.rm=T) 
    VY[mm]<-round(sum(AAVY<(maxVar/100),na.rm=T)/MSEobj@nsim,3)*100
  }
  
  LTY[LTY > 100] <- 100 # make maximum LTY 100% - too hard to explain in demo why it can sometimes by > 100
  STY[STY > 100] <- 100 # make maximum STY 100% - too hard to explain in demo why it can sometimes by > 100
  
  out <- data.frame(Names=MSEobj@MPs, BMSYref=BMSYref, B0ref=B0ref, PNOF=PNOF, LTY=LTY, 
    STY=STY, VY=VY)
  out
}
	
TradePerf <- function(PerfObject, XAxis=c("Overfishing", "Biomass:BMSY"), 
	YAxis=c("Long-term Yield", "AnnualVar"), XThresh=c(30, 80), YThresh=c(0,50),
	maxVar=15, BmsyRef=0.5, B0Ref=0.2) {
  LTY <- PerfObject$LTY 
  STY <- PerfObject$STY 
  PNOF <- PerfObject$PNOF 
  BMSYref <- PerfObject$BMSYref
  B0ref <- PerfObject$B0ref
  VY <- PerfObject$VY 
  
  X1 <- GetStat(XAxis, LTY, STY, PNOF, BMSYref, B0ref, VY)
  XLab <- StatLab(XAxis, maxVar, BmsyRef, B0Ref)
  Y1 <- GetStat(YAxis, LTY, STY, PNOF, BMSYref, B0ref, VY)
  YLab <- StatLab(YAxis, maxVar, BmsyRef, B0Ref)
  
  Accept <- rep(FALSE, length(X1))
  Accept[X1 >= XThresh & Y1 >=YThresh] <- TRUE
  DF <- data.frame(Names=PerfObject$Names, x=X1, y=Y1, Accept=Accept)
  Dist <- NULL
  for (X in 1:nrow(DF)) Dist[X] <- euc.dist(c(DF[X,2], DF[X,3]), c(100, 100))
  DF <- DF[order(Dist),]
	
  out <- list()
  out[[1]] <- DF
  out[[2]] <- c(XLab, YLab)
  out
}	

TradePlot2 <- function(PerfMetObj, AvailMPs=NULL, clickedMP=NULL, 
  XThresh=NULL, YThresh=NULL) {
  par(mfrow=c(1,1), mar=c(4,4.5,1,1), oma=c(1,1,0,0))
  x <- PerfMetObj[[1]]$x 
  y <- PerfMetObj[[1]]$y 
  labs <- as.character(PerfMetObj[[1]]$Names)
  Accept <- PerfMetObj[[1]]$Accept
  
  xlab <- PerfMetObj[[2]][1]
  ylab <- PerfMetObj[[2]][2]
 
  hl <- YThresh
  vl <- XThresh
  
  
  adjj<-c(0.7,1.3)
  XLim <- c(-5, 105) # c(min(c(-10, min(x,na.rm=T)*adjj)), max(c(max(x,na.rm=T)*adjj, 110)))
  YLim <- c(-5, 105) # c(min(c(-10, min(y,na.rm=T)*adjj)), max(c(max(y,na.rm=T)*adjj, 110)))
  
  # Colors for points 
  coly <- rep("darkgray", length(labs))  # default color 
  coly[Accept] <- "black" 
  if (!is.null(AvailMPs)) coly[labs%in%AvailMPs & Accept] <- "green"
  
  Pch <- 21
  bCex <- 2
  Cex <- rep(bCex, length(labs))
  
  if (!is.null(clickedMP)) {
    coly[labs%in%clickedMP] <- "red"
	Cex[labs%in%clickedMP] <- bCex * 1.5
  }
  plot(NA,xlim=XLim,ylim=YLim,xlab=xlab,ylab=ylab, bty="l", las=1, cex.lab=1.25)
  abline(v=vl,col="#99999940",lwd=2)
  abline(h=hl,col="#99999940",lwd=2)
   
  # Polygons 
  Alpha <- 30
  LeftCol <- rgb(red=255, green=0, blue=0, alpha=Alpha, names = NULL, 
	maxColorValue = 255)
  RightCol <- rgb(red=0, green=255, blue=0, alpha=Alpha, names = NULL, 
	maxColorValue = 255)  
  polygon(x=c(0, vl,  vl, 0), y=c(0, 0, hl, hl), col=LeftCol, border=NA)
  polygon(x=c(0, vl,  vl, 0), y=c(0, 0, 100, 100), col=LeftCol, border=NA)
  polygon(x=c(vl,  100, 100, vl), y=c(0, 0, 100, 100), col=RightCol, border=NA)
  polygon(x=c(vl, 100,  100, vl), y=c(hl, hl, 100, 100), col=RightCol, border=NA)
  
  # Add Points 
  points(x,y, bg=coly, pch=Pch, cex=Cex, col="black" )
}
	
# Supporting functions 
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

GetStat <- function(PM, LTY, STY, PNOF, BMSYref, B0ref, VY) {
  switch(PM,
    "Long-term Yield" = LTY,
	"Short-term Yield" = STY,
	"Overfishing" = PNOF,
	"Biomass:BMSY" = BMSYref,
	"Biomass:B0" = B0ref,
    "AnnualVar" = VY)
}

StatLab <- function(PM, maxVar, BmsyRef, B0Ref) {
  switch(PM,
    "Long-term Yield" = "Long-Term Yield (yrs 26-30, relative to fishing @ FMSY)",
	"Short-term Yield" = "Short-Term Yield (10 yrs, relative to fishing @ FMSY)",
	"Overfishing" = "Not Overfishing (probability of F < FMSY)",
	"Biomass:BMSY" = paste0("Not Overfished (probability Biomass > ", BmsyRef, " BMSY)"),
	"Biomass:B0" = paste0("Not Severely Overfished (probability Biomass > ", B0Ref, " BMSY)"),
    "AnnualVar" = paste0("Inter-Annual Variability in Yield (probability < ", maxVar, "%)")
	)
}

makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

Pplot2 <-function(MSEobj,nam=NA, YLim1=c(0,3), YLim2=c(0,3)){
  
  FMSYr<-quantile(MSEobj@F_FMSY,c(0.001,0.90),na.rm=T)
  BMSYr<-quantile(MSEobj@B_BMSY,c(0.001,0.975),na.rm=T)
  
  colsse<-rainbow(100,start=0,end=0.36)[1:100]
  # colB<-rep(colsse[100],ceiling(BMSYr[2]*100))
  colB<-rep(colsse[100],ceiling(YLim2[2]*100))
  colB[1:100]<-colsse
  colB<-makeTransparent(colB,60)
  colsse<-rainbow(200,start=0,end=0.36)[200:1]
  # colF<-rep(colsse[200],ceiling(FMSYr[2]*100))
  colF<-rep(colsse[200],ceiling(YLim1[2]*100))
  colF[1:200]<-colsse
  colF<-makeTransparent(colF,60)
  
  Yd<-rep(NA,MSEobj@nMPs)
  P10<-rep(NA,MSEobj@nMPs)
  P50<-rep(NA,MSEobj@nMPs)
  P100<-rep(NA,MSEobj@nMPs)
  POF<-rep(NA,MSEobj@nMPs)
  yind<-max(MSEobj@proyears-4,1):MSEobj@proyears
  RefYd<-MSEobj@OM$RefY
  
  for(mm in 1:MSEobj@nMPs){
    Yd[mm]<-round(mean(apply(MSEobj@C[,mm,yind],1,mean,na.rm=T)/RefYd,na.rm=T)*100,1)
    #cbind(MSEobj@C[,mm,yind],unlist(MSEobj@OM$MSY))
    POF[mm]<-round(sum(MSEobj@F_FMSY[,mm,]>1,na.rm=T)/prod(dim(MSEobj@F_FMSY[,mm,]),na.rm=T)*100,1)
    P10[mm]<-round(sum(MSEobj@B_BMSY[,mm,]<0.1,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
    P50[mm]<-round(sum(MSEobj@B_BMSY[,mm,]<0.5,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
    P100[mm]<-round(sum(MSEobj@B_BMSY[,mm,]<1,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
  }
    
  nr<-ceiling(MSEobj@nMPs/8)
  nc<-ceiling(MSEobj@nMPs/nr)
  nr<-nr*2
  MSEcols<-c('red','green','blue','orange','brown','purple','dark grey','violet','dark red','pink','dark blue','grey')
  temp<-array(0,c(nr*2+(nr/2-1),nc*2))
  i<-0
  for(c in 1:nc){
    for(r in 1:nr){
      i<-i+1
      temp[(ceiling(r/2)-1)+(1:2)+(r-1)*2,(1:2)+(c-1)*2]<-((c-1)*nr)+r
    }
  }
  par(mfcol=c(nr,nc),mai=c(0.2,0.35,0.3,0.01),omi=c(0.5,0.4,0.4,0.05))
  layout(temp)
  #dev.new2(width=nc*3,height=nr*3)
  #
  lwdy <- 3
  Cex <- 1.5
  Las <- 1
  AxCex <- 1.35
  for(mm in 1:MSEobj@nMPs){
    # plot(MSEobj@F_FMSY[1,mm,],ylim=FMSYr,col=colF[ceiling(mean(MSEobj@F_FMSY[1,mm,],na.rm=T)*100)],type='l',lwd=lwdy, bty="n", axes=FALSE)
	 plot(MSEobj@F_FMSY[1,mm,],ylim=YLim1,col=colF[ceiling(mean(MSEobj@F_FMSY[1,mm,],na.rm=T)*100)],type='l',lwd=lwdy, bty="n", axes=FALSE)
	if (mm ==1) axis(side=2, las=Las, cex.axis=AxCex)
	if (mm > 1) axis(side=2, label=FALSE)
	axis(side=1, label=FALSE)
    for(i in 1:MSEobj@nsim)lines(MSEobj@F_FMSY[i,mm,],col=colF[ceiling(mean(MSEobj@F_FMSY[i,mm,],na.rm=T)*100)],lwd=lwdy)
    abline(h=100,col="grey",lwd=3)
    mtext(MSEobj@MPs[mm],3,outer=F,line=0.6, cex=Cex)
    # legend('topright',c(paste(POF[mm],"% POF",sep=""),
                 # paste(Yd[mm],"% FMSY yield",sep="")),bty='n',cex=0.8)
    if(mm%in%(1:(nr/2)))mtext("F/FMSY",2,line=3,outer=F, cex=Cex)
    abline(h=1,col=makeTransparent("grey",60),lwd=2.5)
  
    plot(MSEobj@B_BMSY[1,mm,],ylim=YLim2,col=colB[ceiling(MSEobj@B_BMSY[1,mm,MSEobj@proyears]*100)],type='l',lwd=lwdy, axes=FALSE)
    if (mm ==1) axis(side=2, las=Las, cex.axis=AxCex)
	if (mm > 1) axis(side=2, label=FALSE)
	axis(side=1, las=Las, cex.axis=AxCex)
    for(i in 1:MSEobj@nsim)lines(MSEobj@B_BMSY[i,mm,],col=colB[ceiling(MSEobj@B_BMSY[i,mm,MSEobj@proyears]*100)],lwd=lwdy)
    abline(h=100,col="grey",lwd=3)
    # legend('topright',c(paste(P100[mm],"% < BMSY",sep=""),
                 # paste(P50[mm],"% < 0.5BMSY",sep=""),
                 # paste(P10[mm],"% < 0.1BMSY",sep="")),bty='n',cex=0.8)
    if(mm%in%(1:(nr/2)))mtext("B/BMSY",2,line=3,outer=F, cex=Cex)
    abline(h=1,col=makeTransparent("grey",60),lwd=2.5)
  
  }
  mtext("Projection year",1,outer=T,line=1.2, cex=Cex)
  if(is.na(nam))mtext(deparse(substitute(MSEobj)),3,outer=T,line=0.3,font=2, cex=Cex)
  if(!is.na(nam))mtext(MSEobj@Name,3,outer=T,line=0.3,font=2, cex=Cex)
}



##Function to return actual name of MP (derived from documentation)
MPname <- function(MP){
  switch(as.character(MP),
    " "=" ",
    "AvC"="AvC: TAC set at Average Historical Catch",
    "CC1" ="CC1: TAC set at average catch from last 5 years",
	"DBSRA" = "DBSRA (Depletion-Based Stock Reduction Analysis): TAC set at estimate of MSY",
    "DD" ="DD (Delay - Difference Model): TAC set at estimate of MSY ",
    "DCAC" ="DCAC (Depletion Corrected Average Catch): TAC set at average catch adjusted for estimate of current depletion",
	"EDCAC"="EDCAC (Extra Depletion Corrected Average Catch): TAC set at average catch adjusted for estimate of current depletion",
	"Islope1"="ISlope: Incrementally adjusted TAC to maintain a constant CPUE",
	"IT5"="IT5: Incrementally adjusted TAC according to trend in index of abundance",
	"Itarget1"="Itarget1: Incrementally adjusted TAC towards a target CPUE",
	"Ltarget1"="Ltarget1: Incrementally adjusted TAC towards a target mean length",
	"MCD"="MCD: TAC set at average catch adjusted by estimate of depletion",
	"ItargetE4"="ItargetE4: Incrementally adjusted Fishing Effort towards a target CPUE",
	"LstepCE1"="LstepCE1: Incrementally adjusted Fishing Effort towards a target mean length", 
    "matlenlim" ="matlenlim: Size Limit set equal to maturity curve",
    "curE" ="curE: Fishing effort maintained at current level",
    "MRreal" ="MRreal: A marine reserve in area 1",
	"YPR"="YPR: A simple yield per recruit approximation of FMSY"
  )
}
	
	
### Data Object ###

# Function to modify data object depending on what data types are selected
ModDataObj <- function(AvailData, DLMData) {
  DLMDataOut <- DLMData
  if(AvailData[1] != 1) DLMDataOut@Cat[1,] <- NA
  if(AvailData[2] != 1) DLMDataOut@Ind[1,] <- NA
  if(!all(AvailData == 1)) DLMDataOut@Rec[1,]  <- NA
  if(AvailData[1] != 1) DLMDataOut@t <- NA 
  if(AvailData[1] != 1) DLMDataOut@AvC <- NA
  if(!all(AvailData[1:3] == 1)) DLMDataOut@Dt <- NA
  if(AvailData[3] != 1) DLMDataOut@Mort <- NA
  if(AvailData[3] != 1) DLMDataOut@FMSY_M <- NA
  if(AvailData[3] != 1) DLMDataOut@BMSY_B0 <- NA
  if(AvailData[1] != 1) DLMDataOut@Cref <- NA
  if(AvailData[2] != 1) DLMDataOut@Bref <- NA
  if(AvailData[2] != 1) DLMDataOut@Iref <- NA
  if(AvailData[3] != 1) DLMDataOut@L50 <- NA
  if(AvailData[3] != 1) DLMDataOut@L95 <- NA
  if(AvailData[4] != 1) DLMDataOut@LFC <- NA
  if(AvailData[4] != 1) DLMDataOut@LFS <- NA
  if(AvailData[5] != 1) DLMDataOut@CAA[1,,] <- NA
  if(!all(AvailData[1:3] == 1)) DLMDataOut@Dep <- NA 
  if(AvailData[2] != 1) DLMDataOut@Abun <- NA
  if(AvailData[3] != 1) DLMDataOut@vbK <- NA
  if(AvailData[3] != 1) DLMDataOut@vbLinf <- NA
  if(AvailData[3] != 1) DLMDataOut@vbt0 <- NA
  if(AvailData[3] != 1) DLMDataOut@wla <- NA
  if(AvailData[3] != 1) DLMDataOut@wlb <- NA
  if(AvailData[3] != 1) DLMDataOut@steep <- NA
  if(AvailData[3] != 1) DLMDataOut@MaxAge <- 0
  if(AvailData[6] != 1) DLMDataOut@CAL_bins <- 0
  if(AvailData[6] != 1) DLMDataOut@CAL[1,,] <- NA
  if(AvailData[6] != 1) DLMDataOut@ML[1,] <- NA
  if(AvailData[6] != 1) DLMDataOut@Lbar[1,] <- NA
  if(AvailData[6] != 1) DLMDataOut@Lc[1,] <- NA

  return(DLMDataOut)
} 
 
# Run a MP 
RunMP <- function(X, OurData, AvailMPs) {
  fun <- get(AvailMPs[X])
  Class <- class(fun)
  if (Class == "DLM_output") {
    # rr <- TAC(DLM_data=OurData, MPs=AvailMPs[X], reps=1)
	TAC <- fun(x=1, DLM_data=OurData, reps=1)
	TAC <- round(TAC,0)
	return(paste("Set TAC to", TAC, "ton")) 
  }
  if (Class == "DLM_input") {
    rr <- runInMP(OurData, MPs=AvailMPs[X], reps=1)[[1]][,1,1]
	lastE <- OurData@MPeff
    if (!is.na(rr[5])) return(paste("Set size limit to", round(rr[5],0), "mm")) 
	if(length(grep("MR", AvailMPs[X])>0)) return("Close spatial area") 
    if(length(grep("E", AvailMPs[X])>0)) return(paste("Set Allowable Days at Sea to", min(round(rr[2]*lastE,2)*50, 200))) 
  }
}


# Make Table of Data Object
DataTable <- function(dataobj, maxCol=10, dg=1) {
  nms <- slotNames(dataobj)
  ind <- grep("CV_", nms)
  ind <- c(ind, grep("MPs", nms))
  ind <- c(ind, grep("OM", nms))
  ind <- c(ind, grep("Obs", nms))
  ind <- c(ind, grep("TAC", nms))
  ind <- c(ind, grep("Sense", nms))
  ind <- c(ind, grep("Misc", nms))
  ind <- c(ind, grep("LHYear", nms))
  ind <- c(ind, grep("Name", nms))
  ind <- c(ind, grep("Ref", nms)) 
  ind <- c(ind, grep("MaxAge", nms))
  ind <- c(ind, grep("Units", nms))
  ind <- c(ind, grep("Log", nms))
  ind <- c(ind, grep("params", nms)) 
  ind <- c(ind, grep("PosMPs", nms))
  ind <- c(ind, grep("MPrec", nms))
  ind <- c(ind, grep("MPeff", nms))
  ind <- unique(ind)

  ncal <- dim(dataobj@CAL[1,,])
  ncaa <- dim(dataobj@CAA[1,,])
  if (ncal[1] > 10) lim1 <- (ncal[1]-9):ncal[1]
  if (ncaa[1] > 10) lim2 <- (ncaa[1]-9):ncaa[1]
  CAL <- dataobj@CAL[1,lim1,]
  CAA <- dataobj@CAA[1,lim2,]
  
  nyr <- length(dataobj@Year)
  nCol <- max(1, max(c(ncal[2], ncaa[2], nyr), na.rm=TRUE))
  nRow <- 200
  mat <- matrix(NA, nrow=nRow, ncol=nCol)
 
  mat[1,] <- dataobj@Year
  mat[2,] <- dataobj@Cat[1,]
  mat[3,] <- dataobj@Ind[1,]
  mat[4,] <- dataobj@Rec[1,]
  mat[5,1] <- dataobj@t[1]
  mat[6,1] <- dataobj@AvC[1]
  mat[7,1] <- dataobj@Dt[1]
  mat[8,1] <- dataobj@Mort[1] 
  mat[9,1] <- dataobj@FMSY_M[1]
  mat[10,1] <- dataobj@BMSY_B0[1]
  mat[11,1] <- dataobj@Cref[1]
  mat[12,1] <- dataobj@Bref[1]
  mat[13,1] <- dataobj@Iref[1]
  mat[14,1] <- dataobj@L50[1]
  mat[15,1] <- dataobj@L95[1]
  mat[16,1] <- dataobj@LFC[1]
  mat[17,1] <- dataobj@LFS[1]
  
  ind <- 18+nrow(CAA)
  mat[18:(ind-1),1:ncol(CAA)] <- CAA
  
  mat[ind,1] <- dataobj@Dep[1]
  mat[ind+1,1] <- dataobj@Abun[1]
  mat[ind+2,1] <- dataobj@vbK[1]
  mat[ind+3,1] <- dataobj@vbLinf[1]
  mat[ind+4,1] <- dataobj@vbt0[1]
  mat[ind+5,1] <- dataobj@wla[1]
  mat[ind+6,1] <- dataobj@wlb[1]
  mat[ind+7,1] <- dataobj@steep[1]
  ll <- length(dataobj@CAL_bins)
  if (ll > 1) {
    mat[ind+8,1:ll] <- dataobj@CAL_bins
    ind2 <- (ind+9)+(nrow(CAA))
    mat[(ind+9):(ind2-1),1:(ll-1)] <- CAL
    mat[ind2,1] <- dataobj@Lbar[1]
    mat[ind2+1,1] <- dataobj@Lc[1]
	
	mat[ind2+2,1] <- dataobj@ML[1]
	mat[ind2+3,1] <- dataobj@MPrec[1]
	mat[ind2+4,1] <- dataobj@MPeff[1]
  }   
  mat <-  round(mat, digits=dg)
  
  tmp <- c("Year", "Catch", "Index", "Recruitment", "t", "AvC", "Dt",
    "Mort", "FMSY_M", "BMSY_B0", "Cref", "Bref", "Iref", "L50", "L95", "LFC", "LFS",
	paste0("CAA", 1:10), "Dep", "Abun", "vbK", "vbLinf", "vbt0", "wla", "wlb",
	"steep", "CAL_bins", paste0("CAL", 1:10), "Lbar", "Lc", "ML", "MPrec",
	"MPeff")
  rownames(mat) <- c(tmp, rep(NA, 200 -length(tmp)))

   drp <- which(rowSums(apply(mat, 2, is.na)) == ncol(mat))
   if (length(drp) > 0) mat <- mat[-drp,]
   # mat[is.na(mat)] <- "" 
   mat[,1:maxCol] 
}


#### VOI / Sensitivity Plots #####

VOIplot <- function(MSEobj, MPs=NA, nvars=5, nMP=4, Par=c("Obs", "OM"), 
  YVar=c("Y", "B"), doPlot=TRUE, incStat=FALSE, availMP=NULL, 
  acceptMP=NULL, incNames=TRUE, labcex=0.8) {
  YVar <- match.arg(YVar)
  nvars <- max(nvars, 2) # maximum number of variables 
  Par <- match.arg(Par)  # Operating Model or Observation 
  nMPs <- MSEobj@nMPs # Number of MPs  
  # Subset to specified MPs 
  if (any(is.na(MPs))) MPs <- MSEobj@MPs
  if (class(MPs) == "numeric" | class(MPs) == "integer") MPs <- MSEobj@MPs[MPs]
  if (length(MPs) < 1) stop("No MPss found")
  nMPss <- length(MPs)
  if (nMP > nMPs) nMP <- nMPs 
  if (!all(MSEobj@MPs %in% MPs)) {
    mse <- Sub(MSEobj, MPs=MPs)
	nMPs <- mse@nMPs
  } else {
    mse <- MSEobj 
  }
  
  # Calculate MSE sensitivities per MP 
  if (length(MPs) > 1)  senseDat <- sapply(1:nMPs, calcMSESense, MSEobj=mse, YVar=YVar)
  if (length(MPs) == 1) senseDat <- calcMSESense(MP=MPs, MSEobj=mse, YVar=YVar)
  
  # Y Variable    
  if (nMPs> 1) yvals <- senseDat["YVar",]
  if (nMPs == 1) yvals <- senseDat$YVar
  
  # Operating Model or Observation Statistics 
  if (nMPs > 1) {
    if (Par == "OM") {
      xvals <- senseDat["OMVals",1]
	  stat <- senseDat["OMStat",]
	  smth <- senseDat["OMSmooth",]
	  varNames <- colnames(senseDat["OMVals",1][[1]])
    } else {
      xvals <- senseDat["OBVals",1]
	  stat <- senseDat["OBStat",]
	  smth <- senseDat["OBSmooth",]
	  varNames <- colnames(senseDat["OBVals",1][[1]])
	
    }
  }
  if (nMPs == 1) {
    if (Par == "OM") {
      xvals <- senseDat$OMVals
	  stat <- senseDat$OMStat
	  smth <- senseDat$OMSmooth
	  varNames <- names(stat)
    } else {
      xvals <- senseDat$OBVals
	  stat <- senseDat$OBStat
	  smth <- senseDat$OBSmooth
	  varNames <- names(stat)
    }    
  }
  
  # Check what MPs used what variables 
  used <- matrix(FALSE, nrow=length(varNames), ncol=nMPs) 
  if (Par == "OM") {
    used <- matrix(TRUE, nrow=length(varNames), ncol=nMPs) # all OM parameters used 
	Obsnam <- varNames
	LnName <- c("Reference yield", "Natural mortality", "Depletion", "Abundance",  
    "BMSY/B0", "FMSY/M", "M gradient", "Inter-annual variability M",
    "Recruitment variability", "Inter-annual variability effort",        
    "Final effort", "MSY", "Average change in catchability", 
    "Inter-annual variabilility in catchability", "FMSY", "von Bert. Linf", 
    "von Bert. K", "von Bert. t0", "Steepness", "Linf gradient", "K gradient", 
    "Inter-annual variability in Linf", "Recruitment gradient", 
    "Inter-annual variability in K", "Age at maturity", "Length at 5% selection", 
    "Length at full selection", "Length at first capture", "True MSY", "Size Area 1",                               
    "Prob. Movement", "Auto-correlation recruitment", "Length 50% maturity", "Length 95% maturity")
	# cbind(Obsnam, LnName) 
  }
  if (Par == "Obs") {
     slots <- c("Cat", "Cat", "AvC", "AvC", "CAA", "CAA", "CAL", "CAL", "Ind", "Ind", "Dep",
     "Dep", "Dt", "Dt", "Mort", "FMSY_M", "BMSY_B0", "L50", "L95", "LFC", "LFS", 
     "Abun", "Abun", "vbK", "vbt0", "vbLinf", "Steep", "Iref", "Cref", "Bref", "ML", "ML")
	 Obsnam <- c("Cbias", "Csd", "Cbias", "Csd", "CAA_nsamp", "CAA_ESS", "CAL_nsamp", "CAL_ESS",    
     "Isd", "betas", "Dbias", "Derr", "Dbias", "Derr", "Mbias", "FMSY_Mbias",
     "BMSY_B0Bias", "lenMbias", "lenMbias", "LFCbias", "LFSbias", "Abias", "Aerr",
     "Kbias", "t0bias", "Linfbias", "hbias", "Irefbias", "Crefbias", "Brefbias", "CAL_nsamp", "CAL_ESS")
	 LnName <- c("Catch bias", "Catch error", "Catch bias", "Catch error", "n CAA samples",
     "CAA effective sample size", "n CAL samples", "CAL effective sample size",
     "Index Abundance error", "Hyperstability/hyperdepletion", "Depletion bias", 
     "Depletion error", "Depletion bias", "Depletion error", "M bias", "FMSY/M bias",
     "BMSY/B0 bias", "Length maturity bias", "Length maturity bias", 
     "Length first capture bias", "Length full capture bias", 
     "Current abundance bias", "Current abundance error", "vB K bias",            
     "vB t0 bias", "vB Linf bias",  "Steepness bias", "Reference index bias",
     "Reference catch bias", "Reference biomass bias", "Mean length", "Mean length")
	# cbind(slots, Obsnam, LnName) 
    for (mm in 1:nMPs) {
      ids <- Obsnam[slots%in%unlist(strsplit(Required(MPs[mm])[,2],split=", "))]
	  used[match(ids, varNames),mm] <- TRUE
    }
  }
  colnames(used) <- MPs 
  rownames(used) <- varNames
  
 
  # Find the highest Stat for each variable 
  Stat <- matrix(unlist(stat), ncol=nMPs) * used
  if (max(Stat, na.rm=TRUE) > 100) Stat <- Stat/100
  rownames(Stat) <- varNames
  statord <- apply(Stat, 2, order, decreasing=TRUE)
  
  topStat <- statord[1:nvars,] # highest nvars for each MP
  Out <- list()
  if (Par == "Obs") {
    Out$xvals <- xvals
    Out$stat <- Stat 
    Out$topStat <- topStat
    Out$smth <- smth
    Out$varNames <- varNames
	Out$MPs <- MPs
  }
   
  if(doPlot) {
    ## Create Plotting Space ##
	Ncol <- nvars 
	if (nMPs < 2) Ncol <- min(sum(used[,MPs]), nvars)
	if (nMPs > 1) {
	  temp <- apply(used[,MPs], 2, sum)
	  if (all(temp<nvars)) Ncol <- max(temp)
	}
	Nrow <- min(nMP, sum(apply(used, 2, sum) > 0 ))
	if (sum(apply(used, 2, sum) > 0) == 0) print(paste("No", Par, "used for these MPs"))
   
    mat <- matrix(1:(Nrow*Ncol), nrow=Nrow, byrow=TRUE)  
    par(mfrow=c(Nrow, Ncol), oma=c(3,6,2,0), mar=c(3,2,2,1))
    if (Par == "OM") Title <- "Operating Model Parameters"
    if (Par == "Obs") Title <- "Observation Parameters"
    
    # Colors and Controls
    ncols <- nrow(Stat) * ncol(Stat)
    Cols <- colorRampPalette(c("green", "red"))(ncols)
	# rev(rainbow(ncols,start=0,end=0.36))
	highest <- max(Stat)
    pch <- 18 
    LWD <- 3 
    LCol <- "black"
    count <- 1 
	mm <- 1
	AxCex <- 1.15
	doneMP <- 1 

	# Make MP colors for Available, Acceptable, and Not-Acceptable 
	availCol <- "green"
	acceptCol <- "black"
	nonAAcol <- "darkgray"
	mpcol <- "black" # default
	mpCols <- data.frame(MPs=MPs, col=rep(mpcol, nMPs), stringsAsFactors=FALSE)
	if (is.null(acceptMP)) acceptMP <- MPs 
	if (!is.null(availMP) & (!is.null(acceptMP))) {
	  mpCols[,2] <- nonAAcol
	  mpCols[MPs %in% acceptMP, 2] <- acceptCol
	  mpCols[MPs %in% acceptMP & MPs %in% availMP, 2] <- availCol
	}
    
	AllMPs <- 1:nMPs
	AllMPs <- AllMPs[apply(used, 2, sum) > 0] # only include MPs which use the parameter 
	AllMPs <- AllMPs[1:Nrow] # first nMPs 
	
	for (mm in AllMPs) { # Loop along MPs 
	  for (vr in 1:Ncol) { # Loop along variables
	    if (nMPs > 1) varind <- topStat[vr,mm] # Variable index 
		if (nMPs == 1) varind <- topStat[vr]
		varSN <- varNames[varind]
		varLN <- LnName[match(varSN,  Obsnam)]
		if (nMPs > 1) {
		  xs <- xvals[[1]][,varind]
	  	  ys <- yvals[[mm]]
		} else {
		  xs <- xvals[,varind]
		  ys <- yvals
        }	
		if (used[varSN, MPs[mm]]) { # variable is used
		  Col <- Cols[ceiling(Stat[varind,MPs[mm]]/highest * ncols)]
		  plot(xs, ys, col=Col, pch=pch, bty="n", axes=FALSE, xlab="", ylab="")
	  	  if (vr == 1) {
			MyCol <- mpCols[match(MPs[mm], mpCols[,1]),2]
	  	    axis(side=2, las=1, cex.axis=AxCex)
			mtext(side=2, MPs[mm], line=2.75, cex=1.4, col=MyCol)	
  	      }		  
		  if (vr != 1) axis(side=2, labels=FALSE)
	  	  axis(side=1, cex.axis=AxCex)
	  	  if(incStat) text(max(xs), 0.05*max(ys), round(Stat[varind,MPs[mm]],2), pos=2)		  
	      # Smoother line 
	      if (nMPs > 1) {
	  	    smX <- smth[[mm]][[varind]]$x
	  	    smY <- smth[[mm]][[varind]]$y		
		  } else {
		    smX <- smth[[varind]]$x
		    smY <- smth[[varind]]$y
		  }
          lines(smX, smY, lwd=LWD, col=LCol)
	      # Variable Name
		  if (!incNames) mtext(side=1, varSN, cex=1, line=2.5)	
		  if (incNames) mtext(side=1, varLN, cex=labcex, line=2.5)	
		  
		} else {
          plot(c(0,1), axes=FALSE, type="n", xlab="", ylab="")
        }	
      }
	}  
	mtext(side=3, outer=TRUE, Title, cex=1.5)
    if (YVar == "Y") mtext(side=2, outer=TRUE, "Long-term yield relative to MSY (%)", cex=1.25, line=3)
	if (YVar == "B") mtext(side=2, outer=TRUE, "B/BMSY in last 5 years", cex=1.25, line=3)
  }
 invisible(Out)
}

calcStat <- function(rr, evalbreaks) { # supporting function for above
  ind <- as.integer(evalbreaks/2)
  ind2 <- as.integer(0.1 * evalbreaks)
  ind3 <- as.integer(0.9 * evalbreaks)
  if (all(rr$x == 0)) return(0)
  sum((rr$y - mean(rr$y, na.rm=TRUE))^2)
}


calcMSESense <- function(MP=1, MSEobj, YVar=c("Y", "B")) { # supporting function for above 
  YVar <- match.arg(YVar)
  # Calculate for a single MP 
  if(length(MP) > 1) stop("Only one MP")
  nMPs <- MSEobj@nMPs 
  MPs <- MSEobj@MPs 
  if (class(MP) == "character")  mm <- which(MPs %in% MP)
  if (class(MP) == "numeric" | class(MP) == "integer") {
    mm <- MP
	MP <- MPs[mm]
  }
  nsims <- MSEobj@nsim
  RefYd <- MSEobj@OM$RefY
  yind <- max(MSEobj@proyears-4,1):MSEobj@proyears
  evalbreaks <- as.integer(nsims/4) # number of breaks for loess smoother
  if (YVar == "Y") {
    if (length(dim(MSEobj@C)) > 2) {
      yout <- apply(MSEobj@C[,mm,yind],1,mean,na.rm=T)/RefYd*100 
    } else {
      yout <- apply(MSEobj@C[,yind],1,mean,na.rm=T)/RefYd*100 
    }
  }	
  if (YVar == "B") {
    if (length(dim(MSEobj@B_BMSY)) > 2) {
      yout <- apply(MSEobj@B_BMSY[,mm,yind],1,mean,na.rm=T)
    } else {
      yout <- apply(MSEobj@B_BMSY[,yind],1,mean,na.rm=T)
    }
  }
  
  # Operating Model names to include 
  varnames <- names(MSEobj@OM)
  vars <- MSEobj@OM
  vargood <- (apply(vars,2,sd, na.rm=TRUE)/(apply(vars,2,mean, na.rm=TRUE)^2)^0.5)>0.005
  vargood[grep("qvar", varnames)] <- FALSE
  vargood[is.na(vargood)] <- TRUE
  varnames <- varnames[vargood] 
  
  omvals <- MSEobj@OM[,varnames]
  
  # Ignore these parameters from VOI plot
  omvals$RefY <- 0
  omvals$A <- 0
  omvals$OFLreal <- 0 
  omvals$FMSY <- 0 
  omvals$MSY <- 0 
  omvals$dFfinal <- 0 
  omvals[is.na(omvals)] <- 0

  # Apply loess smoother to Operating Model parameters
  OMSmooth <- suppressWarnings(apply(as.matrix(omvals), 2, loess.smooth, y=yout))
  
  # Calculate stat for OM curve
  OMStat <- unlist(lapply(OMSmooth, calcStat, evalbreaks=evalbreaks))
  
  # Observation Parameters
  varnames <- names(MSEobj@Obs)
  vars <- MSEobj@Obs 
  vargood <- (apply(vars,2,sd)/(apply(vars,2,mean)^2)^0.5)>0.005
  varnames <- varnames[vargood]
  obvals <- MSEobj@Obs[,varnames]
 
  # Apply loess smoother to Operating Model parameters
  OBSmooth <- suppressWarnings(apply(as.matrix(obvals), 2, loess.smooth, y=yout))

  # Calculate stat for OM curve
  OBStat <- unlist(lapply(OBSmooth, calcStat, evalbreaks=evalbreaks)) 
   
  Out <- list()
  Out$OMVals <- omvals
  Out$OMSmooth <- OMSmooth
  Out$OMStat <- OMStat 
  Out$OBVals <- obvals
  Out$OBSmooth <- OBSmooth
  Out$OBStat <-OBStat
  Out$YVar <- yout
  Out$MP <- MP
  Out
}

# Sub function - in DLMtool as well - but included here as server has old version
Sub <- function (MSEobj, MPs = NULL, sims = NULL, years = NULL) 
{
    checkMSE(MSEobj)
    Class <- class(MPs)
    if (Class == "NULL") 
        subMPs <- MSEobj@MPs
    if (Class == "integer" | Class == "numeric") 
        subMPs <- MSEobj@MPs[as.integer(MPs)]
    if (Class == "character") 
        subMPs <- MPs
    SubMPs <- match(subMPs, MSEobj@MPs)
    not <- (subMPs %in% MSEobj@MPs)
    ind <- which(not == FALSE)
    newMPs <- MSEobj@MPs[SubMPs]
    if (length(SubMPs) < 1) 
        stop("MPs not found")
    if (length(ind > 0)) {
        message("Warning: MPs not found - ", paste0(subMPs[ind], 
            " "))
        message("Subsetting by MPs: ", paste0(newMPs, " "))
    }
    ClassSims <- class(sims)
    if (ClassSims == "NULL") 
        SubIts <- 1:MSEobj@nsim
    if (ClassSims == "integer" | ClassSims == "numeric") {
        SubIts <- as.integer(sims)
    }
    if (ClassSims == "logical") 
        SubIts <- which(sims)
    nsim <- length(SubIts)
    ClassYrs <- class(years)
    AllNYears <- MSEobj@proyears
    if (ClassYrs == "NULL") 
        Years <- 1:AllNYears
    if (ClassYrs == "integer" | ClassYrs == "numeric") 
        Years <- years
    if (max(Years) > AllNYears) 
        stop("years exceeds number of years in MSE")
    if (min(Years) <= 0) 
        stop("years must be positive")
    if (min(Years) != 1) {
        message("Not starting from first year. Are you sure you want to do this?")
        message("Probably a bad idea!")
    }
    if (!all(diff(Years) == 1)) 
        stop("years are not consecutive")
    if (length(Years) <= 1) 
        stop("You are going to want more than 1 projection year")
    MSEobj@proyears <- max(Years)
    SubF <- MSEobj@F_FMSY[SubIts, SubMPs, Years, drop = FALSE]
    SubB <- MSEobj@B_BMSY[SubIts, SubMPs, Years, drop = FALSE]
    SubC <- MSEobj@C[SubIts, SubMPs, Years, drop = FALSE]
    SubBa <- MSEobj@B[SubIts, SubMPs, Years, drop = FALSE]
    SubFMa <- MSEobj@FM[SubIts, SubMPs, Years, drop = FALSE]
    SubTACa <- MSEobj@TAC[SubIts, SubMPs, Years, drop = FALSE]
    OutOM <- MSEobj@OM[SubIts, ]
    tt <- try(slot(MSEobj, "Effort"), silent = TRUE)
    if (class(tt) == "try-error") 
        slot(MSEobj, "Effort") <- array(NA)
    if (all(is.na(tt)) || all(tt == 0)) 
        slot(MSEobj, "Effort") <- array(NA)
    if (all(is.na(MSEobj@Effort))) {
        SubEffort <- array(NA)
    }
    else{
        SubEffort <- MSEobj@Effort[SubIts, SubMPs, Years, drop = FALSE]
    }
    tt <- try(slot(MSEobj, "SSB"), silent = TRUE)
    if (class(tt) == "try-error") 
        slot(MSEobj, "SSB") <- array(NA)
    if (all(is.na(tt)) || all(tt == 0)) 
        slot(MSEobj, "SSB") <- array(NA)
    if (all(is.na(MSEobj@SSB))) {
        SubSSB <- array(NA)
    }
    else {
        SubSSB <- MSEobj@SSB[SubIts, SubMPs, Years, drop = FALSE]
    }
    tt <- try(slot(MSEobj, "VB"), silent = TRUE)
    if (class(tt) == "try-error") 
        slot(MSEobj, "VB") <- array(NA)
    if (all(is.na(tt)) || all(tt == 0)) 
        slot(MSEobj, "VB") <- array(NA)
    if (all(is.na(MSEobj@VB))) {
        SubVB <- array(NA)
    }
    else {
        SubVB <- MSEobj@VB[SubIts, SubMPs, Years, drop = FALSE]
    }
    tt <- try(slot(MSEobj, "PAA"), silent = TRUE)
    if (class(tt) == "try-error") 
        slot(MSEobj, "PAA") <- array(NA)
    if (all(is.na(tt)) || all(tt == 0)) 
        slot(MSEobj, "PAA") <- array(NA)
    if (all(is.na(MSEobj@PAA))) {
        SubPAA <- array(NA)
    }
    else {
        SubPAA <- MSEobj@PAA[SubIts, SubMPs, , drop = FALSE]
    }
    tt <- try(slot(MSEobj, "CAL"), silent = TRUE)
    if (class(tt) == "try-error") 
        slot(MSEobj, "CAL") <- array(NA)
    if (all(is.na(tt)) || all(tt == 0)) 
        slot(MSEobj, "CAL") <- array(NA)
    if (all(is.na(MSEobj@CAL))) {
        SubCAL <- array(NA)
    }
    else {
        SubCAL <- MSEobj@CAL[SubIts, SubMPs, , drop = FALSE]
    }
    tt <- try(slot(MSEobj, "CAA"), silent = TRUE)
    if (class(tt) == "try-error") 
        slot(MSEobj, "CAA") <- array(NA)
    if (all(is.na(tt)) || all(tt == 0)) 
        slot(MSEobj, "CAA") <- array(NA)
    if (all(is.na(MSEobj@CAA))) {
        SubCAA <- array(NA)
    }
    else {
        SubCAA <- MSEobj@CAA[SubIts, SubMPs, , drop = FALSE]
    }
    CALbins <- MSEobj@CALbins
    SubResults <- new("MSE", Name = MSEobj@Name, nyears = MSEobj@nyears, 
        proyears = MSEobj@proyears, nMPs = length(SubMPs), MPs = newMPs, 
        nsim = length(SubIts), OM = OutOM, Obs = MSEobj@Obs[SubIts, 
            , drop = FALSE], B_BMSY = SubB, F_FMSY = SubF, B = SubBa, 
        SSB = SubSSB, VB = SubVB, FM = SubFMa, SubC, TAC = SubTACa, 
        SSB_hist = MSEobj@SSB_hist[SubIts, , , , drop = FALSE], 
        CB_hist = MSEobj@CB_hist[SubIts, , , , drop = FALSE], 
        FM_hist = MSEobj@FM_hist[SubIts, , , , drop = FALSE], 
        Effort = SubEffort, PAA = SubPAA, CAL = SubCAL, CAA = SubCAA, 
        CALbins = CALbins)
    return(SubResults)
}
 
# KPlot2
Kplot2 <- function(MSEobj,maxsim=60,nam=NA){ 
  # png("Kplot.png")
  nr<-floor((MSEobj@nMPs)^0.5)
  nc<-ceiling((MSEobj@nMPs)/nr)
  Cex <- 1.5
  TitleCex <- 1.5
    
  FMSYr<-quantile(MSEobj@F_FMSY,c(0.001,0.90),na.rm=T)
  BMSYr<-quantile(MSEobj@B_BMSY,c(0.001,0.975),na.rm=T)
    
  #dev.new2(width=nc*3,height=nr*3.6)
  # par(mfrow=c(nr,nc),mai=c(0.45,0.45,0.45,0.01),omi=c(0.45,0.3,0.35,0.01))
  par(mfcol=c(nr,nc),mai=c(0.2,0.35,0.3,0.01),omi=c(0.5,0.4,0.4,0.05))
  
  colsse<-rainbow(MSEobj@proyears,start=0.63,end=0.95)[1:MSEobj@proyears]
  colsse<-makeTransparent(colsse,95)
  
  XLim <- c(0, 3)
  YLim <- c(0, 2.5)
  
  for(mm in 1:MSEobj@nMPs){
    plot(c(MSEobj@B_BMSY[1,mm,1],MSEobj@B_BMSY[1,mm,2]),
         c(MSEobj@F_FMSY[1,mm,1],MSEobj@F_FMSY[1,mm,2]), xlim=XLim, ylim=YLim, #xlim=BMSYr,ylim=FMSYr,
         col=colsse[1],type='l', bty="n")
    
    OO<-round(sum(MSEobj@B_BMSY[,mm,MSEobj@proyears]<1&MSEobj@F_FMSY[,mm,MSEobj@proyears]>1,na.rm=T)/MSEobj@nsim*100,1)
    OU<-round(sum(MSEobj@B_BMSY[,mm,MSEobj@proyears]>1&MSEobj@F_FMSY[,mm,MSEobj@proyears]>1,na.rm=T)/MSEobj@nsim*100,1)
    UO<-round(sum(MSEobj@B_BMSY[,mm,MSEobj@proyears]<1&MSEobj@F_FMSY[,mm,MSEobj@proyears]<1,na.rm=T)/MSEobj@nsim*100,1)
    UU<-round(sum(MSEobj@B_BMSY[,mm,MSEobj@proyears]>1&MSEobj@F_FMSY[,mm,MSEobj@proyears]<1,na.rm=T)/MSEobj@nsim*100,1)
    
    #alp<-80
    #polygon(c(1,-1000,-1000,1),c(1,1,1000,1000),col=makeTransparent("orange",alp),border=makeTransparent("orange",alp))
    #polygon(c(1,1000,1000,1),c(1,1,1000,1000),col=makeTransparent("yellow",alp),border=makeTransparent("yellow",alp))
    #polygon(c(1,-1000,-1000,1),c(1,1,-1000,-1000),col=makeTransparent("yellow",alp),border=makeTransparent("yellow",alp))
    #polygon(c(1,1000,1000,1),c(1,1,-1000,-1000),col=makeTransparent("green",alp),border=makeTransparent("yellow",alp))
    
    
    abline(h=1,col="grey",lwd=3)
    abline(v=1,col="grey",lwd=3)
    #abline(v=c(0.1,0.5),col="grey",lwd=2)
	y <- 1:(MSEobj@proyears-1)
	y1 <- y + 1 
	x0 <- as.vector(MSEobj@B_BMSY[,mm, y])
	x1 <- as.vector(MSEobj@B_BMSY[,mm, y1])
	y0 <- as.vector(MSEobj@F_FMSY[,mm,y])
	y1 <- as.vector(MSEobj@F_FMSY[,mm,y1])
    segments(x0, y0, x1, y1, col=colsse)
    
	rng <- 1:min(maxsim, MSEobj@nsim)
    points(MSEobj@B_BMSY[rng,mm,1],MSEobj@F_FMSY[rng,mm,1],pch=19,cex=0.8,col=colsse[1])
    points(MSEobj@B_BMSY[rng,mm,MSEobj@proyears],MSEobj@F_FMSY[rng,mm,MSEobj@proyears],pch=19,cex=0.8,col=colsse[MSEobj@proyears])
    
    if(mm==1)legend('right',c("Start","End"),bty='n',text.col=c(colsse[1],colsse[MSEobj@proyears]),pch=19,col=c(colsse[1],colsse[MSEobj@proyears]))
    legend('topleft',paste(OO,"%",sep=""),bty='n',text.font=2)
    legend('topright',paste(OU,"%",sep=""),bty='n',text.font=2)
    legend('bottomleft',paste(UO,"%",sep=""),bty='n',text.font=2)
    legend('bottomright',paste(UU,"%",sep=""),bty='n',text.font=2)
    
    mtext(MSEobj@MPs[mm],3,line=0.6, cex=TitleCex)
  }
  mtext("B/BMSY",1,outer=T,line=1.2, cex=Cex)
  mtext("F/FMSY",2,outer=T,line=0.2, cex=Cex)
  if(is.na(nam))mtext(deparse(substitute(MSEobj)),3,outer=T,line=0.25,font=2, cex=TitleCex)
  if(!is.na(nam))mtext(MSEobj@Name,3,outer=T,line=0.25,font=2, cex=TitleCex)
  # dev.off()
}


# PMname <- function(PM){
  # switch(PM,
   # "Long-term Yield" = "Long-Term Yield (yrs 26-30, relative to fishing @ FMSY)",
   # "Short-term Yield" = "Short-Term Yield (10 yrs, relative to fishing @ FMSY)",
   # "Overfishing" = "Not Overfishing (probability of F < FMSY)",
   # "Biomass:BMSY" = "Not Overfished (probability Biomass > 0.5 BMSY)",
   # "Biomass:B0" = "Not Severely Overfished (probability Biomass > 0.2 BMSY)",
   # "AnnualVar" = "Inter-Annual Variability in Yield (probability < 15%)")
# }	
	
# ##Function to reduce Plotted MPs to available MPs by data
# PlottableMSE <- function(MSE, FeaseMPs) {
  # MSE@MPs <- feaseMPs
  # MSE@nMPs <- length(feaseMPs)
  # return(MSE)
# }
