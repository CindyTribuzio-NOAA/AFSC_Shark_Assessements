#Needed libraries
#library(shiny)
#library(DLMtool)
#library(ggplot2)
#library(grid)
#library(reshape2)
#library(shinyjs)
require(shiny)
require(DLMtool)
require(ggplot2)
require(grid)
require(reshape2)
require(shinyjs)
#Source code to load DLMtool objects
#jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
source('load_DLM.r',local = FALSE)
source('Functions.r',local = FALSE)
#source("D:/JMC/Documents/GitHub/DLmethods tools/Shiny_DLMtool/load_DLM.r")
#load.dlm.stuff<-function() {for(i in 1:length(DLMdat))assign(DLMdat[[i]]@Name,DLMdat[[i]])}
#for(i in 1:length(DLMdat))assign(DLMdat[[i]]@Name,DLMdat[[i]])

#START SHINY SERVER
shinyServer(function(input, output,session) {

#################  
### Functions ###
#################
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  VBGF<-function(Linf,k,t0,ages)
  {
    Lengths_exp<-Linf*(1-exp(-k*(ages-t0)))
    return(Lengths_exp)
  }
  
  VBGF.age<-function(Linf,k,t0,lt)
  {
    ages<-t0-(log(1-(lt/Linf))/k)
    return(ages)
  }
    
  LtWt.fit<-function(p,Lts,Ob.wts,return.type=2)
  { 
    if(return.type==1)
    {
      exp.wts<-p[1]*(Lts)^p[2]
      return(exp.wts)
    }
    if(return.type==2)
    {
      exp.wts<-p[1]*(Lts)^p[2]
      ssq.wts<-sum((exp.wts-Ob.wts)^2)
      return(ssq.wts)
    }    
  }
  
  Mat.fit<-function(p,Lts,Ob.mat,return.type=2)
  {  
    if(return.type==1)
    {
      exp.mat<-1/(1+exp(p[1]*(Lts-p[2])))
      return(exp.mat)
    }
    if(return.type==2)
    {
      exp.mat<-1/(1+exp(p[1]*(Lts-p[2])))
      ssq.matr<-sum((exp.mat-Ob.mat)^2)
      return(ssq.matr)
    }    
  }
  
  Sel.fit<-function(p,met.vec,Ob.sel,return.type=2)
  {
    if(return.type==1)
    {
      Exp.sel<-1/(1+exp(-log(19)*(met.vec-p[1])/p[2]))
      return(Exp.sel)
    }
    if(return.type==2)
    {
      Exp.sel<-1/(1+exp(-log(19)*(met.vec-p[1])/p[2]))
      ssq.sel<-sum((Exp.sel-Ob.sel)^2)
      return(ssq.sel)
    }
  }

############################
### End function section ###
############################

 # dlmstuff<-reactive({load.dlm.stuff()})
  #Start plots  
  output$Catchplot <- renderPlot({    
    inFile <- input$file1
  if (is.null(inFile)) return(NULL)
#  
  dlm_input<-new("DLM_data",stock=inFile$datapath)
  if(all(is.na(dlm_input@Cat))=="FALSE")
  {
    dlm_input.df<-data.frame(t(rbind(dlm_input@Year,dlm_input@Cat,dlm_input@Cat-(dlm_input@Cat*dlm_input@CV_Cat),dlm_input@Cat+(dlm_input@Cat*dlm_input@CV_Cat))))
    colnames(dlm_input.df)<-c("Year","Catch","lower","upper")
    ct.plot<-ggplot(dlm_input.df,aes(Year,Catch))+theme(axis.title = element_text(size=14))+geom_line(lwd=1)+geom_errorbar(aes(ymax = upper, ymin = lower),color="darkblue",width=0)+ggtitle("Removal history")+theme(plot.title = element_text(lineheight=.8,size=18, face="bold"))
    print(ct.plot)
  }
 })
  
  output$Indexplot <- renderPlot({    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    dlm_input<-new("DLM_data",stock=inFile$datapath)
    if(all(is.na(dlm_input@Ind))=="FALSE")
      {
        dlm_input.df<-data.frame(t(rbind(dlm_input@Year,dlm_input@Ind,dlm_input@Ind-(dlm_input@Ind*dlm_input@CV_Ind),dlm_input@Ind+(dlm_input@Ind*dlm_input@CV_Ind))))
        colnames(dlm_input.df)<-c("Year","Index","lower","upper")
        index.plot<-ggplot(dlm_input.df,aes(Year,Index))+theme(axis.title = element_text(size=14))+geom_line(lwd=1,color="black")+geom_errorbar(aes(ymax = upper, ymin = lower),color="brown",width=0)+ggtitle("Index time series")+theme(plot.title = element_text(lineheight=.8,size=18, face="bold"))
        print(index.plot)
      }
    })

  output$LHplots <- renderPlot({    
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    dlm_input<-new("DLM_data",stock=inFile$datapath)
   
    #VBGF
   if(any(is.na(c(dlm_input@vbLinf,dlm_input@vbK,dlm_input@vbt0)))=="FALSE")
    {
      dlm_input.vbgf<-data.frame(Age=c(0:dlm_input@MaxAge),Length=VBGF(dlm_input@vbLinf,dlm_input@vbK,dlm_input@vbt0,c(0:dlm_input@MaxAge)))
      dlm_input.vbgf.plot<-ggplot(dlm_input.vbgf,aes(Age,Length))+geom_line(lwd=2)+theme(axis.title = element_text(size=14))+
      annotate("text",x=0.9*max(dlm_input.vbgf$Age),y=c(0.25*max(dlm_input.vbgf$Length),0.15*max(dlm_input.vbgf$Length),0.05*max(dlm_input.vbgf$Length)),label=c(paste0("Linf=",round(dlm_input@vbLinf,2)),paste0("k=",round(dlm_input@vbK,2)),paste0("t0=",round(dlm_input@vbt0,2))),ymin=0,size=5)
      if(any(is.na(c(dlm_input@L50,dlm_input@L95)))=="FALSE")
       {
        age.mat<-data.frame(Age=VBGF.age(dlm_input@vbLinf,dlm_input@vbK,dlm_input@vbt0,c(dlm_input@L50,dlm_input@L95)),Length=c(dlm_input@L50,dlm_input@L95),Label=c("Lmat50%","Lmat95%"))
        #print(age.mat)
        dlm_input.vbgf.plot<-dlm_input.vbgf.plot+geom_point(data=age.mat,aes(Age,Length), color = "darkorange",size=6) +geom_text(data=age.mat,label=c("Lmat50%","Lmat95%"),nudge_x = 0.15*max(dlm_input.vbgf$Age))
       # dlm_input.vbgf.plot<-dlm_input.vbgf.plot+geom_text(aes(Age,length),data=age.mat,label=c("L50%","L95%")) 
       }
   }
    if(any(is.na(c(dlm_input@vbLinf,dlm_input@vbK,dlm_input@vbt0)))=="TRUE")
    {
        df <- data.frame()
        dlm_input.vbgf.plot<- ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
        theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
        theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        annotate("text",x=0,y=0,label="VBGF not available",size=5)
    }
    
    #Length-weight
    if(any(is.na(c(dlm_input@wla,dlm_input@wlb)))=="FALSE")
    {
      max.lt<-max(dlm_input@L95,dlm_input@vbLinf,na.rm = TRUE)
      if(max.lt== -Inf){max.lt<-100}
      dlm_input.lw<-data.frame(cbind(c(0:max.lt),LtWt.fit(c(dlm_input@wla,dlm_input@wlb),c(0:max.lt),return.type=1)))
      colnames(dlm_input.lw)<-c("Length","Weight")
      dlm_input.lw.plot<-ggplot(dlm_input.lw,aes(Length,Weight))+geom_line(lwd=2)+theme(axis.title = element_text(size=14))+
      annotate("text",x=0.9*max(dlm_input.lw$Length),y=c(0.25*max(dlm_input.lw$Weight),0.15*max(dlm_input.lw$Weight)),label=c(paste0("a=",round(dlm_input@wla,5)),paste0("b=",round(dlm_input@wlb,2))),xmin=0,ymin=0,size=5)
#      print(dlm_input.lw.plot)
    }
    if(any(is.na(c(dlm_input@wla,dlm_input@wlb)))=="TRUE")
    {
      df <- data.frame()
      dlm_input.lw.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      annotate("text",x=0,y=0,label="Lt-Wt not available",size=5)
    }
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(1, 2)))
    print(dlm_input.vbgf.plot, vp = vplayout(1, 1))
    print(dlm_input.lw.plot, vp = vplayout(1, 2))
  })

  output$Parameterplots <- renderPlot({    
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      dlm_input<-new("DLM_data",stock=inFile$datapath)
      dlm_parameters.df<-data.frame(Parameters=c(dlm_input@AvC,dlm_input@Dt,dlm_input@Mort,dlm_input@FMSY_M,dlm_input@BMSY_B0,dlm_input@Dep,dlm_input@Abun,dlm_input@steep),CV=c(dlm_input@CV_AvC,dlm_input@CV_Dt,dlm_input@CV_Mort,dlm_input@CV_FMSY_M,dlm_input@BMSY_B0,dlm_input@CV_Dep,dlm_input@CV_Abun,dlm_input@CV_steep))
      draws<-50000
      #Average catch distribution
      if(any(is.na(dlm_parameters.df[1,]))==FALSE)
      {
        AvC.dist<-data.frame(AvC=rnorm(draws,dlm_parameters.df[1,1],dlm_parameters.df[1,1]*dlm_parameters.df[1,2]))
        AvC.plot<-ggplot(AvC.dist,aes(AvC))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[1,])))
      {
        df <- data.frame()
        AvC.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="Average Catch NA",size=5)
      }
      #Depletion over time distribution
      if(any(is.na(dlm_parameters.df[2,]))==FALSE)
      {
        DepT.dist<-data.frame(DepT=rnorm(draws,dlm_parameters.df[2,1],dlm_parameters.df[2,1]*dlm_parameters.df[2,2]))
        DepT.plot<-ggplot(DepT.dist,aes(DepT))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[2,])))
      {
        df <- data.frame()
        DepT.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="Bnow/Bthen NA",size=5)
      }
      #Natural mortality distribution
      if(any(is.na(dlm_parameters.df[3,]))==FALSE)
      {
        M.dist<-data.frame(M=rnorm(draws,dlm_parameters.df[3,1],dlm_parameters.df[3,1]*dlm_parameters.df[3,2]))
        M.plot<-ggplot(M.dist,aes(M))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[3,])))
      {
        df <- data.frame()
        M.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="Natural mortality NA",size=5)
      }
      #FMSY/M distribution
      if(any(is.na(dlm_parameters.df[4,]))==FALSE)
      {
        FMSY_M.dist<-data.frame(FMSY_M=rnorm(draws,dlm_parameters.df[4,1],dlm_parameters.df[4,1]*dlm_parameters.df[4,2]))
        FMSY_M.plot<-ggplot(FMSY_M.dist,aes(FMSY_M))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[4,])))
      {
        df <- data.frame()
        FMSY_M.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="FMSY/M NA",size=5)
      }
      #BMSY/B0 distribution
      if(any(is.na(dlm_parameters.df[5,]))==FALSE)
      {
        BMSY_B0.dist<-data.frame(BMSY_B0=rnorm(draws,dlm_parameters.df[5,1],dlm_parameters.df[5,1]*dlm_parameters.df[5,2]))
        BMSY_B0.plot<-ggplot(BMSY_B0.dist,aes(BMSY_B0))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[5,])))
      {
        df <- data.frame()
        BMSY_B0.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="BMSY/B0 NA",size=5)
      }
      #Depletion distribution
      if(any(is.na(dlm_parameters.df[6,]))==FALSE)
      {
        Dep.dist<-data.frame(Dep=rnorm(draws,dlm_parameters.df[6,1],dlm_parameters.df[6,1]*dlm_parameters.df[6,2]))
        Dep.plot<-ggplot(Dep.dist,aes(Dep))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[6,])))
      {
        df <- data.frame()
        Dep.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="Depletion NA",size=5)
      }
      #Current abundance distribution
      if(any(is.na(dlm_parameters.df[7,]))==FALSE)
      {
        Abund.dist<-data.frame(Abund=rnorm(draws,dlm_parameters.df[7,1],dlm_parameters.df[7,1]*dlm_parameters.df[7,2]))
        Abund.plot<-ggplot(Abund.dist,aes(Abund))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[7,])))
      {
        df <- data.frame()
        Abund.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="Current abundance NA",size=5)
      }
      #Steepness distribution
      if(any(is.na(dlm_parameters.df[8,]))==FALSE)
      {
        h.dist<-data.frame(h=rnorm(draws,dlm_parameters.df[8,1],dlm_parameters.df[8,1]*dlm_parameters.df[8,2]))
        h.plot<-ggplot(h.dist,aes(h))+geom_density()
      }
      if(any(is.na(dlm_parameters.df[8,])))
      {
        df <- data.frame()
        h.plot<-ggplot(df) + geom_point() + xlim(0, 0) + ylim(0, 0)+xlab("")+ylab("")+
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
          annotate("text",x=0,y=0,label="Steepness NA",size=5)
      }
      
      
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(4, 2)))
      print(AvC.plot, vp = vplayout(1, 1))
      print(DepT.plot, vp = vplayout(1, 2))
      print(M.plot, vp = vplayout(2, 1))
      print(FMSY_M.plot, vp = vplayout(2, 2))
      print(BMSY_B0.plot, vp = vplayout(3, 1))
      print(Dep.plot, vp = vplayout(3, 2))
      print(Abund.plot, vp = vplayout(4, 1))
      print(h.plot, vp = vplayout(4, 2))
      
    })
  
  #Get output control methods that work with data set
  canlist<-reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    dlm_input<-new("DLM_data",stock=inFile$datapath)
    canlist<-as.list(Can(dlm_input))
  })
 
  cantlist<-reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    dlm_input<-new("DLM_data",stock=inFile$datapath)
    cantlist<-Cant(dlm_input)
  })
  
   output$choicelist<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    #print(canlist())
    output.canlist<-canlist()[canlist() %in% avail("DLM_output")]
    checkboxGroupInput("checkGroup","Available data-limited output control methods",output.canlist)
  })

  observe({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    output.canlist<-canlist()[canlist() %in% avail("DLM_output")]
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"checkGroup","Available data-limited output control methods", output.canlist)
    }
    else
    {
      updateCheckboxGroupInput(session,"checkGroup","Available data-limited output control methods",choices= output.canlist,selected= output.canlist)
    }
  })

  #Sensitivity options
  output$sensilist<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    output.canlist<-canlist()[canlist() %in% avail("DLM_output")]
    radioButtons("radio","Choose data-limited output control method to explore sensitivity",output.canlist)
  })


  #output the DL methods that are not available  
  output$MP_NA<-renderPrint({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    else{
      dlm_input<-new("DLM_data",stock=inFile$datapath)
      cant.table<-data.frame(cbind(cantlist(),Needed(dlm_input)))
      colnames(cant.table)<-c("Method","Reason","Needed")
      return(cant.table)
    }
  })
    
  #Run TAC and plot results
  MP.labs<-eventReactive(input$run_dlm,{input$checkGroup})
  
  TAC.out<-eventReactive(input$run_dlm,{
    #if(run.check()>0){
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      dlm_input<-new("DLM_data",stock=inFile$datapath)
      TAC.out<-TAC(dlm_input,MPs=input$checkGroup,reps=input$TACreps)@TAC[,,1]
      return(TAC.out)
    }
  )

    output$TACplots<- renderPlot({    
      TAC.out<-TAC.out()
      TAC.df<-data.frame(t(TAC.out))
      if(is.null(dim(TAC.out))==TRUE){
        TAC.df.melt<-melt(TAC.df)
        TAC.df.melt[,1]<-MP.labs()
        TAC.plot<-ggplot(data=TAC.df.melt,aes(as.factor(variable),value))+geom_boxplot()+ coord_flip()+labs(x="DL Method",y="TAC")+ylim(0,quantile(TAC.out,0.95,na.rm=T))
        }
      if(is.null(dim(TAC.out))==FALSE){
        colnames(TAC.df)<-MP.labs()
        TAC.df.melt<-melt(TAC.df)
        TAC.plot<-ggplot(data=TAC.df.melt,aes(as.factor(variable),value))+geom_boxplot()+ coord_flip()+labs(x="DL Method",y="TAC")+ylim(0,quantile(TAC.out,0.95,na.rm=T))
        }
      print(TAC.plot)
      output$downloadTAC <- downloadHandler(
        filename = function() { paste("TAC",Sys.time(), '.csv', sep='') },
        content = function(file) {write.csv(TAC.df, file)}) 
      output$downloadTACobj <- downloadHandler(
        filename = function() {  paste0("TAC",Sys.time(),".DMP", sep='') },
        content = function(file) {save(TAC.out,file=file)}) 
      output$downloadTACbarplot <- downloadHandler(
        filename = function() { paste('TACbarplot',Sys.time(), '.png', sep='') },
        content = function(file) {
          png(file, type='cairo',width=800,height=720)
          print(TAC.plot)
          dev.off()},contentType = 'image/png') 
    })
    
  
  output$wtedTAC<- renderPlot({    
    TAC.df<-data.frame(t(TAC.out()))
    TAC.df.melt<-melt(TAC.df)
    TAC.densityplot<- ggplot(data=TAC.df.melt,aes(value))+
      geom_density(fill="gray")+xlim(0,quantile(TAC.out(),0.95,na.rm=T))+
      labs(x="TAC",y="Density")+
      geom_vline(xintercept = quantile(TAC.out(),0.5,na.rm=T),color=c("darkblue"),size=1.2)+
      geom_vline(xintercept = quantile(TAC.out(),input$Pstar,na.rm=T),linetype = "longdash",color="darkgreen",size=1)
    print(TAC.densityplot)
    output$downloadTACdensityplot <- downloadHandler(
      filename = function() { paste('TACdensityplot',Sys.time(), '.png', sep='') },
      content = function(file) {
        png(file, type='cairo',width=800,height=720)
        print(TAC.densityplot)
        dev.off()},contentType = 'image/png') 
  })
  

  output$Sensiplot<-renderPlot({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    dlm_input<-new("DLM_data",stock=inFile$datapath)
    TAC.out<-TAC(dlm_input,MPs=input$radio,reps=input$sensireps)
    dlm_TAC_sensi<-Sense(TAC.out,MP=input$radio,nsense=input$nsensi,reps=input$sensireps,perc = c(input$lowperc,0.5,input$upperperc))
    output$downloadSensi <- downloadHandler(
      filename = function() { paste('Sensi',input$radio, '.png', sep='') },
      content = function(file) {
        png(file, type='cairo',width=800,height=720)
        Sense(TAC.out,MP=input$radio,nsense=input$nsensi,reps=input$sensireps,perc = c(input$lowperc,0.5,input$upperperc))
        dev.off()},contentType = 'image/png') 
    output$downloadSensiobj <- downloadHandler(
      filename = function() {  paste0("Sensitivities_",input$radio,"_",Sys.time(),".DMP", sep='') },
      content = function(file) {save(dlm_TAC_sensi,file=file)}) 
  })
  
#######################
######## MSE ##########
#######################
  

  #ID output control rules
  output$can.list.output<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    canlist.output<-canlist()[canlist() %in% avail("DLM_output")]
    checkboxGroupInput("checkGroupout","Available output methods",canlist.output)
  })
  
  
  observe({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    canlist.output<-canlist()[canlist() %in% avail("DLM_output")]
    if(input$allselect == 0) return(NULL) 
    else if (input$allselect%%2 == 0)
    {
      updateCheckboxGroupInput(session,"checkGroupout","Available output methods", canlist.output)
    }
    else
    {
      updateCheckboxGroupInput(session,"checkGroupout","Available output methods",choices= canlist.output,selected= canlist.output)
    }
  })
 
  #ID input control rules
  output$can.list.input<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    canlist.input<-canlist()[canlist() %in% avail("DLM_input")]
    checkboxGroupInput("checkGroupin","Available input methods",canlist.input)
  })
  
  
  observe({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    canlist.input<-canlist()[canlist() %in% avail("DLM_input")]
    if(input$selectinput == 0) return(NULL) 
    else if (input$selectinput%%2 == 0)
    {
      updateCheckboxGroupInput(session,"checkGroupin","Available input methods", canlist.input)
    }
    else
    {
      updateCheckboxGroupInput(session,"checkGroupin","Available input methods",choices= canlist.input,selected= canlist.input)
    }
  })
 
  
  #ID unavailable output control rules
   output$cant.list.output<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    cantlist.output<-cantlist()[cantlist() %in% avail("DLM_output")]
    checkboxGroupInput("checkGroupNAout","Unavailable output methods",cantlist.output)
   })
  
  
  observe({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    cantlist.output<-cantlist()[cantlist() %in% avail("DLM_output")]
    if(input$allselectNAop == 0) return(NULL) 
    else if (input$allselectNAop%%2 == 0)
    {
      updateCheckboxGroupInput(session,"checkGroupNAout","Unavailable output methods", cantlist.output)
    }
    else
    {
      updateCheckboxGroupInput(session,"checkGroupNAout","Unavailable output methods",choices= cantlist.output,selected= cantlist.output)
    }
  })
  
  #ID input control rules
  output$cant.list.input<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    cantlist.input<-cantlist()[cantlist() %in% avail("DLM_input")]
    checkboxGroupInput("checkGroupNAin","Unavailable input methods",cantlist.input)
  })
  
  
  observe({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    cantlist.input<-cantlist()[cantlist() %in% avail("DLM_input")]
    if(input$allselectNAip == 0) return(NULL) 
    else if (input$allselectNAip%%2 == 0)
    {
      updateCheckboxGroupInput(session,"checkGroupNAin","Unvailable input methods", cantlist.input)
    }
    else
    {
      updateCheckboxGroupInput(session,"checkGroupNAin","Unavailable input methods",choices= cantlist.input,selected= cantlist.input)
    }
  })

#Combine all selected MPs into one object
    MPs<-reactive({
    MPs<-c(input$checkGroupout,input$checkGroupin,input$checkGroupNAout,input$checkGroupNAin)
    return(MPs)
  })
  
#Choose with to plot for Kobe, projection and VOI
  output$subMPs<-renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    selectizeInput("plotsubMPs","Choose methods for the Kobe, projection and VOI plots",choices= MPs(),multiple=TRUE)
  })
    
 # updateSelectizeInput(session,'subMPs',choices = MPs(), server = TRUE)
  
  
  #Stock inputs
  output$stock.choicelist<-renderUI({
    if (is.null(avail("Stock"))) return(NULL) 
    else {Stocks <- avail("Stock")
    selectInput("stock","Choose Stock type",as.list(Stocks),selected = Stocks[1])}
  })
  
  observeEvent(input$stock, {output$stock.maxage<-renderUI({
    numericInput("maxage","max. age",value=get(input$stock)@maxage,min=1, max=300, step=1)
    })
  })
  observeEvent(input$stock, {output$stock.M<-renderUI({
    sliderInput("M","M",min=0, max=2, value=get(input$stock)@M,step =0.01)
    })
  })
  observeEvent(input$stock, {output$stock.Msd<-renderUI({
    sliderInput("Msd","SD of M",min=0, max=2, value=get(input$stock)@Msd,step =0.1)
    })
  })
  observeEvent(input$stock, {output$stock.Mgrad<-renderUI({
    sliderInput("Mgrad","M gradient",min=-2, max=2, value=get(input$stock)@Mgrad,step =0.01)
  })
  observe({
    if (input$zerogradients%%2 == 0)
    {
      updateSliderInput(session,"Mgrad","M gradient",min=-2, max=2, value=get(input$stock)@Mgrad,step =0.01)
    }
    else
    {
      updateSliderInput(session,"Mgrad","M gradient",min=-2, max=2, value=c(0,0),step =0.01)
    }
  })
  })
  
  
  observeEvent(input$stock, {output$stock.Linf<-renderUI({
    sliderInput("Linf","Linf",min=0, max=1500, value=get(input$stock)@Linf,step =1)
    })
  })
  observeEvent(input$stock, {output$stock.Linfsd<-renderUI({
    sliderInput("Linfsd","SD of Linf",min=0, max=2, value=get(input$stock)@Linfsd,step =0.001)
    })
  })
  observeEvent(input$stock, {output$stock.Linfgrad<-renderUI({
    sliderInput("Linfgrad","Linf gradient",min=-2, max=2, value=get(input$stock)@Linfgrad,step =0.01)
    })
  observe({
    if (input$zerogradients%%2 == 0)
    {
      updateSliderInput(session,"Linfgrad","Linf gradient",min=-2, max=2, value=get(input$stock)@Linfgrad,step =0.01)
    }
    else
    {
      updateSliderInput(session,"Linfgrad","Linf gradient",min=-2, max=2, value=c(0,0),step =0.01)
    }
  })
  })
  observeEvent(input$stock, {output$stock.K<-renderUI({
    sliderInput("K","K",min=0, max=2, value=get(input$stock)@K,step =0.001)
    })
  })
  observeEvent(input$stock, {output$stock.Ksd<-renderUI({
    sliderInput("Ksd","SD of K",min=0, max=2, value=get(input$stock)@Ksd,step =0.001)
    })
  })
  observeEvent(input$stock, {output$stock.Kgrad<-renderUI({
    sliderInput("Kgrad","K gradient",min=-2, max=2, value=get(input$stock)@Kgrad,step =0.01)
    })
  observe({
    if (input$zerogradients%%2 == 0)
    {
      updateSliderInput(session,"Kgrad","K gradient",min=-2, max=2, value=get(input$stock)@Kgrad,step =0.01)
    }
    else
    {
      updateSliderInput(session,"Kgrad","K gradient",min=-2, max=2, value=c(0,0),step =0.01)
    }
  })
  })
  observeEvent(input$stock, {output$stock.t0<-renderUI({
    sliderInput("t0","t0",min=-10, max=10, value=get(input$stock)@t0,step =0.01)
    })
  })
  observeEvent(input$stock, {output$stock.WtLt_a<-renderUI({
    numericInput("WtLt_a","W-L a",value=get(input$stock)@a,min=0, max=1, step=0.0000001)
    })
  })
  observeEvent(input$stock, {output$stock.WtLt_b<-renderUI({
    numericInput("WtLt_b","W-L b",value=get(input$stock)@b,min=2, max=4, step=0.01)
    })
  })
  observeEvent(input$stock, {output$stock.R0<-renderUI({
    numericInput("R0","Initial recruitment (R0)",value=get(input$stock)@R0,min=0, max=1000000, step=10)
    })
  })
  observeEvent(input$stock, {output$stock.SRrel<-renderUI({
    numericInput("SRrel","SR: BH=1; Ricker=2",value=get(input$stock)@SRrel,min=1, max=2, step=1)
    })
  })
  observeEvent(input$stock, {output$stock.h<-renderUI({
    sliderInput("h","Steepness",min=0.2, max=1, value=get(input$stock)@h,step =0.01)
    })
  })
  observeEvent(input$stock, {output$stock.recgrad<-renderUI({
    sliderInput("recgrad","Rec. gradient",min=-10, max=10, value=get(input$stock)@recgrad,step =0.1)
    })
  observe({
    if (input$zerogradients%%2 == 0)
    {
      updateSliderInput(session,"recgrad","Rec. gradient",min=-10, max=10, value=get(input$stock)@recgrad,step =0.1)
    }
    else
    {
      updateSliderInput(session,"recgrad","Rec. gradient",min=-10, max=10, value=c(0,0),step =0.1)
    }
  })
  })
  observeEvent(input$stock, {output$stock.Perr<-renderUI({
    sliderInput("Perr","Rec. dev. error",min=0, max=2, value=get(input$stock)@Perr,step =0.1)
    })
  })
  observeEvent(input$stock, {output$stock.AC<-renderUI({
    sliderInput("AC","Rec. autocorrelation",min=0, max=1, value=get(input$stock)@AC,step =0.01)
    })
  })
  observeEvent(input$stock, {output$stock.L50<-renderUI({
    sliderInput("L50","Length at 50% maturity",min=0, max=1000, value=get(input$stock)@L50,step =1)
    })
  })
  observeEvent(input$stock, {output$stock.L50_95<-renderUI({
    sliderInput("L50_95","Lt. increment 50%-95% mat.",min=0, max=100, value=get(input$stock)@L50_95,step =1)
    })
  })
  observeEvent(input$stock, {output$stock.D<-renderUI({
    sliderInput("D","Current stock status",min=0, max=1, value=get(input$stock)@D,step =0.01)
    })
  })
  observeEvent(input$stock, {output$stock.Size_area<-renderUI({
    sliderInput("Size_area","Relative size of area",min=0, max=1, value=get(input$stock)@Size_area_1,step =0.01)
    })
  })
  observeEvent(input$stock, {output$stock.Prob_staying<-renderUI({
    sliderInput("Prob_staying","Prob. staying in area",min=0, max=1, value=get(input$stock)@Prob_staying,step =0.01)
    })
  })
  observeEvent(input$stock, {output$stock.Frac_area<-renderUI({
    sliderInput("Frac_area","Fraction of initial unfished B in area",min=0, max=1, value=get(input$stock)@Frac_area_1,step =0.01)
    })
  })
  
  
  #Fleet inputs
  output$fleet.choicelist<-renderUI({
    if (is.null(avail("Fleet"))) return(NULL) 
    else {Fleets <- avail("Fleet")
    selectInput("fleet","Choose Fleet type",as.list(Fleets),selected = Fleets[1])}
  })

  observeEvent(input$fleet, {output$fleet.nyrs<-renderUI({
    numericInput("fleetyrs","# of years before MP engaged",value=get(input$fleet)@nyears,min=1, max=1000, step=1)
    })
  })
  observeEvent(input$fleet, {output$fleet.spattarg<-renderUI({
    sliderInput("Spat_targ","Fishing reltiave to vul. biomass",min=1, max=10, value=get(input$fleet)@Spat_targ,step =0.01)
    })
  })
  observeEvent(input$fleet, {output$fleet.L5<-renderUI({
    sliderInput("L5","Length at 5% selectivity",min=0, max=1, value=get(input$fleet)@L5,step =0.01)
    })
  })
  observeEvent(input$fleet, {output$fleet.LFS<-renderUI({
    sliderInput("LFS","Length at full selectivity",min=0, max=1, value=get(input$fleet)@LFS,step =0.01)
    })
  })
  observeEvent(input$fleet, {output$fleet.Vmaxlen<-renderUI({
    sliderInput("Vmaxlen","Selectivity of the longest length class",min=0, max=1, value=get(input$fleet)@Vmaxlen,step =0.01)
    })
  })
  observeEvent(input$fleet, {output$fleet.Fsd<-renderUI({
    sliderInput("Fsd","Interannual variability in F",min=0, max=10, value=get(input$fleet)@Fsd,step =0.01)
    })
  })
  observeEvent(input$fleet, {output$fleet.qinc<-renderUI({
    sliderInput("qinc","Mean percent change in fishing efficiency",min=-100, max=100, value=get(input$fleet)@qinc,step =1)
    })
  })
  observeEvent(input$fleet, {output$fleet.qcv<-renderUI({
    sliderInput("qcv","Interannual variability in fishing efficiency",min=-0, max=10, value=get(input$fleet)@qcv,step =0.1)
    })
  })

#Observation inputs  
  output$obs.choicelist<-renderUI({
    if (is.null(avail("Observation"))) return(NULL) 
    else {ObsMod <- avail("Observation")}
    selectInput("obs","Choose Observation model",as.list(ObsMod),selected = ObsMod[1])
  })
  
  observeEvent(input$obs, {output$Obs.LenMcv<-renderUI({
    numericInput("LenMcv","Bias: L50%",value=get(input$obs)@LenMcv,min=0, max=5, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Cobs<-renderUI({
    sliderInput("Cobs","Error in catches",min=-0, max=5, value=get(input$obs)@Cobs, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Cbiascv<-renderUI({
    numericInput("Cbiascv","Annual catch bias",value=get(input$obs)@Cbiascv,min=-5, max=5, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.CAA_nsamp<-renderUI({
    sliderInput("CAA_nsamp","Catch at age sample/year",min=-0, max=2500, value=get(input$obs)@CAA_nsamp, step=1)
    })
  })
  observeEvent(input$obs, {output$Obs.CAA_ESS<-renderUI({
    sliderInput("CAA_ESS","Effective age samples/year",min=-0, max=1250, value=get(input$obs)@CAA_ESS, step=1)
    })
  })
  observeEvent(input$obs, {output$Obs.CAL_nsamp<-renderUI({
    sliderInput("CAL_nsamp","Catch at length samples/year",min=-0, max=10000, value=get(input$obs)@CAL_nsamp, step=1)
    })
  })
  observeEvent(input$obs, {output$Obs.CAL_ESS<-renderUI({
    sliderInput("CAL_ESS","Effective age samples/year",min=-0, max=1250, value=get(input$obs)@CAL_ESS, step=1)
    })
  })
  observeEvent(input$obs, {output$Obs.CALcv<-renderUI({
    sliderInput("CALcv","Variability in length at age",min=-0, max=1, value=get(input$obs)@CALcv, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Iobs<-renderUI({
    sliderInput("Iobs","Obs. error in rel. abund. index",min=-0, max=2, value=get(input$obs)@Iobs, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Icv<-renderUI({
    numericInput("Icv","Bias: rel. abund. index",value=get(input$obs)@Icv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Mcv<-renderUI({
    numericInput("Mcv","Bias: natural mortality",value=get(input$obs)@Mcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Linfcv<-renderUI({
    numericInput("Linfcv","Bias: VBGF Linf",value=get(input$obs)@Linfcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Kcv<-renderUI({
    numericInput("Kcv","Bias: VBGF k",value=get(input$obs)@Kcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.t0cv<-renderUI({
    numericInput("t0cv","Bias: VBGF t0",value=get(input$obs)@t0cv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.LFCcv<-renderUI({
    numericInput("LFCcv","Bias: length at first capture",value=get(input$obs)@LFCcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.LFScv<-renderUI({
    numericInput("LFScv","Bias: length at 1st full selectivity",value=get(input$obs)@LFScv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.B0cv<-renderUI({
    numericInput("B0cv","Bias: unfished stock size",value=get(input$obs)@B0cv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.FMSYcv<-renderUI({
    numericInput("FMSYcv","Bias: FMSY",value=get(input$obs)@FMSYcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.FMSY_Mcv<-renderUI({
    numericInput("FMSY_Mcv","Bias: FMSY/M",value=get(input$obs)@FMSY_Mcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.BMSY_B0cv<-renderUI({
    numericInput("BMSY_B0cv","Bias: BMSY/B0",value=get(input$obs)@BMSY_B0cv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.rcv<-renderUI({
    numericInput("rcv","Bias: intrinsic rate of increase",value=get(input$obs)@rcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Dbiascv<-renderUI({
    numericInput("Dbiascv","Bias: stock status",value=get(input$obs)@Dbiascv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Dcv<-renderUI({
    sliderInput("Dcv","Obs. error in stock status",min=-0, max=2, value=get(input$obs)@Dcv, step=0.01)
    })  
  })
  observeEvent(input$obs, {output$Obs.Btbias<-renderUI({
    sliderInput("Btbias","Bounds in obs. error of current stock scale",min=-0, max=2, value=get(input$obs)@Btbias, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Btcv<-renderUI({
    sliderInput("Btcv","Obs. error in current stock scale",min=-0, max=2, value=get(input$obs)@Btcv, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Fcurbiascv<-renderUI({
    numericInput("Fcurbiascv","Bias: current F",value=get(input$obs)@Fcurbiascv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Fcurcv<-renderUI({
    sliderInput("Fcurcv","Obs. error in current F",min=-0, max=2, value=get(input$obs)@Fcurcv, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.hcv<-renderUI({
    numericInput("hcv","Bias: steepness",value=get(input$obs)@hcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.maxagecv<-renderUI({
    numericInput("maxagecv","Bias in max. age",value=get(input$obs)@maxagecv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Reccv<-renderUI({
    sliderInput("Reccv","Obs. error in for recruitment slope",min=-0, max=2, value=get(input$obs)@Reccv, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Irefcv<-renderUI({
    numericInput("Irefcv","Bias: target reference stock status",value=get(input$obs)@Irefcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Crefcv<-renderUI({
    numericInput("Crefcv","Bias: target catch (e.g., MSY)",value=get(input$obs)@Crefcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.Brefcv<-renderUI({
    numericInput("Brefcv","Bias: target reference biomass (e.g. BMSY)",value=get(input$obs)@Brefcv,min=0, max=2, step=0.01)
    })
  })
  observeEvent(input$obs, {output$Obs.beta<-renderUI({
    sliderInput("beta","Bounds index exponent",min=0, max=5, value=get(input$obs)@beta, step=0.01)
  })
  })

  
#Run MSE
    OM<-reactive({
    #Update Stock inputs
    stock.in<-get(input$stock)
    stock.in@maxage<-input$maxage
    stock.in@M<-input$M
    stock.in@Msd<-input$Msd
    stock.in@Mgrad<-input$Mgrad
    stock.in@Linf<-input$Linf
    stock.in@Linfsd<-input$Linfsd
    stock.in@Linfgrad<-input$Linfgrad
    stock.in@K<-input$K
    stock.in@Ksd<-input$Ksd
    stock.in@Kgrad<-input$Kgrad
    stock.in@t0<-input$t0
    stock.in@a<-input$WtLt_a
    stock.in@b<-input$WtLt_b
    stock.in@R0<-input$R0
    stock.in@SRrel<-input$SRrel
    stock.in@h<-input$h
    stock.in@recgrad<-input$recgrad
    stock.in@Perr<-input$Perr
    stock.in@AC<-input$AC
    stock.in@L50<-input$L50
    stock.in@L50_95<-input$L50_95
    stock.in@D<-input$D
    stock.in@Size_area_1<-input$Size_area
    stock.in@Prob_staying<-input$Prob_staying
    stock.in@Frac_area_1<-input$Frac_area

    fleet.in<-get(input$fleet)
    fleet.in@nyears<-input$fleetyrs
    fleet.in@Spat_targ<-input$Spat_targ
    fleet.in@L5<-input$L5
    fleet.in@LFS<-input$LFS
    fleet.in@Vmaxlen<-input$Vmaxlen
    fleet.in@Fsd<-input$Fsd
    fleet.in@qinc<-input$qinc
    fleet.in@qcv<-input$qcv
    
    obsmod.in<-get(input$obs)
    
    obsmod.in@LenMcv<-input$LenMcv
    obsmod.in@Cobs<-input$Cobs
    obsmod.in@Cbiascv<-input$Cbiascv
    obsmod.in@CAA_nsamp<-input$CAA_nsamp
    obsmod.in@CAA_ESS<-input$CAA_ESS
    obsmod.in@CAL_nsamp<-input$CAL_nsamp
    obsmod.in@CAL_ESS<-input$CAL_ESS
    obsmod.in@CALcv<-input$CALcv
    obsmod.in@Iobs<-input$Iobs
    obsmod.in@Icv<-input$Icv
    obsmod.in@Mcv<-input$Mcv
    obsmod.in@Kcv<-input$Kcv
    obsmod.in@t0cv<-input$t0cv
    obsmod.in@LFCcv<-input$LFCcv
    obsmod.in@LFScv<-input$LFScv
    obsmod.in@B0cv<-input$B0cv
    obsmod.in@FMSYcv<-input$FMSYcv
    obsmod.in@FMSY_Mcv<-input$FMSY_Mcv
    obsmod.in@BMSY_B0cv<-input$BMSY_B0cv
    obsmod.in@Dbiascv<-input$Dbiascv
    obsmod.in@Dcv<-input$Dcv
    obsmod.in@Btbias<-input$Btbias
    obsmod.in@Btcv<-input$Btcv
    obsmod.in@Fcurbiascv<-input$Fcurbiascv
    obsmod.in@Fcurcv<-input$Fcurcv
    obsmod.in@hcv<-input$hcv
    obsmod.in@maxagecv<-input$maxagecv
    obsmod.in@Reccv<-input$Reccv
    obsmod.in@Irefcv<-input$Irefcv
    obsmod.in@Crefcv<-input$Crefcv
    obsmod.in@Brefcv<-input$Brefcv
    obsmod.in@beta<-input$beta

    OM<- new('OM',stock.in, fleet.in, obsmod.in)
    return(OM)
  })

  
  
  
 #   output$MPtest <- renderPrint({    
#      c(input$run_dlm_MSE,runMSE.box,run.check,runMSE.box)
#      })

      #runMSE.box<-eventReactive(input$run_dlm,{length(input$checkGroup)})

#  observeEvent(input$run_dlm_MSE,{
#    sfInit(parallel=TRUE, cpus=detectCores())
#    ourMSE <- runMSErobust(OM(), MPs=MPs(), proyears=input$Projyears, interval=input$MSE_intervals, nsim=input$numsims,reps=input$reps)
#    MSEout<-reactive({
#    ourMSE <- runMSE(OM(), MPs=MPs(), proyears=input$Projyears, interval=input$MSE_intervals, nsim=input$numsims,reps=input$reps)
#    return(ourMSE)
#    })
#  })


    runMSE.box<-eventReactive(input$run_dlm_MSE,{
           progress <- shiny::Progress$new(session, min=1, max=2)
           on.exit(progress$close())
       
           progress$set(message = 'MSE in progress',
                        detail = '')
       
           for (i in 1:2) {
             progress$set(value = i)
             Sys.sleep(0.5)
           }
         MSEout <- runMSE(OM(), MPs=MPs(), proyears=input$Projyears, interval=input$MSE_intervals, nsim=input$numsims,reps=input$reps,pstar=input$pstar)
         return(MSEout)
    })

#MSE Summary table     
  output$MSE_summary <- renderPrint({    
      MSE.summary<-summary(runMSE.box())
      MSEout<-runMSE.box()
      output$downloadMSE <- downloadHandler(
        filename = function() { paste0("MSEout",Sys.time(),".DMP", sep='') },
        content = function(file) {save(MSEout,file=file)}) 
      return(MSE.summary)
    })
#MSE Convergence plot    
  output$MSE_Convergence <- renderPlot({   
    MSEout<-runMSE.box()
    print(CheckConverg(MSEout))
    output$downloadMSE_Converge <- downloadHandler(
      filename = function() {paste0("MSE_Converge",Sys.time(),".png")},
      content = function(file) {
        png(file, type='cairo',width=800,height=720)
        CheckConverg(MSEout)
        dev.off()},contentType = 'image/png') 
  })
#MSE trade-off plots    
  output$MSE_TO1_plot1 <- renderPlot({   
    MSEout<-runMSE.box()
    print(Tplot(MSEout,nam="F and biomass prop vs yield trade-offs"))
    output$downloadMSE_TOff1 <- downloadHandler(
      filename = function() {paste0("MSE_TOff1",Sys.time(),".png")},
      content = function(file) {
        png(file, type='cairo',width=800,height=720)
        Tplot(MSEout,nam=" ")
        dev.off()},contentType = 'image/png') 
  })

  output$MSE_TO1_plot2 <- renderPlot({   
    MSEout<-runMSE.box()
    print(Tplot2(MSEout,nam="Short/Long term yield to MSY and variability trade-offs"))
    output$downloadMSE_TOff2 <- downloadHandler(
      filename = function() {paste0("MSE_TOff2",Sys.time(),".png")},
      content = function(file) {
        png(file, type='cairo',width=800,height=500)
        Tplot2(MSEout,nam=" ")
        dev.off()},contentType = 'image/png') 
  })

##################
# Full plots for Kobe, Pplot and VOI
###################  
#  output$MSE_Kobe <- renderPlot({   
#    MSEout<-runMSE.box()
#    print(Kplot(MSEout,nam="Kobe plot"))
#    output$downloadMSE_Kobe <- downloadHandler(
#      filename = function() {paste0("MSE_Kobe",Sys.time(),".png")},
#      content = function(file) {
#        png(file, type='cairo',width=800,height=720)
#        Kplot(MSEout)
#        dev.off()},contentType = 'image/png') 
#  })

#  output$MSE_Projplot <- renderPlot({   
#    MSEout<-runMSE.box()
#    print(Pplot(MSEout))
#    output$downloadMSE_Pplot <- downloadHandler(
#      filename = function() {paste0("MSE_Pplot",Sys.time(),".png")},
#      content = function(file) {
#        png(file, type='cairo',width=800,height=720)
#        Pplot(MSEout)
#        dev.off()},contentType = 'image/png') 
#  })
  
#  output$MSE_VOI <- renderPlot({   
#    MSEout<-runMSE.box()
#    print(VOI(MSEout))
#    output$downloadMSE_VOI <- downloadHandler(
#      filename = function() {paste0("MSE_VOI",Sys.time(),".png")},
#      content = function(file) {
#        png(file, type='cairo',width=800,height=720)
#        VOI(MSEout)
#        dev.off()},contentType = 'image/png') 
#  })
###########################################################

#######################################################
### User controlled Kobe, Projection, and VOI plots ###
#######################################################

  output$MSE_Kobe2 <- renderPlot({   
    MSEout<-runMSE.box()
    subMPs<-input$plotsubMPs
    if (is.null(subMPs)) subMPs<-MPs()
    subMSE <- Sub(MSEout, MPs=subMPs)
    print(Kplot2(subMSE,maxsim=input$Kplotmaxsim,nam="Kobe plot"))
    output$downloadMSE_Kobe2 <- downloadHandler(
      filename = function() {paste0("MSE_Kobe",Sys.time(),".png")},
      content = function(file) {
        png(file, type='cairo',width=800,height=720)
        Kplot2(subMSE,maxsim=input$Kplotmaxsim,nam="Kobe plot")
        dev.off()},contentType = 'image/png') 
  })
  
  output$MSE_Projplot2 <- renderPlot({   
    MSEout<-runMSE.box()
    subMPs<-input$plotsubMPs
    if (is.null(subMPs)) subMPs<-MPs()
    subMSE <- Sub(MSEout, MPs=subMPs)
    print(Pplot2(subMSE,nam="Projection plot"))
    output$downloadMSE_Pplot2 <- downloadHandler(
      filename = function() {paste0("MSE_Pplot",Sys.time(),".png")},
      content = function(file) {
        png(file, type='cairo',width=800,height=720)
        Pplot2(subMSE,nam="Projection plot")
        dev.off()},contentType = 'image/png') 
  })

    output$MSE_VOI2 <- renderPlot({   
      MSEout<-runMSE.box()
      subMPs<-input$plotsubMPs
      if (is.null(subMPs)) subMPs<-MPs()
      subMSE <- Sub(MSEout, MPs=subMPs)
      print(VOIplot(subMSE,nvars=input$VOInvars, nMP=input$VOInMPs))
    output$downloadMSE_VOI2 <- downloadHandler(
      filename = function() {paste0("MSE_VOI",Sys.time(),".png")},
      content = function(file) {
        png(file, type='cairo',width=800,height=720)
        VOIplot(subMSE,nvars=input$VOInvars, nMP=input$VOInMPs)
        dev.off()},contentType = 'image/png') 
    })
  
    output$MSEplots <- renderUI({
      tabsetPanel(id = "subTabPanel1", 
                  tabPanel("MSE summary",verbatimTextOutput("MSE_summary"),downloadButton('downloadMSE', 'Download MSE output')),
                  tabPanel("Convergence plot",plotOutput("MSE_Convergence",width="800px",height="720px"),downloadButton('downloadMSE_Converge', 'Download convergence plots')),
                  tabPanel("Trade-off plots",plotOutput("MSE_TO1_plot1",width="800px",height="720px"), downloadButton('downloadMSE_TOff1', 'Download trade-off plot'), plotOutput("MSE_TO1_plot2",width="800px",height="500px"),downloadButton('downloadMSE_TOff2', 'Download Tradeoff Plot')),
                  tabPanel("Kobe plot",plotOutput("MSE_Kobe2",width="800px",height="720px"),downloadButton('downloadMSE_Kobe2', 'Download Kobe plots')),
                  tabPanel("Projection plot",plotOutput("MSE_Projplot2",width="800px",height="720px"),downloadButton('downloadMSE_Pplot2', 'Download projection plots')),
                  tabPanel("Value of information",plotOutput("MSE_VOI2",width="800px",height="720px"),downloadButton('downloadMSE_VOI2', 'Download VOI plots'))
                  )            
          })
    
#End ShinyServer function
})  
  