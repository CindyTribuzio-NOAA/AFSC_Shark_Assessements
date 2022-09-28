#library(shiny)
#library(shinyFiles)
#library(DLMtool)
require(shiny)
require(shinyFiles)
require(DLMtool)

#source('load_DLM.r',local = FALSE)

shinyUI(fluidPage(
  titlePanel("Welcome to the DLMtool GUI"),
  sidebarLayout(
   sidebarPanel(
    fileInput('file1', 'Choose DLM input file',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv','tmp'
              )
              #              actionButton("resetdata","Flush data file before loading new file")          
    ),
    conditionalPanel(
      condition="input.conditionedPanels==1",
      p("Click", tags$a(href="javascript:window.open('DLMobject_slots.html', '_blank','width=600,height=400')", "here"), "for a desciption of the DLM object inputs."),
      br(),
      h4("Information on the DLMtool"),
      p("The DLMtool library for R was created by Dr. Tom Carruthers with funding provided by the National Resources Defense Council. Dr. Adrian Hordyk (a.hordyk@murdoch.edu.au) is the current developer of the DLMtool R library."), 
      p("More information can be found at", tags$a(href="http://www.datalimitedtoolkit.org/","http://www.datalimitedtoolkit.org/",target="_blank")),
      br(),
      h4("Information on the DLMtool Shiny web application"),
      p("The Shiny app for the DLMtool was developed by Dr. Jason Cope of NOAA Fisheries (jason.cope@noaa.gov). All comments on the app's functionality and requests for features should be directed to him. The sources code for the app can be found ", tags$a(href="https://github.com/shcaba/Shiny_DLMtool", "here",target="_blank") )
    ),
    conditionalPanel(
    condition="input.conditionedPanels==3",wellPanel(uiOutput("choicelist"),
    p("Click", tags$a(href="javascript:window.open('DLMtool_methods_codes.html', '_blank','width=600,height=400')", "here"), "for a key and desciption of the above methods."),
    actionButton("selectall","Select All")
    ),
    fluidRow(column(3,numericInput("TACreps", "# of replicates", value=100,min=1, max=1000000, step=1)),column(3,numericInput("Pstar", "Buffer quantile", value=0.4,min=0, max=1, step=0.01)),column(6,p(br(),br(),strong("solid line")," = median",br(),strong("broken line")," = buffer quantile"))),
    actionButton("run_dlm","Run catch estimates",icon("play-circle"),style="font-size:110%;border:2px solid;background:#ffffcc")
  ),

conditionalPanel(
  condition="input.conditionedPanels==4",wellPanel(uiOutput("sensilist")),
  fluidRow(column(6,numericInput("nsensi", "# of total sensitivty values explored", value=6,min=1, max=100, step=1)),column(6,numericInput("sensireps", "# of samples used for sensitivity", value=100,min=1, max=100000, step=1))),
  fluidRow(column(6,numericInput("lowperc", "Lower percentile reported", value=0.05,min=0, max=1, step=0.01)),column(6,numericInput("upperperc", "Upper percentile reported", value=0.95,min=0, max=1, step=0.01)))
),


  conditionalPanel(
    condition="input.conditionedPanels==5",
    h3("Select control methods to test in MSE"), 
    p("Click", tags$a(href="javascript:window.open('DLMtool_methods_codes.html', '_blank','width=600,height=400')", "here"), "for a key and description of each method."),
    #p("Click", tags$a(href="javascript:window.open('DLMtool_methods_codes.htm', '_blank','width=600,height=400')", "here"), "for a glossary of the terms used in this demo."), style="background-color: #ffffff;"),
    fluidRow(column(6,wellPanel(uiOutput("can.list.output"))),column(6,wellPanel(uiOutput("can.list.input")))),
    fluidRow(column(6,actionButton("allselect","Select All output methods")),column(6,actionButton("selectinput","Select all input methods"))),
    br(),
    br(),
    fluidRow(column(6,wellPanel(uiOutput("cant.list.output"))),column(6,wellPanel(uiOutput("cant.list.input")))),
    fluidRow(column(6,actionButton("allselectNAop","Select All output methods")),column(6,actionButton("allselectNAip","Select all input methods"))),
    br(),
    h3("Plotting MSE output"),
    fluidRow(column(12,wellPanel(uiOutput("subMPs"),
    fluidRow(column(6,numericInput("Kplotmaxsim", "Kobe plot: max # of sims to plot", value=60,min=0, max=10000, step=1))),
    fluidRow(column(6,numericInput("VOInvars", "VOI plot: # of variables to show", value=5,min=0, max=20, step=1)),column(6,numericInput("VOInMPs", "VOI plot: # of MPs to plot", value=4,min=0, max=100, step=1))),
    p("Note: VOI plot will only show up to the first 4 methods")))),
    
    br(),
    
    h3("Build the Operating Model"), 
    p("Click", tags$a(href="javascript:window.open('OM_input_descriptions.html', '_blank','width=600,height=400')", "here"), "for a description of each OM input."),
    
    wellPanel(fluidRow(column(12, uiOutput("stock.choicelist"))),
    h4("User modifications"), 
    fluidRow(column(6, actionButton("zerogradients","Zero all gradient values?"))),
    fluidRow(column(6, uiOutput("stock.maxage"))),
    fluidRow(column(4, uiOutput("stock.M")),column(4, uiOutput("stock.Msd")),column(4, uiOutput("stock.Mgrad"))),
    fluidRow(column(4, uiOutput("stock.Linf")),column(4, uiOutput("stock.Linfsd")),column(4, uiOutput("stock.Linfgrad"))),
    fluidRow(column(4, uiOutput("stock.K")),column(4, uiOutput("stock.Ksd")),column(4, uiOutput("stock.Kgrad"))),
    fluidRow(column(6, uiOutput("stock.t0"))),
    fluidRow(column(6, uiOutput("stock.WtLt_a")),column(6, uiOutput("stock.WtLt_b"))),
    fluidRow(column(6, uiOutput("stock.R0")),column(6, uiOutput("stock.SRrel"))),
    fluidRow(column(6, uiOutput("stock.h")),column(6, uiOutput("stock.recgrad"))),
    fluidRow(column(6, uiOutput("stock.Perr")),column(6, uiOutput("stock.AC"))),
    fluidRow(column(6, uiOutput("stock.L50")),column(6, uiOutput("stock.L50_95"))),
    fluidRow(column(6, uiOutput("stock.Size_area")),column(6, uiOutput("stock.Frac_area"))),
    fluidRow(column(6, uiOutput("stock.Prob_staying")),column(6, uiOutput("stock.D")))
    ),

    wellPanel(fluidRow(column(12, uiOutput("fleet.choicelist"))),
    h4("User modifications"), 
    fluidRow(column(6, uiOutput("fleet.nyrs")),column(6, uiOutput("fleet.spattarg"))),
    fluidRow(column(6, uiOutput("fleet.L5")),column(6, uiOutput("fleet.LFS"))),
    fluidRow(column(6, uiOutput("fleet.Vmaxlen")),column(6, uiOutput("fleet.Esd"))),
    fluidRow(column(6, uiOutput("fleet.qinc")),column(6, uiOutput("fleet.qcv")))),
#    fluidRow(column(6, uiOutput("fleet.LR5")),column(6, uiOutput("fleet.LFR"))),
#    fluidRow(column(6, uiOutput("fleet.Rmaxlen")),column(6, uiOutput("fleet.DR")))),
    
    wellPanel(fluidRow(column(12, uiOutput("obs.choicelist"))),
    h4("User modifications"), 
    h5(em("Bias and error entries in lognormal std. dev.")), 
    fluidRow(column(6, uiOutput("Obs.rcv")),column(6, uiOutput("Obs.maxagecv"))),
    fluidRow(column(6, uiOutput("Obs.Mcv")),column(6, uiOutput("Obs.hcv"))),
    fluidRow(column(6, uiOutput("Obs.Linfcv")),column(6, uiOutput("Obs.Kcv"))),
    fluidRow(column(6, uiOutput("Obs.t0cv")),column(6, uiOutput("Obs.LenMcv"))),
    fluidRow(column(6, uiOutput("Obs.Reccv")),column(6, uiOutput("Obs.FMSYcv"))),
    fluidRow(column(6, uiOutput("Obs.FMSY_Mcv")),column(6, uiOutput("Obs.BMSY_B0cv"))),
    fluidRow(column(6, uiOutput("Obs.Fcurbiascv")),column(6, uiOutput("Obs.Fcurcv"))),
    fluidRow(column(6, uiOutput("Obs.LFCcv")),column(6, uiOutput("Obs.LFScv"))),
    fluidRow(column(6, uiOutput("Obs.Cobs")),column(6, uiOutput("Obs.Cbiascv"))),
    fluidRow(column(6, uiOutput("Obs.CAA_nsamp")),column(6, uiOutput("Obs.CAA_ESS"))),
    fluidRow(column(6, uiOutput("Obs.CAL_nsamp")),column(6, uiOutput("Obs.CAL_ESS"))),
    fluidRow(column(6, uiOutput("Obs.CALcv")),column(6, uiOutput("Obs.beta"))),
    fluidRow(column(6, uiOutput("Obs.Iobs")),column(6, uiOutput("Obs.Icv"))),
    fluidRow(column(6, uiOutput("Obs.Dbiascv")),column(6, uiOutput("Obs.Dcv"))),
    fluidRow(column(6, uiOutput("Obs.B0cv")),column(6, uiOutput("Obs.Btcv"))),
    fluidRow(column(6, uiOutput("Obs.Btbias")),column(6, uiOutput("Obs.Irefcv"))),
    fluidRow(column(6, uiOutput("Obs.Brefcv")),column(6, uiOutput("Obs.Crefcv")))),
    #Get MSE specifications
    wellPanel(h4("MSE specifications"), 
    fluidRow(column(6,numericInput("Projyears", "# of projection years", value=20,min=1, max=1000, step=1)),column(6,numericInput("MSE_intervals", "Method intervals", value=5,min=1, max=200, step=1))),
    fluidRow(column(6,numericInput("numsims", "# of simulations", value=10,min=1, max=1000, step=1)),column(6, numericInput("reps", "Repetitions", value=1,min=1, max=100, step=1))),
    fluidRow(column(6,numericInput("pstar", "P* quantile", value=0.5,min=0, max=0.5, step=0.01))) #,br(),column(6,downloadButton('downloadOM', 'Download OM'))),
    ),
    tags$head(tags$style(HTML('#run{background-color:orange}'))),
    actionButton("run_dlm_MSE",strong("Run MSE"),width="100%" ,icon("play-circle"),style="font-size:120%;border:2px solid;background:#ccffcc"),
    br(),
    br(),
    downloadButton('downloadOM', 'Download Operating Model')
  )
#End of sidebarPanel
   ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Checking inputs",
          column(6,plotOutput("Catchplot")),
          column(6,plotOutput("Indexplot")),
          column(6,plotOutput("LHplots")),
          column(6,plotOutput("Parameterplots")),value=1
        ),
        tabPanel(
          "Unavailable methods", verbatimTextOutput("MP_NA"), value =2
        ),
        tabPanel(
        "TAC estimators",plotOutput("TACplots"),plotOutput("wtedTAC"),
        downloadButton('downloadTAC', 'Download TAC values'),
        downloadButton('downloadTACobj', 'Download TAC R object'),
        downloadButton('downloadTACbarplot', 'Download TAC barplot'),
        downloadButton('downloadTACdensityplot', 'Download TAC denisty plot'),
        value=3
        ),
        tabPanel(
          "TAC sensitivities",column(12,plotOutput("Sensiplot",height = 800, width = 1000)),
          column(2,downloadButton('downloadSensi', 'Download plots')),
          column(2,downloadButton('downloadSensiobj', 'Download R object')),
          value=4
        ),
        tabPanel(
          "Management Strategy Evaluation",
          tabPanel("Tab1", uiOutput("MSEplots")),
          #verbatimTextOutput("MPtest"),
#          plotOutput("MSE_TO1_plot1"), 
#          plotOutput("MSE_TO1_plot2"), 
          value=5
        ), id="conditionedPanels"
        
      )    
  #End of mainPanel
  )
    )
      ))