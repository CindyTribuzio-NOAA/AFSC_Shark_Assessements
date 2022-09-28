#Index Indicator methods
#load library
library(DLMtool)
#access example data objects
#load user defined DLM-object
#setwd("C:/Users/Jason.Cope/Desktop/Indonesia_2017/class materials/Demos/DLMtool/Index-based methods/")
Ex_dlm.data<-new("DLM_data",stock="C:/Users/Jason.Cope/Documents/GitHub/Data-limited-tools/Shiny_DLMtool/DLM_objects_examples/Example_datafile.csv")

###################################
### RUN INDICATOR-BASED METHODS ###
###################################

#Index-based indicator
#DLM-object must have an index to use this method
#Catch-based HCR
Itarget1(x=1, Ex_dlm.data, reps = 100, yrsmth = 5, xx=0, Imulti=1.5)
#Effort-based HCR
ItargetE1(x=1, Ex_dlm.data, reps = 100, yrsmth = 5, xx = 1, Imulti = 1.5)
