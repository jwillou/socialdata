#Cover.R used to manage parameters and values and set scenarios for simulations
#Janna Willoughby, Hannah Henry, TiAnna Olivas 2024

#Set working directory and out directory
setwd("/Users/jrw0107/Google Drive/My Drive/Willoughby lab/projects - active/dolphins and turtles/socialdata/")
directory = getwd()
outdir = paste(directory, "Output/", sep = "")

#Source function scripts
source(paste(directory, "/Source/FunctionSourcer.R", sep = ''))

#Population characteristics
demo.locals = c(0.5, 0.25, 0.25)   #assumes 3 demo groups, proportion in each group
demo.tours  = c(0.5, 0.25, 0.25)   #assumes 3 demo groups, proportion in each group
num_locals  = 100                  #number of locals
num_tours   = 50                   #number of tourists
years       = 25                   #number of years to run simulation
forget      = 0.1                  #likelihood stochastic knowledge reduction per individual per year
knowtrans   = 0.1                  #amount of knowledge transfer possible at interactions
education   = c(0.1, 0.05, 0.05)   #increased in the knowledge parameter average annually, by demo gropu

#Interaction scenarios (we will expand these substantially)
tours_local.P = 0.5                #likelihood of interacting within the year, within demographic groups
tours_tours.P = 0.1                #likelihood of interacting within the year, among and within demographic groups
local_local.P = 0.8                #likelihood of interacting within the year, within demographic groups
local_misd.P  = 0.1                #likelihood of interacting within the year, among demographic groups
scenarios   = expand.grid(tours_local.P, tours_tours.P, local_local.P, local_misd.P)
remove(tours_local.P, tours_tours.P, local_local.P, local_misd.P)                        
colnames(scenarios) = c("tours_local", "tours_tours", "local_local", "local_misd")

#Run model iterating over parameters 
for(r in 1:nrow(scenarios)){
  RunModel(r, directory, demo.locals, demo.tours, num_locals, num_tours, years, forget, knowtrans, education, scenarios)
}


