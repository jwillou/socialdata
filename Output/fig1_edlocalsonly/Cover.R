#Cover.R used to manage parameters and values and set scenarios for simulations
#Janna Willoughby, Hannah Henry, TiAnna Olivas 2024

#Set working directory and out directory
#setwd("/scratch/jrw0107_infoex/tourtolocal1")
#setwd("/Users/jrw0107/Google Drive/My Drive/Willoughby lab/projects - active/dolphins and turtles/socialdata/")
setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/dolphins and turtles/socialdata/")

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
forget      = 0.1#.10              #likelihood of stochastic knowledge reduction per individual per year
forget.amt  = 0.05#.10             #magnitude of knowledge reduction 
education   = c(0.2, 0.2, 0.2)     #potential increased in the knowledge parameter for each individual annually, by demo group
ed.who      = "locals"             #who is educated: locals or all

#Knowledge variations
knowtrans.P = 0.05                 #amount of knowledge transfer possible at interactions: default 0.05, tested seq(0,0.5,0.05)

#Interaction scenarios
tours_local.P = seq(0,1.0,0.1)     #likelihood of interacting within the year, within demographic groups
tours_local.A = c(1,1,0.5)         #interaction adjustment for each group, within demographic groups
tours_tours.P = 0.0#seq(0.1,0.1,0.1)   #likelihood of interacting within the year, among and within demographic groups
tours_tours.A = 1                  #interaction adjustment for each group, among and within demographic groups
local_local.P = 0.0#seq(0.0,0.0,0.0)   #likelihood of interacting within the year, within demographic groups
local_local.A = c(1,1,1)           #interaction adjustment for each group, within demographic groups
local_misd.P  = 0.0 #seq(0.0,0.5,0.1)   #likelihood of interacting within the year, among demographic groups
local_misd.A  = 1                  #interaction adjustment for each group, among demographic groups
tourtolocoal  = 1                  #can tourists reduce locals knowledge - 1=yes, 0=no
scenarios   = expand.grid(tours_local.P, tours_tours.P, local_local.P, local_misd.P,knowtrans.P)
remove(tours_local.P, tours_tours.P, local_local.P, local_misd.P,knowtrans.P)                        
colnames(scenarios) = c("tours_local", "tours_tours", "local_local", "local_misd","knowtrans")

#Model run parameters
reps = 100                         #number of replicated runs

#Run model iterating over parameters 
for(r in 1:nrow(scenarios)){
  RunModel(r, directory, demo.locals, demo.tours, num_locals, num_tours, years, forget, forget.amt, education, ed.who, tourtolocoal, scenarios, reps, tours_local.A, tours_tours.A, local_local.A, local_misd.A)
}


