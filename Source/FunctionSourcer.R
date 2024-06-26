#Source packages and functions

#set working directory, import packages, source functions,
setwd(paste(directory,"/Source/", sep = '')) #set temp working directory

#import packages - only need to do this once then can comment out
#install.packages("reshape2", lib="/home/gfl0003", repos='http://cran.us.r-project.org/')
#.libPaths("/home/jrw0107") #need this when defining location of libraries in HPC

#call installed libraries
#library(reshape2)    #need this for plotting
library(scales)      #need this for plotting, this allows transparency in colors for overlapping lines

#source functions
source(paste(getwd(), "/Initialize.R", sep = ''))
source(paste(getwd(), "/Interact.R", sep = ''))
source(paste(getwd(), "/Output.R", sep = ''))
source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/Forget.R", sep = ''))
source(paste(getwd(), "/Learn.R", sep = ''))
