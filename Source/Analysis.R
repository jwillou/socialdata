library(scales)
setwd("/Users/jrw0107/Google Drive/My Drive/Willoughby lab/projects - active/dolphins and turtles/socialdata/")
setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/dolphins and turtles/socialdata/")

#dataindicator = "fig1_edlocalsonly" 
#dataindicator = "fig2_edrate_localsonly" 
#dataindicator = "fig4a_edlocalsandtourists"
#dataindicator = "fig4b_edlocalsandtourists"
#dataindicator = "fig4c_edlocalsandtourists"
#dataindicator = "fig4d_edlocalsandtourists"
#dataindicator = "fig4e_edlocalsandtourists"
dataindicator = "fig4f_edlocalsandtourists"
#dataindicator = "fig5_interactamongdemo"
#dataindicator = ""

#load data
data = read.table(paste("Output/", dataindicator, "/yearlysummary.csv", sep=""), header=F, sep=",")
colnames(data) = c(read.table(paste("Output/", dataindicator, "/yearlysummaryHeader.csv", sep=""), header=F, sep=","))

#Define a function to calculate summary statistics
calculate_detailed_summary <- function(data, year_col, param_cols, knowledge_cols) {
  # Ensure all columns exist
  all_cols <- c(year_col, param_cols, knowledge_cols)
  if (!all(all_cols %in% names(data))) {stop("One or more specified columns do not exist in the data.")}
  
  #Create a unique group identifier for each combination of year and parameters
  data$group_id = with(data, interaction(data[, c(year_col, param_cols)], drop=TRUE, sep="_"))
  
  #Aggregate data by group_id to calculate mean
  means = aggregate(data[, knowledge_cols], by=list(group_id=data$group_id), FUN=mean)
  names(means)[-1] = paste(names(means)[-1], "Mean", sep="_")
  
  #Aggregate data by group_id to calculate standard error
  ses = aggregate(data[, knowledge_cols], by=list(group_id=data$group_id),
                  FUN=function(x) sd(x, na.rm=TRUE) / sqrt(length(na.omit(x))))
  names(ses)[-1] = paste(names(ses)[-1], "SE", sep="_")
  
  # Merge mean and SE results
  final_data = merge(means, ses, by="group_id")
  
  #Convert group_id into a data frame of parameters
  param_data = do.call(rbind, strsplit(as.character(final_data$group_id), "_", fixed=TRUE))
  param_data = as.data.frame(param_data, stringsAsFactors=FALSE)
  names(param_data) = c(year_col, param_cols)
  
  #Combine the parameter data with the summarized data
  final_data = cbind(param_data, final_data[-1])
  
  return(final_data)
}

#Specify parameter columns and knowledge columns
param_cols <- c("demo.locals1", "demo.locals2", "demo.locals3", "demo.tours1", "demo.tours2", "demo.tours3", "num_locals", "num_tours", "knowtrans", "tours_local", "tours_tours", "local_local", "local_misd")
knowledge_cols <- c("dem1toursK", "dem1localK", "dem2toursK", "dem2localK", "dem3toursK", "dem3localK")

#Apply the function
summary_results = calculate_detailed_summary(data, "year", param_cols, knowledge_cols)
write.table(summary_results, paste("Output/", dataindicator, "/summary_results.csv", sep=""), row.names=F, col.names=T, sep=",")

#define colors
colors3 = c("dodgerblue3", "firebrick3", "goldenrod2")
#
####fig1_edlocalsonly####
subdata = subset(data, data$tours_local==0.5)

#large demo group
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1toursK, col = alpha(colors3[1], 0.5))
}

#small demo group
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem2localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem2toursK, col = alpha(colors3[2], 0.5))
}

#small demo group - reduced interaction
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem3localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem3toursK, col = alpha(colors3[3], 0.5))
}

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1

#Plots over interaction probability
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1)) 

#figure regressions - tours_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})

####fig2_edrate_localsonly####
#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1

#Large demo group
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1))
nlines = length(unique(filtered_data$knowtrans))
colorsat = seq(0.2,1,(1.2/nlines))
knowntrans = unique(filtered_data$knowtrans)
for(k in 1:length(knowntrans)){
  kfiltered_data = filtered_data[filtered_data$knowtrans==knowntrans[k],]
  with(kfiltered_data, {
    lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = alpha(colors3[1], colorsat[k]), bg=alpha(colors3[1], colorsat[k]), cex = 1.2, lwd = 2)
  })
}

#Small demo group
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1))
nlines = length(unique(filtered_data$knowtrans))
colorsat = seq(0.2,1,(1.2/nlines))
knowntrans = unique(filtered_data$knowtrans)
for(k in 1:length(knowntrans)){
  kfiltered_data = filtered_data[filtered_data$knowtrans==knowntrans[k],]
  with(kfiltered_data, {
    lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = alpha(colors3[2], colorsat[k]), bg=alpha(colors3[2], colorsat[k]), cex = 1.2, lwd = 2)
  })
}

#Small demo group reduced interaction probability
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1))
nlines = length(unique(filtered_data$knowtrans))
colorsat = seq(0.2,1,(1.2/nlines))
knowntrans = unique(filtered_data$knowtrans)
for(k in 1:length(knowntrans)){
  kfiltered_data = filtered_data[filtered_data$knowtrans==knowntrans[k],]
  with(kfiltered_data, {
    lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = alpha(colors3[3], colorsat[k]), bg=alpha(colors3[3], colorsat[k]), cex = 1.2, lwd = 2)
  })
}

#Combined for comparison
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1))
knowntrans = c(0.00, 0.05, 0.2, 0.50)
nlines = length(knowntrans)
colorsat = seq(0.2,1,(1.2/nlines))

for(k in 1:length(knowntrans)){
  kfiltered_data = filtered_data[filtered_data$knowtrans==knowntrans[k],]
  with(kfiltered_data, {
    lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = alpha(colors3[1], colorsat[k]), bg=alpha(colors3[1], colorsat[k]), cex = 1.2, lwd = 2)
    lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = alpha(colors3[2], colorsat[k]), bg=alpha(colors3[2], colorsat[k]), cex = 1.2, lwd = 2)
    lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = alpha(colors3[3], colorsat[k]), bg=alpha(colors3[3], colorsat[k]), cex = 1.2, lwd = 2)
  })
}

#fig3_edrate_regression
kfiltered_data = filtered_data[filtered_data$tours_local==0.5,]
plot(-100,-100, xlab="Knowledge Transmission Quantity", ylab="Mean Knowledge", xlim=c(0,0.5), ylim=c(0,1))
lines(kfiltered_data$knowtrans[2:11], kfiltered_data$dem2toursK_Mean[2:11], type="b", pch=19, col=colors3[2], bg=colors3[2])
lines(kfiltered_data$knowtrans[2:11], kfiltered_data$dem3toursK_Mean[2:11], type="b", pch=19, col=colors3[3], bg=colors3[2])
model=lm(as.numeric(c(kfiltered_data$dem3toursK_Mean[2:11]/kfiltered_data$dem2toursK_Mean[2:11]))~log(as.numeric(kfiltered_data$knowtrans[2:11])))
summary(model)
x=seq(0.05,0.5,0.001)
xlog = log(x)
y=summary(model)$coeff[1,1]+(xlog*summary(model)$coeff[2,1])
lines(x,y, col="darkorange2", lwd=2)






####fig4a_edlocalsandtourists####

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}

#basic plot setup
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1))

#figure regressions - tours_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})

####fig4b_edlocalsandtourists####

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}

#basic plot setup
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0.95,1))

#figure regressions - tours_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})

####fig4c_edlocalsandtourists####

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}

#basic plot setup
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1))

#figure regressions - tours_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})

####fig4d_edlocalsandtourists####

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}

#basic plot setup
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0.8,1))

#figure regressions - tours_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})

####fig4e_edlocalsandtourists####

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}

#basic plot setup
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1))

#figure regressions - tours_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})

####fig4f_edlocalsandtourists####

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}

#basic plot setup
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0.75,1))

#figure regressions - tours_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~tours_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=1, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})

####fig5_interactamongdemo####
subdata = subset(data, data$local_local==0.1)

#large demo group
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1toursK, col = alpha(colors3[1], 0.5))
}

#small demo group
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem2localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem2toursK, col = alpha(colors3[2], 0.5))
}

#small demo group - reduced interaction
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem3localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem3toursK, col = alpha(colors3[3], 0.5))
}

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}
filtered_data$local_local = as.numeric(filtered_data$local_local)

#set up initial plot
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0.8,1)) 

#figure regressions - local_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~local_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~local_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~local_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(local_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(local_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(local_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = local_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = local_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = local_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = local_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = local_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = local_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})




####suppfig1_interactamongdemo_localsedonly####
subdata = subset(data, data$local_local==0.5)

#large demo group
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1toursK, col = alpha(colors3[1], 0.5))
}

#small demo group
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem2localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem2toursK, col = alpha(colors3[2], 0.5))
}

#small demo group - reduced interaction
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem3localK, col = alpha("grey50", 0.5))
}
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem3toursK, col = alpha(colors3[3], 0.5))
}

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))}
filtered_data$local_local = as.numeric(filtered_data$local_local)

#set up initial plot
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Population Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1), ylim = c(0,1)) 

#figure regressions - local_local
write.table(t(c("Intercept", "Int_SE",	"tvalue",	"pvalue",	"Slope", "Slope_SE",	"tvalue",	"pvalue",	"Fstat",	"R2",	"DF1", "DF2")), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=F)
model = lm(dem1toursK_Mean~local_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[1], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem2toursK_Mean~local_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[2], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

model = lm(dem3toursK_Mean~local_local, data=filtered_data)
summary(model)
segments(x0=0,y0=((0*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]),x1=1,y1=((1*summary(model)$coeff[2,1])+summary(model)$coeff[1,1]), col=alpha(colors3[3], 0.8), lty=3, lwd=2)
write.table(t(c(summary(model)$coeff[1,], summary(model)$coeff[2,], summary(model)$fstatistic[1], summary(model)$r.squared, summary(model)$fstatistic[2], summary(model)$fstatistic[3])), paste("Output/", dataindicator, "/reg_results.csv", sep=""), row.names=F, col.names=F, sep=",", append=T)

#Add Group data
with(filtered_data, {
  lines(local_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  lines(local_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(local_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  segments(x0 = local_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = local_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  segments(x0 = local_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = local_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  segments(x0 = local_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = local_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})






####helpful other plots####
#Initialize sanity check plot
subdata = subset(data, data$local_local==0.5)
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  

# Loop through each replicate and add it to the plot
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1toursK, col = alpha(colors3[1], 0.5))
  lines(replicate_data$year, replicate_data$dem2toursK, col = alpha(colors3[2], 0.5))
  lines(replicate_data$year, replicate_data$dem3toursK, col = alpha(colors3[3], 0.5))
}

# Loop through each replicate and add it to the plot
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1localK, col = alpha(colors3[1], 0.5))
  lines(replicate_data$year, replicate_data$dem2localK, col = alpha(colors3[2], 0.5))
  lines(replicate_data$year, replicate_data$dem3localK, col = alpha(colors3[3], 0.5))
}

#Filter data 
filtered_data = subset(summary_results, year == 25) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1

#Among group interactions - all lines plotted
plot(-100, -100, type = "l", xlab = "Year", ylab = "Mean Knowledge", ylim = c(0.5,1), xlim = c(1,5))  
localmisd = unique(data$local_misd)
nlines = length(localmisd)
colorsat = seq(0.2,1,(1.2/nlines))

for(k in 1:length(localmisd)){
  localmisd_data = data[data$local_misd==as.character(localmisd[k]),]
  for(i in 1:max(localmisd_data$rep)){
    t = localmisd_data[localmisd_data$rep==i,]
    lines(t$year, t$dem1localK, type = "l", col = alpha(colors3[1], colorsat[k]), lwd = 1)
  }
}
# Loop through each replicate and add it to the plot
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1toursK, col = alpha(colors.demos[1], 0.5))
  lines(replicate_data$year, replicate_data$dem2toursK, col = alpha(colors.demos[2], 0.5))
  lines(replicate_data$year, replicate_data$dem3toursK, col = alpha(colors.demos[3], 0.5))
}
