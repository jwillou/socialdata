library(scales)
#setwd("/Users/jrw0107/Google Drive/My Drive/Willoughby lab/projects - active/dolphins and turtles/socialdata/")
setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - active/dolphins and turtles/socialdata/")

dataindicator = "_6_11_F0.10.80.1tourtolocal0"

#load data
data = read.table(paste("Output/yearlysummary",dataindicator, ".csv", sep=""), header=F, sep=",")
colnames(data) = c(read.table("Output/yearlysummaryHeader_6_11.csv", header=F, sep=","))

#plot change over time
subdata = subset(data, data$tours_local==0.7)

#define colors
colors.demos = c("dodgerblue3", "chartreuse3", "goldenrod2")

#Initialize plot
plot(-100, -100, type = "l", xlab = "Year", ylab = "Knowledge", ylim = c(0,1), xlim = c(1,25))  

# Loop through each replicate and add it to the plot
for (i in 1:max(data$rep)) {
  replicate_data <- subset(subdata, rep == i)
  lines(replicate_data$year, replicate_data$dem1toursK, col = alpha(colors.demos[1], 0.5))
  lines(replicate_data$year, replicate_data$dem2toursK, col = alpha(colors.demos[2], 0.5))
  lines(replicate_data$year, replicate_data$dem1localK, col = alpha(colors.demos[3], 0.5))
}


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
write.table(summary_results, paste("Output/summary_results",dataindicator, ".csv", sep=""), row.names=F, col.names=T, sep=",")

#Filter data 
filtered_data = subset(summary_results, year == 25 ) #tours_tours == 0.1 & local_local == 0.8 & local_misd == 0.1
for(r in 1:nrow(filtered_data)){
  filtered_data[,r] = as.numeric(as.character(filtered_data[,r]))
}

#define colors
colors3 = c("dodgerblue3", "chartreuse3", "goldenrod2")

# Initialize plot with Group 1 data
plot(-100, -100, type = "b", xlab = "Interaction Probability", ylab = "Mean Knowledge", main = "", pch = 19, col = colors3[1], cex = 1.2, lwd = 2, xlim = c(0,1),
     ylim = c(min(c(filtered_data$dem1toursK_Mean, filtered_data$dem2toursK_Mean, filtered_data$dem3toursK_Mean)) - 0.05,
              max(c(filtered_data$dem1toursK_Mean, filtered_data$dem2toursK_Mean, filtered_data$dem3toursK_Mean)) + 0.05))

#Add Group data
with(filtered_data, {
  lines(tours_local, dem1toursK_Mean, type = "b", pch = 21, col = colors3[1], bg=alpha(colors3[1], 0.5), cex = 1.2, lwd = 2)
  lines(tours_local, dem2toursK_Mean, type = "b", pch = 21, col = colors3[2], bg=alpha(colors3[2], 0.5), cex = 1.2, lwd = 2)
  #lines(tours_local, dem3toursK_Mean, type = "b", pch = 21, col = colors3[3], bg=alpha(colors3[3], 0.5), cex = 1.2, lwd = 2)
  #segments(x0 = tours_local, y0 = dem1toursK_Mean - (dem1toursK_SE*1.96), x1 = tours_local, y1 = dem1toursK_Mean + (dem1toursK_SE*1.96), col = colors3[1], lwd = 2)
  #segments(x0 = tours_local, y0 = dem2toursK_Mean - (dem2toursK_SE*1.96), x1 = tours_local, y1 = dem2toursK_Mean + (dem2toursK_SE*1.96), col = colors3[2], lwd = 2)
  #segments(x0 = tours_local, y0 = dem3toursK_Mean - (dem3toursK_SE*1.96), x1 = tours_local, y1 = dem3toursK_Mean + (dem3toursK_SE*1.96), col = colors3[3], lwd = 2)
})


#Legend
#legend("bottomright", legend = c("Group 1", "Group 2", "Group 3"), col = colors3, bg=c(alpha(colors3[1], 0.5),alpha(colors3[2], 0.5),alpha(colors3[3], 0.5)), lwd = 2, pch = 21, border=T, bty="n")
