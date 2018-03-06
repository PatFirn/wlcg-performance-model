library(anytime) # Date and Time conversion
library(ggplot2) # More advanced plots
library(xts) # Time Series
library(dplyr)

## Get a stochastic expression from the supplied histogram
# This ignores all NA values in the data
# To handle long-tailed distributions, a tail cutoff can be specified, all values right of that value are put into a single bin

# x: A vector with the distribution to be converted into a stochastic expression, assumed to be positive
# bins: The total number of bins (excluding cutoff bin)
# cutoffQuantile: The quantile the distribution is cut off. All values right of this value are put into an overflow bin such that their mean value is the middle of the cutoff bin

distToStoEx <- function(x, bins = 100, cutoffQuantile = 1) {
  x <- na.omit(x) # Remove NAs from data
  
  # Create cutoff mark and filter values to be cut off
  cutoffBreak <- quantile(x, cutoffQuantile)
  cutoffValues <- x[x >= cutoffBreak]
  
  # Compute right limit of cutoff bin to preserve mean of cutoff values
  cutoffMiddle <- mean(cutoffValues)
  cutoffMax <- cutoffBreak + 2 * (cutoffMiddle - cutoffBreak)
  
  # Used breaks, +1 for cutoff values
  breaks <- c(seq(0, cutoffBreak, cutoffBreak / bins), cutoffMax)
  
  # Round bins to next integer, remove duplicates
  # Note: Not quite sure this is valid, but avoids empty bucket due to second precision
  breaks <- unique(sapply(breaks, ceiling))

  # Generate histogram
  # Pull values that are out of bounds into the middle of the overflow bin
  # This avoids not counting extreme values outside of the computed overflow bin limits
  histogram <- hist(pmin(x, cutoffMiddle), breaks, plot = T)
  
  # Transform counts in histogram to probability
  histogram$probabilities <- histogram$counts / sum(histogram$counts)

  # Create expressions of the form "(binRight,probability)" for all bins
  # (as specified by Stochastic Expressions definition).
  # Then join them all together.
  result <- paste0(mapply(function(x, y) paste("(", x, ";", y, ")", sep = ""),
                          histogram$breaks[-1], histogram$probabilities),
                   collapse = "")
  return(paste0("DoublePDF[", result, "]"))
  
  # Todo Remove expressions for (consecutive) empty bins to improve efficiency. The rightmost empty bin has to be kept as per the specification of Stochastic Expressions (specifies right limit and probability for each bin).
}

# For now, generate numbered pdf files
# pdf(file = "plots/plot%03d_generated.pdf", onefile = FALSE)

# Load data from csv formatted files
jobs <- read.table("../data/job_data.csv", sep=",", header=TRUE)
nodes <- read.table("../data/gridka-benchmarks-2017.csv", sep=",", header=TRUE)

## Only keep needed columns, drop all others
## Add columns needed for analysis here later
# keptCols <- c("WNHostName", "NCores", "Type", "WrapCPU", "WrapWC", "StartedRunningTimeStamp", "FinishedTimeStamp", "JobType", "GenericType")
# jobs <- jobs[keptCols]

# Remove suffixed site location
jobs$WNHostName <- gsub("*.gridka.de", "", jobs$WNHostName)

## Convert to correct data types

# Convert time stamps to datetime
# Dates are converted to UTC as a default time zone
jobs$StartedRunningDatetime <- anytime(jobs$StartedRunningTimeStamp / 1000, asUTC=TRUE)
jobs$FinishedDatetime <- anytime(jobs$FinishedTimeStamp / 1000, asUTC=TRUE)


## Additional computations on node data
nodes$HSScorePerJobSlot <- nodes$hs06 / nodes$jobslots


library(dplyr)
nodeTypes <- aggregate(nodes$hostname, list(CPU = nodes$cpu.model, jobslots = nodes$jobslots, hs06 = nodes$hs06, cores = nodes$cores, scorePerSlot = nodes$HSScorePerJobSlot), length)

print("GridKa Site Machine Types:")
print(nodeTypes)

## Merge job assignments with nodes

# all: Keep unmatched entries
jobDataRaw <- merge(jobs, nodes, by.x="WNHostName", by.y="hostname", all=TRUE)

# Data include job types in capitalized versions, decapitalize all before factorization
jobTypes <- factor(tolower(jobDataRaw$Type))

## Filter out invalid data

# - Decapitalize job types (data include job types in capitalized versions)
jobDataRaw$JobType <- tolower(jobDataRaw$JobType)
jobTypes <- factor(jobDataRaw$JobType)

# - Omit entries with negative time stamps
jobData <- subset(jobDataRaw, StartedRunningTimeStamp >= 0)
jobData <- subset(jobDataRaw, WrapWC > 0)

# Test for differences in WCTime and wall clock time computed from time stamps
jobData$computedWCTime <- with(jobData, FinishedTimeStamp - StartedRunningTimeStamp)
WCTimeAccuracy <- jobData$computedWCTime / 1000.0 - jobData$WrapWC

# Todo What do we do with those entries not matching the computed wall time?
# Enough to filter them out of the data completely?

pdf(file = "plots/histogram_timestampAccuracy.pdf")
WCTimeAccuracyTrunc <- subset(WCTimeAccuracy, WCTimeAccuracy <= 10 & WCTimeAccuracy >= -10)
hist(WCTimeAccuracyTrunc, main = "Timestamps vs. Walltime Comparison (distribution truncated)", 
     xlab = "Timestamp Diff. - Walltime",
     ylab = "Number of Jobs")
dev.off()

quantile(WCTimeAccuracy, c(0.02, 0.98))


## Find "real" CPU demand
## Note: This assumes high load on each machine to be able to scale the CPU time with the HepSPEC result for the machine

jobData$CPUTimePerCore <- jobData$WrapCPU / jobData$NCores

# Scale with HepSPEC06 result
jobData$CPUDemand <- jobData$WrapCPU * jobData$HSScorePerJobSlot


## Find estimate for I/O time
## Note: This is probably inaccurate and should only be used as a first guideline
jobData$estimatedIOTime <- jobData$WrapWC - jobData$WrapCPU / jobData$NCores


## Data Visualization

# Data Summary

jobCountPerType <- as.data.frame(table(jobData$Type))
colnames(jobCountPerType) <- c("Type", "Count")

pdf(file = "plots/pie_jobTypeCount.pdf")

pieChart <- ggplot(jobCountPerType, aes(x = "", y = Count, fill = Type)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Job Number per Type", x = "", y = "")

pieChart

dev.off()


pdf(file = "plots/pie_jobTypeCPUDemand.pdf")

jobDemandPerType <- aggregate(jobData$CPUDemand, by = list(Type = jobData$Type), FUN = sum, na.rm = TRUE)

pieChartDemands <- ggplot(jobDemandPerType, aes(x = "", y = x, fill = Type)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Total CPU Demand per Type", x = "", y = "")

pieChartDemands

dev.off()

for(i in levels(jobData$Type)) {
  pdf(file = paste("plots/histogram_CPUTime_", i, ".pdf", sep=""))
  
  jobsOfType <- subset(jobData, Type == i)
  jobPlot <- ggplot(jobsOfType, aes(x = WrapCPU)) + geom_histogram(bins = 100)
  
  jobPlot <- jobPlot + ggtitle(paste0("CPU Time (added for cores) for jobs of type: ", i)) +
    labs(x = "Job CPU Time (WrapCPU)")
  
  print(jobPlot)
  
  dev.off()
}

for(i in levels(jobData$Type)) {
  pdf(file = paste("plots/histogram_CPUDemand_", i, ".pdf", sep=""))
  
  jobsOfType <- subset(jobData, Type == i)
  jobPlot <- ggplot(jobsOfType, aes(x = CPUDemand)) + geom_histogram(bins = 100)
  
  jobPlot <- jobPlot + ggtitle(paste0("CPU demand (synthetic, assumes high load) for jobs of type: ", i)) +
    labs(x = "CPU Demand (CPU Time Scaled by HepSPEC result)")
  
  print(jobPlot)
  dev.off()
  
  ## Create stochastic expression for interarrival time
  stoEx <- distToStoEx(jobsOfType$CPUDemand)
  print(paste0("StoEx for CPUDemand of job type: ", i))
  print(stoEx)
}


# Overview over CPU usage of all job types
pdf(file = "plots/histogram_CPUTimeAll.pdf")

(allJobTypes <- ggplot(jobData, aes(x = WrapCPU)) + geom_histogram(bins = 100) +
  facet_wrap(~Type, ncol = 4))

dev.off()


## CPU usage and I/O estimate
# Created plots are very complex, so do not generate these as pdf or inside of an IDE

for(i in levels(jobData$Type)) {
  png(file = paste("plots/scatter_CPUDemand_estIOTime_", i, ".png", sep = ""),
      width = 1600,
      height = 1000,
      res = 144)
  
  jobsOfType <- subset(jobData, Type == i)
  (plotCPUIO <- ggplot(jobsOfType, aes(x = WrapCPU, y = estimatedIOTime)) + geom_point(alpha = 1/2) +
     facet_wrap(~Type, ncol = 4) + labs(title = "Total CPU Time vs. Estimated I/O Time"))
  print(plotCPUIO)
  
  dev.off()
}

## Job Summary

# jobSummary <- tapply(jobData, jobData$Type, summary)
library(data.table)
jobSummaryTable <- data.table(jobData)
jobSummary <- jobSummaryTable[, list(count = .N,
                                     WallTime.mean = mean(WrapWC, na.rm = TRUE),
                                     CPUDemand.mean = mean(CPUDemand, na.rm = TRUE), 
                                     CPUDemand.sd = sd(CPUDemand, na.rm = TRUE),
                                     estimatedIOTime.mean = mean(estimatedIOTime, na.rm = TRUE),
                                     estimatedIOTime.sd = sd(estimatedIOTime, na.rm = TRUE)), by = Type]
setkey(jobSummary, Type)

print("Job Summary by Type:")
print(jobSummary)


# Analyze job arrivals
# This is the time the jobs were started, not their initial arrival time!

dailyJobs <- aggregate(jobData$StartedRunningDatetime, by=
                         list(date = as.Date(jobData$StartedRunningDatetime), 
                              type = jobData$Type), 
                       FUN=length)
colnames(dailyJobs) <- c("date", "type", "count")

pdf(file = "plots/bar_jobsPerDay.pdf")
ggplot(dailyJobs, aes(x = date, y = count, fill = type)) + geom_bar(stat = "identity") +
  ggtitle("Number of Started Jobs per Day")
dev.off()

## Calculate interarrival times for each type of job

interArrTimes <- c()

for(i in levels(jobData$Type)) {
  jobsOfType <- subset(jobData, Type == i)
  
  # Sort by ascending time stamp
  jobsOfType <- jobsOfType[order(jobsOfType$StartedRunningDatetime), ]
  
  # Subtract lagged start time stamp, add NA for first entry
  jobsOfType$timeDiff <- c(NA, diff(jobsOfType$StartedRunningDatetime))
  
  # Remove outliers with large interarrival time
  # Todo: How to avoid this? Removing outliers might skew data (as with very large interarrival times)
  jobsOfTypeCutoff <- subset(jobsOfType,
    jobsOfType$timeDiff < as.numeric(quantile(jobsOfType$timeDiff, c(0.95), na.rm = TRUE)))

  pdf(file = paste("plots/histogram_interarrivalTimes_", i, ".pdf", sep = ""))
  
  cutoffQuantile <- quantile(jobsOfType$timeDiff, c(0.95), na.rm = TRUE)[[1]]
  binWidth <- ceiling(cutoffQuantile / 80); 
  
  
  interarrivalPlot <- ggplot(jobsOfTypeCutoff, aes(x = jobsOfTypeCutoff$timeDiff)) +
    geom_histogram(binwidth = binWidth) +
    ggtitle(paste0("Interarrival times for jobs of type: ", i)) +
    labs(x = "Interarrival Time / s")
  
  print(interarrivalPlot)
  
  dev.off()
  
  interArrTimes <- rbind(interArrTimes, list(i, mean(jobsOfType$timeDiff, na.rm = TRUE), sd(jobsOfType$timeDiff, na.rm = TRUE)))
  
  ## Create stochastic expression for interarrival time
  stoEx <- distToStoEx(jobsOfType$timeDiff, cutoffQuantile = 0.95)
  print(paste0("StoEx for interarrival times of job type: ", i))
  print(stoEx)
}

colnames(interArrTimes) <- c("Type", "interArrTime.mean", "interArrTime.sd")
print("Interarrival times (only calculated between jobs of specified type):")
interArrTimes

# Create a time series for the following analyses
# reprocessingJobs <- subset(jobData, Type == "reprocessing")
# timeSeries <- xts(reprocessingJobs, reprocessingJobs$StartedRunningDatetime)
