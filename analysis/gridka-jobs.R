library(anytime) # Date and Time conversion
library(ggplot2) # More advanced plots
library(xts) # Time Series

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
WCTimeAccuracyTrunc <- subset(WCTimeAccuracy, WCTimeAccuracy <= 10 & WCTimeAccuracy >= -10)
hist(WCTimeAccuracyTrunc)
quantile(WCTimeAccuracy, c(0.02, 0.98))


## Find "real" CPU demand
## Note: This assumes high load on each machine to be able to scale the CPU time with the HepSPEC result for the machine

jobData$CPUTimePerCore <- jobData$WrapCPU / jobData$NCores

# Scale with HepSPEC06 result
jobData$CPUDemand <- jobData$CPUTimePerCore * jobData$HSScorePerJobSlot


## Find estimate for I/O time
## Note: This is probably inaccurate and should only be used as a first guideline
jobData$estimatedIOTime <- jobData$WrapWC - jobData$WrapCPU / jobData$NCores


## Data Visualization

# Data Summary

jobCountPerType <- as.data.frame(table(jobData$Type))
colnames(jobCountPerType) <- c("Type", "Count")

pieChart <- ggplot(jobCountPerType, aes(x = "", y = Count, fill = Type)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0)

pieChart

for(i in levels(jobData$Type)) {
  jobsOfType <- subset(jobData, Type == i)
  jobPlot <- ggplot(jobsOfType, aes(x = WrapCPU)) + geom_histogram(bins = 100)
  
  jobPlot <- jobPlot + ggtitle(paste0("CPU Time (added for cores) for jobs of type: ", i))
  
  print(jobPlot)
}

for(i in levels(jobData$Type)) {
  jobsOfType <- subset(jobData, Type == i)
  jobPlot <- ggplot(jobsOfType, aes(x = CPUDemand)) + geom_histogram(bins = 100)
  
  jobPlot <- jobPlot + ggtitle(paste0("CPU demand (synthetic, assumes high load) for jobs of type: ", i))
  
  print(jobPlot)
  
  
}


# Overview over CPU usage of all job types
(allJobTypes <- ggplot(jobData, aes(x = WrapCPU)) + geom_histogram(bins = 100) +
  facet_wrap(~Type, ncol = 4))

## CPU usage and I/O estimate
# Created plots are very complex, so do not generate these as pdf or inside of an IDE
# png(file = "plots/plot_png%03d_generated.png", width = 1600, height = 1000, res = 144)
# 
# for(i in levels(jobData$Type)) {
#   jobsOfType <- subset(jobData, Type == i)
#   (plotCPUIO <- ggplot(jobsOfType, aes(x = CPUDemand, y = estimatedIOTime)) + geom_point(alpha = 1/2) +
#      facet_wrap(~Type, ncol = 4))
#   print(plotCPUIO)
# 
# }
# 
# dev.off()

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

ggplot(dailyJobs, aes(x = date, y = count, fill = type)) + geom_bar(stat = "identity") +
  ggtitle("Number of Started Jobs per Day")

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
  jobsOfType <- subset(jobsOfType,
    jobsOfType$timeDiff < as.numeric(quantile(jobsOfType$timeDiff, c(0.99), na.rm = TRUE)))

  interarrivalPlot <- ggplot(jobsOfType, aes(x = jobsOfType$timeDiff)) + geom_histogram(bins = 100) +
    ggtitle(paste0("Interarrival times for jobs of type: ", i))
  
  print(interarrivalPlot)
  
  interArrTimes <- rbind(interArrTimes, list(i, mean(jobsOfType$timeDiff), sd(jobsOfType$timeDiff)))
}

colnames(interArrTimes) <- c("Type", "interArrTime.mean", "interArrTime.sd")
print("Interarrival times (only calculated between jobs of specified type):")
interArrTimes

# Create a time series for the following analyses
# reprocessingJobs <- subset(jobData, Type == "reprocessing")
# timeSeries <- xts(reprocessingJobs, reprocessingJobs$StartedRunningDatetime)

# dev.off()

