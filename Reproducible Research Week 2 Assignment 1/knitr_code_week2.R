## Load Needed Library
library(dplyr)


## Load the activity data, then convert to table
rawActivity <- read.csv("activity.csv")
rawActivity <- tbl_df(rawActivity)

## Show Structure of the raw data
str(rawActivity)

##########################################################################

## Create a subset of the data without the NA values
RemoveNA <- rawActivity %>% filter(is.na(steps) == FALSE)  

##########################################################################

## Sum steps for each date
StepsByDate <- RemoveNA %>% group_by(date) %>% summarize(StepsPerDay = sum(steps))

head(StepsByDate)

## Plot a histogram of the summary data
hist(StepsByDate$StepsPerDay, breaks = 10, main="", xlab="Number of Steps Taken Per Day")

## Calculate the Mean
mean(StepsByDate$StepsPerDay) 

## Calculate the Median
median(StepsByDate$StepsPerDay) 

###########################################################################

