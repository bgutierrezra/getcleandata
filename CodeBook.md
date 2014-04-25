##
## Script to solve project requirements in Getting and Cleaning Data course
##

# Initializing environment

rm(list=ls())

#Load required libraries

library(memisc)
library (plyr)
library(reshape2)


## Prepare test dataset by reading files and then combining in a test data frame
# that eventually will be combined with a train data frame

subjectTest<-read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names="subjectID")
testLabels<-read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names="activityID")
xTest<-read.table("./data/UCI HAR Dataset/test/X_test.txt")
features<-read.table("./data/UCI HAR Dataset/features.txt")

colnames(xTest)<-features$V2   # assigns names to the variables

testdf<-data.frame(subjectTest, testLabels, xTest, check.names=FALSE)   # complete test data table


## Prepare train dataset by reading files and then combining in a train data frame
# that eventually will be combined with test data frame prepared above

trainLabels<-read.table("./data/UCI HAR Dataset/train/y_train.txt",  col.names="activityID")
subjectTrain<-read.table("./data/UCI HAR Dataset/train/subject_train.txt",  col.names="subjectID")
xTrain<-read.table("./data/UCI HAR Dataset/train/X_train.txt")

colnames(xTrain)<-features$V2

traindf<-data.frame(subjectTrain, trainLabels, xTrain, check.names=FALSE)  # complete train data frame


# Merge test and train data frames to obtain an intermediate set tidyDF that will be used to
# obtain the final tidy data set as required by the project instructions

tidyDF<-rbind(testdf, traindf)



# identify measurements  on the mean and standard deviation

varsOfInterest<-grep("mean\\(\\)|std\\(\\)", colnames(tidyDF), value=TRUE)  # vector with the name of columns

tidyDF1<-tidyDF[, c("subjectID", "activityID", varsOfInterest)]  # subsets to obtain tidy set


# Label activities using cases function from library(memisc)

tidyDF1$activityName <- cases(
  "Walking"=tidyDF1$activityID == 1,
  "WalkingUpstairs"=tidyDF1$activityID == 2,
  "WalkingDownstairs"=tidyDF1$activityID == 3,
  "Sitting"=tidyDF1$activityID == 4,
  "Standing"=tidyDF1$activityID == 5,
  "Laying"=tidyDF1$activityID == 6)


# Reorder first tidy data frame just for the sake of clarity

tidyDF1<- tidyDF1[with(tidyDF1, order(activityID, subjectID)), ] # ascending order by activity and subject

colorder<-c("activityName", "activityID", "subjectID", varsOfInterest)

tidyDF1<-tidyDF1[ ,colorder] # rearrange column order



# Create a second tidy data set (tidyDF2) with the average of each variable for each activity and each subject

auxDF<-melt(tidyDF1, id.vars=c("activityName", "subjectID"), measure.vars=varsOfInterest )  # auxiliary data frame used as intermediate step towards obtaining avg. of each variable

tidyDF2<-dcast(auxDF, activityName+subjectID ~ variable, fun.aggregate = mean, na.rm = TRUE)


# Write both tidy data frames as text files

write.table(tidyDF1, file="./tidydata1.txt", sep='\t', row.names=F)   

write.table(tidyDF2, file="./tidydata2.txt", sep='\t', row.names=F) 

# tidydata2.txt is the final output of the script
