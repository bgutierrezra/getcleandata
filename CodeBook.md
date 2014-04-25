# Description of Script and variables to solve final project (Getting and Cleaning Data course)

### run_analysis.R does the following: 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


### Assumptions: original unzipped data files reside in the following path of working directory: ~/data/UCI HAR Dataset



### Load required libraries

library(memisc);
library (plyr);
library(reshape2)


### Prepare "test" data set by reading files and joining subject information with activity and feature values in a test data frame (testdf) that eventually will be combined with a "train" data frame 

subjectTest<-read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names="subjectID")
testLabels<-read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names="activityID")
xTest<-read.table("./data/UCI HAR Dataset/test/X_test.txt")
features<-read.table("./data/UCI HAR Dataset/features.txt")

colnames(xTest)<-features$V2     # assigns feature names to the variables

testdf<-data.frame(subjectTest, testLabels, xTest, check.names=FALSE)     # completes test data table


### Prepare "train" dataset by reading files and joining subject information with activity and feature values in a train data frame that eventually will be combined with test data frame prepared above

trainLabels<-read.table("./data/UCI HAR Dataset/train/y_train.txt",  col.names="activityID")
subjectTrain<-read.table("./data/UCI HAR Dataset/train/subject_train.txt",  col.names="subjectID")
xTrain<-read.table("./data/UCI HAR Dataset/train/X_train.txt")

colnames(xTrain)<-features$V2   # assigns feature names to the variables

traindf<-data.frame(subjectTrain, trainLabels, xTrain, check.names=FALSE)  # complete train data frame


### Merge "test" and "train" data frames to obtain an intermediate set called tidyDF that will be used to obtain the final tidy data set as required by the project instructions

tidyDF<-rbind(testdf, traindf)



### Identify measurements  on the mean and standard deviation

varsOfInterest<-grep("mean\\(\\)|std\\(\\)", colnames(tidyDF), value=TRUE)  # vector with the name of variables of interest. Regular expression searches for all features that contain mean() or std() as part of their name

tidyDF1<-tidyDF[, c("subjectID", "activityID", varsOfInterest)]  # subsets merged tidyDF to obtain data frame that will be further processed to obtain final data set


### Label activities using cases function from library(memisc)

tidyDF1$activityName <- cases(
  "Walking"=tidyDF1$activityID == 1,
  "WalkingUpstairs"=tidyDF1$activityID == 2,
  "WalkingDownstairs"=tidyDF1$activityID == 3,
  "Sitting"=tidyDF1$activityID == 4,
  "Standing"=tidyDF1$activityID == 5,
  "Laying"=tidyDF1$activityID == 6)


### Reorder first tidy data frame just for the sake of clarity

tidyDF1<- tidyDF1[with(tidyDF1, order(activityID, subjectID)), ] # ascending order by activity and subject

colorder<-c("activityName", "activityID", "subjectID", varsOfInterest)

tidyDF1<-tidyDF1[ ,colorder]   # rearrange column order



### Create a second tidy data set (tidyDF2) with the average of each variable for each activity and each subject

auxDF<-melt(tidyDF1, id.vars=c("activityName", "subjectID"), measure.vars=varsOfInterest )  # create auxiliary data frame used as intermediate step towards obtaining avg. of each variable

tidyDF2<-dcast(auxDF, activityName+subjectID ~ variable, fun.aggregate = mean, na.rm = TRUE)


### Write both tidy data frames as text files

write.table(tidyDF1, file="./tidydata1.txt", sep='\t', row.names=F)   

write.table(tidyDF2, file="./tidydata2.txt", sep='\t', row.names=F) 

## tidydata2.txt is the final output of the script
