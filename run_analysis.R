#########################################################
##
## Getting and Cleaning Data Course Project
## Jaspreet Gill
## 23rd April, 2015
#
# The run_analysis.R scripts reads data from the following url
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
# and then it 
#
# 1.) Merges the training and the test sets to create one data set.
# 2.) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.) Uses descriptive activity names to name the activities in the data set
# 4.) Appropriately labels the data set with descriptive variable names. 
# 5.) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###################################################################
# Download data and unzip the dataset under data. The unzipped dataset is stored in the folder UCI HAR Dataset

if(file.exists("data")) {
   unlink("data", recursive=TRUE)
}
dir.create("data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = fileUrl, destfile = "data/Dataset.zip", method="curl")
unzip(zipfile="data/Dataset.zip", exdir="data")

filePath <- file.path("data", "UCI HAR Dataset")

###################################################################
# Read files
# The values for features are in tables trainX and testX 
# The values for activity are in tables trainY and testY
# The values for a subject are in tables subjectTrain and subjectTest

trainX <- read.table(file.path(filePath, "train", "X_train.txt" ),header = FALSE)
testX <- read.table(file.path(filePath, "test", "X_test.txt"), header= FALSE)
trainY <- read.table(file.path(filePath, "train", "y_train.txt"), header=FALSE)
testY <- read.table(file.path(filePath, "test", "y_test.txt"), header=FALSE)
subjectTrain <- read.table(file.path(filePath, "train", "subject_train.txt"), header=FALSE)
subjectTest <- read.table(file.path(filePath, "test", "subject_test.txt"), header=FALSE)

featureNames <- read.table(file.path(filePath, "features.txt"), header=FALSE)
activityLabels <- read.table(file.path(filePath, "activity_labels.txt"), header=FALSE)

#Step 1) Combine train and test data
dataXFeature <- rbind(trainX, testX)
dataYActivity <- rbind(trainY, testY)
subjectData <- rbind(subjectTrain, subjectTest)

#Step 2) Extract only the measurements on the mean and standard deviation for each measurement.
# get only columns with mean() or std() in their names
mean_and_std_features <- grep("-(mean|std)\\(\\)", featureNames[, 2])
dataXFeature <- dataXFeature[, mean_and_std_features]


#Step 3) Use descriptive activity names to name the activities in the data set
dataYActivity[, 1] <- activityLabels[dataYActivity[,1], 2]

#Step 4) Appropriately labels the data set with descriptive variable names. 
names(subjectData) <- c("Subject")
names(dataYActivity) <- c("Activity")
names(dataXFeature) <- featureNames[mean_and_std_features, 2]

names(dataXFeature) <- gsub("^t", "Time", names(dataXFeature))
names(dataXFeature) <- gsub("^f", "Freq", names(dataXFeature))
names(dataXFeature) <- gsub("BodyBody", "Body", names(dataXFeature))
names(dataXFeature) <- gsub("-mean[(][)]-?", "Mean", names(dataXFeature))
names(dataXFeature) <- gsub("-std[(][)]-?", "Std", names(dataXFeature))
names(dataXFeature) <- gsub("-meanFreq[(][)]-?", "MeanFreq", names(dataXFeature))

#Merge all columns
Data <- cbind(subjectData, dataYActivity, dataXFeature)

# Step 5
# Create a second, independet tidy data set with average of each variable
# for each activity and each subject

library(plyr)
averages_data <- ddply(Data, .(Subject, Activity), function(x) colMeans(x[, 3:68]))
write.csv(averages_data, "data/averages_data.csv", row.names=FALSE)
write.table(averages_data, "data/averages_data.txt", row.names=FALSE, sep='\t')