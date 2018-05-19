#You should create one R script called run_analysis.R that does the following.

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



#THAT´S OPTIONAL:
#set my work directory 
setwd("D:/MASTER_BIG_DATA/Coursera_DataScientist/03-Getting and Cleaning Data")


#downloading the dataset 
if(!file.exists("./data")){dir.create("./data")}


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl, destfile = "./data/project.zip")

#unzip the file
unzip("./data/project.zip")



#########################################
##PREV 01 - READING DATA
#########################################

# reading training data
trainingSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
trainingValues <- read.table("UCI HAR Dataset/train/X_train.txt")
trainingActivities <- read.table("UCI HAR Dataset/train/y_train.txt")

# reading test data
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("UCI HAR Dataset/test/X_test.txt")
testActivities <- read.table("UCI HAR Dataset/test/y_test.txt")

# read features and activities
features <- read.table("UCI HAR Dataset/features.txt")

activities <- read.table("UCI HAR Dataset/activity_labels.txt")

head(activities)

#########################################
##PREV 01 - COLUMN NAMES
#########################################

#colnames os activities aren´t descriptive (V1, V2). So i will change them
colnames(activities) <- c("ActivityId", "Activity")
colnames(features) <- c("FeatureId", "Feature")


#and i change the names oftraninng and test tables. For "values" i use the second column of features
names(trainingSubjects)<-"SubjectId"

names(trainingValues)<-features$V2

names(trainingActivities)<-"ActivityId"

names(testSubjects)<-"SubjectId"

names(testValues)<-features$V2

names(testActivities)<-"ActivityId"


###########################################################################
#Question 1 : Merge the training and the test sets to create one data set.
###########################################################################

#two merged datasets (test and training: maybe we will want work with them separately)
trainMerged<-cbind(trainingSubjects,trainingValues,trainingActivities)

testMerged<-cbind(testSubjects,testValues,testActivities)

#my final merged dataset:
trainAndTestMerged<-rbind(trainMerged,testMerged)



######################################################################################################
#Question 2 : Extracts only the measurements on the mean and standard deviation for each measurement.
######################################################################################################

slicedMergedSet <<- trainAndTestMerged[,grepl("SubjectId|ActivityId|mean\\(\\)|std\\(\\)",colnames(trainAndTestMerged))]

head(slicedMergedSet)


######################################################################################
#Question 3 : Uses descriptive activity names to name the activities in the data set.
######################################################################################

# replace 1 2 3 4 5 6 by LAYING SITTING STANDING WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS
slicedMergedSet$Activity <- factor(slicedMergedSet$Activity, 
                                 levels = activities[, 1], labels = activities[, 2])

######################################################################################
#Question 4 : Appropriately labels the data set with descriptive variable names.
######################################################################################

#saving the colnames:

colnames_bk <- colnames(slicedMergedSet)

# first i remove "(",")" and "-"
colnames_bk <- gsub("[\\(\\)-]", "", colnames_bk)

#t stands for time and f stands for frequency, so i change that:

colnames_bk <- gsub("^f", "freq", colnames_bk)
colnames_bk <- gsub("^t", "time", colnames_bk)

#and finally change mean for Mean and std for Std

colnames_bk <- gsub("mean", "Mean", colnames_bk)
colnames_bk <- gsub("std", "Std", colnames_bk)


#and we rename the columns
colnames(slicedMergedSet) <- colnames_bk

colnames(slicedMergedSet)

######################################################################################
#Question 5 : From the data set in step 4, creates a second, independent 
#tidy data set with the average of each variable for each activity and each subject.
######################################################################################
library(dplyr)

slicedMergedSetMeans <- slicedMergedSet %>% 
  group_by(SubjectId, ActivityId,Activity) %>%
  summarise_all(funs(mean))

head(slicedMergedSetMeans)

write.table(slicedMergedSetMeans, file = "tidydataset.txt", row.names= FALSE)

