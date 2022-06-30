library(data.table)
library(dplyr)
library(tidyverse)

# download and unzip the dataset into folder 

setwd("./coursera/JHU_DS/c3_clean/Project")
filename <- "project.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists(filename)){
    download.file(fileURL, destfile = filename, method = "curl")}
if(!file.exists("UCI HAR Dataset")){unzip("project.zip")}

# load the data 

features <- fread("UCI HAR Dataset/features.txt", col.names = c("feature_code", "features"))
activities <- fread("UCI HAR Dataset/activity_labels.txt", col.names = c("task_code", "task"))
X_train <- fread("UCI HAR Dataset/train/X_train.txt", col.names = features$features)
Y_train <- fread("UCI HAR Dataset/train/Y_train.txt", col.names = "task_code")
X_test <- fread("UCI HAR Dataset/test/X_test.txt", col.names = features$features)
Y_test <- fread("UCI HAR Dataset/test/Y_test.txt", col.names = "task_code")
subject_train <- fread("UCI HAR Dataset/train/subject_train.txt", col.names = "subject_code")
subject_test <- fread("UCI HAR Dataset/test/subject_test.txt", col.names = "subject_code")

# merges the training and the test sets to create one data set 

X <- rbind(X_train, X_test)
Y <- rbind(Y_train, Y_test)
subject <- rbind(subject_train, subject_test)
mergedData <- cbind(subject, Y, X )

# extracts only the measurements on the mean and standard deviation for each measurement

tidyData <- mergedData %>% select(subject_code, task_code, contains("mean"), contains("std"))

# uses descriptive activity names to name the activities in the data set

tidyData <- merge(activities, tidyData, by = "task_code") %>% select(-task_code)

# appropriately labels the data set with descriptive variable names

names(tidyData)[1] = "activity"
names(tidyData)<- names(tidyData) %>% gsub(pattern = "Acc", replacement = "Accelerometer") %>%
    gsub(pattern = "Gyro", replacement = "Gyroscope") %>%
    gsub(pattern = "BodyBody", replacement = "Body") %>%
    gsub(pattern = "Mag", replacement = "Magnitue") %>%
    gsub(pattern = "angle", replacement = "Angle") %>%
    gsub(pattern = "gravity", replacement = "Gravity") %>%
    gsub(pattern = "^t", replacement = "Time") %>%
    gsub(pattern = "^f", replacement = "Frequency") %>%
    gsub(pattern = "tBody", replacement = "TimeBody") %>%
    gsub(pattern = "-freq()", replacement = "Frequency", ignore.case = TRUE) %>%
    gsub(pattern = "-mean()", replacement = "Mean", ignore.case = TRUE) %>%
    gsub(pattern = "-std()", replacement = "STD", ignore.case = TRUE) 
    
# from the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

tidyData_2 <- tidyData %>% group_by( subject_code, activity) %>% 
    summarise_all(dplyr :: funs(mean))

write.table(tidyData_2, "tidyData.txt", row.name=FALSE)



