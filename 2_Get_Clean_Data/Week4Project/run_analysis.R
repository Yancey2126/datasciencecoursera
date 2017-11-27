## !/usr/bin/env Rstudio 1.0.153
## -*- coding: utf-8 -*-
## Getting and Cleaning Data Course Project 
library(reshape2)

setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/2_Get_Clean_Data")
getwd()

# Download the dataset and unzip it --------------------------------------------------------------
filename <- "Samsung.zip"
if (!file.exists(filename)) {
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(url, filename, method = "curl")
}
if (!file.exists("UCI HAR Dataset")) {
        unzip(filename, list = TRUE) 
}
# list.files("./UCI HAR Dataset/")

# Loead the descriptive labels and features ------------------------------------------------------
Labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
Features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)

# Decide which measurement to extract (with mean and s.d) ----------------------------------------
Feature.filer <- grep("*mean*|*std*", Features[, 2])
Feature.chosed <- Features[Feature.filer, 2]

# Load data from train and test ------------------------------------------------------------------
X.train <- read.table("UCI HAR Dataset/train/X_train.txt")[Feature.filer]
dim(X.train) # Take a look at the dataset
y.train <- read.table("UCI HAR Dataset/train/y_train.txt")
dim(y.train)
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")
dim(subject.train)
# merge data into one dataset
train <- cbind(X.train, y.train, subject.train)

# same for test set ------------------------------------------------------------------------------
X.test <- read.table("UCI HAR Dataset/test/X_test.txt")[Feature.filer]
dim(X.test)
y.test <- read.table("UCI HAR Dataset/test/Y_test.txt")
dim(y.test)
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")
dim(subject.test)
test <- cbind(X.test, y.test, subject.test)
# combine test and train together
CompleteDt <- rbind(train, test)
dim(CompleteDt)
# labels the data set with descriptive variable names
colnames(CompleteDt) <- c(Feature.chosed, "activity", "subjects") 

# Uses descriptive activity names to name the activities in the data set --------------------------
CompleteDt$activity <- factor(CompleteDt$activity, levels = Labels[, 1], labels = Labels[, 2])

# Create the second data set with means -----------------------------------------------------------
DTmelted <- melt(CompleteDt, id = c("subjects", "activity"))
newDt <- dcast(DTmelted, subject + activity ~ variable, mean)
write.table(newDt, "tidy.txt")
