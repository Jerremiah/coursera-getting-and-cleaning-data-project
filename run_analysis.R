url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "dataset.zip")
download.file(url,f)
unzip(f,exdir = "J:/ForR/coursera_R_programming/material")

# create file path
x_train <- "UCI HAR Dataset/train/X_train.txt"
y_train <- "UCI HAR Dataset/train/Y_train.txt"
x_test <- "UCI HAR Dataset/test/X_test.txt"
y_test <- "UCI HAR Dataset/test/Y_test.txt"
subject_train <- "UCI HAR Dataset/train/subject_train.txt"
subject_test <- "UCI HAR Dataset/test/subject_test.txt"
feature <- "UCI HAR Dataset/features.txt"
activity_labels <- "UCI HAR Dataset/activity_labels.txt"

# import every data into R
features <- read.table(feature, col.names = c("n","signals"))
act <- read.table(activity_labels , col.names = c("code", "activity"))
xtrain <- read.table(x_train, col.names = features$signals)
ytrain <- read.table(y_train, col.names = "code")
xtest <- read.table(x_test, col.names = features$signals)
ytest <- read.table(y_test, col.names = "code")
subjecttrain <- read.table(subject_train, col.names = "subject")
subjecttest <- read.table(subject_test, col.names = "subject")


##Answer 1


library(dplyr)
library(purrr)
mergex <- rbind(xtrain, xtest)
mergey <- rbind(ytrain, ytest)
mergesubject <- rbind(subjecttrain, subjecttest)
mergedata <- cbind(mergesubject,mergey,mergex)


##Answer 2


#filter "mean" and "std" put of features
mean_and_std_features <- grep("-(mean|std)\\(\\)", features$signals)
features$signals[mean_and_std_features]


# extract "mean"&"std" out of xtrain, and lable with signals in the features
traindatax <- xtrain[mean_and_std_features]
names(traindatax) <- features$signals[mean_and_std_features]

# extract "mean"&"std" out of xtest, and lable with signals in the features
testdatax <- xtest[mean_and_std_features]
names(testdatax) <- features$signals[mean_and_std_features]

# make one dataset of combine all components we need
mean_and_std_mergedata <- cbind(mergesubject,mergey,rbind(traindatax, testdatax))


## Answer3


# change element of mergey with act's second column value which is matched with mergey'svalue by first column. 
mergey[,1] <- factor(act[mergey[,1],2])
names(mergey) <- "activities"




# Answer4

#label the data set with descriptive variable name
mean_and_std_mergedata[,2] <- factor(act[mean_and_std_mergedata[,2],2])
names(mean_and_std_mergedata)[2:ncol(mean_and_std_mergedata)] <- c("activities", as.character(features$signals))


# Answer5
library(reshape2)
melted_mergedata <- melt(mean_and_std_mergedata, id = c("subject", "activities"))
mean_melted_mergedata <- dcast(melted_mergedata, subject + activities ~ variable, mean)
View(mean_melted_mergedata)


write.table(mean_melted_mergedata, "tidy.txt", row.name=FALSE)
