## Create one R script called run_analysis.R that does the following: 
## 1. Merges the training and test sets to create 1 data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


# 1. Merge training and test sets to create 1 data set

#set working directory
setwd('/Users/MiaMNM/Desktop/Research/UCI HAR Dataset/')

# Import the data
features <- read.table('./features.txt',header=FALSE)
activityType <- read.table('./activity_labels.txt', header=FALSE)
subjectTrain <- read.table('./train/subject_train.txt', header=FALSE) 
xTrain <- read.table('./train/x_train.txt', header=FALSE)
yTrain <- read.table('./train/y_train.txt', header=FALSE) 

# Assign column names to the imported data
colnames(activityType) <- c('activityId','activityType')
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[, 2] 
colnames(yTrain) <- "activityId"

# Create final training data set by merging yTrain, subjectTrain and xTrain
trainingData <- cbind(yTrain, subjectTrain, xTrain)

# Import the test data
subjectTest <- read.table('./test/subject_test.txt', header=FALSE) 
xTest <- read.table('./test/x_test.txt', header=FALSE)
yTest <- read.table('./test/y_test.txt', header=FALSE)

# Assign column names to the imported test data
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[, 2] 
colnames(yTest) <- "activityId"

# Create the final test data set by merging xTest, yTest and subjectTest data
testData <- cbind(yTest, subjectTest, xTest)

# Combine training and test data to create 1 data set
DataSet <- rbind(trainingData, testData)

# Create a vector for the column names from the DataSet
# Use vector to select desired mean and SD columns
colNames <- colnames(DataSet) 

# 2. Extract only the measurements on the mean and SD for each measurement. 

# Create a logicalVector with TRUE values for the ID, mean and SD columns and FALSE for the rest
logicalVector <- (grepl("activity..", colNames) | grepl("subject..", colNames) | grepl("-mean..", colNames) & !grepl("-meanFreq..", colNames) & !grepl("mean..-", colNames) | grepl("-std..", colNames) & !grepl("-std()..-", colNames))

# Subset DataSet table based on the logicalVector, keeping only desired columns
DataSet <- DataSet[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge DataSet with the activityType table to include descriptive activity names
DataSet <- merge(DataSet, activityType, by='activityId', all.x=TRUE)

# Update the colNames vector to include new column names after the DataSet is merged
colNames <- colnames(DataSet) 

# 4. Appropriately label the data set with descriptive activity names. 

# Clean up variable names
for (i in 1:length(colNames)) {
    colNames[i] <- gsub("\\()", "", colNames[i])
    colNames[i] <- gsub("-std$", "StdDev", colNames[i])
    colNames[i] <- gsub("-mean", "Mean", colNames[i])
    colNames[i] <- gsub("^(t)", "TimeOf", colNames[i])
    colNames[i] <- gsub("^(f)", "FreqOf", colNames[i])
    colNames[i] <- gsub("(BodyAcc)", "BodyAcceleration", colNames[i])
    colNames[i] <- gsub("(GravityAcc)", "GravityAcceleration", colNames[i])
    colNames[i] <- gsub("(BodyAccJerk)", "BodyAccelerationJerk", colNames[i])
    colNames[i] <- gsub("(BodyGyro)", "BodyGyro", colNames[i])
    colNames[i] <- gsub("(BodyGyroJerk)", "BodyGyroJerk", colNames[i])
    colNames[i] <- gsub("BodyAccMag", "BodyAccelerationMagnitude", colNames[i])
    colNames[i] <- gsub("GravityAccMag", "BodyAccelerationMagnitude", colNames[i])
    colNames[i] <- gsub("BodyAccJerkMag", "BodyAccelerationJerkMagnitude", colNames[i])
    colNames[i] <- gsub("BodyGyroMag", "BodyGyroMagnitude", colNames[i])
    colNames[i] <- gsub("BodyGyroJerkMag", "BodyGyroJerkMagnitude", colNames[i])
}

# Reassign the new descriptive column names to the DataSet
colnames(DataSet) <- colNames

#5.Create a 2nd independent tidy data set with the average of each variable for each activity and each subject

# Create a new table without the activityType column
DataSetNew <- DataSet[, names(DataSet) != 'activityType']

# Summarize the DataSetNew table to include just the mean of each variable for each activity and each subject
TidyDataSet <- aggregate(DataSetNew[, names(DataSetNew) != c('activityId', 'subjectId')], by=list(activityId=DataSetNew$activityId, subjectId = DataSetNew$subjectId), mean)

# Merge tidyData with activityType to include descriptive activity names
TidyDataSet <- merge(TidyDataSet, activityType, by='activityId', all.x=TRUE)

# Export TidyDataSet
write.table(TidyDataSet, './TidyDataSet.txt', row.names=TRUE,sep='\t')

