#Part 1
#read names of features and activities

featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE)

#read training data

subjectTrain <- read.table("subject_train.txt", header = FALSE)
activityTrain <- read.table("y_train.txt", header = FALSE)
featuresTrain <- read.table("X_train.txt", header = FALSE)

#read test data

subjectTest <- read.table("subject_test.txt", header = FALSE)
activityTest <- read.table("y_test.txt", header = FALSE)
featuresTest <- read.table("X_test.txt", header = FALSE)


#merge training and test set
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#name the features
colnames(features) <- t(featureNames[2])

#merge data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Part 2
#Extract columns with mean and std -> 86 columns
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

#Add activity and subject column (column 562, 563) to the list -> 88 columns
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

#Create extractedData by extracting the required columns from the completeData
extractedData <- completeData[,requiredColumns]

#Part 3
#change Activity field to character
extractedData$Activity <- as.character(extractedData$Activity)

#Use for loop to change it to activity name
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

#change the acitivty name field to factor
extractedData$Activity <- as.factor(extractedData$Activity)

#Part 4
#Appropriately labels the data set with descriptive variable names. Change Acc to Accelerometer, BodyBody to Body, 
#Gyro to Gyroscope, Mag to Magnitude, f to Frequency and t to Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))

#Part 5
#Set subject as factor variable
extractedData$Subject <- as.factor(extractedData$Subject)

#set extractedData as data table
extractedData <- data.table(extractedData)

#Use aggregate to calculate average of each variable for each activity and each subject
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)

#Arrange in ascending order for the subject
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

#Use write.table to write into data file
write.table(tidyData, file = "TidyData.txt", row.names = FALSE)




