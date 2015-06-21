run.analysis <- function ()
{
        # Clear the Memory
        rm(list=ls())
        
        
        ### 1. Merges the training and the test sets to create one data set.
        
        ## Read in all the data
        FeaturesTable = read.table('./UCI HAR Dataset/features.txt')
        ActivityLabelTable = read.table('./UCI HAR Dataset/activity_labels.txt')
        
        XTestTable <- read.table("./UCI HAR Dataset/test/X_test.txt")
        YTestTable <- read.table("./UCI HAR Dataset/test/y_test.txt")
        SubjectTestTable <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        
        XTrainTable <- read.table("./UCI HAR Dataset/train/X_train.txt")
        YTrainTable <- read.table("./UCI HAR Dataset/train/y_train.txt")
        SubjectTrainTable <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        
        ## Assign Column Names to the imported data to make it meaningful
        colnames(FeaturesTable) = c('featureId', 'featureLabel')
        colnames(ActivityLabelTable) = c('activityId', 'activityType')
        
        colnames(XTestTable) = FeaturesTable[,2]
        colnames(YTestTable) = "activityId"
        colnames(SubjectTestTable) = "subjectId"
        
        colnames(XTrainTable) = FeaturesTable[,2]
        colnames(YTrainTable) = "activityId"
        colnames(SubjectTrainTable) = "subjectId"
        
        
        ## Merge the tables together
        
        # Merge Test Table
        TestData <- cbind(SubjectTestTable, YTestTable, XTestTable)
        
        # Merge Train Table
        TrainData <- cbind(SubjectTrainTable, YTrainTable, XTrainTable)
        
        # Merge both Test and Train Table together
        FullData <- rbind(TestData, TrainData)
        
        colNames <- colnames(FullData)
        
        
        
        ### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
        
        SelectedFeatures <- FeaturesTable[grepl("mean\\(\\)", FeaturesTable$featureLabel) | grepl("std\\(\\)", FeaturesTable$featureLabel), ]
        FullData2 <- FullData[, c(c(1, 2, 3), SelectedFeatures$featureId + 3)]
        
        
        
        ### 3. Uses descriptive activity names to name the activities in the data set.
        
        FullData3 <- merge(FullData2, ActivityLabelTable)
        
        
        
        ### 4. Appropriately labels the data set with descriptive variable names.
        
        SelectedFeatures$featureLabel <- gsub("\\(\\)", "", SelectedFeatures$featureLabel)
        SelectedFeatures$featureLabel <- gsub("-", ".", SelectedFeatures$featureLabel)
        
        for (i in 1:length(SelectedFeatures$featureLabel))
        {
                ## FullData2 will be used later to create "a second independent tidy data" for FullData4
                colnames(FullData2)[i + 3] <- SelectedFeatures$featureLabel[i]
                
                ## FullData3 is the "first independent tidy data"
                colnames(FullData3)[i + 3] <- SelectedFeatures$featureLabel[i]
        }
        
        
        
        ### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        
        FullData4 <- aggregate(FullData2, list(subject = FullData2$subjectId, activity = FullData2$activityId), mean, na.rm=TRUE)
        FullData4 <- FullData4[,(3:length(colnames(FullData4)))]
        
        tidyData <- merge(FullData4, ActivityLabelTable)
        
        
        ## Please upload your data set as a txt file created with write.table() using row.name=FALSE
        write.table(tidyData, 'tidyData.txt', row.names=FALSE, sep='\t')
}
