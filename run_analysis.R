# load plyr library
library(plyr)

create_tidy_data <- function() {
    
    # 1. Merges the training and the test sets to create one data set.
    # ----------------------------------------------------------------
    
    # Read in the test data
    sTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
    xTest <- read.table("UCI HAR Dataset/test/X_test.txt")
    yTest <- read.table("UCI HAR Dataset/test/y_test.txt")
    
    # Read in the training data
    sTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
    xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
    yTrain <- read.table("UCI HAR Dataset/train/y_train.txt")
    
    # Combine the rows from train and test into single tables
    sCombined <- rbind(sTest, sTrain)
    xCombined <- rbind(xTest, xTrain)
    yCombined <- rbind(yTest, yTrain)
    

    # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    # ------------------------------------------------------------------------------------------
    featureData <- read.table("UCI HAR Dataset/features.txt")
    
    # Build a vector so we can subset only the measurements that are mean or std
    meanAndStd <- grep("(mean|std)\\(\\)", featureData$V2)
    
    # subset the measurement data by our vector
    xCombined <- xCombined[, meanAndStd]
    
        
    # 3. Uses descriptive activity names to name the activities in the data set
    # -------------------------------------------------------------------------
    
    # Read the activities table
    activities <- read.table("UCI HAR Dataset/activity_labels.txt")
    
    # merge the activities labels with y data
    yCombined[, 1] <- activities[yCombined[, 1], 2]

    
    # Here is where the full data set is actually put together into one
    fullData <- cbind(sCombined, yCombined, xCombined)
    
    
    # 4. Appropriately labels the data set with descriptive variable names.
    # ---------------------------------------------------------------------
    colnames(fullData) <- c("Subject.Id", "Activity.Name", as.vector(featureData[meanAndStd, 2]))

    # attempt to improve the names of the feature data labels
    
    
    # 5. From the data set in step 4, creates a second, independent tidy data set...
    # ------------------------------------------------------------------------------
    tidyData <- ddply(fullData, .(Subject.Id, Activity.Name), function(x) colMeans(x[, 3:68]))
    
    write.table(tidyData, "tidyData.txt", row.name=FALSE)
    
}