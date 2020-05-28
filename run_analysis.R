##  run_analysis
    
    # Downloading Files for analysis
    if(!file.exists("/data")) {dir.create("./data")}
    if(!exists("zipURL")) {zipURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"}
    if(!file.exists("./data/dataset.zip")) {download.file(zipURL, destfile = "./data/dataset.zip", method = "curl")}
    unzip("./data/dataset.zip")
    
    # Read files to R
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                                  col.names = c("Code", "Activity"))
    features <- read.table("./UCI HAR Dataset/features.txt", 
                           col.names = c("Code", "Feature"))
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                               col.names = "Subject")
    xtest <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                         col.names = features$Feature)
    ytest <- read.table("UCI HAR Dataset/test/y_test.txt", 
                         col.names = "Code")
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", 
                                col.names = "Subject")
    xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                         col.names = features$Feature)
    ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt", 
                         col.names = "Code")
    
    # Merge Training and Test Datasets into one
    library(dplyr)
    X <- rbind(xtrain, xtest)
    Y <- rbind(ytrain, ytest)
    Subject <- rbind(subject_train, subject_test)
    Merged_Data <- cbind(Subject, Y, X)
    
    rm("xtrain", "xtest", "subject_train", "subject_test", "ytest", "ytrain", "X", "Y", "Subject")
    
    # Uses descriptive activity names to name the activities in the data set
    Dataset <- merge(activity_labels, Merged_Data, by = "Code")
    Dataset$Code <- NULL
    
    # Extracts only the measurements on the mean and standard deviation for each measurement
    filteredcolumns <- grepl("Subject|Activity|std|mean", colnames(Dataset))
    Dataset <- Dataset[,filteredcolumns]
    
    # Appropriately labels the data set with descriptive variable names
    DatasetCols <- colnames(Dataset)
    DatasetCols <- gsub("[\\(\\)-]", "", DatasetCols) #Remove special chars
    
    DatasetCols <- gsub("^f", "frequencyDomain", DatasetCols)
    DatasetCols <- gsub("^t", "timeDomain", DatasetCols)
    DatasetCols <- gsub("Acc", "Accelerometer", DatasetCols)
    DatasetCols <- gsub("Gyro", "Gyroscope", DatasetCols)
    DatasetCols <- gsub("Mag", "Magnitude", DatasetCols)
    DatasetCols <- gsub("Freq", "Frequency", DatasetCols)
    DatasetCols <- gsub("mean", "Mean", DatasetCols)
    DatasetCols <- gsub("std", "StandardDeviation", DatasetCols)
    DatasetCols <- gsub("BodyBody", "Body", DatasetCols)
    
    colnames(Dataset) <- DatasetCols
    
    # Create a second, independent tidy data set with the average 
    # of each variable for each activity and each subject.
    
    DatasetMeans <- Dataset %>%
                    group_by(Subject, Activity) %>%
                    summarise_each(funs(mean))
    # Output to file
    write.table(DatasetMeans, "tidy_data.txt", row.names = FALSE, 
                quote = FALSE)