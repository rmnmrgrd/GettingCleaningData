# You should create one R script called run_analysis.R that does the following.
# 1 - Merges the training and the test sets to create one data set.
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
# 3 - Uses descriptive activity names to name the activities in the data set
# 4 - Appropriately labels the data set with descriptive variable names.
# 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# - 'README.txt'
# - 'features_info.txt': Shows information about the variables used on the feature vector.
# - 'features.txt': List of all features.
# - 'activity_labels.txt': Links the class labels with their activity name.
# - 'train/X_train.txt': Training set.
# - 'train/y_train.txt': Training labels.
# - 'test/X_test.txt': Test set.
# - 'test/y_test.txt': Test labels.
library(dplyr)
library(tidyr)

# Transformation 1: Load activity labels
activity_labels <- read.table("dataset\\activity_labels.txt", sep = "", header=FALSE, strip.white = TRUE)
names(activity_labels) <- c("code", "activity")

# Transformation 2: Load features
features <- read.table("dataset\\features.txt", sep = "", header=FALSE, strip.white = TRUE)
names(features) <- c("code", "feature")

# Transformation 3: Load test and train data
test <- tbl_df(read.table("dataset\\test\\X_test.txt", sep="", header = FALSE, strip.white = TRUE))
train <- tbl_df(read.table("dataset\\train\\X_train.txt", sep="", header = FALSE, strip.white = TRUE))
dataset <- rbind(test, train)
rm(test, train)

# Transformation 4: Load activities for measures
test_y <- tbl_df(read.table("dataset\\test\\Y_test.txt", sep="", header = FALSE, strip.white = TRUE))
train_y <- tbl_df(read.table("dataset\\train\\Y_train.txt", sep="", header = FALSE, strip.white = TRUE))
dataset_y <- rbind(test_y, train_y)
rm(test_y, train_y)

# Transformation 5: Load subjects for measures
subject_test <- tbl_df(read.table("dataset\\test\\subject_test.txt", sep="", header = FALSE, strip.white = TRUE))
subject_train <- tbl_df(read.table("dataset\\train\\subject_train.txt", sep="", header = FALSE, strip.white = TRUE))
subjects <- rbind(subject_test, subject_train)
rm(subject_test, subject_train)

# Transformation 6: Merge activities, subjects and data
full_dataset <- cbind(dataset_y, subjects, dataset)

# Transformation 7: Name columns, resolve duplicate columns
names(full_dataset) <- c("activity", "subject", as.character(features$feature))
names(full_dataset) <- paste(names(full_dataset), seq_along(full_dataset))

# Transformation 8: Extract desired columns
tidy_dataset <- select(full_dataset, grep("mean\\(|std\\(|^activity|^subject", names(full_dataset)))

# Transformation 9: Clean up the column names as they are unique
names(tidy_dataset) <- gsub(" [0-9]{1,3}$", "", names(tidy_dataset))

# Transformation 10: Set the activity label instead of the code
tidy_dataset$activity <- activity_labels[tidy_dataset$activity, 2]

# Transformation 11: Tidy up the dataset using gather()
tidy_dataset <- gather(tidy_dataset, key="measure", value="value", -(1:2))

# Transformation 12: Average values for all measures per activity and subject is calculated and stored in 
# tidy_dataset_avg.
tidy_dataset_avg <- summarise(group_by(tidy_dataset, activity, subject), avg=mean(value))

write.table(tidy_dataset_avg, file="output.txt", row.name=FALSE)
