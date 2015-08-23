## run_analysis.R Script
## Refer to associated README.md file

## Load required add-on packages
library(reshape2)
library(dplyr)

## Move to base directory and read activity label and feature key tables
setwd(paste(getwd(), "/UCI HAR Dataset", sep = ""))
UCI_home <- getwd()
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

## Read in training data
setwd(paste(UCI_home, "/train", sep = ""))
subject_train <- read.table("subject_train.txt")
X_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")

## Read in test data
setwd(paste(UCI_home, "/test", sep = ""))
subject_test <- read.table("subject_test.txt")
X_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")

## Rename activities in data set with descriptive activity names [ASSIGNMENT TASK #3]
id <- data.frame(row = 1:dim(y_train)[1])
y_train <- cbind(id, y_train)
y_train_labels <- merge(y_train, activity_labels, by.x = "V1", by.y = "V1")
y_train_labels <- arrange(y_train_labels, row)
y_train_labels <- select(y_train_labels, -row)

## Join subject, activity, and measurement tables for training data
subject_train_with_labels <- cbind(subject_train, y_train_labels)
train_table <- cbind(subject_train_with_labels, X_train)

## Assign descriptive variable name column labels to joined training table
## [ASSIGNMENT TASK #4]
names(train_table) <- c("Subject_ID", "Activity_ID", "Activity_Name", as.character(features$V2))

## Rename activities in data set with descriptive activity names [ASSIGNMENT TASK #3]
id <- data.frame(row = 1:dim(y_test)[1])
y_test <- cbind(id, y_test)
y_test_labels <- merge(y_test, activity_labels, by.x = "V1", by.y = "V1")
y_test_labels <- arrange(y_test_labels, row)
y_test_labels <- select(y_test_labels, -row)

## Join subject, activity, and measurement tables for test data
subject_test_with_labels <- cbind(subject_test, y_test_labels)
test_table <- cbind(subject_test_with_labels, X_test)

## Assign descriptive variable name column labels to joined test table
## [ASSIGNMENT TASK #4]
names(test_table) <- c("Subject_ID", "Activity_ID", "Activity_Name", as.character(features$V2))

## Merge the training and test sets to create one data set [ASSIGNMENT TASK #1]
complete_table <- rbind(train_table, test_table)

## Extract only measurements on the mean and standard deviation
## for each measurement [ASSIGNMENT TASK #2]
mean_columns <- grepl("mean()", names(complete_table), fixed = TRUE)
mean_columns2 <- grepl("Mean()", names(complete_table), fixed = TRUE)
stdev_columns <- grepl("std()", names(complete_table), fixed = TRUE)
columns_to_keep <- mean_columns == TRUE | mean_columns2 == TRUE | stdev_columns == TRUE
mean_std_table <- cbind(complete_table[1], complete_table[3], complete_table[columns_to_keep])

## Adjust column names in table for output file - refer to README
names(mean_std_table) <- sub("()-", "_", x = names(mean_std_table), fixed = TRUE)
names(mean_std_table) <- sub("()", "", x = names(mean_std_table), fixed = TRUE)
names(mean_std_table) <- sub("-", ".", x = names(mean_std_table), fixed = TRUE)

## Create tidy data set with average of each variable for each activity and
## each subject. [ASSIGNMENT TASK #5]
melted_data <- melt(mean_std_table, id.vars = c("Subject_ID", "Activity_Name"),
     measure.vars = names(mean_std_table)[3:68])
tidy_data_set <- dcast(melted_data, Subject_ID + Activity_Name ~ variable, mean)
names(tidy_data_set)[3:68] <- paste(names(tidy_data_set)[3:68], "MEAN", sep = "_")

## Return to home directory and write tidy data set to a .txt file to upload
## for project submission.
setwd(UCI_home)
setwd("..")
write.table(tidy_data_set, file = "tidy_data_set.txt", row.names = FALSE)