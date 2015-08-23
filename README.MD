Getting and Cleaning Data Course Project
========================================

## Project Overview

The complete project overview is described on the course website on Coursera. In summary,
the run_analysis.R script takes data from an experiment where accelerometer and gyroscope
measurements were taken on 30 volunteer subjects using a smartphone while the subjects
performed six physical activities.

Using the time and frequency domain variable raw data from the experiment this script
accomplishes the following five project objectives:

1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement. 
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The output variable names are described in the accomanying Codebook from the github repo.

## Assumptions for run_analysis.R Script

The run_analysis.R script is designed with the following assumptions:
*The "UCI HAR Dataset" folder has been extracted from its zipped download form and is stored in its original hierarchical format in the current working directory.
*The "reshape2" and "dplyr" R packages have been downloaded and installed.

## Project Assumptions

### We can accomplish the five objectives in any order

It made more sense to me to do them in the order that I followed.

### The raw data in the inertial signals folders is not necessary for completing the objectives

It doesn't contain any means or standard deviations of measurements. And it is big.

### Only measurement data containing the text "mean()" represents required mean data

Per the "features info" table there are also meanFreq() measurements and vectors obtained
by averaging other mean measurements. I do not believe these vectors are included under the
project instructions to extract the mean for each measurement. The frequency components
also have a regular mean() measurement in addition to the meanFreq(), which is really a
weighted average. The vectors from averaging mean angle measurements are not themselves
means but are additional variables constructed from means. As a result of this assumption,
the script selects 33 mean variables and 33 standard deviation variables from the raw
data to include in the eventual output.

### The measurement feature variable names from the features file are sufficiently readable

These variable names are a little "busy" but they effectively capture what the variable
is measuring within a small amount of space. I considered changing these names but
concluded that expanding on the abbreviations would result in them being too lengthy
to offset any readability benefits gained.

### The output files should read in to R in identical form to the table from which it was created

I did some minor manipulation of the column variable names in step 9 to ensure this occurs.
There may be a better way to accomplish this within the read.table function but after an
hour of searching I opted for the easier approach.

### How the run_analysis.R Script Works

This script contains no functions and will run in its entirity upon being sourced.
The operation of the script proceeds as follows:

### Step 1 - Load reshape2 and dplyr packages into R
This package is required later in the script but is loaded at the beginning.

### Step 2 - Move to base directory and read in activity and feature key tables
The script sets the working directory to the "UCI HAR Dataset" folder stored in the
initial working directory. It then reads the activity labels table (table linking
activity with number code) and the features table (the vector with the time and frequency
domain variable names) into data frames with the same name as the underlying file.

### Step 3 - Move to Training and Test folders to read in the associated data
The script sets the working directory to the "Training" folder within the "USI HAR Dataset"
directory and reads each of the data files there to data frames of the same name.
The "subject train" file contains the subject number for each data record, the "y train"
file contains the activity ID code for each record, and the large "X train" file contains
the underlying measurements for all of the feature variables. After reading in the training
set data, the script performs the same procedure for the test set data in the same manner.

### Step 4 - Rename activities in the data sets with descriptive activity names
This is the step addressing script objective 3 - descriptive activity names.
The script first creates a 1 column vertical data frame the same length as the overall
data set that contains the row ID number. It then binds this data frame to y test/train. This
is necessary in order to later preserve the order of the data when the merge is performed.
The script then merges the activity codes in y test/train with the activity names data frame
on activity number, arranges the data in the order it originally appeared using the script-
created row ID number, and deletes the row ID column now that it is no longer needed.

At this point, the full length activity names have been brough into the table as a variable.
The activity ID numbers are deleted at a later step.

The procedure for this step is effectively identical for both the training and test data sets.

### Step 5 - Join subject, activity, and measurement data tables
The script uses the cbind function to combine first the subject ID and activity name tables,
and then the newly combined table and the 561 column feature measurements data.

The procedure for this step is effectively identical for both the training and test data sets.

### Step 6 - Assign descriptive variable name column labels
This is the step that mainly addresses script objective 4. The script assigns a vector whose
first three values are column names "Subject ID", "Activity ID", and "Activity Name" and
remaining values are the features file column of measurement variables to the column names
of the combined subject ID, activity, and measurement data tables.

The procedure for this step is effectively identical for both the training and test data sets.

### Step 7 - Merge the training and test sets to create one data set
This is the step that addresses script objective 1. The script combines the two data sets
with a simple rbind function. The resulting data frame has 10,299 rows and 564 columns.

### Step 8 - Extract only measurements on the mean and standard deviation for each measurement
This is the step that addresses script objective 2. The script uses the grepl function to
create vectors with the column names that contain either "mean()", "Mean()" or "std()",
combines these three vectors into one using OR expresions that indicates whether a column
name contains either of these character strings, and creates a new table using this vector to
subset the appropriate columns from the old table. Note: this step also drops the no longer
needed "Activity ID" column.

Refer to project assumptions for the basis of using "mean()" and "std()" to identify
applicable fields.

### Step 9 - Adjust column names in table for output file
This step doesn't do much, but when testing the eventual output file to see if it would read
back into R using the read.table function, it kept replacing the metacharacters in the column
names with ".". This is not hard to fix once the table is read but I didn't know how to do so
within the read.table command so I used various sub() functions to adjust the column variable
names slightly by replacing instances of "()-" with a simple underscore, "-" with ".", and
removing stand-alone instances of "()"so that they would not change when read from the text
file back into R.

### Step 10 - Create a tidy data set with average of each variable for each activity and each subject
This is the step that addresses script objective 5. The script uses the melt() and dcast()
functions from the reshape2 package to manipulate the data so that in the resulting table
each row/record is a Subject_ID and Activity Name combinations and the columns contain the
means for the given measurement for the combination of the particular record.

This new table meets the two applicable tidy data criteria. Each variable is in one column
and each observation (subject/activity combo) is in a different row. Because only one
table is required, the other criteria do not apply.

The script also appends "MEAN" onto the end of the measurement variable column names since
they now contain the mean of the variables across each subject/activity combo.

### Step 11 - Write tidy data set to a text file to upload for project submission
The script returns to the original working directory containing the "UCI HAR Dataset"
folder and outputs a text file of the tidy data table in this directory using the
write.table command. Row names are turned off per project instructions.

The text file can be read into R by entering the following command into the console:

        tidy_data_set <- read.table("tidy_data_set", header = TRUE)