# CODE BOOK
## Getting And Cleaning Data. Course Project

Source of the original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Original description: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Working with script.

1. Download data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Unzip it in directory would like (in result: /YOUR_PATH/UCI HAR Dataset/)
3. Set working directory inside data directory. -> Use: setwd("YOUR_PATH/UCI HAR Dataset/")
4. Put file "run_analysis.R" inside directory "YOUR_PATH/UCI HAR Dataset/"
5. Run the script
6. In directory will appear two files: "all_cleaned_data.txt" and "tidy_average_data.txt" when the work of script will be done.


Description of steps "run_analysis.R" script

### 1. Merges the training and the test sets to create one data set.

- x.train	reading data set from file "train/X-train.txt". This data frame includes 7352 observations of 561 variables. ('train/X_train.txt': Training set.)
- x.test	reading data set from file "train/X-test.txt". This data frame includes 2947 observations of 561 variables. ('test/X_test.txt': Test set.)
- x.all		merging x.train and x.test by rows. New data frame includes 10299 observations of 561 variables
- rm(x.train) and rm(x.test) remove x.train and x.test

- y.train	reading data set from file "train/Y-train.txt". This data frame includes 7352 observations of 1 variable. ('train/y_train.txt': Training labels.)
- y.test	reading data set from file "train/Y-test.txt". This data frame includes 2947 observations of 1 variable ('test/y_test.txt': Test labels.)
- y.all		merging y.train and y.test by rows. New data frame includes 10299 observations of 1 variable
- rm(y.train) and rm(y.test) remove y.train and y.test

- s.train	reading data set from file "train/S-train.txt". This data frame includes 7352 observations of 1 variable ('train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.)
- s.test	reading data set from file "train/S-test.txt". This data frame includes 2947 observations of 1 variable
- s.all		merging y.train and y.test by rows. New data frame includes 10299 observations of 1 variable
- rm(s.train) and rm(s.test) remove s.train and s.test

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

- features	reading data set from file "./features.txt". This data frame includes 561 observations of 2 variables. ('features.txt': List of all features.)
- ind.mean_std	integer vector (length = 66) of indecies of names from features. These indecies just for features with mean and standart diviation names. 
- x.mean_std	data frame includes 10299 observations of 66 variables. This data frame is subsetting columns of x.all data frame, according indecies of ind.mean_std variable
- addind column names for x.mean_std according ind.mean_std of features

### 3. Uses descriptive activity names to name the activities in the data set.

- activity	reading data set from file "./activity_labels.txt". Data frame includes 6 observations of 1 variable. ('activity_labels.txt': Links the class labels with their activity name.)
- y.names	new data frame. It includes 10299 observations of 1 variable. Each observation is one of 6 activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)
- addind column name "activity" for y.names 

### 4. Appropriately labels the data set with descriptive variable names. 
- addind column name "subject" for s.all
- all 		merging data frame. includes column s.all as number of subject, y.names as name of activity, x.mean_std - measurements. It includes 10299 observations of 68 variables.
- writing all to "./all_cleaned_data.txt"

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

- only.subj	integer vector (length = 30) unique number of each subject
- only.act	character vector (length = 6) unique names of activities
- tidy.data	creating empty data frame
- for {} 	for each subject and each activity:
- creating temporary data frame temp.df. It equals subsetting one number of subject and one activity
- avg.data	numeric vector which includes mean values of each column of temp.df, besides columns "subject" and "activity" (length = 66)
- avg.data.n	data frame (lke a row) with 1 observation and 68 variables
- tidy.data	data frame which needed. Each observation of tidy_data is one avg.data.n. Dimension tidy-data = 180 observation and 68 variables. where first two variables are "subject" and "activity"

- writing tidy_data to "./tidy_average_data.txt" (row.name=FALSE)