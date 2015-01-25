# Getting And Cleaning Data. Course Project

## Introduction

This repository includes script called "run_analysis.R", CodeBook.md and README.md
The goal of project is to prepare tidy data that can be used for later analysis.

Script "run_analysis.R" does merged and tidy data from original data.
Description of original data: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

CodeBook gives more information about working of script, variables and outputs.

## Working with script.

1. Download data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Unzip it in directory would like (in result: /YOUR_PATH/UCI HAR Dataset/)
3. Set working directory inside data directory. -> Use: setwd("YOUR_PATH/UCI HAR Dataset/")
4. Put file "run_analysis.R" inside directory "YOUR_PATH/UCI HAR Dataset/"
5. Run the script
6. In directory will appear two files: "all_cleaned_data.txt" and "tidy_average_data.txt" when the work of script will be done.
