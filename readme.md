run_analysis.R Readme file

This script reads the Samsung Data Set and executes:

     Reads both test and train data sets from the Samsung trial
     provides descriptive column names to better describe the features(columns)
     adds subject and activity information to each observation  
     merges test and trial into one single data set 
     From this data set creates a second dataset with averages for each variable for each activity and each subject.

    This output dataset is named tidyAveragesDataset.txt

It requires the following R packages/libraries
 library(gdata)
  library(plyr)
  library(dplyr)
  library(reshape2)
  
the source of the Samsung data for the project can be downloaded here:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


It requires that the Samsung data set is unzipped in the same directory as the run_analysis.R script file. the list of files and their location (subdirectory) is as follows

## files that are required in the working directory
##    features.txt
##    activity_labels.txt
## files that are requires in the "test" subdirectory
##    test/X_test.txt
##    test/subject_test.txt
##    test/y_test.txt
## files that are required in the "train" subdirectory
##    train/X_train.txt
##    train/subject_train.txt
##    train/y_train.txt


## It will create an output file names tidyAveragesDataset.txt in the same directory
## files that are created by this script in the working directory
##    tidyAveragesDataset.txt
