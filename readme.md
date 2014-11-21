run_analysis.R Raeadme file

This script reads the Samsung Data Set and executes:

     Reads both test and train data sets from the Samsung trial
     provides descriptive column names to better describe the features(columns)
     adds subject and activity information to each observation  
     merges test and trial into one single data set 
     From this data set creates a second dataset with averages for each variable for each activity and each subject.
	 This second dataset is named tidyAveragesDataset.txt

It requires the following R packages/libraries
 library(gdata)
  library(plyr)
  library(reshape2)
  
It requires that the Samsung data set is unzipped in the same directory as the run_analysis.R script file
It will create an output file names tidyAveragesDataset.txt in the same directory