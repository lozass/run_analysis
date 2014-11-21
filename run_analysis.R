## run_analysis.R
## 
##  this script does:
##     Reads test and train data sets from the Samsung trial
##     provides descriptive column names to better describe the features(columns)
##     adds subject and activity information to each observation  
##     merges test and trial into one single data set 
##     From this data set creates a second (finaldata) datasetwith averages for each variable for each activity and each subject.
##
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
##
## files that are created by this script in the working directory
##    tidyAveragesDataset.txt
    

  library(gdata)
  library(plyr)
  library(dplyr)
  library(reshape2)
  ## read the names of the features (561 features)
  features <- read.csv("features.txt", sep=" ", header=F)
  meancolumns <- grep("mean\\(\\)", features$V2)
  stdcolumns <- grep("std\\(\\)", features$V2)
  interestcolumns = c(meancolumns, stdcolumns)
  ## interestcolumns has the mean and std columns of the dataset
  interestcolumns <- sort(interestcolumns)
  cc <- c(rep(-16,561))
  cc[interestcolumns]<- 16
  ## read from X_test and X_train only the required columns                        
  X_test <- read.fwf("test/X_test.txt", width = cc, col.names=features$V2[interestcolumns])
  X_train <- read.fwf("train/X_train.txt" , width = cc, col.names=features$V2[interestcolumns])
  ## read subjects train and test and append (cbind) them to the end of X_test and X_train
  ## creating X2_test and X2_train
  subject_train <- read.csv("train/subject_train.txt", header=F,col.names=c("subject"))
  subject_test <- read.csv("test/subject_test.txt", header=F, col.names=c("subject"))
  X2_train <- cbind(X_train,subject_train)
  X2_test <- cbind (X_test, subject_test)
  ## read activities and append them to the end of X3_test and X3_train
  activity_train <- read.csv("train/y_train.txt", header=F, col.names=c("activity"))
  activity_test <- read.csv("test/y_test.txt", header=F, col.names=c("activity"))
  X3_train <- cbind(X2_train,activity_train)
  X3_test <- cbind (X2_test, activity_test)
  ##binds test and train datasets <- rbind
  X <- rbind(X3_train, X3_test)
  # reaplaces activity codes by activity labels  
  act_codes<-read.fwf("activity_labels.txt", width=c(1,-1,21), header=F, col.names=c("code","activity"))
  
  X$activity[X$activity==1] <- "WALKING"
  X$activity[X$activity==2] <- "WALKING_UPSTAIRS"
  X$activity[X$activity==3] <- "WALKING_DOWNSTAIRS"
  X$activity[X$activity==4] <- "SITTING"
  X$activity[X$activity==5] <- "STANDING"
  X$activity[X$activity==6] <- "LAYING"

  ## replace feature names with better descriptive names coming from a csv file 
  betternames <- read.csv("newgoodlabels.csv", header=T, sep=";")
  names(X) <- betternames[,3]

  
  ## start here the creation of the avgs data set 
  X2 <- tbl_df(X)
  ## reorders columns to have subject and activity as columns 1 and 2   
  X3 <- select(X2, subject, activity, Temporal.Body.Acceleration.mean.X:Frequency.Body.Gyro.Jerk.Magnitude.stddev)
  ## group by subject and activity
  X4 <- group_by(X3,subject, activity)
  finaldata <- ddply(X4,c("subject","activity"), summarise, 
                     Avg.Temporal.Body.Acceleration.mean.X = mean(Temporal.Body.Acceleration.mean.X),
                     Avg.Temporal.Body.Acceleration.mean.Y = mean(Temporal.Body.Acceleration.mean.Y),
                     Avg.Temporal.Body.Acceleration.mean.Z = mean(Temporal.Body.Acceleration.mean.Z),
                     Avg.Temporal.Body.Acceleration.stdev.X = mean(Temporal.Body.Acceleration.stdev.X),
                     Avg.Temporal.Body.Acceleration.stdev.Y = mean(Temporal.Body.Acceleration.stdev.Y),
                     Avg.Temporal.Body.Acceleration.stdev.Z = mean(Temporal.Body.Acceleration.stdev.Z),
                     Avg.Temporal.Gravity.Acceleration.mean.X = mean(Temporal.Gravity.Acceleration.mean.X),
                     Avg.Temporal.Gravity.Acceleration.mean.Y = mean(Temporal.Gravity.Acceleration.mean.Y),
                     Avg.Temporal.Gravity.Acceleration.mean.Z = mean(Temporal.Gravity.Acceleration.mean.Z),
                     Avg.Temporal.Gravity.Acceleration.stddev.X = mean(Temporal.Gravity.Acceleration.stddev.X),
                     Avg.Temporal.Gravity.Acceleration.stddev.Y = mean(Temporal.Gravity.Acceleration.stddev.Y),
                     Avg.Temporal.Gravity.Acceleration.stddev.Z = mean(Temporal.Gravity.Acceleration.stddev.Z),
                     Avg.Temporal.Body.Acceleration.Jerk.mean.X = mean(Temporal.Body.Acceleration.Jerk.mean.X),
                     Avg.Temporal.Body.Acceleration.Jerk.mean.Y = mean(Temporal.Body.Acceleration.Jerk.mean.Y),
                     Avg.Temporal.Body.Acceleration.Jerk.mean.Z = mean(Temporal.Body.Acceleration.Jerk.mean.Z),
                     Avg.Temporal.Body.Acceleration.Jerk.stddev.X = mean(Temporal.Body.Acceleration.Jerk.stddev.X),
                     Avg.Temporal.Body.Acceleration.Jerk.stddev.Y = mean(Temporal.Body.Acceleration.Jerk.stddev.Y),
                     Avg.Temporal.Body.Acceleration.Jerk.stddev.Z = mean(Temporal.Body.Acceleration.Jerk.stddev.Z),
                     Avg.Temporal.Body.Gyro.mean.X = mean(Temporal.Body.Gyro.mean.X),
                     Avg.Temporal.Body.Gyro.mean.Y = mean(Temporal.Body.Gyro.mean.Y),
                     Avg.Temporal.Body.Gyro.mean.Z = mean(Temporal.Body.Gyro.mean.Z),
                     Avg.Temporal.Body.Gyro.stddev.X = mean(Temporal.Body.Gyro.stddev.X),
                     Avg.Temporal.Body.Gyro.stddev.Y = mean(Temporal.Body.Gyro.stddev.Y),
                     Avg.Temporal.Body.Gyro.stddev.Z = mean(Temporal.Body.Gyro.stddev.Z),
                     Avg.Temporal.Body.Gyro.Jerk.mean.X = mean(Temporal.Body.Gyro.Jerk.mean.X),
                     Avg.Temporal.Body.Gyro.Jerk.mean.Y = mean(Temporal.Body.Gyro.Jerk.mean.Y),
                     Avg.Temporal.Body.Gyro.Jerk.mean.Z = mean(Temporal.Body.Gyro.Jerk.mean.Z),
                     Avg.Temporal.Body.Gyro.Jerk.stddev.X = mean(Temporal.Body.Gyro.Jerk.stddev.X),
                     Avg.Temporal.Body.Gyro.Jerk.stddev.Y = mean(Temporal.Body.Gyro.Jerk.stddev.Y),
                     Avg.Temporal.Body.Gyro.Jerk.stddev.Z = mean(Temporal.Body.Gyro.Jerk.stddev.Z),
                     Avg.Temporal.Body.Acceleration.Magnitude.mean.X = mean(Temporal.Body.Acceleration.Magnitude.mean.X),
                     Avg.Temporal.Body.Acceleration.Magnitude.stddev.X = mean(Temporal.Body.Acceleration.Magnitude.stddev.X),
                     Avg.Temporal.Gravity.Acceleration.Magnitude.mean = mean(Temporal.Gravity.Acceleration.Magnitude.mean),
                     Avg.Temporal.Gravity.Acceleration.Magnitude.stddev = mean(Temporal.Gravity.Acceleration.Magnitude.stddev),
                     Avg.Temporal.Body.Acceleration.Magnitude.mean = mean(Temporal.Body.Acceleration.Magnitude.mean),
                     Avg.Temporal.Body.Acceleration.Magnidude.stddev = mean(Temporal.Body.Acceleration.Magnidude.stddev),
                     Avg.Temporal.Body.Gyro.Magnitude.mean = mean(Temporal.Body.Gyro.Magnitude.mean),
                     Avg.Temporal.Body.Gyro.Magnitude.stddev = mean(Temporal.Body.Gyro.Magnitude.stddev),
                     Avg.Temporal.Body.Gyro.Jerk.Magnitude.mean = mean(Temporal.Body.Gyro.Jerk.Magnitude.mean),
                     Avg.Temporal.Body.Gyro.Jerk.Magnitude.stddev = mean(Temporal.Body.Gyro.Jerk.Magnitude.stddev),
                     Avg.Frequency.Body.Acceleration.mean.X = mean(Frequency.Body.Acceleration.mean.X),
                     Avg.Frequency.Body.Acceleration.mean.Y = mean(Frequency.Body.Acceleration.mean.Y),
                     Avg.Frequency.Body.Acceleration.mean.Z = mean(Frequency.Body.Acceleration.mean.Z),
                     Avg.Frequency.Body.Acceleration.stddev.X = mean(Frequency.Body.Acceleration.stddev.X),
                     Avg.Frequency.Body.Acceleration.stddev.Y = mean(Frequency.Body.Acceleration.stddev.Y),
                     Avg.Frequency.Body.Acceleration.stddev.Z = mean(Frequency.Body.Acceleration.stddev.Z),
                     Avg.Frequency.Body.Acceleration.Jerk.mean.X = mean(Frequency.Body.Acceleration.Jerk.mean.X),
                     Avg.Frequency.Body.Acceleration.Jerk.mean.Y = mean(Frequency.Body.Acceleration.Jerk.mean.Y),
                     Avg.Frequency.Body.Acceleration.Jerk.mean.Z = mean(Frequency.Body.Acceleration.Jerk.mean.Z),
                     Avg.Frequency.Body.Acceleration.Jerk.stddev.X = mean(Frequency.Body.Acceleration.Jerk.stddev.X),
                     Avg.Frequency.Body.Acceleration.Jerk.stddev.Y = mean(Frequency.Body.Acceleration.Jerk.stddev.Y),
                     Avg.Frequency.Body.Acceleration.Jerk.stddev.Z = mean(Frequency.Body.Acceleration.Jerk.stddev.Z),
                     Avg.Frequency.Body.Gyro.mean.X = mean(Frequency.Body.Gyro.mean.X),
                     Avg.Frequency.Body.Gyro.mean.Y = mean(Frequency.Body.Gyro.mean.Y),
                     Avg.Frequency.Body.Gyro.mean.Z = mean(Frequency.Body.Gyro.mean.Z),
                     Avg.Frequency.Body.Gyro.stddev.X = mean(Frequency.Body.Gyro.stddev.X),
                     Avg.Frequency.Body.Gyro.stddev.Y = mean(Frequency.Body.Gyro.stddev.Y),
                     Avg.Frequency.Body.Gyro.stddev.Z = mean(Frequency.Body.Gyro.stddev.Z),
                     Avg.Frequency.Body.Acceleration.Magnitude.mean = mean(Frequency.Body.Acceleration.Magnitude.mean),
                     Avg.Frequency.Body.Acceleration.Magnitude.stddev = mean(Frequency.Body.Acceleration.Magnitude.stddev),
                     Avg.Frequency.Body.Acceleration.Jerk.Magnitude.mean = mean(Frequency.Body.Acceleration.Jerk.Magnitude.mean),
                     Avg.Frequency.Body.Acceleration.Jerk.Magnitude.stddev = mean(Frequency.Body.Acceleration.Jerk.Magnitude.stddev),
                     Avg.Frequency.Body.Gyro.Magnitude.mean = mean(Frequency.Body.Gyro.Magnitude.mean),
                     Avg.Frequency.Body.Gyro.Magnitude.stddev = mean(Frequency.Body.Gyro.Magnitude.stddev),
                     Avg.Frequency.Body.Gyro.Jerk.Magnitude.mean = mean(Frequency.Body.Gyro.Jerk.Magnitude.mean),
                     Avg.Frequency.Body.Gyro.Jerk.Magnitude.stddev = mean(Frequency.Body.Gyro.Jerk.Magnitude.stddev))
          
  ## write the tidy data set
  write.table(finaldata, "tidyAveragesDataset.txt", row.name=FALSE)  
