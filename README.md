Readme
===========

This is the Readme markdown file for the GitHub repo called Getting-and-Cleaning-Data.
This Readme is divided in the following sections:

1. Content of `Getting-and-Cleaning-Data repo`
2. Instructions on how to run the script named `run_analysis.R`

###1. Content of `Getting-and-Cleaning-Data repo`

As required by the assignment, the `Getting-and-Cleaning-Data repo` contains the following files:

1. This `Readme` markdown file: it gives you a roadmap of the repo and describes how to run the R script included in the repo  
2. An R script named `run_analysis.R`: the script used to generate the final tidy data set required by the project
3. A markdown file called `Codebook`: it describes the tidy data set, why it is tidy and the definition of the columns in the final dataset
 
###2. Instructions on how to run the script named `run_analysis.R`

Assumptions - A MUST: 

1. Use the SAME file structure as the `UCI HAR Dataset` dowloaded for the assignment
2. Set your working directory at the top level of the `UCI HAR Dataset` like so: 
   setwd("YOUR_PATH_HERE/UCI HAR Dataset")
 
To run the R script:

<!-- -->
    1. step 1: look at the script; it has `comments` for each step required by the assignment. Summary:
    
    * does not use the Inertial Signal data
    * gets the activity and features data common for the test and train data
    * gets the test data
    * gets the train data
    * clips test and train data into one frame: they have the SAME columns ("Subject", "Activity", 561 
    features variables)
    * extracts only the measurements on the mean and standard deviation for each measurement
    * uses descriptive activity names to name the activities in the data set
    * filters out columns containing "BodyBody" because they do not make sense 
    * creates a second, independent tidy data set with the average of each variable for each activity and each 
    subject.
    
    2. step 2: load the script in RStudio, select all the lines and run it OR 
    source(run_analysis.R)
    
    3. step 3: check the output file called `final_dataset.txt` located in the UCI HAR Dataset             
    folder/directory (also loaded in the project submission via Coursera); to read it do: 
    `final<-read.table("final_dataset", header=TRUE)`

Note: I have a Mac and when the `run_analysis.R`script saves the file `final_dataset.txt` using `write.table(tt_subset_summary, "final_dataset", row.names = FALSE)` as requested by the the project instructions, the kind of file is not `txt` but `TextEd...ument`.

Here is the result of `str(tt_subset_summary)`, where `tt_subset_summary` is the dataframe written into `final_dataset.txt`; it is basically a summary table with 180 rows and 72 variables:

<!-- -->
    str(tt_subset_summary)
    'data.frame':	180 obs. of  72 variables:
    $ Activity                                    : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
    $ Subject                                     : int  1 2 3 4 5 6 7 8 9 10 ...
    $ SummaryMean_ tBodyAcc-mean()-X              : num  0.222 0.281 0.276 0.264 0.278 ...
    $ SummaryMean_ tBodyAcc-mean()-Y              : num  -0.0405 -0.0182 -0.019 -0.015 -0.0183 ...
    $ SummaryMean_ tBodyAcc-mean()-Z              : num  -0.113 -0.107 -0.101 -0.111 -0.108 ...
    $ SummaryMean_ tBodyAcc-std()-X               : num  -0.928 -0.974 -0.983 -0.954 -0.966 ...
    $ SummaryMean_ tBodyAcc-std()-Y               : num  -0.837 -0.98 -0.962 -0.942 -0.969 ...
    $ SummaryMean_ tBodyAcc-std()-Z               : num  -0.826 -0.984 -0.964 -0.963 -0.969 ...
    $ SummaryMean_ tGravityAcc-mean()-X           : num  -0.249 -0.51 -0.242 -0.421 -0.483 ...
    $ SummaryMean_ tGravityAcc-mean()-Y           : num  0.706 0.753 0.837 0.915 0.955 ...
    $ SummaryMean_ tGravityAcc-mean()-Z           : num  0.446 0.647 0.489 0.342 0.264 ...
    $ SummaryMean_ tGravityAcc-std()-X            : num  -0.897 -0.959 -0.983 -0.921 -0.946 ...
    $ SummaryMean_ tGravityAcc-std()-Y            : num  -0.908 -0.988 -0.981 -0.97 -0.986 ...
    $ SummaryMean_ tGravityAcc-std()-Z            : num  -0.852 -0.984 -0.965 -0.976 -0.977 ...
    $ SummaryMean_ tBodyAccJerk-mean()-X          : num  0.0811 0.0826 0.077 0.0934 0.0848 ...
    $ SummaryMean_ tBodyAccJerk-mean()-Y          : num  0.00384 0.01225 0.0138 0.00693 0.00747 ...
    $ SummaryMean_ tBodyAccJerk-mean()-Z          : num  0.01083 -0.0018 -0.00436 -0.00641 -0.00304 ...
    $ SummaryMean_ tBodyAccJerk-std()-X           : num  -0.958 -0.986 -0.981 -0.978 -0.983 ...
    $ SummaryMean_ tBodyAccJerk-std()-Y           : num  -0.924 -0.983 -0.969 -0.942 -0.965 ...
    $ SummaryMean_ tBodyAccJerk-std()-Z           : num  -0.955 -0.988 -0.982 -0.979 -0.985 ...
    $ SummaryMean_ tBodyGyro-mean()-X             : num  -0.01655 -0.01848 -0.02082 -0.00923 -0.02189 ...
    $ SummaryMean_ tBodyGyro-mean()-Y             : num  -0.0645 -0.1118 -0.0719 -0.093 -0.0799 ...
    $ SummaryMean_ tBodyGyro-mean()-Z             : num  0.149 0.145 0.138 0.17 0.16 ...
    $ SummaryMean_ tBodyGyro-std()-X              : num  -0.874 -0.988 -0.975 -0.973 -0.979 ...
    $ SummaryMean_ tBodyGyro-std()-Y              : num  -0.951 -0.982 -0.977 -0.961 -0.977 ...
    $ SummaryMean_ tBodyGyro-std()-Z              : num  -0.908 -0.96 -0.964 -0.962 -0.961 ...
    $ SummaryMean_ tBodyGyroJerk-mean()-X         : num  -0.107 -0.102 -0.1 -0.105 -0.102 ...
    $ SummaryMean_ tBodyGyroJerk-mean()-Y         : num  -0.0415 -0.0359 -0.039 -0.0381 -0.0404 ...
    $ SummaryMean_ tBodyGyroJerk-mean()-Z         : num  -0.0741 -0.0702 -0.0687 -0.0712 -0.0708 ...
    $ SummaryMean_ tBodyGyroJerk-std()-X          : num  -0.919 -0.993 -0.98 -0.975 -0.983 ...
    $ SummaryMean_ tBodyGyroJerk-std()-Y          : num  -0.968 -0.99 -0.987 -0.987 -0.984 ...
    $ SummaryMean_ tBodyGyroJerk-std()-Z          : num  -0.958 -0.988 -0.983 -0.984 -0.99 ...
    $ SummaryMean_ tBodyAccMag-mean()             : num  -0.842 -0.977 -0.973 -0.955 -0.967 ...
    $ SummaryMean_ tBodyAccMag-std()              : num  -0.795 -0.973 -0.964 -0.931 -0.959 ...
    $ SummaryMean_ tGravityAccMag-mean()          : num  -0.842 -0.977 -0.973 -0.955 -0.967 ...
    $ SummaryMean_ tGravityAccMag-std()           : num  -0.795 -0.973 -0.964 -0.931 -0.959 ...
    $ SummaryMean_ tBodyAccJerkMag-mean()         : num  -0.954 -0.988 -0.979 -0.97 -0.98 ...
    $ SummaryMean_ tBodyAccJerkMag-std()          : num  -0.928 -0.986 -0.976 -0.961 -0.977 ...
    $ SummaryMean_ tBodyGyroMag-mean()            : num  -0.875 -0.95 -0.952 -0.93 -0.947 ...
    $ SummaryMean_ tBodyGyroMag-std()             : num  -0.819 -0.961 -0.954 -0.947 -0.958 ...
    $ SummaryMean_ tBodyGyroJerkMag-mean()        : num  -0.963 -0.992 -0.987 -0.985 -0.986 ...
    $ SummaryMean_ tBodyGyroJerkMag-std()         : num  -0.936 -0.99 -0.983 -0.983 -0.984 ...
    $ SummaryMean_ fBodyAcc-mean()-X              : num  -0.939 -0.977 -0.981 -0.959 -0.969 ...
    $ SummaryMean_ fBodyAcc-mean()-Y              : num  -0.867 -0.98 -0.961 -0.939 -0.965 ...
    $ SummaryMean_ fBodyAcc-mean()-Z              : num  -0.883 -0.984 -0.968 -0.968 -0.977 ...
    $ SummaryMean_ fBodyAcc-std()-X               : num  -0.924 -0.973 -0.984 -0.952 -0.965 ...
    $ SummaryMean_ fBodyAcc-std()-Y               : num  -0.834 -0.981 -0.964 -0.946 -0.973 ...
    $ SummaryMean_ fBodyAcc-std()-Z               : num  -0.813 -0.985 -0.963 -0.962 -0.966 ...
    $ SummaryMean_ fBodyAcc-meanFreq()-X          : num  -0.159 -0.146 -0.074 -0.274 -0.136 ...
    $ SummaryMean_ fBodyAcc-meanFreq()-Y          : num  0.0975 0.2573 0.2385 0.3662 0.4665 ...
    $ SummaryMean_ fBodyAcc-meanFreq()-Z          : num  0.0894 0.4025 0.217 0.2013 0.1323 ...
    $ SummaryMean_ fBodyAccJerk-mean()-X          : num  -0.957 -0.986 -0.981 -0.979 -0.983 ...
    $ SummaryMean_ fBodyAccJerk-mean()-Y          : num  -0.922 -0.983 -0.969 -0.944 -0.965 ...
    $ SummaryMean_ fBodyAccJerk-mean()-Z          : num  -0.948 -0.986 -0.979 -0.975 -0.983 ...
    $ SummaryMean_ fBodyAccJerk-std()-X           : num  -0.964 -0.987 -0.983 -0.98 -0.986 ...
    $ SummaryMean_ fBodyAccJerk-std()-Y           : num  -0.932 -0.985 -0.971 -0.944 -0.966 ...
    $ SummaryMean_ fBodyAccJerk-std()-Z           : num  -0.961 -0.989 -0.984 -0.98 -0.986 ...
    $ SummaryMean_ fBodyAccJerk-meanFreq()-X      : num  0.132 0.16 0.176 0.182 0.24 ...
    $ SummaryMean_ fBodyAccJerk-meanFreq()-Y      : num  0.0245 0.1212 -0.0132 0.0987 0.1957 ...
    $ SummaryMean_ fBodyAccJerk-meanFreq()-Z      : num  0.0244 0.1906 0.0448 0.077 0.0917 ...
    $ SummaryMean_ fBodyGyro-mean()-X             : num  -0.85 -0.986 -0.97 -0.967 -0.976 ...
    $ SummaryMean_ fBodyGyro-mean()-Y             : num  -0.952 -0.983 -0.978 -0.972 -0.978 ...
    $ SummaryMean_ fBodyGyro-mean()-Z             : num  -0.909 -0.963 -0.962 -0.961 -0.963 ...
    $ SummaryMean_ fBodyGyro-std()-X              : num  -0.882 -0.989 -0.976 -0.975 -0.981 ...
    $ SummaryMean_ fBodyGyro-std()-Y              : num  -0.951 -0.982 -0.977 -0.956 -0.977 ...
    $ SummaryMean_ fBodyGyro-std()-Z              : num  -0.917 -0.963 -0.967 -0.966 -0.963 ...
    $ SummaryMean_ fBodyGyro-meanFreq()-X         : num  -0.00355 0.10261 -0.08222 -0.06609 -0.02272 ...
    $ SummaryMean_ fBodyGyro-meanFreq()-Y         : num  -0.0915 0.0423 -0.0267 -0.5269 0.0681 ...
    $ SummaryMean_ fBodyGyro-meanFreq()-Z         : num  0.0105 0.0553 0.1477 0.1529 0.0414 ...
    $ SummaryMean_ fBodyAccMag-mean()             : num  -0.862 -0.975 -0.966 -0.939 -0.962 ...
    $ SummaryMean_ fBodyAccMag-std()              : num  -0.798 -0.975 -0.968 -0.937 -0.963 ...
    $ SummaryMean_ fBodyAccMag-meanFreq()         : num  0.0864 0.2663 0.237 0.2417 0.292 ...


Note 1: Per requested, I extracted the columns which represent the measurements on the `mean and standard deviation for each measurement`. I intentionally excluded  the `angle` columns where the `Mean` is a paramenter.

Note 2: I decided to filter out the following 9 columns containing "BobyBody", that is "Body" repeated twice as I point out in the Codebook, because I could not explain that scenario:

    $ SummaryMean_ fBodyBodyAccJerkMag-mean()     : num  -0.933 -0.985 -0.976 -0.962 -0.977 ...
    $ SummaryMean_ fBodyBodyAccJerkMag-std()      : num  -0.922 -0.985 -0.975 -0.958 -0.976 ...
    $ SummaryMean_ fBodyBodyAccJerkMag-meanFreq() : num  0.266 0.342 0.239 0.274 0.197 ...
    $ SummaryMean_ fBodyBodyGyroMag-mean()        : num  -0.862 -0.972 -0.965 -0.962 -0.968 ...
    $ SummaryMean_ fBodyBodyGyroMag-std()         : num  -0.824 -0.961 -0.955 -0.947 -0.959 ...
    $ SummaryMean_ fBodyBodyGyroMag-meanFreq()    : num  -0.1398 0.0186 -0.0229 -0.2599 0.1024 ...
    $ SummaryMean_ fBodyBodyGyroJerkMag-mean()    : num  -0.942 -0.99 -0.984 -0.984 -0.985 ...
    $ SummaryMean_ fBodyBodyGyroJerkMag-std()     : num  -0.933 -0.989 -0.983 -0.983 -0.983 ...
    $ SummaryMean_ fBodyBodyGyroJerkMag-meanFreq(): num  0.1765 0.2648 0.1107 0.2029 0.0247 ...
 



