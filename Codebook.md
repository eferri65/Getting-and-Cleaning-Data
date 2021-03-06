Codebook
=================

This is the Codebook for the final dataset contained in `final_dataset.txt`,  generated by the `run_analysis.R` script.

This Codebook is divided into the following sections:

1. Column descriptions for the final dataset
2. Column filters applied to initial downloaded data 
3. Is the final dataset tidy?
4. Raw Data collection and variables estimations  

###2. Column descriptions for the final dataset
The final dataset has 72 columns:

* 1 Column = called `Activity`
* 1 Column =  called `Subject`
* 70 Columns = Summary Means for 70 variables, called `SummaryMean_ variable name`

`Activity`: describes the activity perfomed during the experiment; it is descriptive and in R 
is implemented as Factor w/ 6 levels: 
1. WALKING
2. WALKING_UPSTAIR
3. WALKING_DOWNSTAIRS
4. SITTING
5. STANDING
6. LAYING

`Subject`: identifies the individual (aka Subject) who is performing the Activity; it is a numeric id which
ranges from 1 to 30; in R is implemented as an int =  1 2 3 4 5 6 7 8 9 10 ...30
As expected, in the final set there are 180 rows, because there are 6 activities and 30 subjects, so 6x30 = 180.
    
`SummaryMean_ variable name`: All the remaining 70 columns are computed means; they all have the prefix `SummaryMean_` to indicate that they ares summary means of the variables whose names are captured in the character string that follows the prefix. For Example:
    
`SummaryMean_ tBodyAcc-mean()-X` is the calculated summary mean of `tBodyAcc-mean()-X` by Activity by Subject
    
Find below the definitions of the variable names for which the SummaryMean_ are computed; they all are num (numeric):   

 1. tBodyAcc-mean()-X:  mean of time body accelation signals on X axis              
 2. tBodyAcc-mean()-Y:  mean of time body accelation signals on Y axis               
 3. tBodyAcc-mean()-Z:  mean of time body accelation signals on Z axis
 4. tBodyAcc-std()-X: standard deviation of time body accelation signals on X axis
 5. tBodyAcc-std()-Y: standard deviation of time body accelation signals on Y axis
 6. tBodyAcc-std()-Z: standard deviation of time body accelation signals on Z axis
 7. tGravityAcc-mean()-X: mean of time gravity accelation signals on X axis
 8. tGravityAcc-mean()-Y: mean of time gravity accelation signals on Y axis
 9. tGravityAcc-mean()-Z: mean of time gravity accelation signals on Z axis
 10.  tGravityAcc-std()-X: standard deviation of time gravity accelation signals on X axis
 11.  tGravityAcc-std()-Y: standard deviation of time gravity accelation signals on Y axis
 12.  tGravityAcc-std()-Z: standard deviation of time gravity accelation signals on Z axis
 13.  tBodyAccJerk-mean()-X: mean of derived jerk signals for time body accelation signals on X axis        
 14.  tBodyAccJerk-mean()-Y: mean of derived jerk signals for time body accelation signals on Y axis   
 15.  tBodyAccJerk-mean()-Z: mean of derived jerk signals for time body accelation signals on Z axis         
 16.  tBodyAccJerk-std()-X: standard deviation of derived jerk signals for time body acceleration signals on X axis
 17.  tBodyAccJerk-std()-Y: standard deviation of derived jerk signals for time body acceleration signals on Y axis
 18.  tBodyAccJerk-std()-Z: standard deviation of derived jerk signals for time body acceleration signals on Z axis
 19.  tBodyGyro-mean()-X: mean of time body gyroscope signals on X axis              
 20.  tBodyGyro-mean()-Y: mean of time body gyroscope signals on Y axis               
 21.  tBodyGyro-mean()-Z: mean of time body gyroscope signals on Z axis                
 22.  tBodyGyro-std()-X: standard deviation of time body gyroscope signals on X axis              
 23.  tBodyGyro-std()-Y: standard deviation of time body gyroscope signals on Y axis            
 24.  tBodyGyro-std()-Z: standard deviation of time body gyroscope signals on Z axis            
 25.  tBodyGyroJerk-mean()-X: mean of derived jerk signals for time body gyroscope signals on X axis        
 26.  tBodyGyroJerk-mean()-Y: mean of derived jerk signals for time body gyroscope signals on Y axis        
 27.  tBodyGyroJerk-mean()-Z: mean of derived jerk signals for time body gyroscope signals on Z axis
 28.  tBodyGyroJerk-std()-X: standard deviation of derived jerk signals for time body gyroscope signals on X axis
 29.  tBodyGyroJerk-std()-Y: standard deviation of derived jerk signals for time body gyroscope signals on Y axis
 30.  tBodyGyroJerk-std()-Z: standard deviation of derived jerk signals for time body gyroscope signals on Z axis
 31.  tBodyAccMag-mean(): mean of computed magnitude (Euclidean norm) of time body acceleration signals         
 32.  tBodyAccMag-std(): standard deviation of computed magnitude (Euclidean norm) of time body acceleration signal
 33.  tGravityAccMag-mean(): mean of computed magnitude (Euclidean norm) of time gravity acceleration signals 
 34.  tGravityAccMag-std():standard deviation of computed magnitude (Euclidean norm) of time gravity acceleration signals
 35.  tBodyAccJerkMag-mean(): mean of computed magnitude (Euclidean norm) of jerk signals for time body acceleration signals
 36.  tBodyAccJerkMag-std(): standard deviation of computed magnitude (Euclidean norm) of jerk signals for time body acceleration signal
 37.  tBodyGyroMag-mean(): mean of computed magnitude (Euclidean norm) of time body gyroscope signals             
 38.  tBodyGyroMag-std():  standard devitaion of computed magnitude (Euclidean norm) of time body gyroscope signals 
 39.  tBodyGyroJerkMag-mean(): mean of computed magnitude (Euclidean norm) of jerk signals for of time body gyroscope signals          
 40.  tBodyGyroJerkMag-std(): standard deviation of computed magnitude (Euclidean norm) of jerk signals for time body gyroscope signals 
 41.  fBodyAcc-mean()-X: mean of computed (FFT) frequency domain signals for  body acceleration signals on X axis 
 42.  fBodyAcc-mean()-Y: mean of computed (FFT) frequency domain signals for  body acceleration signals on Y axis 
 43.  fBodyAcc-mean()-Z: mean of computed (FFT) frequency domain signals for  body acceleration signals on Z axis 
 44.  fBodyAcc-std()-X:  standard deviation of computed (FFT) frequency domain signals for body acceleration signals on X axis 
 45.  fBodyAcc-std()-Y:  standard deviation of computed (FFT) frequency domain signals for body acceleration signals on Y axis 
 46.  fBodyAcc-std()-Z:  standard deviation of computed (FFT) frequency domain signals for body acceleration signals on Z axis 
 47.  fBodyAcc-meanFreq()-X: weighted average of computed (FFT) frequency domain signals for body acceleration signals on X axis           
 48.  fBodyAcc-meanFreq()-Y: weighted average of computed (FFT) frequency domain signals for body acceleration signals on Y axis           
 49.  fBodyAcc-meanFreq()-Z: weighted average of computed (FFT) frequency domain signals for body acceleration signals on Z axis           
 50.  fBodyAccJerk-mean()-X:  mean of computed (FFT) frequency domain signals of derived jerk signals for body acceleration signals on X axis           
 51.  fBodyAccJerk-mean()-Y:  mean of computed (FFT) frequency domain signals of derived jerk signals for body acceleration signals on Y axis           
 52.  fBodyAccJerk-mean()-Z:  mean of computed (FFT) frequency domain signals of derived jerk signals for body acceleration signals on Z axis           
 53.  fBodyAccJerk-std()-X: standard deviation of computed (FFT) frequency domain signals of derived jerk signals for body acceleration signals on X axis                      
 54.  fBodyAccJerk-std()-Y: standard deviation of computed (FFT) frequency domain signals of derived jerk signals for body acceleration signals on Y axis                                 
 55.  fBodyAccJerk-std()-Z: standard deviation of computed (FFT) frequency domain signals of derived jerk signals for body acceleration signals on Z axis                          
 56.  fBodyAccJerk-meanFreq()-X: weighted average of computed (FFT) frequency domain signals of derived jerk signals for body acceleration signals on X axis      
 57.  fBodyAccJerk-meanFreq()-Y:  weighted average of computed (FFT) frequency domain signals of derived jerk signalsfor body acceleration signals on Y axis     
 58.  fBodyAccJerk-meanFreq()-Z: weighted average  of computed (FFT) frequency domain signals of derived jerk signalsfor  body acceleration signals on Z axis       
 59.  fBodyGyro-mean()-X:  mean of computed (FFT) frequency domain signals for body gyroscope signals on X axis
 60.  fBodyGyro-mean()-Y:  mean of computed (FFT) frequency domain signals for body gyroscope signals on Y axis
 61.  fBodyGyro-mean()-Z:  mean of computed (FFT) frequency domain signals for body gyroscope  signals on Z axis  
 62. fBodyGyro-std()-X: standard deviation of computed (FFT) frequency domain signals for body gyroscope signals on X axis
 63. fBodyGyro-std()-Y: standard deviation  of computed (FFT) frequency domain signals for body gyroscope signals on Y axis               
 64. fBodyGyro-std()-Z: standard deviation  of computed (FFT) frequency domain signals for body gyroscope signals on Z axis
 65. fBodyGyro-meanFreq()-X: weighted average of computed (FFT) frequency domain signals for body gyroscope signals on X axis
 66. fBodyGyro-meanFreq()-Y: weighted average of computed (FFT) frequency domain signals for time body gyroscope signals on Y axis
 67. fBodyGyro-meanFreq()-Z: weighted average of computed (FFT) frequency domain signals for body gyroscope signals on Z axis                       
 68. fBodyAccMag-mean(): mean of of computed (FFT) frequency domain signals for body acceleration signal magnitude  
 69. fBodyAccMag-std(): standard deviation of computed (FFT) frequency domain signals for body acceleration signal magnitude  
 70. fBodyAccMag-meanFreq(): weighted average of computed (FFT) frequency domain signals for body acceleration signal magnitude   
 

###2. Column filters applied to initial downloaded data 
The final dataset created by the R script named `run_analysis.R`, is intended to be an independent tidy data set with the `average of each variable for each activity and each subject`.

The project requested only the measurements on the mean and standard deviation. So the script extracts only the columns containing `Subject|Activity|std|mean`. 
The function used in the script is case sensitive and excludes the occurrenes of upper case `Mean`, which is used in the `angle` columns which I do not want anyways because the mean is a parameter. As a result, the following columns are excluded:

* angle(tBodyAccMean,gravity)        
* angle(tBodyAccJerkMean),gravityMean) 
* angle(tBodyGyroMean,gravityMean)     
* angle(tBodyGyroJerkMean,gravityMean) 
* angle(X,gravityMean)                 
* angle(Y,gravityMean)                 
* angle(Z,gravityMean) 

I also decided to exclude the following 9 columns, as they contain "Body" twice and I do not think they make sense:

 * fBodyBodyAccJerkMag-mean()
 * fBodyBodyAccJerkMag-std()        
 * fBodyBodyAccJerkMag-meanFreq()  
 * fBodyBodyGyroMag-mean()        
 * fBodyBodyGyroMag-std()           
 * fBodyBodyGyroMag-meanFreq() 
 * fBodyBodyGyroJerkMag-mean()    
 * fBodyBodyGyroJerkMag-std()      
 * fBodyBodyGyroJerkMag-meanFreq() 

After these filters the number of features which were initially 561 decreased to 70.

###3. Is the final dataset tidy?
As David Hood points out in one of the discussion forums the `"tidy for"` principle is hard to apply here because we don't have a specific problem to apply it to.

However, the final daset satisfies the checklist suggested by David in the 
Tidy data thread at [https://class.coursera.org/getdata-014/forum/thread?thread_id=31](https://class.coursera.org/getdata-014/forum/thread?thread_id=31):

1. The final dataset has headings so that I know which columns are which; the string headings are compact and I like the way they are built because you can actuallly infer what they mean with the abbreviations used; the recipe is given in section 4, sourced from feauture_info.txt
2. The variables are in different columns  
3. There are no duplicate columns (I programmatically checked with `dup<-duplicated(tt_subset_summary[,1:72])` )
4. All the rows contain different observations

Some examples of why the variables have a built-in recepi:

* all the variable names have a `t` or an `f` the beginning, distinguishing bewteen the time or frequency domain
* the feature name follows immediately the prefix `t` or `f`
* the measurement type (example: mean() or std()) always follows the variable name
* when applicable, the variable names terminates with X or Y or Z to specify the axial direction of the signal

Now, I do not know if the data is correct, i.e. if the values are meaningful.
I checked there are no N/A's with `sapply(tt_subset_summary , function(x)all(is.na(x)))`

###4. Raw Data collection and variables estimations   

Source: `feautures_info.ext`

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals `tAcc-XYZ` and `tGyro-XYZ`. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (`tBodyAcc-XYZ` and `tGravityAcc-XYZ`) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (`tBodyAccJerk-XYZ` and `tBodyGyroJerk-XYZ`). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (`tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag`). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing `fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag`. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions:

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* mad(): Median absolute deviation 
* max(): Largest value in array
* min(): Smallest value in array
* sma(): Signal magnitude area
* energy(): Energy measure. Sum of the squares divided by the number of values. 
* iqr(): Interquartile range 
* entropy(): Signal entropy
* arCoeff(): Autorregresion coefficients with Burg order equal to 4
* correlation(): correlation coefficient between two signals
* maxInds(): index of the frequency component with largest magnitude
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency
* skewness(): skewness of the frequency domain signal 
* kurtosis(): kurtosis of the frequency domain signal 
* bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
* angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean

The complete list of 561 variables of each feature vector is in 'features.txt'.

I observed there are duplicate column names although the values are different. I am not including the analysis because the assignment is asking to focus on other columns.
