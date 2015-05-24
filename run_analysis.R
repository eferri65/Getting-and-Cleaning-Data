##ASSUMPTION-MUST: you are in UCI HAR Dataset directory generated after unzipping the data provided for the course project
##Example: setwd("/Users/rodele/Desktop/UCI HAR Dataset")

## inclduding the "plyr" package used later with "ddply" function
library("plyr")

##saving the working directory as the root
root<-getwd()
setwd(root)

##----------------------1
##Merge the training and the test sets to create one data set.
##Both training and test sets have the same 561 features columns, they only differ for 
##the subjects being measured: 9 for test and 21 for train.

##get the activity and features data common for the test and train data
activity_labels<-read.table("activity_labels.txt")
features<-read.table("features.txt", stringsAsFactors = FALSE ) 

##get the test data
setwd("test") 
X_test<-read.table("X_test.txt")
y_test<-read.table("y_test.txt")
y_test<-rename(y_test, c("V1"="Activity"))
subject_test = read.table("subject_test.txt")
subject_test<-rename(subject_test, c("V1"="Subject"))
#column clip subject+activity+test_data into one frame
test<-cbind(subject_test, y_test, X_test)

##get the train data
setwd(root)
setwd("train") 
X_train<-read.table("X_train.txt")
y_train<-read.table("y_train.txt")
y_train<-rename(y_train, c("V1"="Activity"))
subject_train<-read.table("subject_train.txt")
subject_train<-rename(subject_train, c("V1"="Subject"))
#column clip subject+activity+train_data into one frame
train<-cbind(subject_train, y_train, X_train)

##row clip test and train data into one frame: they have the SAME columns ("Subject", "Activity", 561 features)
tt<-rbind(train,test)

##----------------------2
##Extract only the measurements on the mean and standard deviation for each measurement. 
##At this point the columns from 3 to 563 are "V1", "V2", "V3", etc..; so let's rename them using the feautures frame 
##(the first two columns are "Subject" and "Activity")
default.colnames<-colnames(tt)
for(i in 3:ncol(tt) ) {  
        colnames(tt)[colnames(tt)==default.colnames[i]] <- features[i-2, "V2"]
}
##Extract the columns which contain "std" OR "mean" OR "Subject" Or "Activity" (this call is case sensitive, i.e. it will 
##not select "Mean" for example)
tt.subset<-tt[ , grepl("Subject|Activity|std|mean", colnames(tt))]

##----------------------3
##Use descriptive activity names to name the activities in the data set
tt.subset$Activity<-activity_labels[,2][match(tt$Activity, activity_labels[,1])]

##----------------------4
##Appropriately labels the data set with descriptive variable names. 
##Done already is step 2; for more details look at the Codebook 

##----------------------5
##From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.
groupColumns<-c("Activity","Subject")
tt_subset_summary = ddply(tt.subset, groupColumns, colwise(mean))
##Prefix the columns with "SummaryMean_" to indicate the "mean"computation
n<-ncol(tt_subset_summary)
for(i in 3:n ) {  
        colnames(tt_subset_summary)[i]<- paste("SummaryMean_", colnames(tt_subset_summary)[i])
}

##----------------------6
##Last step: per requested, write "tt_subset_summary" set as a txt file created with "write.table()" using row.name=FALSE 
setwd(root)
write.table(tt_subset_summary, "final_dataset", row.names = FALSE)