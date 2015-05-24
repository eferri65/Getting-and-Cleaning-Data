##ASSUMPTION: you are in UCI HAR Dataset directory
##Example: setwd("/Users/rodele/Desktop/UCI HAR Dataset")

library("plyr")

root<-getwd()
setwd(root)

##----------------------1
##Merges the training and the test sets to create one data set.
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
#column clip subject+activity+test_data 
test<-cbind(subject_test, y_test, X_test)

##get the train data
setwd(root)
setwd("train") 
X_train<-read.table("X_train.txt")
y_train<-read.table("y_train.txt")
y_train<-rename(y_train, c("V1"="Activity"))
subject_train<-read.table("subject_train.txt")
subject_train<-rename(subject_train, c("V1"="Subject"))
#column clip subject+activity+train_data 
train<-cbind(subject_train, y_train, X_train)

##row clip test and train data: they have the SAME columns (subect, activity, 561 features)
tt<-rbind(train,test)

##----------------------2
##Extracts only the measurements on the mean and standard deviation for each measurement. 
##First name the columns using the feautures frame
default.colnames<-colnames(tt)
for(i in 3:ncol(tt) ) {  
        colnames(tt)[colnames(tt)==default.colnames[i]] <- features[i-2, "V2"]
}
##Case sensitive
tt.subset<-tt[ , grepl("Subject|Activity|std|mean", colnames(tt))]

##----------------------3
##Uses descriptive activity names to name the activities in the data set
tt.subset$Activity<-activity_labels[,2][match(tt$Activity, activity_labels[,1])]

##----------------------4
##Appropriately labels the data set with descriptive variable names. 
##Done is step 2 already; for more details look at the Codebook 

##----------------------5
##From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.
groupColumns<-c("Activity","Subject")
tt_subset_summary = ddply(tt.subset, groupColumns, colwise(mean))

##----------------------6
##Upload tt_subset_summary set as a txt file created with write.table() using row.name=FALSE 
setwd(root)
write.table(tt_subset_summary, "final_dataset", row.names = FALSE)