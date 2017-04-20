## This script merges data from a number of .txt files and produces a tidy data set which may be used for further analysis.

## check for required packages
if (!("reshape2" %in% rownames(installed.packages())) ) {
	print("Installing package \"reshape2\" before proceeding")
	install.packages("reshape2")
}
if (!("data.table" %in% rownames(installed.packages())) ) {
	print("Installing package \"data.table\" before proceeding")
	install.packages("data.table")
}

## Open required libraries
library(data.table)
library(reshape2)

## First, read all required .txt files and label the datasets

## Read all activities and their names and label the aproppriate columns 
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",col.names=c("activity_id","activity_name"))

## Read the dataframe's column names
features <- read.table("./UCI HAR Dataset/features.txt")
feature_names <-  features[,2]

## Read the test data and label the dataframe's columns
testdata <- read.table("./UCI HAR Dataset/test/X_test.txt")
colnames(testdata) <- feature_names

## Read the training data and label the dataframe's columns
traindata <- read.table("./UCI HAR Dataset/train/X_train.txt")
colnames(traindata) <- feature_names

## Read the ids of the test subjects and label the the dataframe's columns
test_subject_id <- read.table("./UCI HAR Dataset/test/subject_test.txt")
colnames(test_subject_id) <- "subject_id"

## Read the activity id's of the test data and label the the dataframe's columns
test_activity_id <- read.table("./UCI HAR Dataset/test/y_test.txt")
colnames(test_activity_id) <- "activity_id"

## Read the ids of the test subjects and label the the dataframe's columns
train_subject_id <- read.table("./UCI HAR Dataset/train/subject_train.txt")
colnames(train_subject_id) <- "subject_id"

## Read the activity id's of the training data and label the dataframe's columns
train_activity_id <- read.table("./UCI HAR Dataset/train/y_train.txt")
colnames(train_activity_id) <- "activity_id"

## Combine the test subject id's, the test activity id's and the test data into one dataframe
test_data <- cbind(test_subject_id , test_activity_id , testdata)

## Combine the test subject id's, the test activity id's and the test data into one dataframe
train_data <- cbind(train_subject_id , train_activity_id , traindata)

## Combine the test data and the train data into one dataframe
all_data <- rbind(train_data,test_data)

## Keep only columns refering to mean() or std() values
mean_col_idx <- grep("mean",names(all_data),ignore.case=TRUE)
mean_col_names <- names(all_data)[mean_col_idx]
std_col_idx <- grep("std",names(all_data),ignore.case=TRUE)
std_col_names <- names(all_data)[std_col_idx]
meanstddata <-all_data[,c("subject_id","activity_id",mean_col_names,std_col_names)]

## Merge the activities dataset with the mean/std values dataset to get one dataset with descriptive activity names
descrnames <- merge(activity_labels,meanstddata,by.x="activity_id",by.y="activity_id",all=TRUE)

## Melt the dataset with the descriptive activity names for better handling
data_melt <- melt(descrnames,id=c("activity_id","activity_name","subject_id"))

## Cast the melted dataset according to the average of each variable for each activity and each subjec
mean_data <- dcast(data_melt,activity_id + activity_name + subject_id ~ variable,mean)

# Cleaning up the variable names to set appropriate label with descriptive activity names
colNames  = colnames(mean_data); 
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
colnames(mean_data) = colNames;

## Create a file with the new tidy dataset
write.table(mean_data,"./tidy_data.txt")
