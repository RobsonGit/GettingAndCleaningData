# Set your working directory with setwd() function – if you want.
# The following commands download the file and unzip the data.
url = "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./getdata-projectfiles-UCI HAR Dataset.zip", mode = "wb")
unzip("getdata-projectfiles-UCI HAR Dataset.zip", exdir=".", overwrite=TRUE)

# readFile function reads the txt file pointed by user
readFile <- function(filename_to_read, filename_to_save) {
               file_name_to_save = read.table(filename_to_read)
               assign(filename_to_save, file_name_to_save, envir=.GlobalEnv)
}

# all txt files needed in the project are read
readFile("./UCI HAR Dataset/activity_labels.txt", "activity_labels")
readFile("./UCI HAR Dataset/features.txt", "features")

readFile("./UCI HAR Dataset/test/subject_test.txt", "subject_test")
readFile("./UCI HAR Dataset/test/X_test.txt", "x_test")
readFile("./UCI HAR Dataset/test/y_test.txt", "y_test")

readFile("./UCI HAR Dataset/train/subject_train.txt", "subject_train")
readFile("./UCI HAR Dataset/train/X_train.txt", "x_train")
readFile("./UCI HAR Dataset/train/y_train.txt", "y_train")


# Step 1. Merge the training and the test sets to create one data set.
# The merge function changes the order of observations thus at first I combine the data for test and train subsets.

test = cbind(subject_test, y_test, x_test)
train = cbind(subject_train, y_train, x_train)

# Now test and train datasets are combined, one below the other.
combined = rbind(test, train)

# The names of variables are extracted from features file. Two new variables names, i.e. subjectNumber and activityNumber
# are added to match the combined file structure.
features = as.character(features[,2])
features = c("subjectNumber", "activityNumber", features)
colnames(combined) = features

# Step 2. Extract only the measurements on the mean and standard deviation for each measurement.
# Based on information from features_info.txt we can find among variables such that have word "Mean" in their name.
# These variables are left up to user if they want to include them in tidy dataset or not.
# I chose to exclude them as these variables seem not to be the real means/averages.
# You can add "ignore.case=TRUE" as a 3rd argument in grep command below if you want to include those variables
# (7 variables more will be included in final dataset).
# Also there are variables with the weighted mean which have "meanFreq" in their names.
# Seems that those should stay in the dataset. If someone wants to remove then first argument in grep command should
# be modified to "mean[^F]|std"

vars_list = grep("mean|std", colnames(combined))
combined2 = combined[ , c(1, 2, vars_list)]
# combined2 file has 10299 observations and 81 variables.

# Step 3. Use descriptive activity names to name the activities in the data set.
# The activityNumber variable gets translated to activityLabel using dictionary from activity_labels file
# activityNumber variable is removed as it is no longer required.
# I also sort the data by subjectNumber and activityLabel.

colnames(activity_labels) = c("activityNumber","activityLabel")
combined3 = merge(combined2, activity_labels, by.x="activityNumber", by.y="activityNumber", all=TRUE)
combined3$activityNumber = NULL
library(plyr)
combined3 = arrange(combined3, subjectNumber, activityLabel)

# Step 4. Appropriately label the data set with descriptive variable names.
# I follow the rules described in point 4 of Readme.md file when changing the names, i.e.:
# - keep the names as short as possible to save the time when working with data
# - enhance "Acc" to "Accel" so that it brings to the mind "Acceleration"
# - all "mean"/"std" measures are moved to the end of variable's name
# - remove "t" from beginning of the variable's names while keeping "f" for variables transformed with FFT
# (don't need to differentiate "(t)ime" variables from "(f)ft" variables by keeping this "t" in vars names)
# - get rid of all "-" and brackets from labels as these are not valid variable names in R
# - get rid of duplicated words like "BodyBody"

names = as.character(colnames(combined3))
names = sub("tBody","Body",names)
names = sub("tGravity","Gravity",names)
names = sub("BodyAcc","BodyAccel",names)
names = sub("GravityAcc","GravityAccel",names)
names = gsub("[-()]","",names)
names = ifelse(grepl("mean",names),paste0(sub("mean","",names),"_mean"),names)
names = ifelse(grepl("std",names),paste0(sub("std","",names),"_sd"),names)
names = sub("BodyBody","Body",names)
colnames(combined3) = names

# Step 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
# After calculating the averages I need to change the labels of 2 grouping variables subjectNumber and activityLabel 
# (they are called Group.1 and Group.2 after calculation of the mean) while original 2 variables need to be deleted.
# warnings during running aggregate function are related to calculation of mean on non-numeric variables like activityLabel.
# I prefer to see first all activities for each subject so I re-order the tidy dataset a little.

tidy = aggregate(combined3, by=list(combined3$subjectNumber, combined3$activityLabel), mean)
tidy$subjectNumber = NULL
tidy$activityLabel = NULL
names(tidy)[1] = "subjectNumber"
names(tidy)[2] = "activityLabel"
tidy = arrange(tidy, subjectNumber, activityLabel)
write.table(tidy, "tidy.txt",row.names=FALSE)
