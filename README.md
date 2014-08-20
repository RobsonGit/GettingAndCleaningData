### Description of run_analysis.R code

The code run_analysis.R prepares a tidy data from datasets provided
in "Getting and Cleaning Data" course.

The user can set his working directory with setwd() function before 
running the code. The code downloads the data and unzips it in the working
directory automatically. Then it reads the txt files with below function.

```r
# readFile function reads the txt file pointed by user
readFile <- function(filename_to_read, filename_to_save) {
               file_name_to_save = read.table(filename_to_read)
               assign(filename_to_save, file_name_to_save, envir=.GlobalEnv)
}

readFile("./UCI HAR Dataset/activity_labels.txt", "activity_labels")
readFile("./UCI HAR Dataset/features.txt", "features")

readFile("./UCI HAR Dataset/test/subject_test.txt", "subject_test")
readFile("./UCI HAR Dataset/test/X_test.txt", "x_test")
readFile("./UCI HAR Dataset/test/y_test.txt", "y_test")

readFile("./UCI HAR Dataset/train/subject_train.txt", "subject_train")
readFile("./UCI HAR Dataset/train/X_train.txt", "x_train")
readFile("./UCI HAR Dataset/train/y_train.txt", "y_train")
```

After reading all the data from txt files I analyzed the structure of 
files. Dimensions of files, checked with dim() command on each file:
* subject_test - 2947 observations and 1 variable
* x_test - 2947 observations and 561 variables
* y_test - 2947 observations and 1 variable (activity number)
* subject_train - 7352 observations and 1 variable
* x_train - 7352 observations and 561 variables
* y_train - 7352 observations and 1 variable (activity number)
* activity_labels - 6 observations and 2 variables. This is a dictionary
for activity numbers, for example: activity 1 = WALKING.
* features – has 561 observations and 2 variables. 2nd variable contains
the names of 561 variables from x_test and y_test files.

Some notes on files:

a) There is no merging key in x_test and y_test files so based on # of 
observations I concluded that those files must be combined with subject_test
file using cbind function (by row number). Similarly for x_train, y_train
and subject_train files.

b) Our goal is to create tidy file with the variables that include information
on the mean and standard deviation for each measurement. This means that the 
original measurement files from "Inertial Signals" folders will not be needed
as this is data before pre-processing and does not include information on means
and standard deviations.

c) From the number of observations and variables in each file it is visible 
that the structure of the new file will need to follow the scheme:


```r
#  variables names:    (subject name)    (activity label)   (features.txt)
#                  --------------------------------------------------------
#  observations:   | subject_test.txt   +   y_test.txt    +  X_test.txt   |
#  observations:   | subject_train.txt  +   y_train.txt   +  X_train.txt  |
```

d) Based of this file we are going to calculate means for each variable that
includes "mean" or "std" in its name (features.txt provides with information
regarding names of measurements).

e) The final tidy file will be on combined subject + activity level.

In order to create the tidy dataset I followed the steps requested in project:

1. Merge the training and the test sets to create one data set.

2. Extract only the measurements on the mean and standard deviation for each
measurement.

3. Use descriptive activity names to name the activities in the data set.

4. Appropriately label the data set with descriptive variable names.

5. Create a second, independent tidy data set with the average of each variable
for each activity and each subject.

Below I provide the description of specific steps of the code.

Step 1. Merge the training and the test sets to create one data set.
- The merge function changes the order of observations thus at first I combine 
the data for test and train subsets.


```r
test = cbind(subject_test, y_test, x_test)
train = cbind(subject_train, y_train, x_train)
```

- Now test and train datasets are combined, one below the other.


```r
combined = rbind(test, train)
```


- The names of variables are extracted from features file. Two new variables names,
i.e. subjectNumber and activityNumber are added to match the combined file structure.


```r
features = as.character(features[,2])
features = c("subjectNumber", "activityNumber", features)
colnames(combined) = features
```

Step 2. Extract only the measurements on the mean and standard deviation for each
measurement.
- Based on information from features_info.txt we can find among variables such that 
have word "Mean" in their name. These variables are left up to user if they want to 
include them in tidy dataset or not. I chose to exclude them as these variables 
seem not to be the real means/averages. You can add "ignore.case=TRUE" as a 3rd 
argument in grep command below if you want to include those variables (7 variables 
more will be included in final dataset).
- Also there are variables with the weighted mean which have "meanFreq" in their names.
Seems that those should stay in the dataset. If someone wants to remove then first 
argument in grep command should be modified to "mean[^F]|std"


```r
vars_list = grep("mean|std", colnames(combined))
combined2 = combined[ , c(1, 2, vars_list)]
```
combined2 file has 10299 observations and 81 variables.

Step 3. Use descriptive activity names to name the activities in the data set.
- The activityNumber variable gets translated to activityLabel using dictionary from 
activity_labels file.
- activityNumber variable is removed as it is no longer required.
- I also sort the data by subjectNumber and activityLabel.


```r
colnames(activity_labels) = c("activityNumber","activityLabel")
combined3 = merge(combined2, activity_labels, by.x="activityNumber", by.y="activityNumber", all=TRUE)
combined3$activityNumber = NULL
library(plyr)
combined3 = arrange(combined3, subjectNumber, activityLabel)
```

Step 4. Appropriately label the data set with descriptive variable names.
I follow the rules:
- keep the names as short as possible to save the time when working with data
- enhance "Acc" to "Accel" so that it brings to the mind "Acceleration"
- all "mean"/"std" measures are moved to the end of variable's name
- remove "t" from beginning of the variable's names while keeping "f" for
variables transformed with FFT
- get rid of all "-" and brackets from labels as these are not valid variable
names in R
- get rid of duplicated words like "BodyBody"


```r
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
```

Step 5. Create a second, independent tidy data set with the average of each variable 
for each activity and each subject.
- After calculating the averages I need to change the labels of 2 grouping variables 
subjectNumber and activityLabel (they are called Group.1 and Group.2 after calculation
of the mean) while original 2 variables need to be deleted.
- warnings during running aggregate function are related to calculation of mean on 
non-numeric variables like activityLabel.
- I prefer to see first all activities for each subject so I re-order the tidy dataset
a little. And finally save the file to the disc.


```r
tidy = aggregate(combined3, by=list(combined3$subjectNumber, combined3$activityLabel), mean)
tidy$subjectNumber = NULL
tidy$activityLabel = NULL
names(tidy)[1] = "subjectNumber"
names(tidy)[2] = "activityLabel"
tidy = arrange(tidy, subjectNumber, activityLabel)
write.table(tidy, "tidy.txt",row.names=FALSE)
```
