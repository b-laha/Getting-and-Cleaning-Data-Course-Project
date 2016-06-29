
#BL - Getting and Cleaning Data/Project
#One of the most exciting areas in all of data science right now is wearable computing - 
#see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to 
#develop the most advanced algorithms to attract new users. The data linked to from the 
#course website represent data collected from the accelerometers from the Samsung Galaxy S 
#smartphone. A full description is available at the site where the data was obtained:
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Here are the data for the project:
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#You should create one R script called run_analysis.R that does the following.

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of   
#   each variable for each activity and each subject.


library(dplyr)
library(tidyr)

# Step 1
# Merge the training and test sets to create one data set
###############################################################################

#readin files
features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")

x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

#adding col names to both test and train datasets
names(x_test) <- features[,2]
names(x_train) <- features[,2]

#add activity and subject labels for test dataset. 
y_test_label <- merge(y_test, activity_labels, by="V1")
y_test_label <- cbind(subject_test, y_test_label)
names(y_test_label) <- c("subject","activity_code", "activity")
test_labeled <- cbind(y_test_label, x_test)

#add activity and subject labels for training dataset. 
y_train_label <- merge(y_train, activity_labels, by="V1")
y_train_label <- cbind(subject_train, y_train_label)
names(y_train_label) <- c("subject","activity_code", "activity")
train_labeled <- cbind(y_train_label, x_train)

#test how many unique features, this shows there are duplicates!
#length(unique(features[,2]))

#merge test and trainig data set. the resulted dataframe has less features due to duplicates.
combined <- rbind(test_labeled, train_labeled) 




# Step 2
# Extract only the measurements on the mean and standard deviation for each measurement
###############################################################################
# Step 3
# Use descriptive activity names to name the activities in the data set
###############################################################################
# Step 4
# Appropriately label the data set with descriptive variable names
###############################################################################


# get only columns with mean() or std() in their names

mean_and_std_features <- grep("-(mean|std)\\(\\)", features[, 2])
mean_std <- combined[, mean_and_std_features]

#sel_mean <- select(combined, contains("mean()"))
#sel_std <- select(combined, contains("std()"))
#mean_std <- bind_cols(sel_mean, sel_std)
ordered_mean_std <- mean_std[, order(colnames(mean_std))]

#final labeled dataset
final_df <- cbind(combined[,c(1,3)], ordered_mean_std)




# Step 5
# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
###############################################################################

#tidy_set <- final_df %>% group_by(subject, activity) %>% summarise_each(funs(mean))
#write.table(tidy_set,"tidy_set.txt", row.name=FALSE)

averages_data <- ddply(final_df, .(subject, activity), numcolwise(mean))
write.table(averages_data, "tidy_averages_data.txt", row.name=FALSE)


