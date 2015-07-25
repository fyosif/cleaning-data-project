train_data <- read.table("./train/X_train.txt")
test_data <- read.table("./test/X_test.txt")
join_data <- rbind(train_data, test_data)

train_label <- read.table("./train/y_train.txt")
test_label <- read.table("./test/y_test.txt") 
join_label <- rbind(train_label, test_label)

train_subject <- read.table("./train/subject_train.txt")
test_subject <- read.table("./test/subject_test.txt")
join_subject <- rbind(train_subject, test_subject)

features <- read.table("./features.txt")
mean_stds <- grep("mean\\(\\)|std\\(\\)", features[, 2])
join_data <- join_data[, mean_stds]
names(join_data) <- gsub("\\(\\)", "", features[mean_stds, 2]) 
activity <- read.table("./activity_labels.txt")

activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[join_label[, 1], 2]
join_label[, 1] <- activityLabel
names(join_label) <- "activity"
names(join_subject) <- "subject"
merged_data_set <- cbind(join_subject, join_label, join_data)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subject_len <- length(table(join_subject))
activity_len <- dim(activity)[1]
column_len <- dim(merged_data_set)[2]
row_len <- subject_len*activity_len
result <- matrix(NA, nrow=row_len, ncol=column_len) 
result <- as.data.frame(result)
colnames(result) <- colnames(merged_data_set)
row <- 0
for(i in 1:subject_len) {
    for(j in 1:activity_len) {
        row <- row + 1
        result[row, 1] <- sort(unique(join_subject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        result[row, 3:column_len] <- colMeans(merged_data_set[(i == merged_data_set$subject)&(activity[j, 2] == merged_data_set$activity), 3:column_len])
    }
}

write.table(result, "result.txt")
